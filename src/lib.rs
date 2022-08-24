use std::collections::HashMap;

use serde_json::json;

pub mod parser;
pub use parser::arithmetic;
// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    VariableAssignment {
        offset: usize,
        name: String,
        expression: Box<Expression>,
    },
    Comment(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Import {
    Path(String),
    Name(String),
    None,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Conditional {
        condition: Box<Expression>,
        inner: Box<Expression>,
        other: Option<Box<Expression>>,
    },
    Not(Box<Expression>),
    Var(String),
    VarWithDefault(String, Box<Expression>),
    Operation(Operation),
    Comparison(Comparison),
    Atomic(Value),
    TimeInterval(Box<Expression>, String),
    Array(Vec<Expression>),
    ArrayOperation(Box<Expression>, String, Box<Expression>),
    ArrayOperationWithArguments(Box<Expression>, String, Box<Expression>, Vec<Expression>),
    Function(String, Vec<Expression>),
    Comment(String),
}

fn get(value: &serde_json::Value, path: &str) -> Option<Expression> {
    if path.is_empty() {
        return Some(Expression::Atomic(Value::from(value)));
    }
    let splits = path.split('.');
    let mut expression = Expression::Atomic(Value::Null);
    for s in splits {
        if let Some(a) = value.get(s) {
            expression = Expression::Atomic(Value::from(a));
        } else {
            return None;
        }
    }
    Some(expression)
}
impl From<&serde_json::Value> for Value {
    fn from(v: &serde_json::Value) -> Self {
        match v {
            serde_json::Value::Null => Self::Null,
            serde_json::Value::Bool(b) => Self::Bool(*b),
            serde_json::Value::Number(n) if n.as_f64().is_some() => {
                Self::Float(n.as_f64().unwrap())
            }
            serde_json::Value::Number(n) if n.as_i64().is_some() => {
                Self::Int(n.as_i64().unwrap() as i128)
            }
            serde_json::Value::Number(n) if n.as_u64().is_some() => {
                Self::Int(n.as_u64().unwrap() as i128)
            }
            serde_json::Value::Number(_) => Self::Null,
            serde_json::Value::String(s) => Self::String(s.to_string()),
            serde_json::Value::Array(a) => {
                let mut inner = Vec::with_capacity(a.len());
                for v in a {
                    inner.push(Self::from(v));
                }
                Self::Array(inner)
            }
            serde_json::Value::Object(v) => {
                let mut inner = HashMap::with_capacity(v.len());
                for (key, value) in v {
                    inner.insert(key.to_string(), Self::from(value));
                }
                Self::Object(inner)
            }
        }
    }
}

impl Expression {
    pub fn as_bool(&self) -> Result<bool, String> {
        if let Expression::Atomic(Value::Bool(b)) = self {
            Ok(*b)
        } else {
            Err(String::from("Not a bool"))
        }
    }
    pub fn eval(&self, data: &serde_json::Value) -> Result<Expression, String> {
        match self {
            Expression::Conditional {
                condition,
                inner,
                other,
            } => {
                let condition = condition.eval(data)?.as_bool()?;
                if condition {
                    inner.eval(data)
                } else if let Some(other) = other {
                    other.eval(data)
                } else {
                    Err(String::from("Non exhaustive if"))
                }
            }
            Expression::Not(expr) => {
                let bool_expr = expr.eval(data)?.as_bool()?;
                Ok(Expression::Atomic(Value::Bool(bool_expr)))
            }
            Expression::Var(key) => {
                if let Some(val) = get(data, key) {
                    Ok(val)
                } else {
                    Ok(Expression::Atomic(Value::Null))
                }
            }
            Expression::VarWithDefault(key, def) => {
                if let Some(v) = get(data, key) {
                    Ok(v)
                } else {
                    def.eval(data)
                }
            }
            Expression::Operation(op) => op.eval(data),
            Expression::Comparison(comp) => comp.eval(data),
            Expression::Atomic(_) => Ok(self.to_owned()),
            Expression::TimeInterval(amount, unit) => {
                let amount = amount.eval(data)?;
                Ok(Expression::TimeInterval(Box::new(amount), unit.to_owned()))
            }
            Expression::Array(arr) => {
                let mut new_array = vec![];
                for ele in arr {
                    let result = ele.eval(data)?;
                    if let Expression::Atomic(atomic_value) = result {
                        new_array.push(atomic_value);
                    } else {
                        new_array.push(Value::Null);
                    }
                }
                Ok(Expression::Atomic(Value::Array(new_array)))
            }
            Expression::ArrayOperation(array, function, block) => {
                let array = array.eval(data)?;
                match function.as_str() {
                    "filter" => match array {
                        Expression::Atomic(Value::Array(arr)) => {
                            let new_arr: Vec<_> = arr
                                .into_iter()
                                .filter(|a| {
                                    block
                                        .eval(&a.to_serde_json())
                                        .and_then(|a| a.as_bool())
                                        .unwrap_or(false)
                                })
                                .collect();
                            Ok(Expression::Atomic(Value::Array(new_arr)))
                        }
                        _ => Err("Array operation can only be applied to an array".into()),
                    },
                    _ => Err("Function not implemented".into()),
                }
            }
            Expression::ArrayOperationWithArguments(..) => todo! {},
            Expression::Function(function_name, arguments) => match function_name.as_str() {
                "in" => {
                    if !arguments.len() == 2 {
                        return Err(format!(
                            "In function needs to arguments. {} were given",
                            arguments.len()
                        ));
                    }
                    let what = arguments[0].eval(data)?;
                    let array = arguments[1].eval(data)?;

                    if let Expression::Atomic(Value::Array(arr)) = array {
                        for exp in &arr {
                            if Expression::Atomic(exp.to_owned()) == what {
                                return Ok(Expression::Atomic(Value::Bool(true)));
                            }
                        }
                    }
                    Ok(Expression::Atomic(Value::Bool(false)))
                }
                _ => todo!("Only in is supported"),
            },
            Expression::Comment(_) => Ok(self.to_owned()),
        }
    }
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Expression::Conditional {
                condition,
                inner,
                other,
            } => {
                let cond = condition.to_json_logic();
                let inner = inner.to_json_logic();
                if let Some(other) = other.as_ref() {
                    let other = other.to_json_logic();
                    json!({

                        "if" : [
                            cond,
                            inner,
                            other
                        ]
                    })
                } else {
                    json!({
                        "if" : [
                            cond,
                            inner
                        ]
                    })
                }
            }
            Expression::Var(v) => {
                json!({ "var": v })
            }
            Expression::VarWithDefault(v, fall_back) => {
                let value = fall_back.to_json_logic();
                json!({"var" : [v, value] })
            }
            Expression::Operation(a) => a.to_json_logic(),
            Expression::Not(i) => {
                let inner = i.to_json_logic();
                json!({ "!": [inner] })
            }
            Expression::Comparison(comp) => comp.to_json_logic(),
            Expression::Atomic(inner) => inner.to_serde_json(),
            Expression::Array(expressions) => {
                let mut json_logic_expressions = vec![];
                for exp in expressions {
                    json_logic_expressions.push(exp.to_json_logic());
                }
                json!(json_logic_expressions)
            }
            Expression::Function(func_name, args) => {
                let mut the_args = vec![];
                for arg in args {
                    the_args.push(arg.to_json_logic());
                }
                json!({ func_name: the_args })
            }
            Expression::ArrayOperation(var, func_name, inner_expr) => {
                let array = var.to_json_logic();
                let inner_expr = inner_expr.to_json_logic();

                json!({
                    func_name : [
                        array,
                        inner_expr
                    ]
                })
            }
            Expression::ArrayOperationWithArguments(var, func_name, inner_expr, args) => {
                let array = var.to_json_logic();
                let inner_expr = inner_expr.to_json_logic();
                let mut operands = vec![array, inner_expr];

                for arg in args {
                    let arg = arg.to_json_logic();
                    operands.push(arg);
                }
                json!({ func_name: operands })
            }
            Expression::TimeInterval(a, b) => {
                let inner = a.to_json_logic();
                json!({
                    "timeSpan" : [
                        inner,
                        b
                    ]
                })
            }
            _ => serde_json::Value::Null,
        }
    }
}
impl std::fmt::Display for Expression {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let exp =
            serde_json::to_string_pretty(&self.to_json_logic()).unwrap_or_else(|_| "".to_string());
        let _ = fmt.write_str(&exp);
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Bool(bool),
    String(String),
    Int(i128),
    Float(f64),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    Null,
}

impl Value {
    pub fn to_serde_json(&self) -> serde_json::Value {
        match self {
            &Self::Bool(b) => serde_json::Value::Bool(b),
            Self::String(s) => serde_json::Value::String(s.to_owned()),
            &Self::Int(i) => serde_json::Value::Number((i as i64).into()),
            &Self::Float(f) => {
                serde_json::Value::Number(serde_json::value::Number::from_f64(f).unwrap())
            }
            Self::Array(array) => {
                let value_array = array.iter().map(|a| a.to_serde_json()).collect::<Vec<_>>();
                serde_json::Value::Array(value_array)
            }
            Self::Object(obj) => {
                let json_dict = obj
                    .iter()
                    .map(|(key, value)| (key.to_string(), value.to_serde_json()))
                    .collect::<serde_json::Map<_, _>>();
                serde_json::Value::Object(json_dict)
            }
            _ => serde_json::Value::Null,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Comparison {
    LessThan(Box<Expression>, Box<Expression>),
    LessThanEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanEqual(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    ExactEqual(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    NotExactEqual(Box<Expression>, Box<Expression>),
    Before(Box<Expression>, Box<Expression>),
    NotBefore(Box<Expression>, Box<Expression>),
    After(Box<Expression>, Box<Expression>),
    NotAfter(Box<Expression>, Box<Expression>),
}

impl std::fmt::Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Comparison::LessThan(_, _) => f.write_str("<"),
            Comparison::LessThanEqual(_, _) => f.write_str("<="),
            Comparison::GreaterThan(_, _) => f.write_str(">"),
            Comparison::GreaterThanEqual(_, _) => f.write_str(">="),
            Comparison::Equal(_, _) => f.write_str("=="),
            Comparison::ExactEqual(_, _) => f.write_str("==="),
            Comparison::NotEqual(_, _) => f.write_str("!="),
            Comparison::NotExactEqual(_, _) => f.write_str("!=="),
            Comparison::Before(_, _) => f.write_str("before"),
            Comparison::NotBefore(_, _) => f.write_str("not-before"),
            Comparison::After(_, _) => f.write_str("after"),
            Comparison::NotAfter(_, _) => f.write_str("not-after"),
        }
    }
}

impl Comparison {
    pub fn eval(&self, data: &serde_json::Value) -> Result<Expression, String> {
        match self {
            Comparison::LessThan(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a < b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a < b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) < *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a < (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a < b))),
                    (Expression::Atomic(Value::Null), Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(true)))
                    }
                    (Expression::Atomic(Value::Null), _) => {
                        Ok(Expression::Atomic(Value::Bool(false)))
                    }
                    (_, Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(false)))
                    }

                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::LessThanEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a <= b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a <= b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) <= *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a <= (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a <= b))),
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::GreaterThan(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a > b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a > b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) > *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a > (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a <= b))),
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::GreaterThanEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a >= b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a >= b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) >= *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a >= (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a >= b))),
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::Equal(a, b) | Comparison::ExactEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a == b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a == b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) == *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a == (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a == b))),
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::NotEqual(a, b) | Comparison::NotExactEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a != b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a != b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) != *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a != (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a != b))),
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::Before(_, _) => todo!(),
            Comparison::NotBefore(_, _) => todo!(),
            Comparison::After(_, _) => todo!(),
            Comparison::NotAfter(_, _) => todo!(),
        }
    }
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Comparison::LessThan(a, b)
            | Comparison::LessThanEqual(a, b)
            | Comparison::GreaterThan(a, b)
            | Comparison::GreaterThanEqual(a, b)
            | Comparison::Equal(a, b)
            | Comparison::ExactEqual(a, b)
            | Comparison::NotEqual(a, b)
            | Comparison::NotExactEqual(a, b)
            | Comparison::After(a, b)
            | Comparison::NotAfter(a, b)
            | Comparison::Before(a, b)
            | Comparison::NotBefore(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                let token = self.to_string();
                json!({
                    token: [
                        a,
                        b
                    ]
                }

                )
            }
        }
    }
}
#[derive(Clone, PartialEq, Debug)]
pub enum Operation {
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    PlusTime(Box<Expression>, Box<Expression>),
    MinusTime(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Modulo(Box<Expression>, Box<Expression>),
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Plus(_, _) => f.write_str("+"),
            Operation::Minus(_, _) => f.write_str("-"),
            Operation::PlusTime(_, _) => f.write_str("plusTime"),
            Operation::MinusTime(_, _) => f.write_str("minusTime"),
            Operation::And(_, _) => f.write_str("and"),
            Operation::Or(_, _) => f.write_str("or"),
            Operation::Modulo(_, _) => f.write_str("%"),
        }
    }
}

impl Operation {
    pub fn eval(&self, data: &serde_json::Value) -> Result<Expression, String> {
        match self {
            Operation::Plus(_, _) => todo!(),
            Operation::Minus(_, _) => todo!(),
            Operation::PlusTime(_, _) => todo!(),
            Operation::MinusTime(_, _) => todo!(),
            Operation::And(_, _) => todo!(),
            Operation::Or(_, _) => todo!(),
            Operation::Modulo(left, right) => {
                let left = left.eval(data)?;
                let right = right.eval(data)?;
                match (left, right) {
                    (
                        Expression::Atomic(Value::Int(left)),
                        Expression::Atomic(Value::Int(right)),
                    ) => Ok(Expression::Atomic(Value::Int(left % right))),
                    (
                        Expression::Atomic(Value::Float(left)),
                        Expression::Atomic(Value::Int(right)),
                    ) => Ok(Expression::Atomic(Value::Int(left as i128 % right))),
                    (
                        Expression::Atomic(Value::Int(left)),
                        Expression::Atomic(Value::Float(right)),
                    ) => Ok(Expression::Atomic(Value::Int(left as i128 % right as i128))),
                    _ => todo! {},
                }
            }
        }
    }
    fn extract_inner_if_same<'other>(
        &'other self,
        other: &'other Expression,
        storage: &mut Vec<serde_json::Value>,
    ) {
        match (self, other) {
            (Operation::Plus(_, _), Expression::Operation(Operation::Plus(a, b)))
            | (Operation::And(_, _), Expression::Operation(Operation::And(a, b)))
            | (Operation::Or(_, _), Expression::Operation(Operation::Or(a, b))) => {
                self.extract_inner_if_same(a, storage);
                self.extract_inner_if_same(b, storage);
            }
            _ => {
                storage.push(other.to_json_logic());
            }
        }
    }
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Operation::Plus(a, b)
            | Operation::Minus(a, b)
            | Operation::And(a, b)
            | Operation::Or(a, b)
            | Operation::Modulo(a, b) => {
                let mut args = vec![];
                self.extract_inner_if_same(a, &mut args);
                self.extract_inner_if_same(b, &mut args);

                let token = self.to_string();
                json!({ token: args })
            }
            Self::PlusTime(a, b) | Self::MinusTime(a, b) => match b.as_ref() {
                Expression::TimeInterval(amount, unit) => {
                    let a = a.to_json_logic();
                    let amount = amount.to_json_logic();
                    let token = self.to_string();
                    json!({
                        token : [
                            a,
                            amount,
                            unit
                        ]
                    })
                }
                _ => json!({}),
            },
        }
    }
}

#[cfg(test)]
mod tests {

    use serde_json::json;

    use crate::Expression;

    #[test]
    fn test_eval() {
        let mut duration_eval = 0;
        let mut duration_json = 0;
        let logic: Expression = super::arithmetic::expression(
            r#"if (999 >= b) {[1,2,3,4]::filter { this % 2 == 0 }} else { false }"#,
        )
        .unwrap();
        let json_logic = logic.to_json_logic();
        for i in 0..=10000 {
            let start_eval = std::time::Instant::now();
            let result_eval = logic.eval(&json!({ "b": i })).unwrap();
            let end_eval = std::time::Instant::now();

            let start_json = std::time::Instant::now();
            let result = jsonlogic::apply(&json_logic, &json!({ "b": i })).unwrap();
            let end_json = std::time::Instant::now();
            duration_eval += (end_eval - start_eval).as_nanos();
            duration_json += (end_json - start_json).as_nanos();
            if let Expression::Atomic(val) = result_eval {
                assert_eq!(val.to_serde_json(), result);
            } else {
                panic!("Result eval did not evaluate to an atomic value");
            }
        }
        println!();
        println!("Eval: {}", duration_eval / 10000);
        println!("JsonLogic: {}", duration_json / 10000);
    }
    #[test]
    fn test_array_op_eval() {
        let logic: Expression =
            super::arithmetic::expression("[1,2,3,4]::filter { this % 2 == 0}").unwrap();
        let result = logic.eval(&json! {{}}).unwrap();
        if let Expression::Atomic(crate::Value::Array(arr)) = result {
            assert_eq!(arr.len(), 2);
            assert_eq!(arr[0], crate::Value::Int(2));
            assert_eq!(arr[1], crate::Value::Int(4));
        } else {
            panic!("should be array")
        }
    }
}
