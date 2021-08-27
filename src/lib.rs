use serde_json::json;

pub mod parser;
pub use parser::arithmetic;
// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Conditional {
        condition: Box<Expression>,
        inner: Box<Expression>,
        other: Option<Box<Expression>>,
    },
    Not(Box<Expression>),
    Var(String),
    VarWithDefault(String, Value),
    Operation(Operation),
    Comparison(Comparison),
    Atomic(Value),
    TimeInterval(Box<Expression>, String),
    Array(Vec<Expression>),
    ArrayOperation(Box<Expression>, String, Box<Expression>),
    ArrayOperationWithArguments(Box<Expression>, String, Box<Expression>, Vec<Expression>),
    Function(String, Vec<Expression>)
}

impl Expression {
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
                    return json!({

                        "if" : [
                            cond,
                            inner,
                            other
                        ]
                    });
                } else {
                    return json!({
                        "if" : [
                            cond,
                            inner
                        ]
                    });
                }
            }
            Expression::Var(v) => {
                json!({ "var": v })
            }
            Expression::VarWithDefault(v, fall_back) => {
                let value = fall_back.to_serde_json();
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
                json!({
                    func_name: the_args
                })
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
                let mut operands = vec![];
                operands.push(array);
                operands.push(inner_expr);
                for arg in args {
                    let arg = arg.to_json_logic();
                    operands.push(arg);
                }
                json!({
                    func_name : operands
                })
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
        }
    }
}
impl std::fmt::Display for Expression {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let exp = serde_json::to_string_pretty(&self.to_json_logic()).unwrap_or("".to_string());
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
            | Comparison::After(a,b)
            | Comparison::NotAfter(a,b)
            | Comparison::Before(a,b)
            | Comparison::NotBefore(a,b) => {
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
    fn extract_inner_if_same<'other>(
        &'other self,
        other: &'other Box<Expression>,
        storage: &mut Vec<serde_json::Value>,
    ) {
        match (self, other.as_ref()) {
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
