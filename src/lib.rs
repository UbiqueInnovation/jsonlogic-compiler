use serde_json::json;

pub mod parser;
pub use parser::arithmetic;
// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Conditional{condition: Box<Expression>, inner: Box<Expression>, other: Option<Box<Expression>>},
    Not(Box<Expression>),
    Var(String),
    Operation(Operation),
    Comparison(Comparison),
    Atomic(Value),
    TimeInterval(Box<Expression>, String)
}

impl Expression {
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Expression::Conditional{ condition, inner, other } => {
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
                    })
                } else {
                    return json!({
                        "if" : [
                            cond,
                            inner
                        ]
                    })
                }
            }
            Expression::Var(v) => {
                json!({
                    "var" : [v]
                })
            }
            Expression::Operation(a) => {
                a.to_json_logic()
            }
            Expression::Not(i) => {
                let inner = i.to_json_logic();
                json!({
                    "!" : [
                        inner
                    ]
                })
            }
            Expression::Comparison(comp) => {
                comp.to_json_logic()
            }
            Expression::Atomic(inner) => {
                inner.to_serde_json()
            }
            _ => {
                json!({})
            }
        }
    }
}
impl std::fmt::Display for Expression {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> 
    {
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
    Float(f64)
}

impl Value {
    pub fn to_serde_json(&self) -> serde_json::Value {
        match self {
            &Self::Bool(b) => serde_json::Value::Bool(b),
            Self::String(s) => serde_json::Value::String(s.to_owned()),
            &Self::Int(i) => serde_json::Value::Number((i as i64).into()),
            &Self::Float(f) => serde_json::Value::Number(serde_json::value::Number::from_f64(f).unwrap()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Comparison {
    LessThan(Box<Expression>,Box<Expression>),
    LessThanEqual(Box<Expression>,Box<Expression>),
    GreaterThan(Box<Expression>,Box<Expression>),
    GreaterThanEqual(Box<Expression>,Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    ExactEqual(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    NotExactEqual(Box<Expression>, Box<Expression>)
}

impl Comparison {
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Self::LessThan(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "<" : [
                        a,
                        b
                    ]
                })
            },
             Self::LessThanEqual(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "<=" : [
                        a,
                        b
                    ]
                })
            },
             Self::GreaterThan(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    ">" : [
                        a,
                        b
                    ]
                })
            }
            Self::GreaterThanEqual(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    ">=" : [
                        a,
                        b
                    ]
                })
            }
            Self::Equal(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "==" : [
                        a,
                        b
                    ]
                })
            }
            Self::ExactEqual(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "===" : [
                        a,
                        b
                    ]
                })
            },
            Self::NotEqual(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "!=" : [
                        a,
                        b
                    ]
                })
            }
            Self::NotExactEqual(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "!==" : [
                        a,
                        b
                    ]
                })
            }
        }
    }
}
#[derive(Clone, PartialEq,  Debug)]
pub enum Operation {
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    PlusTime(Box<Expression>, Box<Expression>),
    MinusTime(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),  
}

impl Operation {
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Self::Plus(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "+" : [
                        a,
                        b
                    ]
                })
            }
            Self::Minus(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "-" : [
                        a,
                        b
                    ]
                })
            }
            Self::And(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "and" : [
                        a,
                        b
                    ]
                })
            }
            Self::Or(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                json!({
                    "or" : [
                        a,
                        b
                    ]
                })
            }
            Self::PlusTime(a,b) => {
                match b.as_ref() {
                    Expression::TimeInterval(amount, unit) => {
                        let a = a.to_json_logic();
                        let amount = amount.to_json_logic();
                        json!({
                            "plusTime" : [
                                a,
                                amount,
                                unit
                            ]
                        })
                    },
                    _ => json!({})
                }
            }
            Self::MinusTime(a,b) => {
                match b.as_ref() {
                    Expression::TimeInterval(amount, unit) => {
                        let a = a.to_json_logic();
                        let amount = amount.to_json_logic();
                        json!({
                            "minusTime" : [
                                a,
                                amount,
                                unit
                            ]
                        })
                    },
                    _ => json!({})
                }
            }
        }
    } 
}