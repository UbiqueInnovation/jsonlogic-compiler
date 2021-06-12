use peg::parser;
use serde_json::json;

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

parser!{
/// Doc comment
pub grammar arithmetic() for str {
    rule var() -> &'input str = $(['a'..='z' | 'A'..='Z']+[ '.' | 'a'..='z' | 'A'..='Z' |'0'..='9' | '_' |'-']*)
    rule number() -> &'input str = $(['0'..='9']+)
    rule float() -> &'input str = $(['0'..='9']+"."['0'..='9']+)
    rule time_interval() -> &'input str = $("#" ("years" / "year" / "months" / "month" / "days" / "day" / "hours" / "hour" /"minutes"/ "minute" / "seconds"/ "second"))
    rule bool() -> &'input str = $("true" / "false")
    rule _ = [' ' | '\n']*
    rule plus() = _ ("+"/ "plus")  _
    rule minus() = _ ("-" /"minus") _
    rule and() = _ ("&&" / "and") _
    rule or() = _ ("||" / "or") _
    rule lt() = _ "<" _
    rule lte() = _ "<=" _
    rule gt() = _ ">" _
    rule gte() = _ ">=" _
    rule eq() = _ "==" _
    rule eeq() = _ "===" _
    rule ne() = _ "!=" _
    rule ene() = _ "!==" _
    rule not() = _ ("!" / "not") _


    rule operation() -> Expression = precedence!{
        x:(@) or() y:@ { Expression::Operation(Operation::Or(Box::new(x), Box::new(y))) }
        --
        x:(@) and() y:@ { Expression::Operation(Operation::And(Box::new(x), Box::new(y))) }
        --
        not() y:(@) {
            Expression::Not(Box::new(y))
        }
        x:(@) plus() y:@ { 
            match y {
                Expression::TimeInterval(..) => {
                    Expression::Operation(Operation::PlusTime(Box::new(x), Box::new(y))) 
                }
                _ => {
                    Expression::Operation(Operation::Plus(Box::new(x), Box::new(y))) 
                }
            }
        }
        x:(@) minus() y:@ { 
           match y {
                Expression::TimeInterval(..) => {
                    Expression::Operation(Operation::MinusTime(Box::new(x), Box::new(y))) 
                }
                _ => {
                    Expression::Operation(Operation::Minus(Box::new(x), Box::new(y))) 
                }
            }
        }
        
        --
        x:(@) lt() y:@ { Expression::Comparison(Comparison::LessThan(Box::new(x), Box::new(y))) }
        x:(@) lte() y:@ { Expression::Comparison(Comparison::LessThanEqual(Box::new(x), Box::new(y))) }
        x:(@) gt() y:@ { Expression::Comparison(Comparison::GreaterThan(Box::new(x), Box::new(y))) }
        x:(@) gte() y:@ { Expression::Comparison(Comparison::GreaterThanEqual(Box::new(x), Box::new(y))) }
        x:(@) eq() y:@ { Expression::Comparison(Comparison::Equal(Box::new(x), Box::new(y))) }
        x:(@) eeq() y:@ { Expression::Comparison(Comparison::ExactEqual(Box::new(x), Box::new(y))) }
        x:(@) ne() y:@ { Expression::Comparison(Comparison::NotEqual(Box::new(x), Box::new(y))) }
        x:(@) ene() y:@ { Expression::Comparison(Comparison::NotExactEqual(Box::new(x), Box::new(y))) }
        --
        v:var() l:time_interval() {
            let l = l.strip_prefix("#").unwrap();
            let l = if let Some(l) = l.strip_suffix("s") { l } else { l };
            Expression::TimeInterval(Box::new(Expression::Var(v.to_owned())), l.to_owned())
        }
        v:number() l:time_interval() {
            let l = l.strip_prefix("#").unwrap();
            let l = if let Some(l) = l.strip_suffix("s") { l } else { l };
            Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(v.parse::<i128>().unwrap()))), l.to_owned())
        }
        l:bool() {Expression::Atomic(Value::Bool(l.parse::<bool>().unwrap()))}
        --
        v:var() {Expression::Var(v.to_owned())}
        --
        f:float() {Expression::Atomic(Value::Float(f.parse::<f64>().unwrap()))}
        l:number() {Expression::Atomic(Value::Int(l.parse::<i128>().unwrap()))}
        --
         _ "(" _ e:expression() _ ")" _ { e }
        
    }
    rule conditional() -> Expression = _  "if" _ "(" _ e:expression() _ ")" _ "{" _ i:expression() _ "}" _ {
        Expression::Conditional{condition: Box::new(e), inner: Box::new(i), other: None}
    }
    rule conditionalWithElse() -> Expression = _  "if" _ "(" _ e:expression() _ ")" _ "{" _ i:expression() _ "}" _ "else" _ "{" _ o:expression() _ "}" _ {
        Expression::Conditional{condition: Box::new(e), inner: Box::new(i), other: Some(Box::new(o))}
    }

    pub rule expression() -> Expression = conditionalWithElse() / conditional() / operation()
}}