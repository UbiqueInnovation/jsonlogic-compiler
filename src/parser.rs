// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

use peg::parser;
use super::*;


parser!{
/// Doc comment
pub grammar arithmetic() for str {
    rule var() -> &'input str = $(['a'..='z' | 'A'..='Z']+[ '.' | 'a'..='z' | 'A'..='Z' |'0'..='9' | '_' |'-']*)
    rule number() -> &'input str = $(['0'..='9']+)
    rule string() -> &'input str = $([^'"']*)
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
    rule null_coercion() = _ "??" _


    rule operation() -> Expression = precedence!{
        v:var() null_coercion() f:float() {
            Expression::VarWithDefault(v.to_owned(),Value::Float(f.parse::<f64>().unwrap()))
        }
        v:var() null_coercion() l:number() {
             Expression::VarWithDefault(v.to_owned(),Value::Int(l.parse::<i128>().unwrap()))
        }
        v:var() null_coercion() "\"" s:string()"\"" {
             Expression::VarWithDefault(v.to_owned(),Value::String(s.to_owned()))
        }
        --
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
        "\"" s:string() "\"" {

            Expression::Atomic(Value::String(s.to_owned()))
        }
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