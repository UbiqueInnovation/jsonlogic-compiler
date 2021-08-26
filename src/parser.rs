// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

use super::*;
use peg::parser;

parser! {
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
    rule modulo() = _ ("%" / "mod") _
    rule and() = _ ("&&" / "and") _
    rule or() = _ ("||" / "or") _
    rule lt() = _ ("<" / "is before") _
    rule lte() = _ ("<=" / "is not after") _
    rule gt() = _ (">" / "is after") _
    rule gte() = _ (">=" / "is not before") _
    rule eq() = _ "==" _
    rule eeq() = _ "===" _
    rule ne() = _ "!=" _
    rule ene() = _ "!==" _
    rule not() = _ ("!" / "not") _
    rule null_coercion() = _ "??" _
    rule now() = _ "now" _ "(" _ ")" _

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
           x:(@) modulo() y:@ {
            Expression::Operation(Operation::Modulo(Box::new(x), Box::new(y)))
        }
        --
        _ f:function() _ {f}
        --
         _ now() _ {Expression::Var("external.validationClock".to_owned())}
         _ t:this() _ {t}
        --
        _ v:var() l:time_interval() _ {
            let l = l.strip_prefix("#").unwrap();
            let l = if let Some(l) = l.strip_suffix("s") { l } else { l };
            Expression::TimeInterval(Box::new(Expression::Var(v.to_owned())), l.to_owned())
        }
        _ v:number() l:time_interval() {
            let l = l.strip_prefix("#").unwrap();
            let l = if let Some(l) = l.strip_suffix("s") { l } else { l };
            Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(v.parse::<i128>().unwrap()))), l.to_owned())
        }
        _ l:bool() _ {Expression::Atomic(Value::Bool(l.parse::<bool>().unwrap()))}
        --
        _ v:var() _ {Expression::Var(v.to_owned())}
        --
        _ f:float() _ {Expression::Atomic(Value::Float(f.parse::<f64>().unwrap()))}
        _ l:number() _ {Expression::Atomic(Value::Int(l.parse::<i128>().unwrap()))}
        _ "\"" s:string() "\"" _ {

            Expression::Atomic(Value::String(s.to_owned()))
        }
        --
         _ "[" _ e:expression()** _ "," _ "]" _ { Expression::Array(e)}
        --
         _ "(" _ e:expression() _ ")" _ { e }



    }
    rule conditional() -> Expression = _  "if" _ "(" _ e:expression() _ ")" _ "{" _ i:expression() _ "}" _ {
        Expression::Conditional{condition: Box::new(e), inner: Box::new(i), other: None}
    }
    rule conditionalWithElse() -> Expression = _  "if" _ "(" _ e:expression() _ ")" _ "{" _ i:expression() _ "}" _ "else" _ "{" _ o:expression() _ "}" _ {
        Expression::Conditional{condition: Box::new(e), inner: Box::new(i), other: Some(Box::new(o))}
    }
    rule unary() -> Expression = _ now() _ {
        Expression::Var("external.validationClock".to_owned())
    }

    rule array() -> Expression =  _ "[" _ e:expression()** "," _ "]" _ { Expression::Array(e)}

    rule varUnary() -> Expression = _ v:var() _ {Expression::Var(v.to_owned())}

    rule arrayOperation() -> Expression = _ expr:(array() / varUnary()) _ "." _ function:var() _ "{" _ inner:expression() _"}" {
        Expression::ArrayOperation(Box::new(expr), function.to_owned(), Box::new(inner))
    }
      rule arrayOperationWithArguments() -> Expression = _ expr:(array() / varUnary()) _ "." _ function:var() _ "(" args:expression()** "," _ ")" _ "{" _ inner:expression() _"}" {
        Expression::ArrayOperationWithArguments(Box::new(expr), function.to_owned(), Box::new(inner), args)
    }
    rule this() -> Expression = _ "this" _ {Expression::Var("".to_owned())}
    rule function() -> Expression = _ func_name:var() _ "(" _ args:expression()++ "," _ ")" _ {Expression::Function(func_name.to_owned(), args)}

    pub rule expression() -> Expression = conditionalWithElse() / conditional() / operation() / arrayOperationWithArguments() / arrayOperation() / array()  / unary() / function()
}}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        let expression = super::arithmetic::expression("now() + 3#years").unwrap();
        println!("{}", expression.to_json_logic());
    }
    #[test]
    fn array_test() {
        let expression =
            super::arithmetic::expression("[now(), a, 3, now() + 3#years, [now() + 6#days]]")
                .unwrap();
        println!("{}", expression.to_json_logic());
    }

    #[test]
    fn array_expr_test() {
        let array_expression =
            super::arithmetic::expression("[1,2,3,4,5].filter { this % 2 == 0 }").unwrap();
        println!("{}", array_expression.to_json_logic());
    }
}
