// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT
#![allow(clippy::redundant_closure_call)]

use super::*;
use peg::parser;

parser! {
/// Doc comment
pub grammar arithmetic() for str {
    rule var() -> &'input str = quiet!{$(!keyword() ['a'..='z' | 'A'..='Z']+[ '.' | 'a'..='z' | 'A'..='Z' |'0'..='9' | '_' |'-']*)}/expected!("Variable")
    rule number() -> &'input str = quiet!{$("-"?['0'..='9']+)} / expected!("Number")
    rule string() -> &'input str = quiet!{$([^'"']*)} / expected!("String")
    rule float() -> &'input str = quiet!{$(['0'..='9']+"."['0'..='9']+)} / expected!("Float")
    rule date() -> &'input str = $(['0'..='9']*<4>"-"['0'..='9']*<2>"-"['0'..='9']*<2>)
    rule time() -> &'input str = $(['0'..='9']*<2>":"['0'..='9']*<2>(":"['0'..='9']*<2>("."['0'..='9']+)?)?)
    rule offset() -> &'input str = $((("+"/"-")['0'..='9']*<2>(":"['0'..='9']*<2>)?)/"Z")
    rule dateTime() -> &'input str = $(date()("T" time()(offset())?)?)
    rule time_interval() -> &'input str = quiet!{$(_ "#" _ ("years" / "year" / "months" / "month" / "days" / "day" / "hours" / "hour"))} / expected!("Time interval (year,month,hour).")
    rule bool() -> &'input str = quiet!{$("true" / "false")} /expected!("Boolean")
    rule _ = quiet!{[' ' | '\n']*}
    rule plus() = _ ("+"/ "plus")  _
    rule minus() = _ ("-" /"minus") _
    rule modulo() = _ ("%" / "mod") _
    rule and() = _ ("&&" / "and") _
    rule or() = _ ("||" / "or") _
    rule lt() = _ ("<" ) _
    rule before() = _ "is before" _
    rule notBefore() = _ ("is not before" / "not before") _
    rule after() = _ "is after" _
    rule notAfter() = _ ("is not after" / "not after") _
    rule lte() = _ ("<=" ) _
    rule gt() = _ (">") _
    rule gte() = _ (">=") _
    rule eq() = _ "==" _
    rule eeq() = _ "===" _
    rule ne() = _ "!=" _
    rule ene() = _ "!==" _
    rule not() = _ ("!" / "not") _
    rule null_coercion() = _ "??" _
    rule now() = _ quiet!{"now" _ "(" _ ")"} _
    rule min() = _ "min" _
    rule max() = _ "max" _
    rule dateString() = _ "\"" d:dateTime() "\""
    rule comment() -> Statement = _ "/*" _ comment:($((!("*/")['\0'..='\x7f'])*)) _ "*/" _ {
        Statement::Comment(comment.to_string())
    }

    rule timeOperation(stmts: &[Statement]) -> Expression = precedence!{
          x:(@) plus() y:@ {
            Expression::Operation(Operation::PlusTime(Box::new(x), Box::new(y)))

        }
        x:(@) minus() y:@ {
             Expression::Operation(Operation::MinusTime(Box::new(x), Box::new(y)))
        }
        --
        x:(@) before() y:@ { Expression::Comparison(Comparison::Before(Box::new(x), Box::new(y))) }
        x:(@) notBefore() y:@ { Expression::Comparison(Comparison::NotBefore(Box::new(x), Box::new(y))) }
        x:(@) after() y:@ { Expression::Comparison(Comparison::After(Box::new(x), Box::new(y))) }
        x:(@) notAfter() y:@ { Expression::Comparison(Comparison::NotAfter(Box::new(x), Box::new(y))) }
        --
        _ now() _ {Expression::Var("external.validationClock".to_owned())}
        _ now() _ "as DateTime" _ {
            let unit = Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(0))), "day".to_string());
             Expression::Operation(Operation::PlusTime(Box::new(Expression::Var("external.validationClock".to_owned())), Box::new(unit)))
        }
        --
         x:(@) or() y:@ {
            Expression::Not(
                Box::new(
                    Expression::Operation(
                        Operation::And(Box::new(Expression::Not(Box::new(x))), Box::new(Expression::Not(Box::new(y))))
                    )
                )
            )
        }
        --
        x:(@) and() y:@ { Expression::Operation(Operation::And(Box::new(x), Box::new(y))) }
        --
        _ v:var() l:time_interval() _ {
            let l = l.trim().strip_prefix('#').unwrap();
            let l = if let Some(l) = l.strip_suffix('s') { l } else { l };
            let ex =  replace_variable(stmts, v, Expression::Var(v.to_owned()));
            Expression::TimeInterval(Box::new(ex), l.trim().to_owned())
        }
        _ v:number() l:time_interval() {
            let l = l.trim().strip_prefix('#').unwrap();
            let l = if let Some(l) = l.strip_suffix('s') { l } else { l };
            Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(v.parse::<i128>().unwrap()))), l.trim().to_owned())
        }
        --
         _ "\"" d:dateTime() "\"" _ "as String" _ {
             Expression::Atomic(Value::String(d.to_string()))
         }
        --
        _ "\"" d:dateTime() "\"" {
            let date = Expression::Atomic(Value::String(d.to_owned()));
            let unit = Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(0))), "day".to_string());
            Expression::Operation(Operation::PlusTime(Box::new(date), Box::new(unit)))
        }
        --
        _ v:var() _ "as DateTime" _ {
            replace_variable(stmts, v, Expression::Var(v.to_owned()))
        }
        --
        _ "(" _ e:expression_with_satements(stmts) _ ")" _ { e }
    }

    rule booleanCast(stmts: &[Statement]) -> Expression = _ v:var() _ "as Boolean" _ {
        let ex =  replace_variable(stmts, v, Expression::Var(v.to_owned()));
        Expression::Not(Box::new(Expression::Not(Box::new(ex))))
    }

    rule operation(stmts: &[Statement]) -> Expression = precedence!{
        v:var() null_coercion() e:expression() {
            Expression::VarWithDefault(v.to_owned(),Box::new(e))
        }
        --
        x:(@) or() y:@ {
            Expression::Not(
                Box::new(
                    Expression::Operation(
                        Operation::And(Box::new(Expression::Not(Box::new(x))), Box::new(Expression::Not(Box::new(y))))
                    )
                )
            )
        }
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
           _ v:varUnary(stmts) _ "in" _ a:(array(stmts) / varUnary(stmts)) {
            Expression::Function("in".to_string(),vec![v,a])
        }
        --
        _ f:function(stmts) _ {f}
        _ a:arrayOperation(stmts) _ {a}
        _ a:arrayOperationWithArguments(stmts) _ {a}

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
         _ t:this() _ {t}
        _ l:bool() _ {Expression::Atomic(Value::Bool(l.parse::<bool>().unwrap()))}
        --
        _ v:var() _ {
            replace_variable(stmts, v,  Expression::Var(v.to_owned()))
        }
        --
        _ f:float() _ {Expression::Atomic(Value::Float(f.parse::<f64>().unwrap()))}
        _ l:number() _ {Expression::Atomic(Value::Int(l.parse::<i128>().unwrap()))}
        _ "\"" s:string() "\"" _ {

            Expression::Atomic(Value::String(s.to_owned()))
        }
        --
         _ "[" _ e:expression_with_satements(stmts)** _ "," _ "]" _ { Expression::Array(e)}
        --
         _ "(" _ e:expression_with_satements(stmts) _ ")" _ { e }
    }
    rule switch_if(stmts: &[Statement]) -> Expression = _ _ ":" _ "if" second:expression_with_satements(stmts) {
        second
    }

    rule switch_block(stmts: &[Statement]) -> (Expression,Expression, Option<Expression>) = _ label:expression_with_satements(stmts) second:switch_if(stmts)? _"=>" _ "{" _ e:expression_with_satements(stmts) _"}" _ {
       (label, e, second)
    }
    rule default_switch_block(stmts: &[Statement]) -> Expression = _ "_" _ "=>" _ "{" _ e:expression_with_satements(stmts)  _ "}" _ {
        e
    }
    rule switch(stmts: &[Statement]) -> Expression = _ quiet!{"switch"} _ "(" _ e:expression_with_satements(stmts) _ ")" _ "{" _ switch_statements:switch_block(stmts)++ _ default_block:(quiet!{default_switch_block(stmts)}/ expected!("Exhaustive Switch")) _ "}" _ {
        let mut switch_statements = switch_statements;
        let expressions = switch_statements.pop().unwrap();

        let comparison = if matches!(expressions.0, Expression::Array(..) | Expression::ArrayOperation( ..) | Expression::ArrayOperationWithArguments(..)) {
            Expression::Function("in".into(), vec![e.clone(), expressions.0])
        } else {
            Expression::Comparison(Comparison::ExactEqual(Box::new(e.clone()), Box::new(expressions.0)))
       };
       let comparison = if let Some(expression) = expressions.2 {
            Expression::Operation(Operation::And(Box::new(comparison), Box::new(expression)))
       } else {
           comparison
       };
       let last = Expression::Conditional{condition: Box::new(comparison), inner: Box::new(expressions.1), other: Some(Box::new(default_block))};
        let mut final_element = last;
        while let Some((cond, inner, second)) = switch_statements.pop() {
             let comparison = if matches!(cond, Expression::Array(..) | Expression::ArrayOperation( ..) | Expression::ArrayOperationWithArguments(..)) {
                Box::new(Expression::Function("in".into(), vec![e.clone(), cond]))
            } else {
                let comparison = Comparison::ExactEqual(Box::new(e.clone()), Box::new(cond));
                Box::new(Expression::Comparison(comparison))
            };
            let condition = if let Some(second) = second {
                Box::new(Expression::Operation(Operation::And(comparison, Box::new(second))))
            } else {
                comparison
            };
            let next = Expression::Conditional{
                condition,
                inner: Box::new(inner),
                other: Some(Box::new(final_element))
            };
            final_element = next;
        }
        final_element
    }
    rule conditional(stmts: &[Statement]) -> Expression = _  quiet!{"if"} _ "(" _ e:expression_with_satements(stmts) _ ")" _ "{" _ i:expression_with_satements(stmts) _ "}" _ {
        Expression::Conditional{condition: Box::new(e), inner: Box::new(i), other: None}
    }
    rule conditionalWithElse(stmts: &[Statement]) -> Expression = _  quiet!{"if"} _ "(" _ e:expression_with_satements(stmts) _ ")" _ "{" _ i:expression_with_satements(stmts) _ "}" _ "else" _ "{" _ o:expression_with_satements(stmts) _ "}" _ {
        Expression::Conditional{condition: Box::new(e), inner: Box::new(i), other: Some(Box::new(o))}
    }
    rule unary(stmts: &[Statement]) -> Expression = _ now() _ {
        Expression::Var("external.validationClock".to_owned())
    }

    rule keyword() = "if"/"switch"/"else"/"this"/ "true" / "false"

    rule array(stmts: &[Statement]) -> Expression =  _ "[" _ e:expression_with_satements(stmts)** "," _ "]" _ { Expression::Array(e)}

    rule varUnary(stmts: &[Statement]) -> Expression = _ v:var() _ {
        replace_variable(stmts, v, Expression::Var(v.to_owned()))
    }

    rule arrayOperation(stmts: &[Statement]) -> Expression = _ expr:(array(stmts) / varUnary(stmts)) _ "::" _ function:var() _ "{" _ inner:expression_with_satements(stmts) _"}" {
        Expression::ArrayOperation(Box::new(expr), function.to_owned(), Box::new(inner))
    }
      rule arrayOperationWithArguments(stmts: &[Statement]) -> Expression = _ expr:(array(stmts) / varUnary(stmts)) _ "::" _ function:var() _ "(" args:expression_with_satements(stmts)** "," _ ")" _ "{" _ inner:expression_with_satements(stmts) _"}" {
        Expression::ArrayOperationWithArguments(Box::new(expr), function.to_owned(), Box::new(inner), args)
    }
    rule booleanExpression(stmts: &[Statement]) -> Expression = b:bool() {
        Expression::Atomic(Value::Bool(b.parse().unwrap()))
    }
    rule this() -> Expression = _ quiet!{"this"} _ {Expression::Var("".to_owned())}
    rule function(stmts: &[Statement]) -> Expression = _ !(keyword()) func_name:var() _ "(" _ args:expression_with_satements(stmts)++ "," _ ")" _ {
        match func_name {
            "min" => {

                match args.len() {
                    0 => {
                         Expression::Function(func_name.to_owned(), args)
                    },
                    1 => {
                        args[0].clone()
                    },
                    2 => {
                        if matches!(args[0], Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..)))
                            | matches!(args[1], Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..))) {
                            Expression::Conditional {
                                condition: Box::new( Expression::Comparison(Comparison::Before(Box::new(args[0].clone()), Box::new(args[1].clone())))),
                                inner: Box::new(args[0].clone()),
                                other: Some(Box::new(args[1].clone()))
                            }
                        } else {
                            Expression::Conditional{
                                condition: Box::new( Expression::Comparison(Comparison::LessThan(Box::new(args[0].clone()), Box::new(args[1].clone())))),
                                inner: Box::new(args[0].clone()),
                                other: Some(Box::new(args[1].clone()))
                            }
                        }
                    },
                    _ => {
                        let (a,b) = (args[0].clone(), args[1].clone());
                          if matches!(a, Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..)))
                            | matches!(b, Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..))) {
                                args.iter().fold(
                                Expression::Conditional {
                                    condition: Box::new(Expression::Comparison(Comparison::Before(Box::new(a.clone()), Box::new(b.clone())))),
                                    inner: Box::new(a),
                                    other: Some(Box::new(b))
                                }, |prev, next| {
                                    let (a,b) = (prev, next.clone());
                                    Expression::Conditional {
                                        condition: Box::new(Expression::Comparison(Comparison::Before(Box::new(a.clone()), Box::new(b.clone())))),
                                        inner: Box::new(a),
                                        other: Some(Box::new(b))
                                    }
                                }
                            )
                        } else {
                            args.iter().fold(
                                Expression::Conditional {
                                    condition: Box::new(Expression::Comparison(Comparison::LessThan(Box::new(a.clone()), Box::new(b.clone())))),
                                    inner: Box::new(a),
                                    other: Some(Box::new(b))
                                }, |prev, next| {
                                    let (a,b) = (prev, next.clone());
                                    Expression::Conditional {
                                        condition: Box::new(Expression::Comparison(Comparison::LessThan(Box::new(a.clone()), Box::new(b.clone())))),
                                        inner: Box::new(a),
                                        other: Some(Box::new(b))
                                    }
                                }
                            )
                        }

                    }
                }
            },
            "max" => {
                match args.len() {
                    0 => {
                         Expression::Function(func_name.to_owned(), args)
                    },
                    1 => {
                        args[0].clone()
                    },
                    2 => {
                        Expression::Conditional{condition: Box::new( Expression::Comparison(Comparison::GreaterThan(Box::new(args[0].clone()), Box::new(args[1].clone())))),
                        inner: Box::new(args[0].clone()),
                        other: Some(Box::new(args[1].clone()))
                    }
                    },
                    _ => {
                        let (a,b) = (args[0].clone(), args[1].clone());
                        args.iter().fold(
                            Expression::Conditional {
                                condition: Box::new(Expression::Comparison(Comparison::GreaterThan(Box::new(a.clone()), Box::new(b.clone())))),
                                inner: Box::new(a),
                                other: Some(Box::new(b))
                            }, |prev, next| {
                                let (a,b) = (prev, next.clone());
                                Expression::Conditional {
                                    condition: Box::new(Expression::Comparison(Comparison::GreaterThan(Box::new(a.clone()), Box::new(b.clone())))),
                                    inner: Box::new(a),
                                    other: Some(Box::new(b))
                                }
                            }
                        )
                    }
                }
            },
            _ => {
                Expression::Function(func_name.to_owned(), args)
            }
        }

    }
    rule variable_assignment(stmts: &[Statement]) -> Statement = _ "let" _ v:var() _ "=" _ e:expression_with_satements(stmts) _ ";" _ {
        Statement::VariableAssignment{name: v.to_string(), expression: Box::new(e)}
    }
    rule statement(stmts: &[Statement]) -> Statement = _ c:(comment() / variable_assignment(stmts)) _ {
        c
    }

    rule expression_with_satements(old: &[Statement]) -> Expression = stmts:(statement(old))* e:(booleanCast((&extended_statements(&stmts, old))) / (switch((&extended_statements(&stmts, old))) / expected!("Switch")) / (conditionalWithElse((&(extended_statements(&stmts, old))))/ conditional((&(extended_statements(&stmts, old))))/ expected!("Conditional"))  / ( timeOperation((&extended_statements(&stmts, old))) / operation((&extended_statements(&stmts, old)))   /expected!("Binary operator"))/ (arrayOperationWithArguments((&extended_statements(&stmts, old))) / arrayOperation((&extended_statements(&stmts, old))) / array((&extended_statements(&stmts, old)))/expected!("Array expression"))  / (unary((&extended_statements(&stmts, old))) / function((&extended_statements(&stmts, old)))/expected!("Function"))) {
        e
    }

    pub rule expression() -> Expression = stmts:(statement((&[])))* e:(booleanCast((&stmts)) / (switch((&stmts)) / expected!("Switch")) / (conditionalWithElse((&(stmts)))/ conditional((&(stmts)))/ expected!("Conditional"))  / ( timeOperation((&stmts)) / operation((&stmts))   /expected!("Binary operator"))/ (arrayOperationWithArguments((&stmts)) / arrayOperation((&stmts)) / array((&stmts))/expected!("Array expression"))  / (unary((&stmts)) / function((&stmts))/expected!("Function"))) {
        e
    }
}}
fn replace_variable(stmts: &[Statement], var_name: &str, expr: Expression) -> Expression {
     if let Some(variable_replacement) = stmts.iter().find_map(|a| match a {
            Statement::VariableAssignment {name, expression} if name == var_name => {
                Some(expression)
            }
            _ => None
        }) {
            let v = variable_replacement.clone();
            *v
        } else {
            expr
        }
}
fn extended_statements(a: &[Statement], b: &[Statement]) -> Vec<Statement> {
    let mut v = vec![];
    v.extend(a.iter().cloned().chain(b.iter().cloned()));
    v
}

#[cfg(test)]
mod tests {
    use crate::to_json_logic;

    #[test]
    fn test_null_coercion() {
        let logic = "a ?? true";
        let logic = super::arithmetic::expression(logic).unwrap();
        let json_logic = logic.to_json_logic();
        let res = jsonlogic::apply(&json_logic, &serde_json::Value::Null)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(res);
    }

    #[test]
    fn test_statements() {
        let stuff = r#"
        /* This is a comment */
        let two_dose_vaccines = [
            "a",
            "b"
        ];
        if (payload.v.0.mp in two_dose_vaccines) {
            true
        } else {
            false
        }
        "#;
        let expression = super::arithmetic::expression(stuff).unwrap();
        println!("{}", expression.to_json_logic());
    }
    #[test]
    fn test_assignment_replacement() {
        let stuff = r#"
            let a = if( 1 < 2 ) { "test" }  else { "other" };
            if (b === "test") {
                let c = a;
                c
            } else {
                "nope"
            }
        "#;
        let expression = super::arithmetic::expression(stuff).unwrap();
        println!("{}", expression.to_json_logic());
    }
    #[test]
    fn or_test() {
        let expression = super::arithmetic::expression("true or false").unwrap();
        println!("{}", expression.to_json_logic());
        let json_logic = expression.to_json_logic();
        let res = jsonlogic::apply(&json_logic, &serde_json::Value::Null)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(res);

        let expression = super::arithmetic::expression("false or true").unwrap();
        let json_logic = expression.to_json_logic();
        let res = jsonlogic::apply(&json_logic, &serde_json::Value::Null)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(res);

        let expression = super::arithmetic::expression("true or true").unwrap();
        let json_logic = expression.to_json_logic();
        let res = jsonlogic::apply(&json_logic, &serde_json::Value::Null)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(res);

        let expression = super::arithmetic::expression("false or false").unwrap();
        let json_logic = expression.to_json_logic();
        let res = jsonlogic::apply(&json_logic, &serde_json::Value::Null)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(!res);
    }
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
            super::arithmetic::expression("[1,2,3,4,5]::filter { this % 2 == 0 }").unwrap();
        println!("{}", array_expression.to_json_logic());
    }

    #[test]
    fn test_in() {
        let in_expression = super::arithmetic::expression("a in [1,2,3,4]").unwrap();
        println!("{}", in_expression.to_json_logic());
    }
    #[test]
    fn test_switch() {
        let switch_expression = super::arithmetic::expression(
            r#"
        switch(a) {
            ["my", "and", "case"] : if a.mp === "test" => {
                e
            }
            "test" => {
                b
            }
            
            "other" => {
                c
            }
           
            "third" => {
                d
            }
            ["fourth", "fifth", "sixth"] => {
                e
            }
            _ => {
                undefined
            }
        }
        "#,
        )
        .unwrap();
        println!("{}", switch_expression.to_json_logic());
    }

    #[test]
    fn test_min_desugar() {
        let min_desugar = super::arithmetic::expression("if (a < b) {a} else {b}").unwrap();
        let min = super::arithmetic::expression("min(a,b)").unwrap();
        assert_eq!(min, min_desugar);

        let min_desugared = super::arithmetic::expression("min(6,1,3,7)").unwrap();
        let logic = min_desugared.to_json_logic();
        let result = jsonlogic::apply(&logic, &serde_json::Value::Null)
            .unwrap()
            .as_i64()
            .unwrap();
        assert_eq!(result, 1);

        let min_desugared = super::arithmetic::expression("min(-10,200,1,87)").unwrap();
        let logic = min_desugared.to_json_logic();
        let result = jsonlogic::apply(&logic, &serde_json::Value::Null)
            .unwrap()
            .as_i64()
            .unwrap();
        assert_eq!(result, -10);
    }
    #[test]
    fn test_max_desugar() {
        let min_desugar = super::arithmetic::expression("if (a > b) {a} else {b}").unwrap();
        let min = super::arithmetic::expression("max(a,b)").unwrap();
        assert_eq!(min, min_desugar);

        let max_desugared = super::arithmetic::expression("max(6,1,3,7)").unwrap();
        let logic = max_desugared.to_json_logic();
        let result = jsonlogic::apply(&logic, &serde_json::Value::Null)
            .unwrap()
            .as_i64()
            .unwrap();
        assert_eq!(result, 7);

        let max_desugared = super::arithmetic::expression("max(-10,200,1,87)").unwrap();
        let logic = max_desugared.to_json_logic();
        let result = jsonlogic::apply(&logic, &serde_json::Value::Null)
            .unwrap()
            .as_i64()
            .unwrap();
        assert_eq!(result, 200);

        let max_desugared = super::arithmetic::expression("max(1000,200,1,87)").unwrap();
        let logic = max_desugared.to_json_logic();
        let result = jsonlogic::apply(&logic, &serde_json::Value::Null)
            .unwrap()
            .as_i64()
            .unwrap();
        assert_eq!(result, 1000);
    }
    #[test]
    fn test_time() {
        let time =
            super::arithmetic::expression(r#""2020-01-01" is not before "2020-02-02T00:00""#)
                .unwrap();
        println!("{}", time.to_json_logic());
        let time =
            super::arithmetic::expression(r#""2020-01-01" is not before "2020-02-02T00:00:00""#)
                .unwrap();
        println!("{}", time.to_json_logic());
        let time = super::arithmetic::expression(
            r#""2020-01-01" is not before "2020-02-02T00:00:00.000""#,
        )
        .unwrap();
        println!("{}", time.to_json_logic());
        let time =
            super::arithmetic::expression(r#""2020-01-01" is not before "2020-02-02T00:00Z""#)
                .unwrap();
        println!("{}", time.to_json_logic());
        let time =
            super::arithmetic::expression(r#""2020-01-01" is not before "2020-02-02T00:00:00Z""#)
                .unwrap();
        println!("{}", time.to_json_logic());
        let time =
            super::arithmetic::expression(r#""2020-01-01" is not before "2020-02-02T00:00+03""#)
                .unwrap();
        println!("{}", time.to_json_logic());
        let time =
            super::arithmetic::expression(r#""2020-01-01" is not before "2020-02-02T00:00+03:00""#)
                .unwrap();
        println!("{}", time.to_json_logic());
        let time = super::arithmetic::expression(
            r#""2020-01-01" is not before "2020-02-02T00:00:00.999+03""#,
        )
        .unwrap();
        println!("{}", time.to_json_logic());
        let time = super::arithmetic::expression(
            r#""2020-01-01" is not before "2020-02-02T00:00:00.999+03:00""#,
        )
        .unwrap();
        println!("{}", time.to_json_logic());
        let time = super::arithmetic::expression(
            r#"(a as DateTime) is not before "2020-02-02T00:00:00.999+03:00""#,
        )
        .unwrap();
        println!("{}", time.to_json_logic());
        let time = super::arithmetic::expression(r#"a < 1"#).unwrap();
        println!("{}", time.to_json_logic());
    }
    #[test]
    fn test_comment() {
        let _ = to_json_logic! {{/* This is a comment */
            /* This is a comment */
            if (a < b) {
                /*comment*/
                if (c < b) {
                    /*
                        This is a multiline comment
                    */
                    c
                }
            }
        }};
    }

    #[test]
    fn test_boolean_cast() {
        let logic = super::arithmetic::expression("a as Boolean").unwrap();
        let expr = super::arithmetic::expression("not not a").unwrap();
        assert_eq!(expr.to_json_logic(), logic.to_json_logic());
    }

    #[test]
    fn test_macro() {
        let logic = to_json_logic! ({
            if (payload.v.0) {
                if (payload.v.0.mp in ["EU/1/20/1525"]
                    && payload.v.0.dn === 1) {
                    (payload.v.0.dt as DateTime) + 386#days
                } else {
                    if(payload.v.0.mp in ["BBIBP-CorV_T","CoronaVac_T","Covaxin_T"]) {
                        if (payload.h.iat) {
                                min((payload.v.0.dt as DateTime) + 364#days, (payload.h.iat as DateTime) + 29#days)
                        } else {
                            undefined
                        }
                    }  else {
                            (payload.v.0.dt as DateTime) + 364#days
                    }
                }
            } else {
                if (payload.t.0) {
                    if (payload.t.0.tt === "LP6464-4") {
                        (payload.t.0.sc as DateTime) + 72#hours
                    }else {
                        if (payload.t.0.tt === "LP217198-3") {
                            (payload.t.0.sc as DateTime) + 48#hours
                        } else {
                            if(payload.t.0.tt === "94504-8"){
                                (payload.t.0.sc as DateTime) + 89#days
                            }  else {
                                    undefined
                            }
                        }
                    }
                } else {
                    if (payload.r.0) {
                        (payload.r.0.fr as DateTime) + 364#days
                    } else {
                        undefined
                    }
                }
            }
        });
        println!("{}", logic.to_string());
    }
}

#[macro_export]
macro_rules! to_json_logic {
    ($the_code:tt) => {{
        println!("{}", stringify!($the_code));
        let block_string = stringify!($the_code)
            .strip_prefix("{")
            .unwrap()
            .strip_suffix("}")
            .unwrap();
        let block_string = block_string.replace("== =", "===");
        crate::arithmetic::expression(&block_string)
            .unwrap()
            .to_json_logic()
    }};
}
