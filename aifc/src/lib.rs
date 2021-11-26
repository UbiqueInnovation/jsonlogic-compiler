use std::{error::Error, fmt::Display};

use colorize::AnsiColor;

pub fn compile_logic(input: &str, minified: bool) -> Result<String, AifcCompileError> {
    let json_logic = match jlc::arithmetic::expression(input) {
        Ok(logic) => logic,
        Err(e) => {
            let location = e.location;
            let line_num = location.line - 1;
            let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();
            let line = &lines[line_num];
            let mut output = String::from("\n");
            if line_num > 0 {
                output.push_str(&(lines[line_num - 1].clone().green()));
                output.push('\n');
            }
            output.push_str(&line.clone().red());
            output.push('\n');
            for _ in 0..location.column - 1 {
                output.push_str(&"-".red());
            }
            output.push_str(&"^".red());
            output.push(' ');
            output.push_str(&format!("Expected: [{}]", e.expected).red());
            output.push('\n');
            if line_num < lines.len() - 1 {
                output.push_str(&(lines[line_num + 1].clone().green()));
            }
            format!(
                "{}{}",
                format!("SyntaxError at {:?}", location).red(),
                output
            );
            return Err(AifcCompileError::SyntaxError(format!(
                "{}{}",
                format!("SyntaxError at {:?}", location).red(),
                output
            )));
        }
    };
    if minified {
        Ok(json_logic.to_string())
    } else {
        serde_json::to_string_pretty(&json_logic.to_json_logic())
            .map_err(|e| AifcCompileError::PrettyfyError(Box::new(e)))
    }
}

pub mod ffi {
    use jni::{
        objects::{JObject, JString},
        sys::{jboolean, jstring, JNI_TRUE},
        JNIEnv,
    };

    #[no_mangle]
    pub extern "system" fn Java_ch_ubique_aifc_AifcCompiler_compile(
        env: JNIEnv,
        _instance: JObject,
        input: JString,
        minified: jboolean,
    ) -> jstring {
        let input = match env.get_string(input) {
            Ok(input) => input,
            Err(e) => {
                let _ = env.throw(("ch/ubique/aifc/AifcCompilerError",format!("{:?}", e)));
                return JObject::null().into_inner();
            }
        };
        let input = match input.to_str() {
            Ok(input) => input,
            Err(e) => {
                let _ = env.throw(("ch/ubique/aifc/AifcCompilerError",format!("{:?}", e)));
                return JObject::null().into_inner();
            }
        };
        let logic = match super::compile_logic(input, minified == JNI_TRUE) {
            Ok(logic) => logic,
            Err(e) => {
                 let _ = env.throw(("ch/ubique/aifc/AifcCompilerError",format!("{:?}", e)));
                return JObject::null().into_inner();
            }
        };
        let logic_string = match env.new_string(logic) {
            Ok(logic) => logic,
            Err(e) => {
                let _ = env.throw(("ch/ubique/aifc/AifcCompilerError",format!("{:?}", e)));
                return JObject::null().into_inner();
            }
        };
        logic_string.into_inner()
    }
}

#[derive(Debug)]
pub enum AifcCompileError {
    SyntaxError(String),
    PrettyfyError(Box<dyn std::error::Error>),
}
impl Display for AifcCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AifcCompileError::SyntaxError(e) => f.write_str(e),
            AifcCompileError::PrettyfyError(e) => write!(f, "{:?}", e),
        }
    }
}
impl Error for AifcCompileError {}
