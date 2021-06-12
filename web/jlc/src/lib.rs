use wasm_bindgen::prelude::*;
use web_sys::console;
use jlc::arithmetic::expression;

#[wasm_bindgen]
pub fn get_json_logic(normal: String) -> String {
    match expression(&normal) {
        Ok(exp) => exp.to_string(),
        Err(err) => format!("{:#?}", err) 
    }
}
