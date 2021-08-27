use wasm_bindgen::prelude::*;
use jlc::arithmetic::expression;

#[wasm_bindgen]
pub fn get_json_logic(normal: String) -> String {
    match expression(&normal) {
        Ok(exp) => exp.to_string(),
        Err(err) => format!("{:#?}", err) 
    }
}

#[wasm_bindgen]
pub fn apply_logic(logic: String, data: String) -> String {
    if let (Ok(logic), Ok(data)) = (serde_json::from_str(&logic), serde_json::from_str(&data)) {
        if let Ok(result) = jsonlogic::apply(&logic, &data) {
            serde_json::to_string(&result).unwrap()
        } else {
            "<<Error>>".to_string()
        }
    } else {
        "<<Error>>".to_string()
    }
}