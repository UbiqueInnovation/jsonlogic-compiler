use std::io::{Read, Write};

use flate2::Compression;
use image::{DynamicImage, ImageFormat};
use jlc::arithmetic::expression;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

// macro_rules! console_log {
//     // Note that this is using the `log` function imported above during
//     // `bare_bones`
//     ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
// }

#[wasm_bindgen]
pub fn get_json_logic(normal: String) -> String {
    match expression(&normal) {
        Ok(exp) => exp.to_string(),
        Err(err) => format!("{:#?}", err),
    }
}

#[wasm_bindgen]
pub fn evaluate_aifc(normal: String, data: String) -> String {
    let exp = match expression(&normal) {
        Ok(exp) => exp,
        Err(err) => return format!("{:#?}", err),
    };
    let data: serde_json::Value = match serde_json::from_str(&data) {
        Ok(t) => t,
        Err(err) => return format!("{:#?}", err),
    };

    match exp.eval(&data) {
        Ok(e) => e.to_string(),
        Err(err) => format!("{:#?}", err),
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

#[wasm_bindgen]
pub fn to_qrcode(payload: String) -> String {
    let qr_code = create_qr_code(&payload);
    base64::encode_config(qr_code, base64::STANDARD)
}

#[wasm_bindgen]
pub fn from_qrcode(payload: String) -> String {
    let data = base64::decode_config(&payload, base64::STANDARD).expect("INVALID BASE64");
    decode_qr_code(&data)
}

fn create_qr_code(payload: &str) -> Vec<u8> {
    use image::Luma;
    use qrcode::QrCode;

    let data = payload.as_bytes();
    let mut compressed_data = vec![];
    let mut encoder = flate2::write::DeflateEncoder::new(&mut compressed_data, Compression::best());
    encoder.write_all(data).unwrap();
    encoder.finish().unwrap();

    let code =
        QrCode::with_error_correction_level(&base64::encode(&compressed_data), qrcode::EcLevel::L)
            .unwrap();

    let image = code.render::<Luma<u8>>().build();
    let image = DynamicImage::ImageLuma8(image);
    let mut png_buffer = vec![];
    image.write_to(&mut png_buffer, ImageFormat::Png).unwrap();
    png_buffer
}

fn decode_qr_code(payload: &[u8]) -> String {
    let img = image::load_from_memory(payload).expect("Could not load from memory");
    // Use default decoder
    let decoder = bardecoder::default_builder().build();
    let results = decoder.decode(&img);

    let res = results[0].as_ref().expect("No data found");
    let mut decompressed_data = vec![];

    let res = base64::decode(res).unwrap();
    let mut decoder = flate2::read::DeflateDecoder::new(&res[..]);
    decoder
        .read_to_end(&mut decompressed_data)
        .expect("Could not decompress");
    String::from_utf8(decompressed_data).unwrap()
}
