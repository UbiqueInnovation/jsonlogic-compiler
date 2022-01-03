import("../pkg/index.js").then(function(m) {
    window.logic_to_json_logic = m.get_json_logic;
    window.apply_logic = m.apply_logic;
    window.to_qrcode = m.to_qrcode;
    window.from_qrcode = m.from_qrcode;
}).catch(console.error);
