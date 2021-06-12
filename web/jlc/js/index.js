import("../pkg/index.js").then(function(m) {
    window.logic_to_json_logic = m.get_json_logic
}).catch(console.error);
