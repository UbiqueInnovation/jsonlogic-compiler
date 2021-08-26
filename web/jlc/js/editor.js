// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

import * as monaco from 'monaco-editor';


var logic = monaco.editor.create(document.getElementById("logic"), {
    value: "payload.v.0",
    language: "javascript",
    automaticLayout: true
});

var json = monaco.editor.create(document.getElementById("json"), {
    value: "",
    language: "json",
    automaticLayout: true
});

var dataEditor = monaco.editor.create(document.getElementById("data"), {
    value: "{}",
    language: "json",
    automaticLayout: true
});

var resultEditor = monaco.editor.create(document.getElementById("result"), {
    value: "{}",
    language: "json",
    readOnly: true,
    automaticLayout: true
});

var otherBinding = logic.onDidChangeModelContent((e) => {
    compileLogic();
    applyLogic();
});
var jsonLogicBinding = json.onDidChangeModelContent((e) => {
    applyLogic();
});
var blubBinding = dataEditor.onDidChangeModelContent((e) => {
    applyLogic();
});



function compileLogic() {
    var l = logic.getModel().getValue();
    let compiledLogic = window.logic_to_json_logic(l);
    json.getModel().setValue(compiledLogic);
}

function applyLogic() {
    let data = dataEditor.getModel().getValue();
    let compiledLogic = json.getModel().getValue();
    let result = window.apply_logic(compiledLogic, data);
    resultEditor.getModel().setValue(result)
}