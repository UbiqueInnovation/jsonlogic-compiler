// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

import * as monaco from 'monaco-editor';


var logic = monaco.editor.create(document.getElementById("logic"), {
    value: "payload.v.0",
    language: "javascript"
});

var json = monaco.editor.create(document.getElementById("json"), {
    value: "",
    language: "json",
    readOnly: true
});

var dataEditor = monaco.editor.create(document.getElementById("data"), {
    value: "{}",
    language: "json"
});

var resultEditor = monaco.editor.create(document.getElementById("result"), {
    value: "{}",
    language: "json",
    readOnly: true
});

var otherBinding = logic.onDidChangeModelContent((e) => {
    parse();
});
var blubBinding = dataEditor.onDidChangeModelContent((e) => {
    parse();
});

function parse() {
 var l = logic.getModel().getValue();
 let compiledLogic = window.logic_to_json_logic(l);
 let data = dataEditor.getModel().getValue();
 let result = window.apply_logic(compiledLogic, data);
 json.getModel().setValue(compiledLogic);
 resultEditor.getModel().setValue(result)
}