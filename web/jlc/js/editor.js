// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

import * as monaco from 'monaco-editor';
import "./aifc";

var logic = monaco.editor.create(document.getElementById("logic"), {
    value: "payload.v.0",
    language: "aifc",
    theme: 'aifcTheme',
    automaticLayout: true
});

logic.addAction({
    // An unique identifier of the contributed action.
    id: 'save_logic',

    // A label of the action that will be presented to the user.
    label: 'Save Logic',

    // An optional array of keybindings for the action.
    keybindings: [
        // chord
        monaco.KeyMod.chord(
            monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S,
        )
    ],

    // A precondition for this action.
    precondition: null,

    // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
    keybindingContext: null,

    contextMenuGroupId: 'navigation',

    contextMenuOrder: 1.5,

    // Method that will be executed when the action is triggered.
    // @param editor The editor instance is passed in as a convenience
    run: function (ed) {
        let files = ed.getModel().getValue();
        const jsonBlob = new Blob([files], { type: 'text/javascript' });
         const elem = window.document.createElement('a');
         elem.href = window.URL.createObjectURL(jsonBlob);
         elem.download = "nationalRules.js";
         document.body.appendChild(elem);
         elem.click();
         document.body.removeChild(elem);
    }
});

var json = monaco.editor.create(document.getElementById("json"), {
    value: "",
    language: "json",
    automaticLayout: true
});

json.addAction({
    // An unique identifier of the contributed action.
    id: 'save_rule',

    // A label of the action that will be presented to the user.
    label: 'Save Rule',

    // An optional array of keybindings for the action.
    keybindings: [
        // chord
        monaco.KeyMod.chord(
            monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S,
        )
    ],

    // A precondition for this action.
    precondition: null,

    // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
    keybindingContext: null,

    contextMenuGroupId: 'navigation',

    contextMenuOrder: 1.5,

    // Method that will be executed when the action is triggered.
    // @param editor The editor instance is passed in as a convenience
    run: function (ed) {
        let files = ed.getModel().getValue();
        const jsonBlob = new Blob([files], {
            type: 'application/json'
        });
        const elem = window.document.createElement('a');
        elem.href = window.URL.createObjectURL(jsonBlob);
        elem.download = "nationalRules.json";
        document.body.appendChild(elem);
        elem.click();
        document.body.removeChild(elem);
    }
});

var dataEditor = monaco.editor.create(document.getElementById("data"), {
    value: "{}",
    language: "json",
    automaticLayout: true
});

dataEditor.addAction({
    // An unique identifier of the contributed action.
    id: 'save_rule',

    // A label of the action that will be presented to the user.
    label: 'Save Rule',

    // An optional array of keybindings for the action.
    keybindings: [
        // chord
        monaco.KeyMod.chord(
            monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S,
        )
    ],

    // A precondition for this action.
    precondition: null,

    // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
    keybindingContext: null,

    contextMenuGroupId: 'navigation',

    contextMenuOrder: 1.5,

    // Method that will be executed when the action is triggered.
    // @param editor The editor instance is passed in as a convenience
    run: function (ed) {
        let files = ed.getModel().getValue();
        const jsonBlob = new Blob([files], {
            type: 'application/json'
        });
        const elem = window.document.createElement('a');
        elem.href = window.URL.createObjectURL(jsonBlob);
        elem.download = "nationalRules.json";
        document.body.appendChild(elem);
        elem.click();
        document.body.removeChild(elem);
    }
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
    try {
        window.dataModel = JSON.parse(dataEditor.getModel().getValue());
    } catch (ex) {
        
    }
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