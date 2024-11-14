// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

import * as monaco from "monaco-editor";
import "./aifc";

import { Buffer } from "buffer";

self.MonacoEnvironment = {
  getWorkerUrl: function (moduleId, label) {
    if (label === "json") {
      return "./json.worker.bundle.js";
    }
    return "./editor.worker.bundle.js";
  },
};

var logic = monaco.editor.create(document.getElementById("logic"), {
  value: "payload.v.0",
  language: "aifc",
  theme: "aifcTheme",
  automaticLayout: true,
});

logic.addAction({
  // An unique identifier of the contributed action.
  id: "save_logic",

  // A label of the action that will be presented to the user.
  label: "Save Logic",

  // An optional array of keybindings for the action.
  keybindings: [
    // chord
    monaco.KeyMod.chord(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS),
  ],

  // A precondition for this action.
  precondition: null,

  // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
  keybindingContext: null,

  contextMenuGroupId: "navigation",

  contextMenuOrder: 1.5,

  // Method that will be executed when the action is triggered.
  // @param editor The editor instance is passed in as a convenience
  run: function (ed) {
    let files = ed.getModel().getValue();
    const jsonBlob = new Blob([files], {
      type: "text/javascript",
    });
    const elem = window.document.createElement("a");
    elem.href = window.URL.createObjectURL(jsonBlob);
    elem.download = "nationalRules.js";
    document.body.appendChild(elem);
    elem.click();
    document.body.removeChild(elem);
  },
});

var json = monaco.editor.create(document.getElementById("json"), {
  value: "",
  language: "json",
  automaticLayout: true,
});

json.addAction({
  // An unique identifier of the contributed action.
  id: "save_rule",

  // A label of the action that will be presented to the user.
  label: "Save Rule",

  // An optional array of keybindings for the action.
  keybindings: [
    // chord
    monaco.KeyMod.chord(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS),
  ],

  // A precondition for this action.
  precondition: null,

  // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
  keybindingContext: null,

  contextMenuGroupId: "navigation",

  contextMenuOrder: 1.5,

  // Method that will be executed when the action is triggered.
  // @param editor The editor instance is passed in as a convenience
  run: function (ed) {
    let files = ed.getModel().getValue();
    const jsonBlob = new Blob([files], {
      type: "application/json",
    });
    const elem = window.document.createElement("a");
    elem.href = window.URL.createObjectURL(jsonBlob);
    elem.download = "nationalRules.json";
    document.body.appendChild(elem);
    elem.click();
    document.body.removeChild(elem);
  },
});

var dataEditor = monaco.editor.create(document.getElementById("data"), {
  value: "{}",
  language: "json",
  automaticLayout: true,
});

dataEditor.addAction({
  // An unique identifier of the contributed action.
  id: "save_rule",

  // A label of the action that will be presented to the user.
  label: "Save Rule",

  // An optional array of keybindings for the action.
  keybindings: [
    // chord
    monaco.KeyMod.chord(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS),
  ],

  // A precondition for this action.
  precondition: null,

  // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
  keybindingContext: null,

  contextMenuGroupId: "navigation",

  contextMenuOrder: 1.5,

  // Method that will be executed when the action is triggered.
  // @param editor The editor instance is passed in as a convenience
  run: function (ed) {
    let files = ed.getModel().getValue();
    const jsonBlob = new Blob([files], {
      type: "application/json",
    });
    const elem = window.document.createElement("a");
    elem.href = window.URL.createObjectURL(jsonBlob);
    elem.download = "nationalRules.json";
    document.body.appendChild(elem);
    elem.click();
    document.body.removeChild(elem);
  },
});

var resultEditor = monaco.editor.create(document.getElementById("result"), {
  value: "{}",
  language: "json",
  readOnly: true,
  automaticLayout: true,
});
var fragmentPart = window.location.hash;
if (fragmentPart != undefined && fragmentPart !== "") {
  var fragment = JSON.parse(Buffer.from(window.location.hash, "base64"));

  if (fragment != undefined) {
    dataEditor.getModel().setValue(fragment.data);
    if (fragment.json != undefined) {
      json.getModel().setValue(fragment.json);
    }
    if (fragment.logic != undefined) {
      logic.getModel().setValue(fragment.logic);
      compileLogic();
    }
    applyLogic();
  }
}

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
  } catch (ex) {}
});

function compileLogic() {
  var l = logic.getModel().getValue();
  let compiledLogic = window.logic_to_json_logic(l);
  json.getModel().setValue(compiledLogic);
}

function applyLogic() {
  let data = dataEditor.getModel().getValue();
  let compiledLogic = logic.getModel().getValue();
  let result = window.apply_logic(compiledLogic, data);
  resultEditor.getModel().setValue(result);

  // share();
}

function share() {
  var includeLogic = document.querySelector("input[name='withLogic']").checked;
  var model = undefined;
  if (includeLogic) {
    model = {
      logic: logic.getModel().getValue(),
      data: dataEditor.getModel().getValue(),
    };
  } else {
    model = {
      json: json.getModel().getValue(),
      data: dataEditor.getModel().getValue(),
    };
  }

  var encoded = Buffer.from(JSON.stringify(model)).toString("base64");
  var png = window.to_qrcode(encoded);
  document.getElementById("shareQrCode").src = `data:image/png;base64, ${png}`;
}

document.getElementById("pasteField").addEventListener(
  "paste",
  function (thePasteEvent) {
    retrieveImageFromClipboardAsBase64(thePasteEvent, function (imageUrl) {
      var res = imageUrl.replace("data:image/png;base64,", "").trim();
      console.log(res);
      var data = window.from_qrcode(res);

      var fragment = JSON.parse(Buffer.from(data, "base64"));

      if (fragment != undefined) {
        dataEditor.getModel().setValue(fragment.data);
        if (fragment.json != undefined) {
          json.getModel().setValue(fragment.json);
        }
        if (fragment.logic != undefined) {
          logic.getModel().setValue(fragment.logic);
          compileLogic();
        }
        applyLogic();
      }
      document.getElementById("pasteField").value = "";
    });
  },
  false,
);

document.getElementById("shareButton").onclick = share;

/**
 * This handler retrieves the images from the clipboard as a base64 string and returns it in a callback.
 *
 * @param pasteEvent
 * @param callback
 */
function retrieveImageFromClipboardAsBase64(pasteEvent, callback, imageFormat) {
  if (pasteEvent.clipboardData == false) {
    if (typeof callback == "function") {
      callback(undefined);
    }
  }

  var items = pasteEvent.clipboardData.items;

  if (items == undefined) {
    if (typeof callback == "function") {
      callback(undefined);
    }
  }
  let regs =
    /<img.*src=\"data:image\/png;base64,(?<code>[A-Za-z0-9\/+=]+)\".*>/;
  for (var i = 0; i < items.length; i++) {
    // Skip content if not image
    console.log(items[i]);
    if (items[i].kind === "string") {
      console.log("blub");
      items[i].getAsString(function (s) {
        if (regs.test(s)) {
          let m = s.match(regs);
          callback(m.groups["code"]);
        } else {
          callback(s);
        }
      });
      return;
    }
    if (items[i].type.indexOf("image") == -1) continue;
    // Retrieve image on clipboard as blob
    var blob = items[i].getAsFile();

    // Create an abstract canvas and get context
    var mycanvas = document.createElement("canvas");
    var ctx = mycanvas.getContext("2d");

    // Create an image
    var img = new Image();

    // Once the image loads, render the img on the canvas
    img.onload = function () {
      // Update dimensions of the canvas with the dimensions of the image
      mycanvas.width = this.width;
      mycanvas.height = this.height;

      // Draw the image
      ctx.drawImage(img, 0, 0);

      // Execute callback with the base64 URI of the image
      if (typeof callback == "function") {
        callback(mycanvas.toDataURL(imageFormat || "image/png"));
      }
    };

    // Crossbrowser support for URL
    var URLObj = window.URL || window.webkitURL;

    // Creates a DOMString containing a URL representing the object given in the parameter
    // namely the original Blob
    img.src = URLObj.createObjectURL(blob);
  }
}
