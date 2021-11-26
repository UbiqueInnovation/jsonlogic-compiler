// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

import * as monaco from 'monaco-editor';

// Register a new language
monaco.languages.register({
    id: 'aifc'
});

// Register a tokens provider for the language
monaco.languages.setMonarchTokensProvider('aifc', {
    tokenizer: {
        root: [
            [/\/\*[^]*\*\//sm, "comment"],
            [/\b(if|else|switch|as|null|undefined)\b/, "keyword"],
            [/\:\:(\w+)/, "function"],
            [/_\s+=>/, "default-case"],
            [/=>/, "arrow"],
            [/(>=|<=|<|(?<!=)>|===|and|&&|\+|\-|is\s+before|(is\s+)?not\s+before|is\s+after|(is\s+)?not\s+after)/, "operator"],
            [/(!=|==|or)/, "not-supported-operator"],
            [/\d+\#(years|year|months|month|weeks|week|days|day|hours|hour|minutes|minute|seconds|second)/, "timespan"],
            [/"\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}(:\d{2}(\.\d+)?)?(([+-]\d{2}(:\d{2})?)|Z)?)?"/, "date-literal"],
            [/\b(DateTime|Boolean)\b/, "type"],
            [/\b(this|accumulator|current)\b/, "context-keywords"],
            [/\b([A-z]+[A-z0-9_]*)(\.[A-z0-9]+[A-z0-9_]*)*\b/, "variable"],
        ]
    }
});
monaco.languages.setLanguageConfiguration('aifc', {
    wordPattern: /\b([A-z]+[A-z0-9_]*)(\.[A-z0-9]+[A-z0-9_]*)*\b/
});

// Define a new theme that contains only rules that match this language
monaco.editor.defineTheme('aifcTheme', {
    base: 'vs',
    inherit: false,
    rules: [{
            token: 'comment',
            foreground: 'AAAAAA',
            fontStyle: "italic"
        },
        {
            token: 'keyword',
            foreground: '181D27',
            fontStyle: 'underline'
        },
        {
            token: 'operator',
            foreground: '69B578',
            fontStyle: 'underline'
        },
        {
            token: 'not-supported-operator',
            foreground: 'ff0000',
            fontStyle: 'bold'
        },
        {
            token: 'timespan',
            foreground: '3A7D44',
            fontStyle: "italic"
        },
        {
            token: 'default-case',
            foreground: 'ff0000',
            fontStyle: "italic"
        }, {
            token: 'function',
            foreground: '69B578',
            fontStyle: "italic"
        },
        {
            token: "type",
            foreground: '69B578'
        },
        {
            token: 'date-literal',
            foreground: '3A7D44'
        },
        {
            token: 'arrow',
            foreground: '3A7D44'
        },
        {
            token: 'variable',
            foreground: '254D32'
        },
        {
            token: 'context-keywords',
            foreground: '181D27',
            fontStyle: "italic"
        },
    ],
    colors: {
        'editor.foreground': '#000000',
        'editor.background': '#FFFFFF',
        'editorCursor.foreground': '#8B0000',
        'editor.lineHighlightBackground': '#11111111',
        'editorLineNumber.foreground': '#008800',
        'editor.selectionBackground': '#88000030',
        'editor.inactiveSelectionBackground': '#88000015'
    }
});

// Register a completion item provider for the new language
monaco.languages.registerCompletionItemProvider('aifc', {
    provideCompletionItems: (model, position) => {

        var word = model.getWordUntilPosition(position);
        var range = {
            startLineNumber: position.lineNumber,
            endLineNumber: position.lineNumber,
            startColumn: word.startColumn,
            endColumn: word.endColumn
        };
        var path = word.word.split(".");
        console.log(path);
        var prefix = "";
        var last = window.dataModel === undefined ? [] : window.dataModel;
        for (var segment of path) {
            if (last[segment.trim()] !== undefined) {
                last = last[segment];
                prefix += prefix == "" ? segment : "." + segment;
            } else {
                // last = undefined;
                break;
            }
        }
        var suggestions = last === undefined ? [] : Object.keys(last).map((val, i, a) => {
            return {
                label: prefix == "" ? val : prefix + "." + val,
                kind: monaco.languages.CompletionItemKind.Field,
                documentation: prefix == "" ? val : prefix + "." + val,
                insertText: prefix == "" ? val : prefix + "." + val
            };
        });
        console.log(suggestions);
        var combinedSuggestions = suggestions.concat([{
                label: 'is before',
                kind: monaco.languages.CompletionItemKind.Operator,
                documentation: "Checks if event a *is before* event b",
                insertText: 'is before'
            },
            {
                label: 'is not before',
                kind: monaco.languages.CompletionItemKind.Operator,
                documentation: "Checks if event a *is not before* event b",
                insertText: 'is not before'
            },
            {
                label: 'is after',
                kind: monaco.languages.CompletionItemKind.Operator,
                documentation: "Checks if event a *is after* event b",
                insertText: 'is after'
            },
            {
                label: 'is not after',
                kind: monaco.languages.CompletionItemKind.Operator,
                documentation: "Checks if event a *is not after* event b",
                insertText: 'is not after'
            },
            {
                label: 'if',
                kind: monaco.languages.CompletionItemKind.Snippet,
                documentation: "If statement",
                insertText: 'if(${1:args})\n {\n\t${2:block} \n} else {\n \t${3:undefined} \n}',
                insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                range: range
            },
            {
                label: 'switch',
                kind: monaco.languages.CompletionItemKind.Snippet,
                documentation: "Switch statement",
                insertText: 'switch(${1:args})\n {\n\t${2:expr1} => {\n\t\t${3:block}\n \t}\n \t_ => {\n\t\tundefined\n\t}\n}',
                insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                range: range
            }
        ]);
        return {
            suggestions: combinedSuggestions
        }
    }
});