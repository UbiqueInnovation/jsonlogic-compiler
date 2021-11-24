const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const dist = path.resolve(__dirname, "dist");
const MONACO_DIR = path.resolve(__dirname, './node_modules/monaco-editor');
// const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');


module.exports = {
  mode: "production",
  entry: {
    index: "./js/index.js",
    editor: "./js/editor.js",
    aifc: "./js/aifc.js",
    'editor.worker': 'monaco-editor/esm/vs/editor/editor.worker.js',
    'json.worker': 'monaco-editor/esm/vs/language/json/json.worker',
  },
  experiments: { asyncWebAssembly: true },
  output: {
    globalObject: 'self',
    filename: '[name].bundle.js',
    path: path.resolve(__dirname, 'dist')
  },
  module: {
    rules: [{
      test: /\.css$/,
      include: MONACO_DIR,
        use: ['style-loader', 'css-loader']
    },
       {
         test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
         use: ["url-loader?limit=10000&minetype=application/font-woff"]
       },
      {
        test: /\.ttf$/,
        use: ['file-loader']
      }
    ]
  },
  devServer: {
    static: { directory: "static" },
  },
  plugins: [
    // new MonacoWebpackPlugin({
    //   languages: ["json"],
    //   features: ["codelens", "documentSymbols", "multicursor", "indentation", "folding"],
    //   globalApi: true
    //  }),
    new CopyPlugin({
      patterns: [
        path.resolve(__dirname, "static")
      ],
      options: {
        concurrency: 100,
      }
    }),

    new WasmPackPlugin({
      crateDirectory: __dirname,
    }),
  ]
};
