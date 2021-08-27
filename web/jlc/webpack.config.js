const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const dist = path.resolve(__dirname, "dist");
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');


module.exports = {
  mode: "production",
  entry: {
    index: "./js/index.js",
    editor: "./js/editor.js"
  },
  experiments: { asyncWebAssembly: true },
  output: {
    path: dist,
    filename: '[name].js',
  },
  module: {
    rules: [{
        test: /\.css$/,
        use: ['style-loader', 'css-loader']
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
    new MonacoWebpackPlugin({
      globalApi: true
     }),
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
