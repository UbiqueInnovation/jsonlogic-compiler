package ch.ubique.aifc;
// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT


public class AifcCompiler {
    static {
        System.loadLibrary("aifc");
    }

    public String compileAifc(String input, boolean minify) throws AifcCompilerError {
        return this.compile(input, minify);
    }
    private native String compile(String input, boolean minify);
}