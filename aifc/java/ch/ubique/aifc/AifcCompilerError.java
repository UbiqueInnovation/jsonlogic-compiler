package ch.ubique.aifc;
// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

public class AifcCompilerError extends Exception {
    public AifcCompilerError(String input) {
        super(input);
    }
}
