// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT
import ch.ubique.aifc.AifcCompiler;
import ch.ubique.aifc.AifcCompilerError;

public class Main {
    private static String input = "a === b";
    public static void main(String[] args) {
        var c = new AifcCompiler();
       try {
        System.out.println(c.compileAifc(input, false));
    } catch (AifcCompilerError e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    }
    }
}
