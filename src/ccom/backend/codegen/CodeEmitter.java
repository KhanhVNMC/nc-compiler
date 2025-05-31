package ccom.backend.codegen;

import java.util.ArrayList;
import java.util.List;

import ccom.ast.GlobalDefinitions.ProgramAST;

public class CodeEmitter {
    private List<String> instructions = new ArrayList<>();
    private int labelCounter = 0;

    public void emit(String line) {
        instructions.add(line);
    }

    public String newLabel(String base) {
        return base + "_" + (labelCounter++);
    }

    public void printProgram() {
        for (String instr : instructions) {
            System.out.println(instr);
        }
    }
}
