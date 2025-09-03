package com.gkvn.parser.ast.definitions;

import java.util.List;

import com.gkvn.parser.ast.expressions.identifiables.Identifier;
import com.gkvn.parser.ast.statements.DeclarationStatement;

/**
 * Represents a struct declaration
 */
public class StructDefinition extends GlobalDefinitionNode {
    public Identifier name;
    public List<DeclarationStatement> fields;

    public StructDefinition(Identifier name, List<DeclarationStatement> fields) {
        this.name = name;
        this.fields = fields;
    }

    @Override
    public String toString() {
        return "DeclareStruct(" + name + ", fields: " + fields + ")";
    }
}