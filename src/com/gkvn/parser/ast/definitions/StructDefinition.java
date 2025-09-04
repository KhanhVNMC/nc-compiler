package com.gkvn.parser.ast.definitions;

import java.util.List;

import com.gkvn.parser.ast.expressions.identifiables.Identifier;
import com.gkvn.parser.ast.statements.DeclarationStatement;

/**
 * Represents a struct declaration
 */
public class StructDefinition extends GlobalDefinitionNode {
    private Identifier name;
    private List<DeclarationStatement> fields;

    public StructDefinition(Identifier name, List<DeclarationStatement> fields) {
        this.name = name;
        this.fields = fields;
    }
    
    public Identifier identifier() {
    	return name;
    }
    
    public List<DeclarationStatement> fields() {
    	return this.fields;
    }

    @Override
    public String toString() {
        return "DeclareStruct(" + name + ", fields: " + fields + ")";
    }
}