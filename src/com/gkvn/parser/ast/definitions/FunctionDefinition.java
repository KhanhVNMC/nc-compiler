package com.gkvn.parser.ast.definitions;

import java.util.List;

import com.gkvn.parser.ast.TypeSpecifier;
import com.gkvn.parser.ast.expressions.identifiables.Identifier;
import com.gkvn.parser.ast.statements.DeclarationStatement;
import com.gkvn.parser.ast.statements.ScopedStatements;

/**
 * Represents a function declaration.
 */
public class FunctionDefinition extends GlobalDefinitionNode {
	private TypeSpecifier returnType; // null == void
	private int returnPtrLevel = 0; // the return ptr level (Eg. void**)
	private Identifier identifier; // the func name
	private List<DeclarationStatement> parameters; 
	private ScopedStatements body;
	
	public FunctionDefinition(TypeSpecifier type, int ptrLevel, Identifier name, List<DeclarationStatement> params, ScopedStatements body) {
		this.returnType = type;
		this.identifier = name;
		this.returnPtrLevel = ptrLevel;
		this.parameters = params;
		this.body = body;
	}
	
	public Identifier identifier() {
		return identifier;
	}
	
	@Override
	public String toString() {
		return "DeclareFunc(" + returnType + ", ptrLevel: " + returnPtrLevel + ", " + identifier + ", params: " + parameters + ", Body{" + body + "})";
	}
}