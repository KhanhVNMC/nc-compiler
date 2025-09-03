package com.gkvn.parser.ast.definitions;

import java.util.List;

import com.gkvn.parser.ast.TypeSpecifier;
import com.gkvn.parser.ast.expressions.identifiables.Identifiable;
import com.gkvn.parser.ast.statements.DeclarationStatement;
import com.gkvn.parser.ast.statements.ScopedStatements;

/**
 * Represents a function declaration.
 */
public class FunctionDeclaration extends GlobalDefinitionNode {
	public TypeSpecifier returnType; // null == void
	public int returnPtrLevel = 0; // the return ptr level (Eg. void**)
	public Identifiable identifier; // the func name
	public List<DeclarationStatement> parameters; 
	public ScopedStatements body;
	
	public FunctionDeclaration(TypeSpecifier type, int ptrLevel, Identifiable name, List<DeclarationStatement> params, ScopedStatements body) {
		this.returnType = type;
		this.identifier = name;
		this.returnPtrLevel = ptrLevel;
		this.parameters = params;
		this.body = body;
	}
	
	@Override
	public String toString() {
		return "DeclareFunc(" + returnType + ", ptrLevel: " + returnPtrLevel + ", " + identifier + ", params: " + parameters + ", Body{" + body + "})";
	}
}