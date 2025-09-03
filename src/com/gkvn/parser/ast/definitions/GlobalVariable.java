package com.gkvn.parser.ast.definitions;

import com.gkvn.parser.ast.statements.DeclarationStatement;

public class GlobalVariable extends GlobalDefinitionNode {
	public DeclarationStatement statement;
	
	public GlobalVariable(DeclarationStatement statement) {
		this.statement = statement;
	}
	
	@Override
	public String toString() {
		return "GlobalDeclaration(" + this.statement + ")";
	}
}
