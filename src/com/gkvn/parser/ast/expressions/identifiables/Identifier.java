package com.gkvn.parser.ast.expressions.identifiables;

import com.gkvn.parser.ast.expressions.ExpressionNode;

public class Identifier extends ExpressionNode {
	private final String name;
	
	public Identifier(String name) {
		this.name = name;
	}
	
	public String name() {
		return name;
	}
	
	@Override
	public String toString() {
		return "IdentifierNode(" + name + ")";
	}
}