package com.gkvn.parser.ast.expressions.identifiables;

public class Identifier extends Identifiable {
	private String name;
	
	public Identifier(String name) {
		this.name = name;
	}
	
	@Override
	public String toString() {
		return "IdentifierNode(" + name + ")";
	}
}