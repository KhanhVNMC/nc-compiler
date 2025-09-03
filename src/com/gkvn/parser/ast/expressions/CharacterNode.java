package com.gkvn.parser.ast.expressions;

public class CharacterNode extends NumberNode {
	public CharacterNode(char c) {
		super((int) c);
	}

	@Override
	public String toString() {
		return "CharNode(" + ((char) value) + ")";
	}
}