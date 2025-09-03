package com.gkvn.parser.ast.expressions;

public class NumberNode extends ExpressionNode {
	public int value;

	public NumberNode(int value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return "NumberNode(" + value + ")";
	}
}
