package com.gkvn.parser.ast.expressions.identifiables;

import com.gkvn.parser.ast.expressions.ExpressionNode;

public class SubscriptNode extends Identifiable {
	public ExpressionNode identifier;
	public ExpressionNode offset;

	public SubscriptNode(ExpressionNode identifier, ExpressionNode offset) {
		this.offset = offset;
		this.identifier = identifier;
	}

	@Override
	public String toString() {
		return "Subscript(" + identifier + "[" + offset + "])";
	}
}