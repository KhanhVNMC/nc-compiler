package com.gkvn.parser.ast.statements;

import com.gkvn.parser.ast.expressions.ExpressionNode;

public class ExpressionStatement extends StatementNode {
	public ExpressionNode expression;
	
	public ExpressionStatement(ExpressionNode expressionNode) {
		this.expression = expressionNode;
	}
	
	@Override
	public String toString() {
		return expression.toString();
	}
}