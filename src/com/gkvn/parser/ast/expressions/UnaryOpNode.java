package com.gkvn.parser.ast.expressions;

import com.gkvn.lexer.TokenType;

public class UnaryOpNode extends ExpressionNode {
	public TokenType operator;
	public ExpressionNode expression;

	public UnaryOpNode(TokenType op, ExpressionNode expr) {
		this.expression = expr;
		this.operator = op;
	}

	@Override
	public String toString() {
		return "UnaryNode(" + operator + ", " + expression + ")";
	}
}