package com.gkvn.parser.ast.expressions;

import com.gkvn.lexer.TokenType;

public class UnaryArithmeticNode extends ExpressionNode {
	public TokenType operator;
	public ExpressionNode expression;
	public boolean post = false;

	public UnaryArithmeticNode(boolean post, TokenType op, ExpressionNode expr) {
		this.expression = expr;
		this.operator = op;
		this.post = post;
	}
	
	public static UnaryArithmeticNode postfix(TokenType operator, ExpressionNode expression) {
		return new UnaryArithmeticNode(true, operator, expression);
	}
	
	public static UnaryArithmeticNode prefix(TokenType operator, ExpressionNode expression) {
		return new UnaryArithmeticNode(false, operator, expression);
	}

	@Override
	public String toString() {
		return "UANode(" + operator + ", " + expression + ", " + post + ")";
	}
}