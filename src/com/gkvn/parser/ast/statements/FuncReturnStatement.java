package com.gkvn.parser.ast.statements;

import com.gkvn.parser.ast.expressions.ExpressionNode;

/**
 * Represents a return statement in a function.
 */
public class FuncReturnStatement extends StatementNode {
	public ExpressionNode returnValue;

	public FuncReturnStatement(ExpressionNode returnValue) {
		this.returnValue = returnValue;
	}

	@Override
	public String toString() {
		return "ReturnFunc(" + returnValue + ")";
	}
}
