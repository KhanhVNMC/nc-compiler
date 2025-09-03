package com.gkvn.parser.ast.expressions;

import java.util.List;

public class CallNode extends ExpressionNode {
	public ExpressionNode callee;
	public List<ExpressionNode> arguments;
	
	public CallNode(ExpressionNode expr, List<ExpressionNode> node) {
		this.callee = expr;
		this.arguments = node;
	}
	
	@Override
	public String toString() {
		return "CallNode(" + callee.toString() + ", args: " + arguments.toString() + ")";
	}
}