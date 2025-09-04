package com.gkvn.parser.ast.expressions;

import com.gkvn.parser.ast.TypeSpecifier;

public class CCastNode extends ExpressionNode {
	final TypeSpecifier type;
	final int ptrLevel;
	final ExpressionNode toCast;
	
	public CCastNode(TypeSpecifier type, int ptrLevel, ExpressionNode expr) {
		this.type = type;
		this.ptrLevel = ptrLevel;
		this.toCast = expr;
	}
	
	@Override
	public String toString() {
		return "CastNode(" + type + ", " + ptrLevel + ", toCast: " + toCast.toString() + ")";
	}
}
