package com.gkvn.parser.ast.expressions;

/**
 * This is a special node, special in a case that its defined once
 * in the assembly, then refer later by address
 */
public class StringLiteralNode extends ExpressionNode {
	public String string;
	
	public StringLiteralNode(String str) {
		this.string = str + '\0';
	}
	
	@Override
	public String toString() {
		return "StringLiteral(" + string + ")";
	}
}