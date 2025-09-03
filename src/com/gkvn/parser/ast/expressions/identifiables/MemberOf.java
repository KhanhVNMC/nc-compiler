package com.gkvn.parser.ast.expressions.identifiables;

import com.gkvn.parser.ast.expressions.ExpressionNode;

public class MemberOf extends Identifiable {
	public ExpressionNode parent;
	public ExpressionNode child;
	public boolean arrow = false;
	
	public MemberOf(ExpressionNode parent, ExpressionNode child, boolean arrow) {
		this.parent = parent;
		this.child = child;
		this.arrow = arrow;
	}
	
	@Override
	public String toString() {
		return "MemberOf(" + parent + " " + (arrow ? "->" : ".") + " " + child + ")";
	}
}

