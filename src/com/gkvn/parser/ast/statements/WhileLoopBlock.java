package com.gkvn.parser.ast.statements;

import com.gkvn.parser.ast.expressions.ExpressionNode;

/**
 * Represents a while loop construct.
 */
public class WhileLoopBlock extends StatementNode implements OptionalScopedStatement {
	public ExpressionNode condition;
	public StatementNode whileBody;
	
	public WhileLoopBlock(ExpressionNode condition, StatementNode whileBody) {
		this.condition = condition;
		this.whileBody = whileBody;
	}
	
	@Override
	public boolean hasScopedBody() {
		return whileBody instanceof ScopedStatements;
	}
	
	@Override
	public String toString() {
		return "WhileLoop(Condition(" + condition + "), Body{" + whileBody + "})";
	}
}

