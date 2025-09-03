package com.gkvn.parser.ast.statements;

import com.gkvn.parser.ast.expressions.ExpressionNode;

/**
 * Represents a for loop.
 */
public class ForStatement extends StatementNode implements OptionalScopedStatement {
	public StatementNode initializer;
	public ExpressionNode condition;
	public StatementNode iteration;
	public StatementNode loopBody;

	public ForStatement(
		StatementNode initializer, ExpressionNode condition,  StatementNode iter,
		StatementNode loopBody
	) {
		this.initializer = initializer;
		this.condition = condition;
		this.iteration = iter;
		this.loopBody = loopBody;
	}

	@Override
	public String toString() {
		return "ForLoop(" + initializer + ", Condition(" + condition + ")," + iteration + ", Body{" + loopBody + "})";
	}

	@Override
	public boolean hasScopedBody() {
		if (loopBody instanceof OptionalScopedStatement oss) {
			return oss.hasScopedBody();
		}
		return loopBody instanceof ScopedStatements;
	}
}