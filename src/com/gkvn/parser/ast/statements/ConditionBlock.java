package com.gkvn.parser.ast.statements;

import com.gkvn.parser.ast.expressions.ExpressionNode;

/**
 * Represents an if-else conditional block.
 */
public class ConditionBlock extends StatementNode implements OptionalScopedStatement {
	public ExpressionNode condition;
	public StatementNode ifBody;
	public StatementNode elseBody;
	
	public ConditionBlock(ExpressionNode condition, StatementNode ifbody, StatementNode elsebody) {
		this.condition = condition;
		this.ifBody = ifbody;
		this.elseBody = elsebody;
	}
	
	@Override
	public boolean hasScopedBody() {
		if (elseBody instanceof OptionalScopedStatement oss) {
			return oss.hasScopedBody();
		}
		if (elseBody == null) return ifBody instanceof ScopedStatements;
		else return elseBody instanceof ScopedStatements;
	}
	
	@Override
	public String toString() {
		return "If(Condition(" + condition + "), Body{" + ifBody + "}, Else(Body{" + elseBody + "}))";
	}
}