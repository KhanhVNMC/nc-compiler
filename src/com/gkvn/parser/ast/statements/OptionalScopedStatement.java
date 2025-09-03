package com.gkvn.parser.ast.statements;

public interface OptionalScopedStatement {
	/**
	 * @return true if the body of the statement (like if, else, while, for, etc)
	 * is a scoped body { hello(); } and not an independent statement
	 */
	public boolean hasScopedBody();
}
