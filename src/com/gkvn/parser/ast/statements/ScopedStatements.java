package com.gkvn.parser.ast.statements;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a block of statements enclosed in a scope.
 */
public class ScopedStatements extends StatementNode {
	public List<StatementNode> statements = new ArrayList<>();
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder("Scoped{\n");
		statements.forEach(statement -> builder.append(statement).append(";\n"));
		return builder.append("}").toString();
	}
}