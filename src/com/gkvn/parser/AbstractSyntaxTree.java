package com.gkvn.parser;

import java.util.ArrayList;
import java.util.List;

import com.gkvn.parser.ast.definitions.GlobalDefinitionNode;

public class AbstractSyntaxTree {
	protected final List<GlobalDefinitionNode> globalVariables = new ArrayList<>();
	protected final List<GlobalDefinitionNode> structs = new ArrayList<>();
	protected final List<GlobalDefinitionNode> functions = new ArrayList<>();

	public AbstractSyntaxTree() {}
	
	@Override
	public String toString() {
		return "AbstractSyntaxTree{" + globalVariables + ",\n" + structs + ",\n" + functions + "}";
	}
}
