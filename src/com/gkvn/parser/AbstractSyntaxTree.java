package com.gkvn.parser;

import java.util.ArrayList;
import java.util.List;

import com.gkvn.parser.ast.definitions.FunctionDefinition;
import com.gkvn.parser.ast.definitions.GlobalVariable;
import com.gkvn.parser.ast.definitions.StructDefinition;

public class AbstractSyntaxTree {
	public final List<GlobalVariable> globalVariables = new ArrayList<>();
	public final List<StructDefinition> structs = new ArrayList<>();
	public final List<FunctionDefinition> functions = new ArrayList<>();

	public AbstractSyntaxTree() {}
	
	@Override
	public String toString() {
		return "AbstractSyntaxTree{" + globalVariables + ",\n" + structs + ",\n" + functions + "}";
	}
}
