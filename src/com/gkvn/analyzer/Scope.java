package com.gkvn.analyzer;

import java.util.HashMap;
import java.util.Map;

import com.gkvn.parser.ast.TypeSpecifier;

class Scope {
	final Map<String, TypeSpecifier> variables = new HashMap<>();
	final Scope parent;

	Scope(Scope parent) {
		this.parent = parent;
	}

	void defineVariable(String name, TypeSpecifier type) {
		variables.put(name, type);
	}

	TypeSpecifier lookupVariable(String name) {
		if (variables.containsKey(name)) {
			return variables.get(name);
		}
		if (parent != null) {
			return parent.lookupVariable(name);
		}
		return null;
	}
}
