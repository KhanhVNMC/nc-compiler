package com.gkvn.analyzer;

import java.util.HashMap;
import java.util.Map;

import com.gkvn.parser.AbstractSyntaxTree;
import com.gkvn.parser.ast.TypeSpecifier;
import com.gkvn.parser.ast.definitions.FunctionDefinition;
import com.gkvn.parser.ast.definitions.GlobalDefinitionNode;
import com.gkvn.parser.ast.definitions.GlobalVariable;
import com.gkvn.parser.ast.definitions.StructDefinition;
import com.gkvn.parser.ast.expressions.BinaryOpNode;
import com.gkvn.parser.ast.expressions.ExpressionNode;
import com.gkvn.parser.ast.expressions.NumberNode;
import com.gkvn.parser.ast.expressions.UnaryOpNode;
import com.gkvn.parser.ast.expressions.identifiables.Identifier;
import com.gkvn.parser.ast.statements.DeclarationStatement;

public class SemanticAnalyzer {
	private final AbstractSyntaxTree ast;

	// symbol tables
	private final Map<String, TypeSpecifier> globals = new HashMap<>();
	private final Map<String, StructDefinition> structs = new HashMap<>();
	private final Map<String, FunctionDefinition> functions = new HashMap<>();

	private Scope currentScope = new Scope(null); // global

	private void enterScope() {
		currentScope = new Scope(currentScope);
	}

	private void leaveScope() {
		currentScope = currentScope.parent;
	}

	public SemanticAnalyzer(AbstractSyntaxTree ast) {
		this.ast = ast;
	}

	// TODO
	private void error(String error) {
		throw new RuntimeException(error);
	}

	public void analyze() {
		collectSymbols();
		for (GlobalDefinitionNode func : ast.functions) {
			analyzeFunction(func);
		}
	}

	private void collectSymbols() {
		for (GlobalVariable gv : ast.globalVariables) {
			DeclarationStatement decl = gv.statement;
			String name = decl.identifier().name();
			if (globals.containsKey(name)) {
				error("Redefinition of global variable " + name);
			}
			globals.put(name, decl.type());
		}

		for (StructDefinition struct : ast.structs) {
			String name = struct.identifier().name();
			if (structs.containsKey(name)) {
				error("Redefinition of struct " + name);
			}
			structs.put(name, struct);
		}

		for (FunctionDefinition func : ast.functions) {
			String name = func.identifier().name();
			if (functions.containsKey(name)) {
				error("Redefinition of function " + name);
			}
			functions.put(name, func);
		}
	}

	private TypeSpecifier analyzeExpr(ExpressionNode expr) {
		if (expr instanceof NumberNode num) {
			expr.setResolvedType(TypeSpecifier.UINT); // default int
			expr.setLvalue(false);
			return expr.getResolvedType();
		}
		if (expr instanceof Identifier id) {
			TypeSpecifier type = currentScope.lookupVariable(id.name());
			expr.setResolvedType(type);
			expr.setLvalue(true);
			return type;
		}
		if (expr instanceof BinaryOpNode bin) {
			TypeSpecifier lt = analyzeExpr(bin.left());
			bin.setResolvedType(lt);
			bin.setLvalue(false);
			return lt;
		}
		if (expr instanceof UnaryOpNode un) {
			// TODO
			return un.getResolvedType();
		}
		if (expr instanceof CCastNode cast) {
			TypeSpecifier target = cast.getType();
			TypeSpecifier src = analyzeExpr(cast.getToCast());
			cast.setResolvedType(target);
			cast.setLvalue(false);
			return target;
		}
		throw new RuntimeException("Unsupported expr " + expr);
	}

}
