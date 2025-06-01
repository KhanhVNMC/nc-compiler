package ccom.backend.codegen;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import ccom.CompileLexer;
import ccom.ast.AbstractSyntaxTree;
import ccom.ast.Expressions.BinaryOpNode;
import ccom.ast.Expressions.ExpressionNode;
import ccom.ast.GlobalDefinitions.FunctionDeclaration;
import ccom.ast.GlobalDefinitions.ProgramAST;
import ccom.ast.Statements.DeclarationStatement;
import ccom.ast.Statements.StatementNode;
import ccom.backend.codegen.StructTable.*;
import ccom.backend.codegen.SymbolTable.Symbol;

public class TestOrder {
	public static void mains(String[] args) throws IOException {
		var cc = Files.readString(Path.of("testc/s.c"));
		CompileLexer lex = new CompileLexer(cc);
		// lex.scanTokens().forEach(tok -> System.out.println(tok));
		AbstractSyntaxTree syntaxTree = new AbstractSyntaxTree(lex);

		ProgramAST tree = (syntaxTree.parse());
		StatementNode node = ((FunctionDeclaration) tree.functions.get(0)).body.statements.get(0);

		ExpressionNode initialValue = ((DeclarationStatement) node).initialValue;
		List<String> strings = new ArrayList<String>();

		((BinaryOpNode) initialValue).emitAsm(strings, initialValue);

		strings.forEach(s -> System.out.println(s));

	}
}
