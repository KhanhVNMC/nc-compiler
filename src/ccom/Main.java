package ccom;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import ccom.CompileToken.TokenType;
import ccom.ast.AbstractSyntaxTree;

public class Main {

	public static void main(String[] args) throws IOException {
		var cc = Files.readString(Path.of("testc/test5.c"));
		System.out.println(cc);
		CompileLexer lex = new CompileLexer(cc);
		//lex.scanTokens().forEach(tok -> System.out.println(tok));
		AbstractSyntaxTree syntaxTree = new AbstractSyntaxTree(lex);
		System.out.println(syntaxTree.parse());
	}

}
