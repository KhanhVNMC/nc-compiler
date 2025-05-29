package ccom;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import ccom.ast.AbstractSyntaxTree;

public class Main {

	public static void main(String[] args) throws IOException {
		var cc = Files.readString(Path.of("testc/s.c"));
		CompileLexer lex = new CompileLexer(cc);
		System.out.println(lex.scanTokens().size());
		//lex.scanTokens().forEach(tok -> System.out.println(tok));
		AbstractSyntaxTree syntaxTree = new AbstractSyntaxTree(lex);
		System.out.println(syntaxTree.parse());
	}
}
