package com.gkvn.main;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.gkvn.lexer.SourceLexer;
import com.gkvn.parser.ASTBuilder;

public class Main {

	public static void main(String[] args) throws IOException {
		var cc = Files.readString(Path.of("testc/test6.c")).replace("CXX_COMPILER", "100");
		SourceLexer lex = new SourceLexer(cc);
		ASTBuilder syntaxTree = new ASTBuilder(lex);
		syntaxTree.build();
		System.out.println(syntaxTree.ast);
	}
}
