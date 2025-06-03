package ccom.backend.codegen;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;

import ccom.CompileLexer;
import ccom.CompileToken.TokenType;
import ccom.ast.AbstractSyntaxTree;
import ccom.ast.Expressions.IdentifierNode;
import ccom.ast.Expressions.NumberNode;
import ccom.ast.GlobalDefinitions.ProgramAST;
import ccom.ast.GlobalDefinitions.StructDefinition;
import ccom.backend.codegen.StructTable.*;

public class TestOrder {
	SymbolTable scope = new SymbolTable();
	Map<String, StructLayout> declaredStructs = new LinkedHashMap<>();
	
	public static void main(String[] args) throws IOException {
		var cc = Files.readString(Path.of("testc/s.c"));
		CompileLexer lex = new CompileLexer(cc);
		// lex.scanTokens().forEach(tok -> System.out.println(tok));
		AbstractSyntaxTree syntaxTree = new AbstractSyntaxTree(lex);

		ProgramAST tree = (syntaxTree.parse());
		var e = new TestOrder(tree);
		e.createStructTable();
		System.out.println(e.declaredStructs);
	}
	
	ProgramAST ast;
	public TestOrder(ProgramAST ast) {
		this.ast = ast;
	}
	
	public void createStructTable() {
		ast.structs.forEach(dec -> {
			StructDefinition struct = (StructDefinition) dec;
			StructField fields[] = new StructField[struct.fields.size()];
			
			int i = 0;
			for (var structField : struct.fields) {
				if (!structField.type.isPrimitive()) {
					fields[i++] = new StructField(((IdentifierNode)structField.identifier).name, declaredStructs.get(structField.type.typedefType));
					continue;
				}
				int size = 0;
				if (structField.pointerLevel > 0) {
					size = 2;
				} else if (structField.type.isPrimitive()) {
					size = structField.type.primitiveType == TokenType.UINT ? 2 : 1;
				}
				
				if (structField.arraySize != null) size *= ((NumberNode)structField.arraySize).value;
				
				fields[i++] = new StructField(((IdentifierNode)structField.identifier).name, size);
			}
			
			StructLayout layout = new StructLayout(struct.name.name, fields);
			declaredStructs.put(struct.name.name, layout);
		});
	}
}
