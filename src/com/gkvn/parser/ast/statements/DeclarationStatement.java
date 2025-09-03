package com.gkvn.parser.ast.statements;

import com.gkvn.parser.ast.TypeSpecifier;
import com.gkvn.parser.ast.expressions.ArrayLiteral;
import com.gkvn.parser.ast.expressions.ExpressionNode;
import com.gkvn.parser.ast.expressions.StructLiteral;
import com.gkvn.parser.ast.expressions.identifiables.Identifiable;

public class DeclarationStatement extends StatementNode {
	// rudamentary components of a declaration
	private TypeSpecifier type;
	private int pointerLevel;
	private Identifiable identifier;
	
	// assignments
	private ExpressionNode initialValue;
	private ExpressionNode arraySize; // new field (for type thing[num])

	/**
	 * Constructor for scalar (non-array) variable declarations.
	 */
	private DeclarationStatement(
		TypeSpecifier type, int ptrLevel, Identifiable name, 
		ExpressionNode sizeValue, // for rudamentary arrays, like "uint a[10]"
		ExpressionNode initialValue
	) {
		this.type = type;
		this.pointerLevel = ptrLevel;
		this.identifier = name;
		this.arraySize = sizeValue;
		this.initialValue = initialValue;
	}
	
	public boolean isPointer() {
		return this.pointerLevel > 0;
	}
	
	public boolean isArray() {
		return this.arraySize != null || initialValue instanceof ArrayLiteral;
	}
	
	public boolean isStructDeclared() {
		return initialValue instanceof StructLiteral;
	}
	
	// factory methods
	public static DeclarationStatement variable(
		TypeSpecifier type, int ptrLevel, Identifiable name, ExpressionNode init
	) {
		return new DeclarationStatement(type, ptrLevel, name, null, init);
	}
	
	public static DeclarationStatement struct(
		TypeSpecifier type, int ptrLevel, Identifiable name, StructLiteral init
	) {
		return new DeclarationStatement(type, ptrLevel, name, null, init);
	}
	
	public static DeclarationStatement arrayWithSize(
		TypeSpecifier type, int ptrLevel, Identifiable name, ExpressionNode sizeExpression
	) {
		return new DeclarationStatement(type, ptrLevel, name, 
			sizeExpression /*sized array*/, null /*e.g. uint a[3];*/
		);
	}
	
	public static DeclarationStatement array(
		TypeSpecifier type, int ptrLevel, Identifiable name, ArrayLiteral arrayLiteral
	) {
		return new DeclarationStatement(type, ptrLevel, name, 
			null /*unsized array*/, arrayLiteral /*e.g. {1,2,3}*/
		);
	}
	
	@Override
	public String toString() {
		if (isArray()) {
			return "DeclareArray(" + type + ", " + pointerLevel + ", " + identifier + "[" + arraySize + "])";
		}
		return "DeclareVariable(" + type + ", " + pointerLevel + ", " + identifier + ", " + initialValue + ")";
	}
}