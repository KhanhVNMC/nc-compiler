package com.gkvn.parser.ast;

import com.gkvn.lexer.Token;
import com.gkvn.lexer.TokenType;

public class TypeSpecifier {
	public static final TypeSpecifier UINT = new TypeSpecifier(TokenType.UINT);
	public static final TypeSpecifier CHAR = new TypeSpecifier(TokenType.CHAR);
	public static final TypeSpecifier VOID = new TypeSpecifier(TokenType.VOID);
	
	private final TokenType primitiveType;
	private final String definedType;

	public TypeSpecifier(TokenType primitive) {
		this.primitiveType = primitive;
		this.definedType = null;
	}

	public TypeSpecifier(Token type) {
		this.definedType = type.lexeme;
		this.primitiveType = null;
	}
	
	public static TypeSpecifier from(Token t) {
		if (t.type.isPrimitiveType()) {
			return switch (t.type) {
				case CHAR: {
					yield CHAR;
				}
				case UINT: {
					yield UINT;
				}
				case VOID: {
					yield VOID;
				}
				default: yield null;
			};
		}
		return new TypeSpecifier(t);
	}

	public boolean isPrimitive() {
		return definedType == null;
	}
	
	public boolean isTypedef() {
	    return !isPrimitive();
	}

	@Override
	public String toString() {
		if (isPrimitive()) {
			return "PrimitiveType(" + primitiveType + ")";
		} else {
			return "DefinedType(" + definedType + ")";
		}
	}
}