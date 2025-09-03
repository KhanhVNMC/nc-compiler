package com.gkvn.parser.ast;

import com.gkvn.lexer.Token;
import com.gkvn.lexer.TokenType;

public class TypeSpecifier {
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