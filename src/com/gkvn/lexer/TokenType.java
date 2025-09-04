package com.gkvn.lexer;

import java.util.HashSet;
import java.util.Set;

public enum TokenType {
	// --- Keywords: types ---
	UINT, // unsigned int
	CHAR, // char
	VOID, // void

	// --- Keywords: control flow ---
	RETURN, // return
	IF, // if
	ELSE, // else
	WHILE, // while
	FOR, // for
	CONTINUE, // continue
	BREAK, // break

	// --- Identifiers & literals ---
	IDENTIFIER, // user-defined identifier
	NUMBER, // numeric literal
	STRING, // string literal
	LITERAL_CHAR, // character literal

	// --- Arithmetic operators ---
	PLUS, // +
	PLUSPLUS, // ++
	MINUS, // -
	MINUSMINUS, // --
	STAR, // *
	SLASH, // /
	MOD, // %

	// --- Logical / unary operators ---
	BANG, // !
	ANDAND, // &&
	OROR, // ||

	// --- Assignment & comparison ---
	EQ, // =
	ADDEQ, // +=
	SUBEQ, // -=
	DIVEQ, // /=
	MULEQ, // *=
	BSREQ, // >>=
	BSLEQ, // <<=
	OREQ, // |=
	ANDEQ, // &=
	XOREQ, // ^=
	MODEQ, // %=
	EQEQ, // ==
	DIFF, // !=
	LT, // <
	GT, // >
	LE, // <=
	GE, // >=

	// --- Bitwise/pointer operators ---
	AMPERSAND, // &
	OR, // |
	XOR, // ^
	BSR, // >>
	BSL, // <<
	TILDE, // ~

	// --- Punctuation / delimiters ---
	COMMA, // ,
	DOT, // .
	ARROW, // ->
	LPAREN, // (
	RPAREN, // )
	LBRACE, // {
	RBRACE, // }
	LSQUARE, // [
	RSQUARE, // ]
	SEMICOLON, // ;
	COLON, // :

	// --- Other keywords ---
	TYPEDEF, // typedef
	STRUCT, // struct
	TRUE, // true
	FALSE, // false

	// --- Special ---
	EOF; // end of file
	
	static final Set<TokenType> ASSIGNMENT_OPERATORS = Set.of(
		PLUSPLUS, // ++
		MINUSMINUS, // --
		EQ, // =
		ADDEQ, // +=
		SUBEQ, // -=
		DIVEQ, // /=
		MULEQ, // *=
		BSREQ, // >>=
		BSLEQ, // <<=
		OREQ, // |=
		ANDEQ, // &=
		XOREQ, // ^=
		MODEQ, // %=
		EQEQ, // ==
		DIFF // !=
	);
	
	public boolean isAssignmentOperator() {
		return ASSIGNMENT_OPERATORS.contains(this);
	}
	
	public boolean isStructAccessOperator() {
		return this == DOT || this == ARROW;
	}
}
