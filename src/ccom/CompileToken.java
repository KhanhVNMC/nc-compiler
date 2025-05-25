package ccom;

public class CompileToken {
	public static enum TokenType {
	    UINT, CHAR, VOID, RETURN, IF, ELSE, WHILE, FOR,
	    IDENTIFIER, NUMBER, STRING, LITERAL_CHAR,
	    PLUS, PLUSPLUS, MINUS, MINUSMINUS, 
	    STAR, SLASH, BANG,
	    EQ, EQEQ, LT, GT, LE, GE, COMMA, DOT, ARROW,
	    LPAREN, RPAREN, LBRACE, RBRACE,
	    SEMICOLON, COLON, AMPERSAND, 
	    ANDAND, OR, OROR, BSR, BSL, XOR,
	    EOF, TYPEDEF, STRUCT, LSQUARE, RSQUARE,
	    TRUE, FALSE, DIFF
	}
	
	public static class Token {
	    public final TokenType type;
	    public final String lexeme;
	    public final int line;
	    public final int column;
	    
	    public Token(TokenType type, String lexeme, int line, int column) {
	        this.type = type;
	        this.lexeme = lexeme;
	        this.line = line;
	        this.column = column;
	    }

	    public String toString() {
	        return type + " '" + lexeme + "'";
	    }
	}

}
