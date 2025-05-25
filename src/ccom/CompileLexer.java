package ccom;

import java.util.ArrayList;
import java.util.List;

import ccom.CompileToken.Token;
import ccom.CompileToken.TokenType;

public class CompileLexer {
	private final String source;
	private final List<Token> tokens = new ArrayList<>();
	private int start = 0;
	private int current = 0;
	private int line = 1;

	public CompileLexer(String source) {
		this.source = source;
	}

	public List<Token> scanTokens() {
		// loop until the thing has not ended, if not ended, assign the
		// current char index to the current one, and then scan the token
		while (!isAtEnd()) {
			// assign
			start = current;
			// tokenize syntax shit
			scanToken();
		}
		
		// end of file
		tokens.add(new Token(TokenType.EOF, "", line, current));
		return tokens;
	}

	private void scanToken() {
		char c = advance();
		
		// handle characters
		switch (c) {
		case '(':
			addToken(TokenType.LPAREN);
			break;
		case ')':
			addToken(TokenType.RPAREN);
			break;
		case '{':
			addToken(TokenType.LBRACE);
			break;
		case '}':
			addToken(TokenType.RBRACE);
			break;
		case '+':
			addToken(match('+') ? TokenType.PLUSPLUS : TokenType.PLUS);
			break;
		case '-':
			if (match('>')) {
				addToken(TokenType.ARROW);
				break;
			}
			addToken(match('-') ? TokenType.MINUSMINUS : TokenType.MINUS);
			break;
		case '/':
			addToken(TokenType.SLASH);
			break;
		case '=':
			// if the next token after = is "=" too, then EQEQ
			addToken(match('=') ? TokenType.EQEQ : TokenType.EQ);
			break;
		case '*':
			addToken(TokenType.STAR);
			break;
		case '|':
			addToken(match('|') ? TokenType.OROR : TokenType.OR);
			break;
		case '&':
			addToken(match('&') ? TokenType.ANDAND : TokenType.AMPERSAND);
			break;
		case '<':
			if (match('=')) {
				addToken(TokenType.LE);
				break;
			}
			addToken(match('<') ? TokenType.BSL : TokenType.LT);
			break;
		case '>':
			if (match('=')) {
				addToken(TokenType.GE);
				break;
			}
			addToken(match('>') ? TokenType.BSR : TokenType.GT);
			break;
		case '^':
			addToken(TokenType.XOR);
			break;
		case ' ':
		case '\r':
		case '\t':
			break; // ignore whitespaces
		case '\n':
			line++; // next line
			break;
		case ';':
			addToken(TokenType.SEMICOLON);
			break;
		case ':':
			addToken(TokenType.COLON);
			break;
		case ',':
			addToken(TokenType.COMMA);
			break;
		case '!':
			addToken(TokenType.BANG);
			break;
		case '.':
			addToken(TokenType.DOT);
			break;
		case '[':
			addToken(TokenType.LSQUARE);
			break;
		case ']':
			addToken(TokenType.RSQUARE);
			break;
		case '\'':
			charLiteral();
			break;
		case '"':
			stringLiteral();
			break;
		default:
			// if the number is digit, hand over the control to number();
			if (isDigit(c)) {
				number();
			} else if (isAlpha(c)) { // if found char, handle identifier
				identifier();
			} else {
				throw new RuntimeException("Unexpected character: " + c); // TODO
			}
		}
	}
	
	/**
	 * Scan the next identifier
	 */
	private void identifier() {
		while ( isAlphaNumeric(peek()) ) {
			advance();
		}
		// at this point, the current pointer would point at
		// hello|
		//      ^ here
		String lexeme = source.substring(start, current);
		addToken(keywordOrIdentifier(lexeme));
	}
	
	/**
	 * Scan the next number
	 */
	private void number() {
		while (isDigit(peek())) {
			advance();	
		}
		addToken(TokenType.NUMBER);
	}
	
	private void charLiteral() {
		if (isAtEnd()) throw new RuntimeException("Unterminated character literal");
		char c = advance();
		// Handle escape characters like '\n' or '\''
		if (c == '\\') {
			if (isAtEnd()) throw new RuntimeException("Unterminated escape in character literal");
			advance(); // skip the escaped char
		}
		if (peek() != '\'') throw new RuntimeException("Unterminated character literal or too many characters");
		advance(); // consume closing '
		addToken(TokenType.LITERAL_CHAR);
	}
	
	private void stringLiteral() {
		while (!isAtEnd() && peek() != '"') {
			if (peek() == '\n') line++; // allow multi-line strings if desired
			if (peek() == '\\') advance(); // skip escape prefix
			advance();
		}
		if (isAtEnd()) throw new RuntimeException("Unterminated string literal");
		advance(); // consume closing quote
		addToken(TokenType.STRING);
	}

	// Utility functions
	private boolean isAtEnd() {
		return current >= source.length();
	}
	
	/**
	 * @return same thing as peek(), but advances the pointer
	 */
	private char advance() {
		return source.charAt(current++);
	}
	
	/**
	 * @return the character at the current pointer, nullchar (\0) if at end
	 */
	private char peek() {
		return isAtEnd() ? '\0' : source.charAt(current);
	}
	
	/**
	 * If the character matches, advance forward (using advance())
	 * @param expected
	 * @return true if matches
	 */
	private boolean match(char expected) {
		if (isAtEnd() || peek() != expected) {
			return false;
		}
		this.advance();
		return true;
	}

	private void addToken(TokenType type) {
		// "start" is the beginning of a token (set at each interval of scanTokens())
		String lexeme = source.substring(start, current);
		tokens.add(new Token(type, lexeme, line, start));
	}

	/**
	 * @return true if number (digit 0-9)
	 */
	private boolean isDigit(char c) {
		return c >= '0' && c <= '9';
	}
	
	/**
	 * @return true if character (non-whitespace)
	 */
	private boolean isAlpha(char c) {
		return Character.isLetter(c) || c == '_';
	}
	
	/**
	 * @return true if either character or digit (a-Z, 0-9)
	 */
	private boolean isAlphaNumeric(char c) {
		return isAlpha(c) || isDigit(c);
	}

	private TokenType keywordOrIdentifier(String text) {
	    switch (text) {
	        case "if": return TokenType.IF;
	        case "else": return TokenType.ELSE;
	        case "while": return TokenType.WHILE;
	        case "for": return TokenType.FOR;
	        case "return": return TokenType.RETURN;
	        case "typedef": return TokenType.TYPEDEF;
	        case "struct": return TokenType.STRUCT;
	        case "uint": return TokenType.UINT;
	        case "char": return TokenType.CHAR;
	        case "void": return TokenType.VOID;
	        case "true": return TokenType.TRUE;
	        case "false": return TokenType.FALSE;
	        default: return TokenType.IDENTIFIER;
	    }
	}
}
