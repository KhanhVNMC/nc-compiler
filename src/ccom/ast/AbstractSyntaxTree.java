package ccom.ast;

import java.util.ArrayList;
import java.util.List;

import ccom.CompileLexer;
import ccom.CompileToken.Token;
import ccom.CompileToken.TokenType;
import ccom.ast.Expressions.BinaryOpNode;
import ccom.ast.Expressions.ExpressionNode;
import ccom.ast.Expressions.Identifiable;
import ccom.ast.Expressions.IdentifiableExpression;
import ccom.ast.Expressions.IdentifierNode;
import ccom.ast.Expressions.MemberOf;
import ccom.ast.Expressions.NumberNode;
import ccom.ast.Expressions.SubscriptNode;
import ccom.ast.Expressions.UnaryArithmeticNode;
import ccom.ast.Expressions.UnaryOpNode;
import ccom.ast.GlobalDefinitions.FunctionDeclaration;
import ccom.ast.GlobalDefinitions.FunctionParam;
import ccom.ast.GlobalDefinitions.GlobalDefinitionNode;
import ccom.ast.GlobalDefinitions.ProgramAST;
import ccom.ast.GlobalDefinitions.StructDefinition;
import ccom.ast.GlobalDefinitions.GlobalVarDefinition;
import ccom.ast.Expressions.CallNode;
import ccom.ast.Expressions.CharacterNode;
import ccom.ast.Statements.DeclarationStatement;
import ccom.ast.Statements.DeclaredType;
import ccom.ast.Statements.ForStatement;
import ccom.ast.Statements.FuncReturnStatement;
import ccom.ast.Statements.ConditionBlock;
import ccom.ast.Statements.AssignmentStatment;
import ccom.ast.Statements.ScopedStatements;
import ccom.ast.Statements.StatementNode;
import ccom.ast.Statements.StatementedExpression;
import ccom.ast.Statements.WhileLoopBlock;

public class AbstractSyntaxTree {
	public List<Token> tokens;
	
	// token pointer
	int current;

	public AbstractSyntaxTree(CompileLexer lexer) {
		this.tokens = lexer.scanTokens();
	}
	
	/**
	 * @return the token at the current pointer, null if at end
	 */
	public Token peek() {
		return isAtEnd() ? null : this.tokens.get(current);
	}
	
	/**
	 * @return same as peek(), but advances the pointer
	 */
	public Token advance() {
		return this.tokens.get(current++);
	}
	
	/**
	 * @return true if the pointer is at the end of the token stream
	 */
	public boolean isAtEnd() {
		return this.current >= tokens.size();
	}
	
	/**
	 * Return true if the CURRENT (peek()) token match one of the given
	 * token types
	 * 
	 * Also advances the pointer forward, if matches
	 * 
	 * @param types given token types
	 * @return true if matches
	 */
	public boolean matchOneOf(TokenType... types) {
		for (TokenType type : types) {
			if (check(type)) {
				advance();
				return true;
			}
		}
		return false;
	}
	
	/**
	 *
	 */
	public boolean lookAheadIfMatch(TokenType... types) {
		int look = current; 
		for (TokenType type : types) {
			if (look >= tokens.size() || tokens.get(look).type != type) {
				return false;
			}
			look++;
		}
		return true;
	}
	
	/**
	 * @return true if the token at the pointer is the same as the given
	 * type, false if the pointer is at the end of the stream
	 * 
	 * Same as peek().type == type
	 */
	private boolean check(TokenType type) {
		return !isAtEnd() && peek().type == type;
	}
	
	/**
	 * @return the previous token (the one before the current pointer)
	 */
	public Token previous() {
		if (current == 0) throw new IllegalArgumentException("Pointer at 0!");
		return tokens.get(current - 1);
	}
	
	/**
	 * Consume a token (skips it), if does not match, throw an error
	 * @param type the type of token expected to consume
	 * @param message error message
	 * @return the consumed token (most of the time, FOR NOW, unused)
	 */
	public Token consume(TokenType type, String message) {
		// if token matches
		if (check(type)) {
			// advance pointer
			return advance();
		}
		throw new RuntimeException(message);
	}
	
	public ProgramAST parse() {
		ProgramAST ast = new ProgramAST();
		while (!isAtEnd() && !matchOneOf(TokenType.EOF)) {
			// parse struct
			switch (peek().type) {
			case STRUCT: {
				ast.structs.add(parseStructDefinition());
				break;
			}
			// for function and global var declarations
			case IDENTIFIER:
			case VOID:
			case CHAR:
			case UINT: {
				GlobalDefinitionNode parsed = parseGlobalDeclaration();
				if (parsed instanceof FunctionDeclaration) {
					ast.functions.add(parsed);
				} else if (parsed instanceof GlobalVarDefinition) {
					ast.globalVariables.add(parsed);
				} else {
					throw new RuntimeException("Something went wrong!");
				}
				break;
			}
			case SEMICOLON: {
				advance(); // skip semicolons
			}
			default:
				throw new IllegalArgumentException("Unexpected value: " + peek().type);
			}
		}
		return ast;
	}
	
	/**
	 * Parse a struct definition
	 * Example:
	 * struct Thingy {
	 *   uint a;
	 *   uint b[10];
	 * };
	 * 
	 * @return a StructDefinition object representing the struct
	 */
	public StructDefinition parseStructDefinition() {
	    consume(TokenType.STRUCT, "Expected 'struct' keyword");
	    
		// pretty much the same as declaring variables
	    Token structNameToken = consume(TokenType.IDENTIFIER, "Expected struct name");
	    IdentifierNode structName = new IdentifierNode(structNameToken.lexeme);
	    consume(TokenType.LBRACE, "Expected '{' to start struct body");
	    
	    List<DeclarationStatement> fields = new ArrayList<>();
	    // parse declarations until closing brace '}'
	    while (!matchOneOf(TokenType.RBRACE)) {
	        // skip extra semicolons
	        if (peek().type == TokenType.SEMICOLON) {
	            advance();
	            continue;
	        }
	        // parse a declaration within the struct body
	        Token typeToken = advance();  // advance to the type token of the field
	        fields.add(parseDeclaration(typeToken));
	        consume(TokenType.SEMICOLON, "Expected ';' after struct field declaration");
	    }
	    
        consume(TokenType.SEMICOLON, "Expected ';' after struct declaration");
	    return new StructDefinition(structName, fields);
	}
	
	/**
	 * Parse a function declaration AND global variables
	 * Example 1:
	 * void function() {
	 *     call();
	 *     if (thing) {
	 *     		...;
	 *     };
	 * }
	 * 
	 * Example 2:
	 * uint shit = 10;
	 * 
	 * @return a function object
	 */
	public GlobalDefinitionNode parseGlobalDeclaration() {
		// pretty much the same as declaring variables
		Token typeToken = advance();
		
		// type and pointer level (parse expected return type)
		DeclaredType type = null;
		int ptrLevel = 0; // int* = 1 ptrLevel

		// count pointers
		while (matchOneOf(TokenType.STAR)) {
			ptrLevel++;
		}
		
		// primitives (uint, char)
		// or void ptr (void*)
		if (typeToken.type == TokenType.UINT 
		|| typeToken.type == TokenType.CHAR
		|| (typeToken.type == TokenType.VOID && ptrLevel > 0)
		) {
			type = new DeclaredType(typeToken.type);
		} else if (typeToken.type == TokenType.IDENTIFIER) {
			// for typedefs and structs
			type = new DeclaredType(typeToken);
		} else {
			type = null;
		}
		
		// the name of the function
		Token ahead = consume(TokenType.IDENTIFIER, "Expected name");
		
		// parse function
		if (check(TokenType.LPAREN)) {
			return parseFunctionDeclaration(type, ptrLevel, ahead);
		}
		
		// if the user defines "void thingy;" error.
		if (type == null) {
			throw new RuntimeException("Void cannot be a type!");
		}
		
		// parse global variable definition
		GlobalVarDefinition global = new GlobalVarDefinition(
			// classic declaration statement, wrapped in global
			new DeclarationStatement(
				type, ptrLevel, 
				new IdentifierNode(ahead.lexeme), null // no initial value for now
			)
		);
		
		// if followed by '=', parse the expression that comes after it
		if (peek().type == TokenType.EQ) {
			advance(); // consume the '=' token
			global.statement.initialValue = parseExpression(); // parse the expr that comes after =
		} else if (peek().type == TokenType.LSQUARE) {
			advance(); // consume the opening bracket
			global.statement.isArray = true;
			global.statement.arraySize = parseExpression();
			consume(TokenType.RSQUARE, "Expected ']' after array declaration");
		}
		
		// expected a ';'
		consume(TokenType.SEMICOLON, "Expected ;");
		
		return global;
	}
	
	public FunctionDeclaration parseFunctionDeclaration(DeclaredType type, int ptrLevel, Token ahead) {
		// parse parameters "(void* param1, uint param2...)"
		consume(TokenType.LPAREN, "Expected '(' after function name declaration");
		List<FunctionParam> params = new ArrayList<>();
		// parse list of parameters
		if (peek().type != TokenType.RPAREN) {
			do {
				// reuse the declaration statement parser
				// for quickies
				params.add(FunctionParam.fromDeclaration(
					parseDeclaration(advance())
				));
			} while (matchOneOf(TokenType.COMMA)); // for each ",", a new param
		}
		consume(TokenType.RPAREN, "Expected ')' after function parameters");
		
		return new FunctionDeclaration(
			type, ptrLevel, new IdentifierNode(ahead.lexeme), // type[*] name
			params, // (params...)
			parseScopedBody() // { body }
		);
	}
	
	/**
	 * Recursively parses subscript expressions of the form identifier[index],
	 * supporting chained subscripts like array[0][1]. Converts them into nested
	 * SubscriptNode instances.
	 *
	 * @param expr The base identifiable expression
	 * @return An Identifiable representing the entire subscript chain
	 */
	private Identifiable parseSubscriptNodes(Identifiable expr) {
		Identifiable base = expr; // base node
		if (peek().type != TokenType.LSQUARE) return base;
		consume(TokenType.LSQUARE, "Expected '['");
		// parse the expression inside the brackets
		base = new SubscriptNode(base, parseExpression());
		consume(TokenType.RSQUARE, "Expected ']' after opening subscript node");
		// recursively check for and parse additional subscript expressions (e.g., `a[0][1]`).
		return parseSubscriptNodes(base);
	}

	/**
	 * Parses complex identifier expressions, including subscripted access
	 * and chained member access. Handles:
	 *
	 * - Simple identifiers:          `thing`
	 * - Subscripted identifiers:     `array[0]`
	 * - Member access:               `object.field` or `object->field`
	 * - Combined expressions:        `object[0]->field[1]`
	 *
	 * @param identifier The initial token representing the base identifier.
	 * @return An Identifiable representing the full parsed identifier chain.
	 */
	public Identifiable parseIdentifier(Token identifier) {
		// create a base identifier node
		Identifiable expr = parseSubscriptNodes(
			new IdentifierNode(identifier.lexeme)
		);
		TokenType next = peek().type;
		
		// parent -> child recursive
		if (next == TokenType.DOT || next == TokenType.ARROW) {
			consume(next, "Expected . or ->");
			// next children
			expr = new MemberOf(
				expr, parseIdentifier(advance()), // parent, child
				next == TokenType.ARROW // -> or .
			);
		}
		return expr;
	}
	
	///// STATEMENT PARSING START /////
	/**
	 * Parse a scoped (enclosed) block of code
	 * like { hello(); }
	 * @return a block of statements
	 */
	private ScopedStatements parseScopedBody() {
		if (!matchOneOf(TokenType.LBRACE)) return null;
		ScopedStatements body = new ScopedStatements();
		while (!matchOneOf(TokenType.RBRACE)) {
			// skips
			if (peek().type == TokenType.SEMICOLON) {
				advance(); // skip an extra semicolon
				continue;
			}
			// recursively parse freelance scopes
			// scopes within scopes
			if (peek().type == TokenType.LBRACE) {
				body.statements.add(parseScopedBody());
				continue;
			}
			// parse each NORMAL statement, line by line
			body.statements.add(parseStatement(0));
			// end of statement (must end with a semicolon)
			consume(TokenType.SEMICOLON, "Expected ;");
		}
		return body;
	}
	
	/**
	 * Parse a declaration statement (TYPE{ptrLevel} NAME [= INITIAL];)
	 * 
	 * @apiNote this is also used in {@link parseFunctionDeclaration()}, but
	 * INITIAL value is not permitted
	 * 
	 * @param typeToken the TYPE token
	 * @return a declaration statement (declare var)
	 */
	public DeclarationStatement parseDeclaration(Token typeToken) {
		// type and pointer level
		DeclaredType type = null;
		int ptrLevel = 0; // int* = 1 ptrLevel
		
		// count pointers
		while (matchOneOf(TokenType.STAR)) {
			ptrLevel++;
		}
		
		// the name of the declaration
		Token identifier = advance(); 
		
		// primitives (uint, char) 
		// or void ptr (void*)
		if (typeToken.type == TokenType.UINT 
		 || typeToken.type == TokenType.CHAR 
		 || (typeToken.type == TokenType.VOID && ptrLevel > 0)
		) {
			type = new DeclaredType(typeToken.type);
		} else if (typeToken.type == TokenType.IDENTIFIER) {
			// for typedefs and structs
			type = new DeclaredType(typeToken);
		} else {
			throw new RuntimeException("Void cannot be a type, should be a ptr!");
		}
		
		DeclarationStatement statement = new DeclarationStatement(
			type, ptrLevel, new IdentifierNode(identifier.lexeme), null
		);
		
		// if there is a =, its a declaration w/ assignment
		if (peek().type == TokenType.EQ) {
			advance(); // consume the '=' token
			statement.initialValue = parseExpression(); // assigned value
		} else if (peek().type == TokenType.LSQUARE) {
			advance(); // consume the opening bracket
			statement.isArray = true;
			statement.arraySize = parseExpression();
			consume(TokenType.RSQUARE, "Expected ']' after array declaration");
		}
		
		return statement;
	}
	
	/**
	 * Parse a statement (almost anything in the C-language, ends with a ;)
	 * 
	 * This is a recursive-friendly function
	 * 
	 * @param ptrLevel for recursive, default is 0
	 * @return a statement
	 */
	private StatementNode parseStatement(int ptrLevel) {
		// the first token of the statement
		Token token = advance();
		
		switch (token.type) {
			///// BEGIN: FOR CONDITIONALS AND LOOPS /////
			case IF: {
				// the initial condition
				consume(TokenType.LPAREN, "Expected ( after \"if\"");
				ExpressionNode condition = parseExpression();
				consume(TokenType.RPAREN, "Expected ) after \"if (\"");
				
				// the two body of the if block
				StatementNode ifBody = null;
				StatementNode elseBody = null;
				
				// if found a block then parse the body instead of a single
				// statement
				if (peek().type == TokenType.LBRACE) {
					ifBody = parseScopedBody();
				} else {
					ifBody = parseStatement(0);
				}
				
				// parse "else" block
				if (peek().type == TokenType.ELSE) {
					advance(); // consume the ELSE token
					// if found a block then parse the body instead 
					// of a single statement
					if (peek().type == TokenType.LBRACE) {
						elseBody = parseScopedBody();
					} else {
						elseBody = parseStatement(0);
					}
				}
				
				// construct if block with else (optional)
				return new ConditionBlock(condition, ifBody, elseBody);
			}
			
			case WHILE: {
				// runs until "whileCondition" is false
				consume(TokenType.LPAREN, "Expected ( after \"while\"");
				ExpressionNode whileCondition = parseExpression();
				consume(TokenType.RPAREN, "Expected ) after \"while (\"");
				
				StatementNode whileBody;
				
				// if found a block then parse the body
				// instead of a single statement
				if (peek().type == TokenType.LBRACE) {
					whileBody = parseScopedBody();
				} else {
					whileBody = parseStatement(0);
				}
				
				return new WhileLoopBlock(whileCondition, whileBody);
			}
			
			case FOR: {
				// initial statement
				consume(TokenType.LPAREN, "Expected ( after \"for\"");
				StatementNode initial = null;
				ExpressionNode condition = null;
				StatementNode run = null;
				// for (initial; condition; run) {}
				
				// check if the next token is a ';', if yes, skip
				if (peek().type != TokenType.SEMICOLON) {
					initial = parseStatement(0);
				}
				// the same...
				consume(TokenType.SEMICOLON, "Expected ; after initial");
				if (peek().type != TokenType.SEMICOLON) {
					condition = parseExpression();
				}
				consume(TokenType.SEMICOLON, "Expected ;");
				// if the last token is a ')', it is the end of the statement
				// skip the parsing
				if (peek().type != TokenType.RPAREN) {
					run = parseStatement(0);
				}
				consume(TokenType.RPAREN, "Expected ) after \"for (\"");
				
				// the main for body
				StatementNode forBody;
				
				// if found a block then parse the body
				// instead of a single statement
				if (peek().type == TokenType.LBRACE) {
					forBody = parseScopedBody();
				} else {
					forBody = parseStatement(0);
				}
				
				// for (initial; condition; run) { forBody }
				return new ForStatement(initial, condition, run, forBody);
			}
			
			///// END: FOR CONDITIONALS AND LOOPS /////
			
			///// BEGIN: FOR VAR DECLARATION AND ASSIGNMENT / FUNCTIONS /////
			case RETURN: {
				// if theres no expr after return, just an empty return
				if (peek().type == TokenType.SEMICOLON) {
					return new FuncReturnStatement(null);
				}
				
				// this is super simple, just parse the expression that
				// comes after return [expression];
				return new FuncReturnStatement(parseExpression());
			}
			
			// count de-reference pointer(s), like **a = thing;
			case STAR: {
				int pointerLevel = 1; // initial
				// count pointer levels
				while (matchOneOf(TokenType.STAR)) {
					pointerLevel++;
				}
				
				// recursive ptr parsing (next should be a statement)
				return parseStatement(pointerLevel);
			}
			case VOID:
			case UINT:
			case CHAR: {
				// for primitive types (uint, char)
				if (peek().type == TokenType.IDENTIFIER || peek().type == TokenType.STAR) {
					return parseDeclaration(token);
				}
				// wrong shit
				throw new RuntimeException("Error, primitive types must followed by variable declaration!");
			}
			// increments / decrements (pre) [++a]
			case PLUSPLUS:
			case MINUSMINUS: {
				if (peek().type != TokenType.IDENTIFIER) throw new RuntimeException("Pre-inc/dec unary expected an identifier");
				// parse the identifier with the first token, consuming it and its children
				Identifiable identifier = parseIdentifier(advance());
				// token type is PLUSPLUS or MINUSMINUS (++, --)
				return new StatementedExpression(new UnaryArithmeticNode(
					false, token.type, identifier
				));
			}
			// this could either be
			// Typedefed declare = thing;
			// OR Identifier (+ - * /)= thing;
			case LPAREN:
			case IDENTIFIER: {
				TokenType current = token.type;
				// function calls and declaration cant have '(' as 
				// the first token
				if (current == TokenType.IDENTIFIER) {
					// for Identifier (user-typedefed)
					if (peek().type == TokenType.IDENTIFIER 
						// make sure Typedef* does not conflict with expr *= 
					|| (peek().type == TokenType.STAR && !lookAheadIfMatch(TokenType.STAR, TokenType.EQ))) {
						return parseDeclaration(token);
					}
					
					// the token AFTER the identifier
					// parse function calls
					if (peek().type == TokenType.LPAREN) {
						advance(); // consume the '(' token
						return new StatementedExpression(
							this.parseFunctionCall(token, ptrLevel) 
						);
					}
				}
				
				// parse the identifier node
				Identifiable identifier = null;
				if (current == TokenType.LPAREN) {
					// for cases like *(&p + c)
					identifier = new IdentifiableExpression(parseExpression());
					consume(TokenType.RPAREN, "Expected enclosing ')'");
					
					// only allow pointer arithmetic to be used like this
					if (ptrLevel <= 0) {
						throw new RuntimeException("Expected pointer dereference for expression assignment!");
					}
				} else {
					// for normal cases
					identifier = parseIdentifier(token);
				}
				
				// the token after the parsed identifier [parent->child[here]]
				Token ahead = advance();
				
				switch (ahead.type) {
					// increments / decrements (post) [a++]
					case MINUSMINUS:
					case PLUSPLUS: {
						// ahead.type is PLUSPLUS or MINUSMINUS (++, --)
						return new StatementedExpression(new UnaryArithmeticNode(
							true, ahead.type, identifier
						));
					}
					// for assignments like
					// Identifier (+ - * /)= thing;
					case EQ:
					case MINUS:
					case PLUS:
					case STAR:
					case BSR:
					case BSL:
					case XOR:
					case OR:
					case SLASH: {
						// if the thing does not end with a = (* / + - or ...)
						if (ahead.type != TokenType.EQ) {
							consume(TokenType.EQ, "Expected '=' after operator for assignment!");
						}
						// assignment
						return new AssignmentStatment(
							identifier, ptrLevel, 
							ahead.type,
							parseExpression()
						);
					}
					default: {
						throw new RuntimeException(ahead + " found!");
					}
				}
			}
			///// END: FOR DECLARATION AND ASSIGNMENT / FUNCTIONS /////
			
			default: {
				throw new RuntimeException("Unhandled token: " + token);
			}
		}
	}
	
	///// STATEMENT PARSING ENDS /////
	
	/**
	 * Parse a function call
	 * @param funcName the token containing the function name
	 * @param ptrLevel the pointer level (for error throwing)
	 * @return
	 */
	private CallNode parseFunctionCall(Token funcName, int ptrLevel) {
		// syntax error
		if (ptrLevel > 0) {
			throw new RuntimeException("error");
		}
		List<ExpressionNode> args = new ArrayList<>();
		// if there is arguments, parse
		if (peek().type != TokenType.RPAREN) {
			do {
				// parse each expression
				args.add(parseExpression());
			} while (matchOneOf(TokenType.COMMA)); // for each ","
		}
		
		// a function should end with a )
		consume(TokenType.RPAREN, "Expected ')'");
		return new CallNode(
			new IdentifierNode(funcName.lexeme), args
		);
	}
 	
	// EXPRESSION PARSING START //
	/**
	 * Parse expression. E.g. a * (b + c) >= d<br>
	 * The process starts at the current token (peek())
	 * 
	 * This will consume the entire expression
	 * For example, we have:
	 * 
	 * 1 * (2 + 3)
	 * ^ POINTER HERE
	 * 
	 * After the parse
	 * 1 * (2 + 3)|
	 * 	          ^ POINTER HERE
	 * 
	 * @return a binary tree representing order of expressions
	 */
	private ExpressionNode parseExpression() {
		return parseCombinatory(); // this will initialize a chain
	}
	
	// AND, OR &&, ||
	private ExpressionNode parseCombinatory() {
		// parse comparisons first because precendence (combinatory -> comparisons -> plus -> multiply)
		ExpressionNode exp = parseComparisons();
		// keep marching forward
		while (true) {
			// if the next token is not &&, || return the expr
			if (!matchOneOf(
				TokenType.ANDAND, TokenType.OROR 
			)) {
				break;
			}
			Token operator = previous(); // because the pointer is currently on the token after the operator
			ExpressionNode right = parseComparisons(); // parse the higher priority
			// new branch of the binary tree
			exp = new BinaryOpNode(
				exp,  // left 
				operator.type, // op
				right // right
			);
		}
		
		return exp;
	}
	
	private ExpressionNode parseComparisons() {
		// parse add/sub first because precendence (comparisons -> plus -> multiply)
		ExpressionNode exp = parseAddOrSub();
		// keep marching forward
		while (true) {
			// if the next token is not <, >, >=, <=, ==, return the expr
			if (!matchOneOf(
				TokenType.GE, TokenType.LE, 
				TokenType.GT, TokenType.LT, 
				TokenType.EQEQ
			)) {
				break;
			}
			Token operator = previous(); // because the pointer is currently on the token after the operator
			ExpressionNode right = parseAddOrSub(); // parse the higher priority
			// new branch of the binary tree
			exp = new BinaryOpNode(
				exp,  // left 
				operator.type, // op
				right // right
			);
		}
		
		return exp;
	}
	
	private ExpressionNode parseAddOrSub() {
		// parse multi first because precendence (plus -> multiply)
		ExpressionNode exp = parseMulOrDiv();
		// keep marching forward
		while (true) {
			// if the next token (operator) is not +, -, >> or <<, return the parsed expr
			if (!matchOneOf(
				TokenType.BSL, TokenType.BSR, 
				TokenType.PLUS, TokenType.MINUS
			)) {
				break;
			}
			Token operator = previous(); // because the pointer is currently on the token after the operator
			ExpressionNode right = parseMulOrDiv(); // * / first, + - later
			// new branch of the binary tree
			exp = new BinaryOpNode(
				exp,  // left 
				operator.type, // op
				right // right
			);
		}
		
		return exp;
	}
	
	private ExpressionNode parseMulOrDiv() {
		// parse either number or identifier
		ExpressionNode exp = parseUnary();
		while (true) {
			// if the next token (operator) is not * or /, return the parsed expr
			if (!matchOneOf(TokenType.STAR, TokenType.SLASH)) {
				break;
			}
			Token operator = previous(); // same thing
			ExpressionNode right = parseUnary();
			// new branch of the binary tree
			exp = new BinaryOpNode(
				exp,  // left 
				operator.type, // op
				right // right
			);
		}
		return exp;
	}
	
	/**
	 * @return parse an expression, with unary operator
	 */
	private ExpressionNode parseUnary() {
		// prefixes (-, +, &, *, !)
		if (matchOneOf(
			TokenType.BANG, // !negate
			TokenType.MINUS, // -negative
			TokenType.AMPERSAND, // &pointer
			TokenType.STAR, // *deref
			TokenType.PLUS // +plus
		)) {
			Token operator = previous();
			ExpressionNode right = parseUnary(); // recursion
			return new UnaryOpNode(operator.type, right);
		}	
		
		return parsePrimary(); // base case
	}
	
	/**
	 * Parse expression's operands, can range from simple
	 * numbers to identifiers, pointers, and function calls
	 * 
	 * This also recursively parse enclosed expressions like (1+1)
	 * 
	 * @return the expression node representing it
	 */
	private ExpressionNode parsePrimary() {
		Token token = advance();
		switch (token.type) {
		case LITERAL_CHAR:
			return new CharacterNode(token.lexeme.charAt(1));
		case NUMBER:
			return new NumberNode(Integer.parseInt(token.lexeme));
		case IDENTIFIER:
			// look one token ahead of the identifier
			TokenType next = peek().type;
			// a function is also be a node
			if (next == TokenType.LPAREN) {
				advance(); // we consume the next token (to advance the pointer forward)
				// token here is the function name
				return this.parseFunctionCall(token, 0);
			}
			
			// parse the identifier node
			Identifiable identifier = parseIdentifier(token);
			// the token after the [parent->child[here]]
			TokenType afterIdentifier = peek().type;
			
			// parse ++ and --
			if (afterIdentifier == TokenType.PLUSPLUS || afterIdentifier == TokenType.MINUSMINUS) {
				advance(); // we consume the next token (to advance the pointer forward)
				return new UnaryArithmeticNode(
					false, afterIdentifier, identifier
				);
			}
			
			// parse offset identifier[thingy]
			if (afterIdentifier == TokenType.LSQUARE) {
				return parseSubscriptNodes(identifier);
			}
			
			return identifier;
		case PLUSPLUS:
		case MINUSMINUS: // for ++identifier
			// look one token ahead of the identifier
			Token after = peek();
			if (after.type != TokenType.IDENTIFIER) throw new RuntimeException("dit me may bien dau");
			
			// consume the identifier after ++ or --
			Identifiable suffixIdentifier = parseIdentifier(advance());
			return new UnaryArithmeticNode(
				true, token.type, 
				suffixIdentifier // parse the rest
			);
		case LPAREN: // recursively parse enclosed expressions (inside parentheses)
			ExpressionNode expr = parseExpression();
			// if not enclosed by a ), error
			consume(TokenType.RPAREN, "Expected ')'");
			return expr;
		case TRUE:
		case FALSE:
			return new NumberNode(token.type == TokenType.TRUE ? 1 : 0);
		default:
			throw new RuntimeException(token + ". Expected expression");
		}
	}
	// EXPRESSION PARSING ENDS
}
