package com.gkvn.parser;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.ls.LSException;

import com.gkvn.lexer.SourceLexer;
import com.gkvn.lexer.Token;
import com.gkvn.lexer.TokenType;
import com.gkvn.parser.ast.TypeSpecifier;
import com.gkvn.parser.ast.definitions.FunctionDeclaration;
import com.gkvn.parser.ast.definitions.GlobalDefinitionNode;
import com.gkvn.parser.ast.definitions.GlobalVariable;
import com.gkvn.parser.ast.definitions.StructDefinition;
import com.gkvn.parser.ast.expressions.ArrayLiteral;
import com.gkvn.parser.ast.expressions.AssignmentNode;
import com.gkvn.parser.ast.expressions.BinaryOpNode;
import com.gkvn.parser.ast.expressions.CallNode;
import com.gkvn.parser.ast.expressions.CharacterNode;
import com.gkvn.parser.ast.expressions.ExpressionNode;
import com.gkvn.parser.ast.expressions.NumberNode;
import com.gkvn.parser.ast.expressions.StringLiteralNode;
import com.gkvn.parser.ast.expressions.StructLiteral;
import com.gkvn.parser.ast.expressions.UnaryArithmeticNode;
import com.gkvn.parser.ast.expressions.UnaryOpNode;
import com.gkvn.parser.ast.expressions.identifiables.Identifiable;
import com.gkvn.parser.ast.expressions.identifiables.Identifier;
import com.gkvn.parser.ast.expressions.identifiables.MemberOf;
import com.gkvn.parser.ast.expressions.identifiables.SubscriptNode;
import com.gkvn.parser.ast.statements.ConditionBlock;
import com.gkvn.parser.ast.statements.DeclarationStatement;
import com.gkvn.parser.ast.statements.ExpressionStatement;
import com.gkvn.parser.ast.statements.ForStatement;
import com.gkvn.parser.ast.statements.FuncReturnStatement;
import com.gkvn.parser.ast.statements.LoopBreakStatement;
import com.gkvn.parser.ast.statements.LoopContinueStatement;
import com.gkvn.parser.ast.statements.OptionalScopedStatement;
import com.gkvn.parser.ast.statements.ScopedStatements;
import com.gkvn.parser.ast.statements.StatementNode;
import com.gkvn.parser.ast.statements.WhileLoopBlock;

public class ASTBuilder {
	private SourceLexer lexer;
	private List<Token> tokens;
	// token pointer
	private int currentToken = 0; // bookkeeping 101
	
	public AbstractSyntaxTree ast;
	
	public ASTBuilder(SourceLexer lexer) {
		this.lexer = lexer;
		this.lexer.scanTokens();
		this.tokens = this.lexer.tokens;
		this.ast = new AbstractSyntaxTree();
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
	private StructDefinition parseStructDefinition() {
	    consume(TokenType.STRUCT, "Expected 'struct' keyword");
	    
		// pretty much the same as declaring variables
	    Token structNameToken = consume(TokenType.IDENTIFIER, "Expected struct name");
	    Identifier structName = new Identifier(structNameToken.lexeme);
	    consume(TokenType.LBRACE, "Expected '{' to start struct body");
	    
	    List<DeclarationStatement> fields = new ArrayList<>();
	    // parse declarations until closing brace '}'
	    while (!consumeIfMatch(TokenType.RBRACE)) {
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
	private GlobalDefinitionNode parseGlobalDeclaration() {
		Token typeToken = advance();
		
		// type and pointer level
		TypeSpecifier type;
		int ptrLevel = 0; // e.g. int* = 1 ptrLevel

		// count pointers
		while (consumeIfMatch(TokenType.STAR)) {
			ptrLevel++;
		}
		// the name of the declaration
		Token identifier = consume(TokenType.IDENTIFIER, "Expected name");

		// primitives (uint, char)
		// or void ptr (void*)
		if (typeToken.type == TokenType.UINT 
			|| typeToken.type == TokenType.CHAR
			|| (typeToken.type == TokenType.VOID && ptrLevel > 0)
		) {
			type = new TypeSpecifier(typeToken.type);
		} else if (typeToken.type == TokenType.IDENTIFIER) {
			type = new TypeSpecifier(typeToken); // for structs (custom type)
		} else {
			type = null;
		}
		
		// parse function, allow "void" (no ret type)
		if (check(TokenType.LPAREN)) {
			return parseFunctionDeclaration(type, ptrLevel, identifier);
		}
		
		if (type == null) {
			throw new RuntimeException("Void cannot be used as a type unless it's a pointer!");
		}

		return new GlobalVariable(
			_internalParseDeclaration(
				type, new Identifier(identifier.lexeme), ptrLevel
			)
		);
	}
	
	/**
	 * Parse function declaration, the pointer must be at the '(' token at this point
	 * 
	 * void function(uint param1, uint param2)
	 *              ^ here
	 *              
	 * @param type the return type
	 * @param ptrLevel the pointer level (void**) = 2
	 * @param nameToken the function name 
	 */
	private FunctionDeclaration parseFunctionDeclaration(TypeSpecifier type, int ptrLevel, Token nameToken) {
		// parse parameters "(void* param1, uint param2...)"
		consume(TokenType.LPAREN, "Expected '(' after function name declaration");
		List<DeclarationStatement> params = new ArrayList<>();
		// parse list of parameters
		if (peek().type != TokenType.RPAREN) {
			do {
				// reuse the declaration statement parser
				// for quickies
				params.add(parseDeclaration(advance()));
			} while (consumeIfMatch(TokenType.COMMA)); // for each ",", a new param
		}
		consume(TokenType.RPAREN, "Expected ')' after function parameters");
		
		return new FunctionDeclaration(
			type, ptrLevel, new Identifier(nameToken.lexeme), // type[*] name
			params, // (params...)
			consumeIfMatch(TokenType.SEMICOLON) ? null : // for forward declaration: void func();
			parseScopedBody() // { body }
		);
	}
	
	/**
	 * Parse a declaration statement (TYPE{ptrLevel} NAME [= INITIAL];)
	 * need comment
	 * @param typeToken the TYPE token
	 * @return a declaration statement (declare var)
	 */
	public DeclarationStatement parseDeclaration(Token typeToken) {
		// type and pointer level
		TypeSpecifier type;
		int ptrLevel = 0; // e.g. int* = 1 ptrLevel
		
		// count pointers
		while (consumeIfMatch(TokenType.STAR)) {
			ptrLevel++;
		}
		
		// the name of the declaration
		Token identifier = consume(TokenType.IDENTIFIER, "Expected name");
		
		// primitives (uint, char) 
		// or void ptr (void*)
		if (typeToken.type == TokenType.UINT 
		 || typeToken.type == TokenType.CHAR 
		 || (typeToken.type == TokenType.VOID && ptrLevel > 0)
		) {
			type = new TypeSpecifier(typeToken.type);
		} else if (typeToken.type == TokenType.IDENTIFIER) {
			type = new TypeSpecifier(typeToken); // for structs (custom type)
		} else {
			throw new RuntimeException("Void cannot be used as a type unless it's a pointer!");
		}
		
		return _internalParseDeclaration(
			type, new Identifier(identifier.lexeme), ptrLevel
		);
	}
	
	/**
	 * NEED COMMENT!
	 * @param type
	 * @param name
	 * @param ptrLevel
	 * @return
	 */
	private DeclarationStatement _internalParseDeclaration(TypeSpecifier type, Identifiable name, int ptrLevel) {
		DeclarationStatement statement;
		
		// if the struct is like this "Type name{}", its a struct initializer
		// so peek() must return {
		if (peek().type == TokenType.LBRACE) {
			advance(); // consume the '{' token
			// parse decs like: Person p{10};
			statement = DeclarationStatement.struct(
				type, ptrLevel, name, 
				parseStructInitializer(type, ptrLevel)
			);
		}
		// if there is a =, its a declaration w/ assignment
		else if (peek().type == TokenType.EQ) {
			advance(); // consume the '=' token
			statement = DeclarationStatement.variable(
				type, ptrLevel, name, 
				parseExpression()
			);
		} else if (peek().type == TokenType.LSQUARE) {
			advance(); // consume the opening bracket
			// array declaration without size (e.g. char s[] = "hello";)
			// basically, if the thing is "[]"
			
			if (consumeIfMatch(TokenType.RSQUARE)) {
				// parse declarations like: char s[] = {'s'};
				statement = DeclarationStatement.array(
					type, ptrLevel, name, 
					parseArrayDeclarationWithoutSize()
				);
			} else { // else, the thing is "type arr[<expression>];"
				statement = DeclarationStatement.arrayWithSize(
					type, ptrLevel, name, 
					parseExpression()
				);
				consume(TokenType.RSQUARE, "Expected ']' after array declaration");
			}
		} else { // generic "type name;" (no init)
			statement = DeclarationStatement.variable(
				type, ptrLevel, name, 
				null
			);
		}
		
		return statement;
	}
	
	/**
	 * Parses a struct initializer list and sets it as the initial value of the given declaration statement.
	 * This function will start to parse when the pointer is
	 * on the '{' token
	 * 
	 * Person p{ 25, 180, 75 };
	 *         ^ here
	 *        
	 * @param statement the initial statement
	*/
	public StructLiteral parseStructInitializer(TypeSpecifier type, int pointerLevel) {
		if (type.isPrimitive()) {
			throw new RuntimeException("Struct initializer cannot be placed on primitive types!");
		}
		// no fucking allow typa shit like: Type* thing{};
		if (pointerLevel > 0) {
			throw new RuntimeException("Struct initializer cannot be placed on pointers!");
		}
		
		// the struct initializer members
		List<ExpressionNode> struct = new ArrayList<>();
		if (peek().type != TokenType.RBRACE) {
			do {
				// parse each expression inside the struct initializer list
				struct.add( parseExpression() );
			} while (consumeIfMatch(TokenType.COMMA)); // seperated by ","
		}
		consume(TokenType.RBRACE, "Expected '}' after struct literal initialization");
		return new StructLiteral(
			type, struct
		);
	}
	
	/**
	 * Parse an array declaration with determined data
	 * 
	 * This function will start to parse when the pointer is
	 * on the '=' token
	 * 
	 * uint list[] = {1, 2, 3, 4};
	 * 			   ^ here
	 * 
	 * @param statement the initial statement
	 */
	public ArrayLiteral parseArrayDeclarationWithoutSize() {
		consume(TokenType.EQ, "Array declaration without size must be initialized");
		List<ExpressionNode> declaredArray = new ArrayList<>();
		
		// handle string literal initialization (e.g. char s[] = "hello";)
		if (peek().type == TokenType.STRING) {
			// the string token itself
			String stringLiteral = advance().lexeme;
			for (int i = 1; i < stringLiteral.length() - 1; i++) {
				declaredArray.add(new CharacterNode(stringLiteral.charAt(i)));
			}
			// null terminated character \0
			declaredArray.add(new CharacterNode('\0'));
		} else if (consumeIfMatch(TokenType.LBRACE)) { // handle array initialization via brace-enclosed values (e.g. uint arr[] = {1, 2, 3};)
			// check for elements inside the braces
			if (peek().type != TokenType.RBRACE) {
				do {
					// parse each expression inside the initializer list
					declaredArray.add( parseExpression() );
				} while (consumeIfMatch(TokenType.COMMA)); // seperated by ","
			}
			consume(TokenType.RBRACE, "Expected '}' after array initialization");
		}
		return new ArrayLiteral(declaredArray);
	}
	
	/**
	 * Parse a statement (almost anything in the Ngu-C language, ends with a ;)
	 * 
	 * @return a statement
	 */
	private StatementNode parseStatement() {
		Token token = peek(); // do NOT consume the token, since the expression parser needs it
		Token next = peekAhead(1); // the one after the peek()
		
		switch (token.type) {
			case LPAREN: case PLUSPLUS: case MINUSMINUS:
			case IDENTIFIER: case STAR: {
				System.out.println(token);
				// if the current token is an identifier, but the next token cannot be part of an expression
				// (like assignment operators, increment/decrement, struct access, array indexing, or function calls),
				// falls through the switch into parseNormalStatement
				if (token.type == TokenType.IDENTIFIER 
					&& next != null 
					&& !next.type.isAssignmentOperator() // not a++, b++, a += b, a *= c, etc
					&& !next.type.isStructAccessOperator() // not a.b->c, a.b.c, etc
					&& next.type != TokenType.LSQUARE // not a[array accessor]
					&& next.type != TokenType.LPAREN // not function calls, they are expressions too
				) {
					break;
				}
				return new ExpressionStatement(parseExpression());
			}
			default: break;
		}
		
		// handles normal statements (dec, loops, etc)
		return parseNormalStatement();
	}
	
	private StatementNode parseNormalStatement() {
		// the first token of the statement (CONSUMED)
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
					ifBody = parseStatement();
				}
	
				// parse "else" block
				if (peek().type == TokenType.ELSE) {
					advance(); // consume the ELSE token
					// if found a block then parse the body instead
					// of a single statement
					if (peek().type == TokenType.LBRACE) {
						elseBody = parseScopedBody();
					} else {
						elseBody = parseStatement();
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
					whileBody = parseStatement();
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
					initial = parseStatement();
				}
				// the same...
				consume(TokenType.SEMICOLON, "Expected ';' after initial");
				if (peek().type != TokenType.SEMICOLON) {
					condition = parseExpression();
				}
				consume(TokenType.SEMICOLON, "Expected ';' after condition");
				// if the last token is a ')', it is the end of the statement
				// skip the parsing
				if (peek().type != TokenType.RPAREN) {
					run = parseStatement();
				}
				consume(TokenType.RPAREN, "Expected ) after \"for (\"");
				
				// the main for body
				StatementNode forBody;
				
				// if found a block then parse the body
				// instead of a single statement
				if (peek().type == TokenType.LBRACE) {
					forBody = parseScopedBody();
				} else {
					forBody = parseStatement();
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
	
			// loop break, continue
			case BREAK: {
				return new LoopBreakStatement();
			}
			case CONTINUE: {
				return new LoopContinueStatement();
			}
			// parse declarations
			case IDENTIFIER: {
				return parseDeclaration(token);
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
			///// END: FOR DECLARATION AND ASSIGNMENT / FUNCTIONS /////
	
			default: {
				throw new RuntimeException("Unhandled token: " + token);
			}
		}
	}

	/**
	 * Parse a scoped (enclosed) block of code
	 * like { hello(); }
	 * @return a block of statements
	 */
	private ScopedStatements parseScopedBody() {
		if (!consumeIfMatch(TokenType.LBRACE)) return null;
		ScopedStatements body = new ScopedStatements();
		while (!consumeIfMatch(TokenType.RBRACE)) {
			// skips
			if (peek().type == TokenType.SEMICOLON) {
				advance(); // skip extra semicolons
				continue;
			}
			// recursively parse freelance scopes
			// scopes within scopes
			if (peek().type == TokenType.LBRACE) {
				body.statements.add(parseScopedBody());
				continue;
			}
			// parse each NORMAL statement, line by line
			body.statements.add(parseStatement());
			if (!peekMatch(TokenType.RBRACE)) {
				consume(TokenType.SEMICOLON, "Expected ';' after a statement");
			}
		}
		return body;
	}
	
	/**
     * Starts parsing the tokenized source and constructs
     * the AbstractSyntaxTree class (AST) representation of the Ngu-C program
     */
	public void build() {
		while (!isAtEnd() && !consumeIfMatch(TokenType.EOF)) {
			try {
			// parse struct
			switch (peek().type) {
			case STRUCT: {
				ast.structs.add(parseStructDefinition());
				break;
			}
			// for function and global var declarations
			// they share the same format of
			// [TYPE/IDENTIFIER][IDENTIFIER]{(FOR FUNC) / ; OR = FOR DEC}
			case IDENTIFIER:
			case VOID:
			case CHAR:
			case UINT: {
				GlobalDefinitionNode parsed = parseGlobalDeclaration();
				if (parsed instanceof FunctionDeclaration) {
					ast.functions.add(parsed);
				} else if (parsed instanceof GlobalVariable) {
					ast.globalVariables.add(parsed);
				} else {
					throw new RuntimeException("Something went wrong!");
				}
				break;
			}
			case SEMICOLON: {
				advance(); // skip semicolons
				break;
			}
			default:
				throw new IllegalArgumentException("Unexpected value: " + peek().type);
			}
			} catch (Exception e) {
				System.err.println(peek().line);
				e.printStackTrace();
				System.exit(1);
			}
		}
	}
	
	// EXPRESSION PARSING START //
	/**
	 * Parse expression. E.g. a * (b + c) >= d<br>
	 * The process starts at the current token (peek())<br>
	 * 
	 * This will consume the entire expression For example, we have:<br>
	 * 
	 * 1 * (2 + 3) <br>
	 * ^ POINTER HERE<br>
	 * 
	 * After the parse 1 * (2 + 3)| <br>
	 *                            ^ POINTER HERE (at the |)<br><br>
	 *                            
	 * This supports everything:<br>
	 * - Simple assignment: a = 1 + 1; <br>
	 * - Pointer Deref assignment: *a = 1;<br>
	 * - Complex PD assignment: *(a + b) = b + c;<br>
	 * - Struct accessor/subscripts: a[0]->b = 1; a.b->c.d; a[0] = 1; a[0]->b, etc..<br>
	 * - Function calls: func(); func(param1, param2); func(a = b)<br>
	 * 
	 * @apiNote Precedence: Assignment (=, +=, ...) -> Combinatory (&&, ||) -> Comparison -> Add/Sub/Bsr/Bsl -> Mul/Div/Mod
	 * @return a binary tree representing order of expressions
	 */
	private ExpressionNode parseExpression() {
		// note: LHS = LEFT HAND SIDE, RHS = RIGHT HAND SIDE
		ExpressionNode lhs = parseCombinatory(); // higher precedence
		while (true) {
	        // check for assignment operators
			if (!consumeIfMatch(
				TokenType.EQ, TokenType.ADDEQ, 
				TokenType.SUBEQ, TokenType.MULEQ, 
				TokenType.DIVEQ, TokenType.BSREQ, 
				TokenType.BSLEQ, TokenType.OREQ, 
				TokenType.ANDEQ, TokenType.XOREQ
			)) {
				break;
			}
			Token operator = previous();
			ExpressionNode rhs = parseCombinatory(); // higher precedence
			lhs = new AssignmentNode(
				lhs, // left
				operator.type, // op
				rhs // right
			);
		}
		return lhs;
	}

	// AND, OR (&&, ||)
	private ExpressionNode parseCombinatory() {
		ExpressionNode lhs = parseComparisons(); // higher precedence
		while (true) {
			if (!consumeIfMatch(
				TokenType.ANDAND, 
				TokenType.OROR
			)) {
				break;
			}
			Token operator = previous();
			ExpressionNode rhs = parseComparisons(); // higher precedence
			lhs = new BinaryOpNode(
				lhs, // left
				operator.type, // op
				rhs // right
			);
		}
		return lhs;
	}
	
	// GE, LE, GT, LT... (<, >, >=, <=, ==, !=) 
	private ExpressionNode parseComparisons() {
		ExpressionNode lhs = parseAdditiveOrShift(); // higher precedence
		while (true) {
			if (!consumeIfMatch(
				TokenType.GE, TokenType.LE, 
				TokenType.GT, TokenType.LT, 
				TokenType.EQEQ, TokenType.DIFF
			)) {
				break;
			}
			Token operator = previous();
			ExpressionNode rhs = parseAdditiveOrShift(); // higher precedence
			lhs = new BinaryOpNode(
				lhs, // left
				operator.type, // op
				rhs // right
			);
		}
		return lhs;
	}
	
	// ADD, SUB, BSL, BSR (+, -, <<, >>)
	private ExpressionNode parseAdditiveOrShift() {
		ExpressionNode lhs = parseMulDivMod(); // higher precedence
		while (true) {
			if (!consumeIfMatch(
				TokenType.BSL, TokenType.BSR, 
				TokenType.PLUS, TokenType.MINUS
			)) {
				break;
			}
			Token operator = previous();
			ExpressionNode rhs = parseMulDivMod(); // higher precedence
			// new branch of the binary tree
			lhs = new BinaryOpNode(
				lhs, // left
				operator.type, // op
				rhs // right
			);
		}
		return lhs;
	}
	
	// MUL, DIV, MOD (*, /, %)
	private ExpressionNode parseMulDivMod() {
		ExpressionNode lhs = parseUnary(); // higher precedence
		while (true) {
			if (!consumeIfMatch(
				TokenType.STAR, TokenType.SLASH, TokenType.MOD
			)) {
				break;
			}
			Token operator = previous();
			ExpressionNode rhs = parseUnary(); // higher precedence
			lhs = new BinaryOpNode(
				lhs, // left
				operator.type, // op
				rhs // right
			);
		}
		return lhs;
	}

	/**
	 * @return parse an expression, with unary operator
	 */
	private ExpressionNode parseUnary() {
		// prefixes (-, +, &, *, !)
		if (consumeIfMatch(TokenType.BANG, // !negate
			TokenType.MINUS, // -negative
			TokenType.AMPERSAND, // &pointer
			TokenType.STAR, // *deref
			TokenType.PLUS // +plus
		)) {
			Token operator = previous();
			ExpressionNode right = parseUnary(); // recursion! support chained shi like +-+-a (cursed af ik)
			return new UnaryOpNode(operator.type, right);
		}

		return parseMemberAccessor(); // highest precendence
	}
	
	private ExpressionNode parseMemberAccessor() {
		ExpressionNode lhs = parsePostfixPrefix(); // higher precedence
		while (true) {
			if (!consumeIfMatch(
				TokenType.DOT, TokenType.ARROW
			)) {
				break;
			}
			Token operator = previous();
			ExpressionNode rhs = parsePostfixPrefix(); // higher precedence
			lhs = new MemberOf(
				lhs, // left
				rhs, // right
				operator.type == TokenType.ARROW
			);
		}
		return lhs;
	}
	
	private ExpressionNode parsePostfixPrefix() {
		if (check(TokenType.PLUSPLUS) || check(TokenType.MINUSMINUS)) {
			return UnaryArithmeticNode.prefix(
				advance().type, parsePostfixPrefix()
			);
		}
		ExpressionNode base = parseArraySubscript(); // base case
		if (check(TokenType.PLUSPLUS) || check(TokenType.MINUSMINUS)) {
			base = UnaryArithmeticNode.postfix(
				advance().type /* the operator behind (a++ < this) */, base
			);
		}
		return base;
	}
	
	private ExpressionNode parseArraySubscript() {
		ExpressionNode base = parseBaseExpression(); // base case
		while (consumeIfMatch(TokenType.LSQUARE)) {
			base = new SubscriptNode(base, parseExpression());
			consume(TokenType.RSQUARE, "Expected ']' after array subscript");
		}
		return base;
	}
	
	private CallNode parseFunctionCall(Token funcName) {
		List<ExpressionNode> args = new ArrayList<>();
		// if there is arguments, parse
		if (peek().type != TokenType.RPAREN) {
			do {
				// parse each expression
				args.add(parseExpression());
			} while (consumeIfMatch(TokenType.COMMA)); // for each ","
		}
		
		// a function should end with a )
		consume(TokenType.RPAREN, "Expected ')'");
		return new CallNode(
			new Identifier(funcName.lexeme), args
		);
	}
	
	/**
	 * Parse expression's operands, can range from simple numbers to identifiers,
	 * pointers, and function calls
	 * 
	 * This also recursively parse enclosed expressions like (1+1)
	 * 
	 * @return the expression node representing it
	 */
	private ExpressionNode parseBaseExpression() {
		Token token = advance();
		switch (token.type) {
		case LITERAL_CHAR:
			return new CharacterNode(token.lexeme.charAt(1));
		case NUMBER: {
			String number = token.lexeme.toLowerCase(); // the token (lowercased)
			int radix = 10; // default is
			String digits = number; // the digits

			// check for hex or binary prefix (0x, 0b)
			if (number.startsWith("0x")) {
				radix = 16;
				digits = number.substring(2);
			} else if (number.startsWith("0b")) {
				radix = 2;
				digits = number.substring(2);
			}

			return new NumberNode(Integer.parseInt(digits, radix));
		}
		case LPAREN: // recursively parse enclosed expressions (inside parentheses)
			ExpressionNode expr = parseExpression();
			// if not enclosed by a ), error
			consume(TokenType.RPAREN, "Expected ')'");
			return expr;
		case TRUE: case FALSE:
			return new NumberNode(token.type == TokenType.TRUE ? 1 : 0);
		case STRING:
			return new StringLiteralNode(
				token.lexeme.substring(1, token.lexeme.length() - 1)
			);
		
		// ***** FUNC CALL ***** //
		case IDENTIFIER:
			// look one token ahead of the identifier
			TokenType next = peek().type;
			
			// a function is also an expression, wild huh?
			if (next == TokenType.LPAREN) {
				advance();
				// token here is the function name
				return parseFunctionCall(token);
			}
			
			return new Identifier(token.lexeme);
		default:
			throw new RuntimeException("Found unexpected token: " + token + ". Expected a valid expression");
		}
	}
	
	/**
	 * Recursively parses subscript expressions of the form identifier[index],
	 * supporting chained subscripts like array[0][1]. 
	 * Converts them into nested SubscriptNode instances.
	 *
	 * @param expr The base identifiable expression
	 * @return An Identifiable representing the entire subscript chain
	 */
	private Identifiable parseSubscriptNodes(Identifiable expr) {
		Identifiable base = expr; // base node
		if (peek().type != TokenType.LSQUARE) {
			return base;
		}
		consume(TokenType.LSQUARE, "Expected '['");
		// parse the expression inside the brackets
		base = new SubscriptNode(base, parseExpression());
		consume(TokenType.RSQUARE, "Expected ']' after opening subscript node");
		// recursively check for and parse additional subscript expressions (e.g., `a[0][1]`).
		return parseSubscriptNodes(base);
	}
	// EXPRESSION PARSING ENDS
	
	/**
	 * @return true if the pointer is at the end of the token stream
	 */
	private boolean isAtEnd() {
		return this.currentToken >= tokens.size();
	}
	
	/**
	 * @return the previous token (the one before the current pointer)
	 */
	private Token previous() {
		if (currentToken == 0) throw new IllegalArgumentException("Pointer at 0!");
		return tokens.get(currentToken - 1);
	}
	
	/**
	 * @return the token at the current pointer, null if at end
	 */
	private Token peek() {
		return isAtEnd() ? null : this.tokens.get(currentToken);
	}
	
	/**
	 * @return the token at the current pointer, null if at end
	 */
	private Token peekAhead(int steps) {
		return currentToken + steps >= this.tokens.size() ? null : this.tokens.get(currentToken + steps);
	}
	
	/**
	 * @return same as peek(), but advances the pointer
	 */
	private Token advance() {
		return this.tokens.get(currentToken++);
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
	 * Checks whether the upcoming tokens (starting at the current pointer) match
	 * the exact sequence of given types.
	 *
	 * Example: peekMatch(TokenType.IDENTIFIER, TokenType.LPAREN) -> returns true if
	 * the current token is IDENTIFIER and the next is LPAREN.
	 *
	 * @param types the sequence of token types to test against
	 * @return true if the next tokens exactly match the given sequence, false
	 *         otherwise
	 */
	private boolean peekMatch(TokenType... types) {
		int look = currentToken; 
		for (TokenType type : types) {
			if (look >= tokens.size() || tokens.get(look).type != type) {
				return false;
			}
			look++;
		}
		return true;
	}
	
	/**
	 * If the current token matches one of the given types, 
	 * advance the pointer and return true. Otherwise, return false.
	 */
	public boolean consumeIfMatch(TokenType... types) {
		for (TokenType type : types) {
			if (check(type)) {
				advance();
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Consume a token (advances pass it), if does not match, throw an error
	 * 
	 * @param type the type of token expected to consume
	 * @param errorIfNotMatch error message
	 * @return the consumed token
	 */
	private Token consume(TokenType type, String errorIfNotMatch) {
		// if token matches
		if (check(type)) {
			// advance pointer
			return advance();
		}
		throw new RuntimeException(errorIfNotMatch + " | Last token: " + peek());
	}
}
