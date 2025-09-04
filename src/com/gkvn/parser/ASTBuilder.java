package com.gkvn.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import com.gkvn.lexer.SourceLexer;
import com.gkvn.lexer.Token;
import com.gkvn.lexer.TokenType;
import com.gkvn.parser.ast.TypeSpecifier;
import com.gkvn.parser.ast.definitions.FunctionDefinition;
import com.gkvn.parser.ast.definitions.GlobalDefinitionNode;
import com.gkvn.parser.ast.definitions.GlobalVariable;
import com.gkvn.parser.ast.definitions.StructDefinition;
import com.gkvn.parser.ast.expressions.ArrayLiteral;
import com.gkvn.parser.ast.expressions.AssignmentNode;
import com.gkvn.parser.ast.expressions.BinaryOpNode;
import com.gkvn.parser.ast.expressions.CCastNode;
import com.gkvn.parser.ast.expressions.CallNode;
import com.gkvn.parser.ast.expressions.CharacterNode;
import com.gkvn.parser.ast.expressions.ExpressionNode;
import com.gkvn.parser.ast.expressions.NumberNode;
import com.gkvn.parser.ast.expressions.StringLiteralNode;
import com.gkvn.parser.ast.expressions.StructLiteral;
import com.gkvn.parser.ast.expressions.UnaryArithmeticNode;
import com.gkvn.parser.ast.expressions.UnaryOpNode;
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
import com.gkvn.parser.ast.statements.ScopedStatements;
import com.gkvn.parser.ast.statements.StatementNode;
import com.gkvn.parser.ast.statements.WhileLoopBlock;

public class ASTBuilder {
	private SourceLexer lexer;
	private List<Token> tokens;
	// token pointer
	private int currentToken = 0; // bookkeeping 101
	
	// keep track of defined structs for casting
	private Map<String, StructDefinition> definedStructs = new HashMap<>();
	private boolean isDefinedOrPrimitiveType(Token token) {
		if (token.type == TokenType.IDENTIFIER) {
			return definedStructs.containsKey(token.lexeme);
		}
		return token.type == TokenType.UINT 
			|| token.type == TokenType.CHAR 
			|| token.type == TokenType.VOID; 
	}
	
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
        StructDefinition struct = new StructDefinition(structName, fields);
        this.definedStructs.put(structName.name(), struct);
        return struct;
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
	private FunctionDefinition parseFunctionDeclaration(TypeSpecifier type, int ptrLevel, Token nameToken) {
		// parse parameters "(void* param1, uint param2...)"
		consume(TokenType.LPAREN, "Expected '(' after function name declaration");
		List<DeclarationStatement> params = new ArrayList<>();
		if (peek().type != TokenType.RPAREN) {
			do {
				// reuse the declaration statement parser for quickies
				params.add(parseDeclaration(advance()));
			} while (consumeIfMatch(TokenType.COMMA)); // for each ",", a new param
		}
		consume(TokenType.RPAREN, "Expected ')' after function parameters");
		
		return new FunctionDefinition(
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
	 * Parses a declaration statement for a variable, array, or struct initializer.
	 *
	 * Examples of what this method can parse:
	 *   int x;                 // plain variable
	 *   int x = 5;             // variable with initialization
	 *   char s[] = "hello";    // array without explicit size
	 *   int arr[10];           // array with size
	 *   Person p { 10, "John" }; // struct initializer
	 *
	 * @param type      The type specifier of the variable (e.g., int, char, struct type)
	 * @param name      The identifier (variable name)
	 * @param ptrLevel  Pointer level (0 for non-pointer, 1 for *, 2 for **, etc.)
	 * @return          A DeclarationStatement representing the fully parsed declaration
	 */
	private DeclarationStatement _internalParseDeclaration(TypeSpecifier type, Identifier name, int ptrLevel) {
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
				// if found a block then parse the body instead of a single statement
				if (peek().type == TokenType.LBRACE) {
					ifBody = parseScopedBody();
				} else {
					ifBody = parseStatement();
				}
				// parse "else" block
				if (peek().type == TokenType.ELSE) {
					advance(); // consume the ELSE token
					if (peek().type == TokenType.LBRACE) { // same thign
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
		while (!isAtEnd() && !consumeIfMatch(TokenType.EOF)) {try {
			// parse struct
			switch (peek().type) {
			case STRUCT: {
				ast.structs.add(parseStructDefinition());
				break;
			}
			// for function and global var declarations
			// they share the same format of
			// [TYPE/IDENTIFIER][IDENTIFIER]{(FOR FUNC) / ; OR = FOR DEC}
			case IDENTIFIER: case VOID: case CHAR: case UINT: {
				GlobalDefinitionNode parsed = parseGlobalDeclaration();
				if (parsed instanceof FunctionDefinition funcDec) {
					ast.functions.add(funcDec);
				} else if (parsed instanceof GlobalVariable gloVar) {
					ast.globalVariables.add(gloVar);
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
			}} catch (Exception e) {
				System.err.println(peek().line);
				e.printStackTrace();
				System.exit(1);
			}
		}
	}
	
	// EXPRESSION PARSING START //
	/**
	 * Parse expression. E.g. a * (b + c) >= d<br>
	 * The process starts at the current token (peek())<br><br>
	 * 
	 * This will consume the entire expression For example, we have:<br><br>
	 * 
	 * 1 * (2 + 3) <br>
	 * ^ POINTER HERE<br><br>
	 * 
	 * After the parse 1 * (2 + 3)|<br>
	 * ---------------------------^  POINTER HERE<br><br>
	 * 
	 * ****** THE NGU-C EXPRESSION PRECEDENCE TABLE ******<br>
	 * 0. literals (base) {@link ASTBuilder#parseBaseLiterals()}<br>
	 * 1. postfix: (), [], ->, . (highest) {@link ASTBuilder#parsePostfix()}<br>
	 * 2. casting: (type)(a) {@link ASTBuilder#parseCCast()}<br>
	 * 3. unary: +, -, !, ~, *, & {@link ASTBuilder#parseUnary()}<br>
	 * 4. multiplicative: *, /, % {@link ASTBuilder#parseAritMultiplicative()}<br>
	 * 5. additive: +, - {@link ASTBuilder#parseAritAdditive()}<br>
	 * 6. shift: <<, >> {@link ASTBuilder#parseBitShift()}<br>
	 * 7. relational: <, <=, >, >= {@link ASTBuilder#parseRelational()}<br>
	 * 8. equality: ==, != {@link ASTBuilder#parseEquality()}<br>
	 * 9. bitwise AND: & {@link ASTBuilder#parseBitwiseAnd()}<br>
	 * 10. bitwise XOR: ^ {@link ASTBuilder#parseBitwiseXor()}<br>
	 * 11. bitwise OR: | {@link ASTBuilder#parseBitwiseOr()}<br>
	 * 12. logical AND: && {@link ASTBuilder#parseLogicalAnd()}<br>
	 * 13. logical OR: || {@link ASTBuilder#parseLogicalOr()}<br>
	 * 14. assignment: =, +=, -=, *=, ... {@link ASTBuilder#parseAssignment()}
	 * 
	 * @return a binary tree representing order of expressions
	 */
	private ExpressionNode parseExpression() {
		return this.parseAssignment();
	}
	
	/**
	 * Parse a left-associative binary expression, e.g. a + b - c.<br>
	 * 
	 * This method handles any binary operators that are left-associative by consuming
	 * the left-hand side (LHS) first, then repeatedly consuming operators and right-hand sides (RHS)
	 * until no more matching operators are found.<br>
	 * 
	 * The actual node construction is delegated to a {@link NodeCombiner}, allowing this
	 * method to be reused for different binary node types (e.g., arithmetic, bitwise, logical).<br>
	 * 
	 * @param higherPrecedenceParser a supplier that parses expressions of higher precedence
	 *   (e.g., multiplicative expressions for additive operators)
	 * @param combiner a function that combines LHS, operator, and RHS into an ExpressionNode
	 * @param operators the token types representing the operators this parser should consume
	 * @return an {@link ExpressionNode} representing the fully parsed left-associative binary expression
	 */
	private ExpressionNode parseLeftAssociativeBinary(
		Supplier<ExpressionNode> higherPrecedenceParser,
		NodeCombiner<ExpressionNode, TokenType, ExpressionNode, ExpressionNode> combiner, 
		TokenType... operators
	) {
		ExpressionNode lhs = higherPrecedenceParser.get();
		while (true) {
			if (!consumeIfMatch(operators)) {
				break;
			}
			Token operator = previous();
			ExpressionNode rhs = higherPrecedenceParser.get();
			lhs = combiner.apply(lhs, operator.type, rhs);
		}
		return lhs;
	}
	
	/**
	 * For binary operation node (LHS = left leaf, OP = root, RHS = right leaf)
	 * 
	 * @see {@link ASTBuilder#parseLeftAssociativeBinary(Supplier, NodeCombiner, TokenType...)}
	 */
	private ExpressionNode parseLeftAssociativeBinaryOpNode(
		Supplier<ExpressionNode> higherPrecedenceParser,
		TokenType... operators
	) {
		return parseLeftAssociativeBinary(higherPrecedenceParser,
			(lhs, op, rhs) -> new BinaryOpNode(lhs, op, rhs)
		, operators);
	}
	
	// P14
	private ExpressionNode parseAssignment() {
		return parseLeftAssociativeBinary(
			this::parseLogicalOr,
			(lvalue, op, rvalue) -> new AssignmentNode(lvalue, op, rvalue),
			TokenType.EQ, TokenType.ADDEQ, TokenType.MODEQ,
			TokenType.SUBEQ, TokenType.MULEQ, 
			TokenType.DIVEQ, TokenType.BSREQ, 
			TokenType.BSLEQ, TokenType.OREQ, 
			TokenType.ANDEQ, TokenType.XOREQ
		);
			
	}
	
	// P13
	private ExpressionNode parseLogicalOr() {
		return parseLeftAssociativeBinaryOpNode(this::parseLogicalAnd, TokenType.OROR);
	}
	
	// P12
	private ExpressionNode parseLogicalAnd() {
		return parseLeftAssociativeBinaryOpNode(this::parseBitwiseOr, TokenType.ANDAND);
	}
	
	// P11
	private ExpressionNode parseBitwiseOr() {
		return parseLeftAssociativeBinaryOpNode(this::parseBitwiseXor, TokenType.OR);
	}
	
	// P10
	private ExpressionNode parseBitwiseXor() {
		return parseLeftAssociativeBinaryOpNode(this::parseBitwiseAnd, TokenType.XOR);
	}
	
	// P9
	private ExpressionNode parseBitwiseAnd() {
		return parseLeftAssociativeBinaryOpNode(this::parseEquality, TokenType.XOR);
	}

	// P8
	private ExpressionNode parseEquality() {
		return parseLeftAssociativeBinaryOpNode(this::parseRelational, 
			TokenType.EQEQ, TokenType.DIFF
		);
	}
	
	// P7
	private ExpressionNode parseRelational() {
		return parseLeftAssociativeBinaryOpNode(this::parseBitShift, 
			TokenType.LT, TokenType.LE,
			TokenType.GT, TokenType.GE
		);
	}
	
	// P6
	private ExpressionNode parseBitShift() {
		return parseLeftAssociativeBinaryOpNode(this::parseAritAdditive, 
			TokenType.BSR, TokenType.BSL
		);
	}
	
	// P5
	private ExpressionNode parseAritAdditive() {
		return parseLeftAssociativeBinaryOpNode(this::parseAritMultiplicative, 
			TokenType.PLUS, TokenType.MINUS
		);
	}
	
	// P4
	private ExpressionNode parseAritMultiplicative() {
		return parseLeftAssociativeBinaryOpNode(this::parseCCast, 
			TokenType.STAR, TokenType.SLASH, TokenType.MOD
		);
	}
	
	// P3
	private ExpressionNode parseCCast() {
		// prevents the need to backtrack by only peeking and not consuming
		if (check(TokenType.LPAREN)) {
			Token castTarget = peekAhead(1); // this is the token after the '('
			if (isDefinedOrPrimitiveType(castTarget)) { // only continue if its a type
				// if it is indeed a cast, consume the '(' and the type token
				advance(); // this consumes the '('
				advance(); // this consumes the type (in peekAhead(1))
				
				int ptrLevel = 0;
				while (consumeIfMatch(TokenType.STAR)) {
					ptrLevel++;
				}
				
				consume(TokenType.RPAREN, "Expected closing ')' after cast type");
				return new CCastNode(
					TypeSpecifier.from(castTarget), ptrLevel, 
					parseCCast() // recursive, since you can stack casts: (uint)(char*) a
				);
			}
		}
		// just falls off if found no casting behaviors
		return parseUnary();
	}
	
	// P2
	private ExpressionNode parseUnary() {
		if (consumeIfMatch(TokenType.BANG, // !negate
			TokenType.MINUS, // -negative
			TokenType.AMPERSAND, // &pointer
			TokenType.STAR, // *ptr_deref
			TokenType.PLUS, // +plus
			TokenType.TILDE // ~wiggly
		)) {
			Token operator = previous();
			ExpressionNode next = parseUnary(); // chained: +-+-a support
			return new UnaryOpNode(operator.type, next);
		}
		return parsePostfix(); // highest precendence (P1)
	}
	
	// P1
	private ExpressionNode parsePostfix() {
		ExpressionNode base = parseBaseLiterals(); // higher precedence
		while (true) {
			if (consumeIfMatch(TokenType.LSQUARE)) { // [<index>]
				ExpressionNode index = parseExpression();
				consume(TokenType.RSQUARE, "Expected ']'");
				base = new SubscriptNode(base, index);
			} else if (consumeIfMatch(TokenType.LPAREN)) { // (<param1>,<param-nth>...)
				List<ExpressionNode> args = new ArrayList<>();
				if (peek().type != TokenType.RPAREN) {
					do {
						args.add(parseExpression());
					} while (consumeIfMatch(TokenType.COMMA)); // for each ","
				}
				consume(TokenType.RPAREN, "Expected ')'");
				base = new CallNode(base, args);
			} else if (consumeIfMatch(TokenType.DOT, TokenType.ARROW)) { // a.b or a->b 
				Token op = previous();
				base = new MemberOf(
					base, parseBaseLiterals(), 
					op.type == TokenType.ARROW
				);
			} else if (check(TokenType.PLUSPLUS) || check(TokenType.MINUSMINUS)) { // ++ or -- (a++ <- post)
				base = UnaryArithmeticNode.postfix(advance().type, base);
			} else {
				break; // ran out of postfix
			}
		}
		return base;
	}
	
	// P0
	private ExpressionNode parseBaseLiterals() {
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
		case IDENTIFIER:
			return new Identifier(token.lexeme);
		default:
			throw new RuntimeException("Found unexpected token: " + token + ". Expected a valid expression");
		}
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
