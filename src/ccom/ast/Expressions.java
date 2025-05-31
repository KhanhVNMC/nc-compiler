package ccom.ast;

import java.util.List;

import ccom.CompileToken.TokenType;

public class Expressions {
	public static abstract class ExpressionNode extends ASTNode {}
	public static abstract class Identifiable extends ExpressionNode {}
	
	public static class IdentifiableExpression extends Identifiable {
		public ExpressionNode expression;
		
		public IdentifiableExpression(ExpressionNode expr) {
			this.expression = expr;
		}
		
		@Override
		public String toString() {
			return "IdentifiableExpr(" + expression + ")";
		}
	}
	
	public static class IdentifierNode extends Identifiable {
		public String name;
		
		public IdentifierNode(String name) {
			this.name = name;
		}
		
		@Override
		public String toString() {
			return "IdentifierNode(" + name + ")";
		}
	}
	
	public static class MemberOf extends Identifiable {
		public Identifiable parent;
		public Identifiable child;
		public boolean arrow = false;
		
		public MemberOf(Identifiable parent, Identifiable child, boolean arrow) {
			this.parent = parent;
			this.child = child;
			this.arrow = arrow;
		}
		
		@Override
		public String toString() {
			return "Infer(" + parent + " " + (arrow ? "->" : ".") + " " + child + ")";
		}
	}
	
	public static class SubscriptNode extends Identifiable {
		public ExpressionNode identifier;
		public ExpressionNode offset;
		
		public SubscriptNode(ExpressionNode identifier, ExpressionNode offset) {
			this.offset = offset;
			this.identifier = identifier;
		}
		
		@Override
		public String toString() {
			return "Subscript(" + identifier + "[" + offset + "])";
		}
	}
	
	public static class UnaryOpNode extends ExpressionNode {
		public TokenType operator;
		public ExpressionNode expression;
		
		public UnaryOpNode(TokenType op, ExpressionNode expr) {
			this.expression = expr;
			this.operator = op;
		}
		
		@Override
		public String toString() {
			return "UnaryOpNode(" + operator + ", " + expression + ")";
		}
	}
	
	public static class UnaryArithmeticNode extends ExpressionNode {
		public TokenType operator;
		public ExpressionNode expression;
		public boolean post = false;
		
		public UnaryArithmeticNode(boolean post, TokenType op, ExpressionNode expr) {
			this.expression = expr;
			this.operator = op;
			this.post = post;
		}
		
		@Override
		public String toString() {
			return "UnaryArithmeticNode(" + operator + ", " + expression + ", " + post + ")";
		}
	}
	
	public static class NumberNode extends ExpressionNode {
		public int value;
		
		public NumberNode(int value) {
	    	this.value = value;
	    }
		
		@Override
		public String toString() {
			return "NumberNode(" + value + ")";
		}
	}
	
	public static class CharacterNode extends NumberNode {
		public CharacterNode(char c) {
			super((int)c);
		}
		
		@Override
		public String toString() {
			return "CharNode(" + ((char)value) + ")";
		}
	}
	
	/**
	 * This is a special node, special in a case that its defined once
	 * in the assembly, then refer later by address
	 */
	public static class StringLiteralNode extends ExpressionNode {
		public String string;
		
		public StringLiteralNode(String str) {
			this.string = str + '\0';
		}
		
		@Override
		public String toString() {
			return "StringLiteral(" + string + ")";
		}
	}
	
	public static class CallNode extends ExpressionNode {
		public ExpressionNode callee;
		public List<ExpressionNode> arguments;
		
		public CallNode(ExpressionNode expr, List<ExpressionNode> node) {
			this.callee = expr;
			this.arguments = node;
		}
		
		@Override
		public String toString() {
			return "CallNode(" + callee.toString() + ", args: " + arguments.toString() + ")";
		}
	}
	
	public static class BinaryOpNode extends ExpressionNode {
		public ExpressionNode left, right;
	    public TokenType operator;
	    
	    public BinaryOpNode(ExpressionNode left, TokenType operator, ExpressionNode right) {
	    	this.left = left;
	    	this.right = right;
	    	this.operator = operator;
	    }
	    
	    public void emitAsm(List<String> instructions, ExpressionNode node) {
	    	if (node instanceof NumberNode) {
	    		instructions.add("ipush " + ((NumberNode) node).value);
	    		return;
	    	}
	    	
	    	BinaryOpNode tree = (BinaryOpNode) node;
	    	
	    	emitAsm(instructions, tree.left);
	    	emitAsm(instructions, tree.right);
	    	
	    	instructions.add("pop r1"); // right operand
	    	instructions.add("pop r0"); // result (operator)= right 
	    	
	    	// the operator of the subtree
	    	switch (tree.operator) {
	    		// emit assembly depending on what operator were used
				case PLUS: {
					instructions.add("add r0, r1");
					break;
				}
				case STAR: {
					instructions.add("mul r0, r1");
					break;
				}
				case SLASH: {
					instructions.add("div r0, r1");
					break;
				}
				case MINUS: {
					instructions.add("sub r0, r1");
					break;
				}
				case BSR: {
					instructions.add("shr r0, r1");
					break;
				}
				case BSL: {
					instructions.add("shl r0, r1");
					break;
				}
				case MOD: {
					instructions.add("mod r0, r1");
					break;
				}
			default:
				break;
			}
	    	
	    	instructions.add("push r0");
	    }
	    
	    @Override
	    public String toString() {
	    	return "{"
	    		+ "\"L\":\"" + left.toString() + "\","
	    		+ "\"O\":\"" + operator + "\","
	    		+ "\"R\":\"" + right.toString() + "\""
	    	+ "}";
	    }
	}
}
