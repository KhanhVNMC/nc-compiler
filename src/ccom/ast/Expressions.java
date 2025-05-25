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
		ExpressionNode identifier;
		ExpressionNode offset;
		
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
	
	public static class CallNode extends ExpressionNode {
		ExpressionNode callee;
		List<ExpressionNode> arguments;
		
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
