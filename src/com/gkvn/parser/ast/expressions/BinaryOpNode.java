package com.gkvn.parser.ast.expressions;

import java.util.List;

import com.gkvn.lexer.TokenType;

public class BinaryOpNode extends ExpressionNode {
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