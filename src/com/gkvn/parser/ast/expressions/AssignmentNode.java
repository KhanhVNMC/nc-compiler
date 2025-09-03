package com.gkvn.parser.ast.expressions;

import com.gkvn.lexer.TokenType;

public class AssignmentNode extends ExpressionNode {
    private ExpressionNode lvalue;
    private ExpressionNode rvalue;
    private TokenType operator;

    public AssignmentNode(ExpressionNode lvalue, TokenType op, ExpressionNode rvalue) {
    	this.lvalue = lvalue;
    	this.operator = op;
    	this.rvalue = rvalue;
    }
    
    @Override
    public String toString() {
    	return "Assignment(" + lvalue + ", " + operator + ", " + rvalue + ")";
    }
}
