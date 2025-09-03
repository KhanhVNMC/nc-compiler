package com.gkvn.parser.ast.expressions;

import java.util.List;

public class ArrayLiteral extends ExpressionNode {
	private List<ExpressionNode> values;

    public ArrayLiteral(List<ExpressionNode> values) {
        this.values = values;
    }

    @Override
    public String toString() {
        return "ArrayLiteral(" + values + ")";
    }
}