package com.gkvn.parser.ast.expressions;

import java.util.List;

import com.gkvn.parser.ast.TypeSpecifier;

public class StructLiteral extends ExpressionNode {
	public TypeSpecifier structName;
	public List<ExpressionNode> fieldValues;

    public StructLiteral(TypeSpecifier structName, List<ExpressionNode> fieldValues) {
        this.structName = structName;
        this.fieldValues = fieldValues;
    }

    @Override
    public String toString() {
        return "StructLiteral(" + structName + " {" + fieldValues + "})";
    }
}
