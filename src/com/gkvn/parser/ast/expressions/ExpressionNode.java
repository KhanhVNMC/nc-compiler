package com.gkvn.parser.ast.expressions;

import com.gkvn.parser.ast.ASTNode;
import com.gkvn.parser.ast.TypeSpecifier;

public abstract class ExpressionNode extends ASTNode {
    private TypeSpecifier resolvedType;
    private boolean isLvalue;

    public TypeSpecifier getResolvedType() { return resolvedType; }
    public void setResolvedType(TypeSpecifier type) { this.resolvedType = type; }

    public boolean isLvalue() { return isLvalue; }
    public void setLvalue(boolean lvalue) { this.isLvalue = lvalue; }
}
