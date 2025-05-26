package ccom.ast;

import java.lang.classfile.AnnotationValue.OfAnnotation;
import java.util.ArrayList;
import java.util.List;

import ccom.CompileToken.Token;
import ccom.CompileToken.TokenType;
import ccom.ast.Expressions.ExpressionNode;
import ccom.ast.Expressions.Identifiable;
import ccom.ast.Expressions.IdentifierNode;

/**
 * Contains classes that define the statement structure of the AST.
 */
public class Statements {
	/**
     * Base class for all statement nodes.
     */
    public static abstract class StatementNode extends ASTNode {}
    
    /**
     * Marker interface for statements that may have a scoped body.
     */
    public static interface OptionalScopedStatement {}
    
    /**
     * Represents a return statement in a function.
     */
	public static class FuncReturnStatement extends StatementNode {
		ExpressionNode returnValue;
		
		public FuncReturnStatement(ExpressionNode returnValue) {
			this.returnValue = returnValue;
		}
		
		@Override
		public String toString() {
			return "ReturnFunc(" + returnValue + ")";
		}
	}
	
	/**
     * Represents a break statement in a loop.
     */
	public static class LoopBreakStatement extends StatementNode {
		public LoopBreakStatement() {}
		
		@Override
		public String toString() {
			return "LoopBreak()";
		}
	}
	
	/**
     * Represents a continue statement in a loop.
     */
	public static class LoopContinueStatement extends StatementNode {
		public LoopContinueStatement() {}
		
		@Override
		public String toString() {
			return "LoopContinue()";
		}
	}

    /**
     * Represents a block of statements enclosed in a scope.
     */
	public static class ScopedStatements extends StatementNode {
		List<StatementNode> statements = new ArrayList<>();
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder("Scoped{\n");
			statements.forEach(statement -> builder.append(statement).append(";\n"));
			return builder.append("}").toString();
		}
	}

    /**
     * Represents an if-else conditional block.
     */
	public static class ConditionBlock extends StatementNode implements OptionalScopedStatement {
		ExpressionNode condition;
		StatementNode ifBody;
		StatementNode elseBody;
		
		public ConditionBlock(ExpressionNode condition, StatementNode ifbody, StatementNode elsebody) {
			this.condition = condition;
			this.ifBody = ifbody;
			this.elseBody = elsebody;
		}
		
		@Override
		public String toString() {
			return "If(Condition(" + condition + "), Body{" + ifBody + "}, Else(Body{" + elseBody + "}))";
		}
	}
	
	/**
     * Represents a while loop construct.
     */
	public static class WhileLoopBlock extends StatementNode implements OptionalScopedStatement {
		ExpressionNode condition;
		StatementNode whileBody;
		
		public WhileLoopBlock(ExpressionNode condition, StatementNode whileBody) {
			this.condition = condition;
			this.whileBody = whileBody;
		}
		
		@Override
		public String toString() {
			return "WhileLoop(Condition(" + condition + "), Body{" + whileBody + "})";
		}
	}
	
	 /**
     * Represents a for loop.
     */
	public static class ForStatement extends StatementNode implements OptionalScopedStatement {
		StatementNode initializer;
		ExpressionNode condition;
		StatementNode iteration;
		StatementNode loopBody;
		
		public ForStatement(StatementNode initializer, ExpressionNode condition, StatementNode iter, StatementNode loopBody) {
			this.initializer = initializer;
			this.condition = condition;
			this.iteration = iter;
			this.loopBody = loopBody;
		}
		
		@Override
		public String toString() {
			return "ForLoop(" + initializer + ", Condition(" + condition + ")," + iteration + ", Body{" + loopBody + "})";
		}
	}
	
	public static class DeclarationStatement extends StatementNode {
		DeclaredType type;
		int pointerLevel;
		Identifiable identifier;
		ExpressionNode initialValue;
		
		boolean isArray = false;
		ExpressionNode arraySize; // new field (for type thing[num])
		
		/**
		 * Constructor for scalar (non-array) variable declarations.
		 */
		public DeclarationStatement(DeclaredType type, int ptrLevel, Identifiable name, ExpressionNode initial) {
			this.type = type;
			this.pointerLevel = ptrLevel;
			this.identifier = name;
			this.initialValue = initial;
		}

		/**
		 * Constructor for array declarations.
		 */
		public DeclarationStatement(DeclaredType type, int ptrLevel, Identifiable name, ExpressionNode sizeExpr, boolean isArray) {
			this.type = type;
			this.pointerLevel = ptrLevel;
			this.identifier = name;
			this.isArray = isArray;
			this.arraySize = sizeExpr;
			this.initialValue = null;
		}
		
		@Override
		public String toString() {
			if (isArray && initialValue == null) {
				return "DeclareEmptyArr(" + type + ", " + pointerLevel + ", " + identifier + "[" + arraySize + "])";
			}
			return "DeclareVar(" + type + ", " + pointerLevel + ", " + identifier + ", " + initialValue + ")";
		}
	}
	
	public static class ArrayLiteral extends ExpressionNode {
	    List<ExpressionNode> values;

	    public ArrayLiteral(List<ExpressionNode> values) {
	        this.values = values;
	    }

	    @Override
	    public String toString() {
	        return "ArrayLiteral(" + values + ")";
	    }
	}
	
	public static class StructLiteral extends ExpressionNode {
	    DeclaredType structName;
	    List<ExpressionNode> fieldValues;

	    public StructLiteral(DeclaredType structName, List<ExpressionNode> fieldValues) {
	        this.structName = structName;
	        this.fieldValues = fieldValues;
	    }

	    @Override
	    public String toString() {
	        return "StructLiteral(" + structName + " {" + fieldValues + "})";
	    }
	}

	
	public static class DeclaredType {
		TokenType primitiveType = null;
		String typedefType = null;
		
		public DeclaredType(TokenType primitive) {
			this.primitiveType = primitive;
		}
		
		public DeclaredType(Token type) {
			this.typedefType = type.lexeme;
		}
		
		@Override
		public String toString() {
			if (typedefType == null) {
				return "Primitive(" + primitiveType + ")";
			} else return "Typedef(" + typedefType + ")";
		}
	}
	
	public static class AssignmentStatment extends StatementNode {
		Identifiable identifier;
		ExpressionNode value;
		TokenType operator;
		int pointerLevel;
		
		public AssignmentStatment(Identifiable identifier, int pointerLevel, TokenType operator, ExpressionNode value) {
			this.identifier = identifier;
			this.pointerLevel = pointerLevel;
			this.operator = operator;
			this.value = value;
		}
		
		@Override
		public String toString() {
			return "Assignment(" + pointerLevel + ", " + operator + ", " + identifier + ", " + value + ")";
		}
	}
	
	public static class StatementedExpression extends StatementNode {
		ExpressionNode expression;
		
		public StatementedExpression(ExpressionNode expressionNode) {
			this.expression = expressionNode;
		}
		
		@Override
		public String toString() {
			return expression.toString();
		}
	}
}
