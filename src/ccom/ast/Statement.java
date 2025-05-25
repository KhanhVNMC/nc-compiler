package ccom.ast;

import java.util.ArrayList;
import java.util.List;

import ccom.CompileToken.Token;
import ccom.CompileToken.TokenType;
import ccom.ast.Expression.ExpressionNode;
import ccom.ast.Expression.Identifiable;

public class Statement {
	public static abstract class StatementNode extends ASTNode {}
	/*
	 * This is for while, for, if, etc
	 */
	public static interface OptionalScopedStatement {};
	
	public static class FunctionDeclaration {
		DeclaredType returnType; // null == void
		int returnPtrLevel = 0;
		Identifiable identifier;
		List<FunctionParam> parameters;
		ScopedStatements body;
		
		public FunctionDeclaration(DeclaredType type, int ptrLevel, Identifiable name, List<FunctionParam> params, ScopedStatements body) {
			this.returnType = type;
			this.identifier = name;
			this.returnPtrLevel = ptrLevel;
			this.parameters = params;
			this.body = body;
		}
		
		@Override
		public String toString() {
			return "DeclareFunc(" + returnType + ", ptrLevel: " + returnPtrLevel + ", " + identifier + ", params: " + parameters + ", Body{" + body + "})";
		}
	}
	
	public static class FunctionParam {
		DeclaredType type;
		int ptrLevel = 0;
		Identifiable paramName;
		
		public FunctionParam(DeclaredType type, int ptrLevel, Identifiable name) {
			this.type = type;
			this.ptrLevel = ptrLevel;
			this.paramName = name;
		}
		
		public static FunctionParam fromDeclaration(DeclarationStatement declare) {
			if (declare.initialValue != null) {
				throw new RuntimeException("Function parameter can't have initial value in Ngu-C!");
			}
			return new FunctionParam(declare.type, declare.pointerLevel, declare.identifier);
		}
		
		@Override
		public String toString() {
			return "FuncParam(" + type + ", " + ptrLevel + ", " + paramName + ")";
		}
	}
	
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
	
	public static class ScopedStatements extends StatementNode {
		List<StatementNode> statements = new ArrayList<>();
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder("Scoped{\n");
			statements.forEach(statement -> builder.append(statement).append(";\n"));
			return builder.append("}").toString();
		}
	}
	
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
		
		public DeclarationStatement(DeclaredType type, int ptrLevel, Identifiable name, ExpressionNode initial) {
			this.type = type;
			this.pointerLevel = ptrLevel;
			this.identifier = name;
			this.initialValue = initial;
		}
		
		@Override
		public String toString() {
			return "DeclareVar(" + type + ", " + pointerLevel + ", " + identifier + ", " + initialValue + ")";
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
