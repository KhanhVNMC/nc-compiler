package ccom.ast;

import java.util.ArrayList;
import java.util.List;

import ccom.ast.Expressions.Identifiable;
import ccom.ast.Expressions.IdentifierNode;
import ccom.ast.Statements.DeclarationStatement;
import ccom.ast.Statements.DeclaredType;
import ccom.ast.Statements.ScopedStatements;

public class GlobalDefinitions {
	 /**
     * Base class for all global definition nodes (struct, function, global vars)
     */
    public static abstract class GlobalDefinitionNode extends ASTNode {}
    
    public static class ProgramAST {
    	List<GlobalDefinitionNode> globalVariables = new ArrayList<>();
    	List<GlobalDefinitionNode> structs = new ArrayList<>();
    	List<GlobalDefinitionNode> functions = new ArrayList<>();
    
    	public ProgramAST() {}
    	
    	@Override
    	public String toString() {
    		return "ProgramASTree{" + globalVariables + ",\n" + structs + ",\n" + functions + "}";
    	}
    }
    
    public static class GlobalVarDefinition extends GlobalDefinitionNode {
    	DeclarationStatement statement;
    	
    	public GlobalVarDefinition(DeclarationStatement statement) {
    		this.statement = statement;
    	}
    	
    	@Override
    	public String toString() {
    		return "GlobalDec(" + this.statement + ")";
    	}
    }
    
    /**
     * Represents a struct declaration
     */
    public static class StructDefinition extends GlobalDefinitionNode {
        public IdentifierNode name;
        public List<DeclarationStatement> fields;

        public StructDefinition(IdentifierNode name, List<DeclarationStatement> fields) {
            this.name = name;
            this.fields = fields;
        }

        @Override
        public String toString() {
            return "DeclareStruct(" + name + ", fields: " + fields + ")";
        }
    }
    
    /**
     * Represents a function declaration.
     */
	public static class FunctionDeclaration extends GlobalDefinitionNode {
		DeclaredType returnType; // null == void
		int returnPtrLevel = 0; // the return ptr level (Eg. void**)
		Identifiable identifier; // the func name
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
	
	/**
     * Represents a function parameter.
     */
	public static class FunctionParam {
		DeclaredType type;
		int ptrLevel = 0;
		Identifiable paramName;
		
		public FunctionParam(DeclaredType type, int ptrLevel, Identifiable name) {
			this.type = type;
			this.ptrLevel = ptrLevel;
			this.paramName = name;
		}
		
        /**
         * Constructs a function parameter from a variable declaration.
         * @throws RuntimeException if an initial value is provided (not allowed).
         */
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
}
