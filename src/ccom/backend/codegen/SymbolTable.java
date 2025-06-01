package ccom.backend.codegen;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;

public class SymbolTable {
	private Stack<Map<String, Symbol>> scopeStack = new Stack<>();
	
	private int rspOffsetLocalVar = 1;
	private int rspOffsetParam = 3;

	public void enterScope() {
		scopeStack.push(new HashMap<>());
	}

	public void exitScope() {
		scopeStack.pop().forEach((_, symbol) -> {
			if (!symbol.isParam) {
				rspOffsetLocalVar -= symbol.sizeof();
			}
		});
	}
	
	public int paramSize() {
		return this.rspOffsetParam - 3;
	}

	public void declareSymbol(Symbol symbol) {
		if (symbol.isParam) {
			symbol.offset = rspOffsetParam;
			rspOffsetParam += symbol.sizeof();
		} else {
			symbol.offset = rspOffsetLocalVar;
			rspOffsetLocalVar += symbol.sizeof(); // next
		}
		scopeStack.peek().put(symbol.identifier, symbol);
	}
	
	public int currentParamCount() {
		return (int) scopeStack.peek().values().stream().filter(s -> s.isParam).count();
	}
	
	public Symbol resolve(String name) {
		
	}
	
	public void debugPrint() {
		System.out.println("=== Symbol Table ===");
		int scopeLevel = scopeStack.size();
		for (int i = scopeStack.size() - 1; i >= 0; i--) {
			Map<String, Symbol> scope = scopeStack.get(i);
			System.out.println("Scope Level " + (scopeLevel - i) + ":");
			for (Map.Entry<String, Symbol> entry : scope.entrySet()) {
				System.out.println("  " + entry.getKey() + " -> " + entry.getValue());
			}
		}
		System.out.println("====================");
	}

	public static class Symbol {
		public int size;
		public int offset;
		public boolean isParam;
		public String identifier;

		public Symbol(String name, int size, boolean isParam) {
			this.identifier = name;
			this.size = size;
			this.isParam = isParam;
		}

		public Symbol(String name, int size) {
			this.identifier = name;
			this.size = size;
		}

		public int sizeof() {
			return size;
		}

		public int offset() {
			return offset;
		}

		@Override
		public String toString() {
			return identifier + "(size:" + size + ") @rsp" + (isParam ? "+" : "-") + offset;
		}
	}
}
