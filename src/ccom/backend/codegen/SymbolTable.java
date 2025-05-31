package ccom.backend.codegen;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class SymbolTable {
	private Stack<Map<String, Symbol>> scopeStack = new Stack<>();

	private int rspOffset = 1;

	public void enterScope() {
		scopeStack.push(new HashMap<>());
	}

	public void exitScope() {
		scopeStack.pop().forEach((_, symbol) -> {
			if (!symbol.isParam)
				rspOffset -= symbol.sizeof();
		});
	}

	public void declareSymbol(Symbol symbol) {
		if (symbol.isParam) {
			symbol.offset = 1 + 2 + 2 * currentParamCount();
		} else {
			symbol.offset = rspOffset;
			rspOffset += symbol.sizeof(); // next
		}
		scopeStack.peek().put(symbol.identifier, symbol);
	}

	private int currentParamCount() {
		return (int) scopeStack.peek().values().stream().filter(s -> s.isParam).count();
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

	public static class Struct extends Symbol {
		private final Map<String, Symbol> members = new LinkedHashMap<>();

		public Struct(String name, boolean isParam) {
			super(name);
			this.isParam = isParam;
		}

		public void addMember(Symbol member) {
			member.offset = this.offset + this.size; // offset within the struct
			members.put(member.identifier, member);
			this.size += member.sizeof(); // total size of the struct
		}

		public Symbol getMember(String name) {
			return members.get(name);
		}

		public Collection<Symbol> getMembers() {
			return members.values();
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder(identifier + "(struct, size:" + size + ") {\n");
			for (Symbol member : members.values()) {
				sb.append("    ").append(member.identifier).append("(size:").append(member.size).append(", offset:")
						.append(member.offset).append(")\n");
			}
			sb.append("}");
			return sb.toString();
		}
	}

	public static class Symbol {
		public int size;
		public int offset;
		public boolean isParam;
		public String identifier;

		public Symbol(String name) {
			this.identifier = name;
			this.size = 2;
			this.isParam = true;
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
