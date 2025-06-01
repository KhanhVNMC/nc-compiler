package ccom.backend.codegen;

import java.util.*;

import ccom.backend.codegen.SymbolTable.Symbol;

/**
 * The StructTable class provides a system for defining and managing Ngu-C
 * memory layouts of nested data struct
 */
public class StructTable {
	public static final int UINT = 2;
	public static final int CHAR = 1;

	/**
	 * Represents a single field in a StructLayout.
	 * 
	 * Fields can be either: 
	 * - Primitive fields, having a fixed size in bytes (size != null) 
	 * - Nested struct fields, which reference another StructLayout (nestedStruct != null)
	 */
	public static class StructField {
		public final String name;
		// size of primitive field in bytes, null if this field is a nested struct
		public final Integer size;
		// nested struct layout if this field is a struct, null if primitive
		public final StructLayout nestedStruct;

		/**
		 * Creates a primitive field with the specified name and size.
		 * 
		 * @param name Field name
		 * @param size Size of the primitive type in bytes
		 */
		public StructField(String name, int size) {
			this.name = name;
			this.size = size;
			this.nestedStruct = null;
		}

		/**
		 * Creates a nested struct field with the specified name and nested layout.
		 * 
		 * @param name         Field name
		 * @param nestedLayout Nested StructLayout referenced by this field
		 */
		public StructField(String name, StructLayout nestedLayout) {
			this.name = name;
			this.size = null;
			this.nestedStruct = nestedLayout;
		}

		/**
		 * Returns true if this field represents a nested struct.
		 */
		public boolean isStruct() {
			return nestedStruct != null;
		}
	}

	/**
	 * Represents a Struct definition with a name and ordered fields
	 * 
	 * This class computes and stores the flattened memory layout of all fields,
	 * including nested fields, with their byte offsets relative to the struct
	 * start
	 * 
	 * The layout is linearized such that nested fields are flattend with dot
	 * notation keys, e.g., "loc.look.yaw"
	 */
	public static class StructLayout {
		private final String structName;

		// ordered list of fields declared in the struct
		private final List<StructField> fields = new ArrayList<>();
		
	 	// maps flattened field names to their offset
		private final Map<String, Integer> flatOffsets = new LinkedHashMap<>();

		/**
		 * Constructs a StructLayout with the given name and fields.
		 * 
		 * The constructor calculates the flattened memory layout offsets.
		 * 
		 * @param structName Name of the struct
		 * @param fields     Fields belonging to this struct, ordered as declared
		 */
		public StructLayout(String structName, StructField... fields) {
			this.structName = structName;
			int offset = 0;
			
			for (StructField f : fields) {
				if (!f.isStruct()) {
					// primitive field: simple offset mapping
					flatOffsets.put(f.name, offset);
					offset += f.size;
				} else {
					/*
					 * Nested struct fields are flattened by recursively prefixing nested field
					 * names with parent's field name.
					 * 
					 * EXPLANATION: This flattens nested struct fields into a single namespace with
					 * unique keys. It allows quick offset resolution of deeply nested fields by
					 * concatenating names with '::'
					 */
					for (Map.Entry<String, Integer> e : f.nestedStruct.flatOffsets.entrySet()) {
						String nestedKey = f.name + "::" + e.getKey();
						flatOffsets.put(nestedKey, offset + e.getValue());
					}
					offset += f.nestedStruct.sizeInBytes();
				}
				this.fields.add(f);
			}
		}

		/**
		 * Resolves the offset of a nested field within this struct by its path.
		 * 
		 * Example: resolveLayout("loc", "look", "yaw") returns the offset of field
		 * "loc.look.yaw"
		 * 
		 * @param path Varargs representing the nested field names in order
		 * @return The byte offset of the resolved field within this struct
		 * @throws IllegalArgumentException if the field path is invalid
		 */
		public int resolveLayout(String... path) {
			String key = String.join("::", path);
			Integer offset = flatOffsets.get(key);
			if (offset == null) {
				throw new IllegalArgumentException("NOT DECLARED: " + key);
			}
			return offset;
		}

		/**
		 * @return the total size in bytes of this struct, 
		 * including all nested structs
		 */
		public int sizeInBytes() {
			int total = 0;
			for (StructField f : fields) {
				if (!f.isStruct()) {
					total += f.size;
				} else {
					total += f.nestedStruct.sizeInBytes();
				}
			}
			return total;
		}

		/**
		 * Prints the layout of this struct to standard output, showing all fields and
		 * their flattened offsets.
		 */
		public void printLayout() {
			System.out.println("Struct " + structName + " Layout:");
			for (Map.Entry<String, Integer> entry : flatOffsets.entrySet()) {
				System.out.printf("  %s ; offset = %d%n", entry.getKey(), entry.getValue());
			}
		}
	}

	/**
	 * Example usage and test of the StructTable system.
	 */
	public static void main(String[] args) {
		StructLayout nest = new StructLayout("Nest", 
			new StructField("shit", 100),
			new StructField("abs", CHAR)
		);
		StructLayout s = new StructLayout("Test", 
			new StructField("x", 2),
			new StructField("c", 1),
			new StructField("nest", nest)
		);
		
		SymbolTable sym = new SymbolTable();
		sym.enterScope();
		sym.declareSymbol(new Symbol("teststruct", 2, false));
		sym.declareSymbol(new Symbol("test", 2, false));
		sym.debugPrint();
		
		s.printLayout();
		nest.printLayout();
		
		sym.exitScope();
	}
}
