## 1. Extend builder state for declarations

- [x] 1.1 Add private ordered tables for layout specs, strings, types, attributes, constants, globals, aliases, variables, and functions.
- [x] 1.2 Add deterministic canonical-key encoders for byte strings, numeric values, ordered handle sequences, and tagged payloads.
- [x] 1.3 Add shared owner and index lookup boundaries for every new public handle type with cross-builder tests.

## 2. Implement data layout and type actors

- [x] 2.1 Implement Alignment and AddrSpace value actors with byte-unit conversions, ordering, defaults, and LLVM text forms.
- [x] 2.2 Implement DataLayout parsing for supported endian, integer, float, vector, pointer, native-width, stack-alignment, and non-integral address-space fields.
- [x] 2.3 Implement DataLayout queries and malformed-component SilkError coverage.
- [x] 2.4 Implement primitive and arbitrary-width integer type construction and interning.
- [x] 2.5 Implement pointer, function, fixed/scalable vector, and array type construction and interning.
- [x] 2.6 Implement anonymous, packed, named, opaque, and target-extension structures, including one-time named-body completion.
- [x] 2.7 Implement Type queries for tags, scalar/child types, function signatures, aggregate shapes, size, and alignment.
- [x] 2.8 Add structural interning, recursive structure, boundary width, and cross-owner type tests.

## 3. Implement attributes and constants

- [x] 3.1 Implement the pinned attribute variants and their validated data representations.
- [x] 3.2 Implement canonical parameter, return, and function attribute sets with stable ordering and editing operations.
- [x] 3.3 Implement arbitrary-width signed and unsigned integer constants using bigint normalization.
- [x] 3.4 Implement exact raw bit records and convenience constructors for every supported floating-point format.
- [x] 3.5 Implement null, none, zero, undef, poison, string, array, vector, structure, and splat constants with shape validation.
- [x] 3.6 Implement the constant expressions assigned to this change and defer advanced operation expressions explicitly to add-llvm-advanced-ir.
- [x] 3.7 Add exact-value, NaN-payload, aggregate-mismatch, canonicalization, and owner-validation tests.

## 4. Implement global declaration actors

- [x] 4.1 Implement the ordered Global symbol table, name lookup, collision reservation, anonymous naming, rename, replacement, and deletion semantics.
- [x] 4.2 Implement Variable creation and mutation for initializer, mutability, thread-local model, section, alignment, and debug-placeholder fields.
- [x] 4.3 Implement Alias creation and aliasee mutation with type and address-space validation.
- [x] 4.4 Implement Function declarations with type, linkage, visibility, preemption, calling convention, attributes, section, alignment, and canonical repeat behavior.
- [x] 4.5 Implement supported global-to-variable, global-to-alias, and global-to-function conversions atomically.
- [x] 4.6 Add declaration collision, rename, replacement, conversion, and property round-trip tests.

## 5. Serialize module declarations

- [x] 5.1 Add private record descriptors and adapters for type, parameter-attribute, parameter-attribute-group, constant, module declaration, and string-table records.
- [x] 5.2 Extend IrText with exhaustive renderers for layouts, types, constants, attributes, variables, aliases, and function declarations.
- [x] 5.3 Extend Bitcode with deterministic type widths, constant and global indices, declaration ordering, and every record introduced by this change.
- [x] 5.4 Add pinned Zig fixtures for each type tag, attribute storage kind, constant tag, and global declaration category.
- [x] 5.5 Add representative llvm-as, llvm-dis, verifier, and llvm-bcanalyzer declaration round trips.

## 6. Publish and verify declaration APIs

- [x] 6.1 Add explicit subpath exports and root namespaces for DataLayout, Alignment, Type, Attribute, Constant, Global, Variable, Alias, and Function.
- [x] 6.2 Add didactic package examples for an explicit target/data layout, a global constant, a variable, an alias, and a function declaration.
- [x] 6.3 Run pnpm typecheck, pnpm exec biome check ., and pnpm test in that order and resolve all change-related failures.
- [x] 6.4 Run pnpm check and pnpm release:candidate and record the successful declaration handoff.
