## 1. Establish metadata identity

- [x] 1.1 Add private metadata string, uniqued node, distinct node, local node, named metadata, and forward-reference tables.
- [x] 1.2 Implement owner-bearing Metadata handles, optional metadata boundaries, strings, tuples, constants, and empty tuple identity.
- [x] 1.3 Implement deterministic structural keys for uniqued metadata and unconditional allocation for distinct metadata.
- [x] 1.4 Add identity, distinctness, optional value, cross-builder, and byte-exact metadata string tests.

## 2. Implement forward references and traversal

- [x] 2.1 Implement typed metadata forward-reference creation with permitted target categories.
- [x] 2.2 Implement one-time resolution and reject cross-builder, wrong-kind, and repeated resolution.
- [x] 2.3 Implement iterative reachable-metadata traversal with cycle detection and stable insertion numbering.
- [x] 2.4 Reject unresolved reachable references before text or bitcode output and report all relevant reference context.
- [x] 2.5 Add self-cycle, mutual-cycle, resolved graph, unreachable forward reference, unresolved reachable reference, and invalid resolution tests.

## 3. Implement debug node families

- [x] 3.1 Implement debug file, compile unit, subprogram, lexical block, and location constructors and queries.
- [x] 3.2 Implement basic boolean, signed, unsigned, and floating debug types.
- [x] 3.3 Implement structure, union, enumeration, array, vector, pointer, member, typedef, and subroutine debug types.
- [x] 3.4 Implement enumerator, subrange, expression, and general tuple nodes.
- [x] 3.5 Implement local variable, parameter, global variable, and global-variable-expression nodes.
- [x] 3.6 Implement closed DI flag and subprogram flag value actors with exact bit and text mappings.
- [x] 3.7 Add field, flag, optional reference, recursive type, and distinctness fixtures for every node family.

## 4. Attach and strip metadata

- [x] 4.1 Implement named metadata creation and ordered operand retention.
- [x] 4.2 Implement global debug metadata, subprogram, and global-variable-expression attachments.
- [x] 4.3 Implement instruction debug locations, repeated-location tracking, branch weights, unpredictability, and supported metadata attachments.
- [x] 4.4 Derive emitted instruction ordinals from semantic instruction handles without counting arguments or block markers.
- [x] 4.5 Implement immutable strip and preserve modes that prevent stripped debug allocation and dangling attachments.
- [x] 4.6 Add stripped-versus-preserving semantic equivalence, changing location, repeated location, branch weight, and attachment ordinal tests.

## 5. Serialize metadata

- [x] 5.1 Add private metadata, metadata-kind, named-metadata, and attachment block descriptors for every supported node and attachment.
- [x] 5.2 Implement metadata index adapters for uniqued, distinct, inline, local, optional, and forward-resolved identities.
- [x] 5.3 Extend IrText with deterministic metadata definitions, references, flags, named metadata, locations, and attachments.
- [x] 5.4 Extend Bitcode with module metadata blocks, function attachment blocks, debug-location compression, and metadata-kind records.
- [x] 5.5 Add pinned Zig fixtures for every metadata tag, flag field, attachment kind, and stripping mode.
- [x] 5.6 Add LLVM round trips for recursive debug types, compile units, function locations, local variables, and global expressions.

## 6. Publish and verify metadata APIs

- [x] 6.1 Add explicit Metadata and public debug settings actor subpath exports and root namespaces.
- [x] 6.2 Document debug-preserving and stripped builder examples, forward-reference lifecycle, and attachment usage.
- [x] 6.3 Run pnpm typecheck, pnpm exec biome check ., and pnpm test in that order and resolve all change-related failures.
- [x] 6.4 Run pnpm check and pnpm release:candidate and record the successful metadata handoff.
