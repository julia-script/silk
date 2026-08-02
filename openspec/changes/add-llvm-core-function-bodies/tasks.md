## 1. Establish function-body lifecycle

- [x] 1.1 Add the private FunctionBody draft state for arguments, blocks, instructions, names, extra payloads, value indices, and lifecycle status.
- [x] 1.2 Implement Function.buildBody with an Effect callback, draft invalidation, final validation, atomic commit, and rollback on failure.
- [x] 1.3 Add function-local owner tokens and reject cross-function arguments, blocks, values, instructions, and phi handles.
- [x] 1.4 Test successful commit, callback failure rollback, validation failure rollback, reuse after close, and concurrent use rejection.

## 2. Implement blocks and local values

- [x] 2.1 Implement argument handles and type queries derived from the function signature.
- [x] 2.2 Implement basic block creation, naming, cursor movement, predecessor tracking, and stable block indices.
- [x] 2.3 Implement local Value and Instruction handles, result detection, type queries, stable semantic order, and optional names.
- [x] 2.4 Implement incremental rejection of instructions after a terminator and commit-time rejection of empty or unterminated required blocks.

## 3. Implement core SSA operations

- [x] 3.1 Implement integer and floating unary and binary operations with no-wrap, exact, and core fast-math settings required by this slice.
- [x] 3.2 Implement integer and floating comparisons and select operations with exhaustive predicate mappings.
- [x] 3.3 Implement integer, floating-point, pointer, and address-space casts with source and destination validation.
- [x] 3.4 Implement extract-value and insert-value operations with aggregate path and result-type validation.
- [x] 3.5 Add valid chains and invalid operand/result fixtures for every core opcode family.

## 4. Implement control flow and phi nodes

- [x] 4.1 Implement unconditional and conditional branches with condition and destination ownership validation.
- [x] 4.2 Implement switch construction, case accumulation, duplicate-case rejection, branch weights, and finalization.
- [x] 4.3 Implement return, return-void, and unreachable with enclosing signature validation.
- [x] 4.4 Implement phi reservation, incoming-pair accumulation, normal and fast-math variants, forward values, and sealing.
- [x] 4.5 Implement commit-time CFG validation for terminators, predecessor sets, phi coverage, incoming types, and unresolved local handles.
- [x] 4.6 Add loop, diamond, switch, forward-reference, malformed CFG, and phi mismatch tests.

## 5. Implement calls

- [x] 5.1 Implement direct and indirect call construction with function type and argument validation.
- [x] 5.2 Implement supported calling conventions, attributes, tail kinds, fast-math flags, and operand-bundle storage for core calls.
- [x] 5.3 Implement call and return result typing, including void calls and vararg signature prefixes.
- [x] 5.4 Add call signature, attribute, calling-convention, tail-kind, and invalid return tests.

## 6. Serialize core function bodies

- [x] 6.1 Add private function-block record descriptors for declared blocks, core instructions, calls, control flow, phi nodes, and returns.
- [x] 6.2 Implement final module and function value indexing, relative operands, signed phi offsets, block indices, and constant offsets.
- [x] 6.3 Extend IrText with deterministic labels, local value names, instruction rendering, and function definitions.
- [x] 6.4 Extend Bitcode with function constant pools, core records, block order, and function body attachment to declarations.
- [x] 6.5 Add pinned Zig fixtures for every core instruction tag and multi-block control-flow shape.
- [x] 6.6 Add llvm-as, llvm-dis, verifier, and llvm-bcanalyzer round trips for representative core functions.

## 7. Publish and verify function APIs

- [x] 7.1 Add explicit FunctionBody, Block, and Value subpath exports and root namespaces.
- [x] 7.2 Document scoped body construction with arithmetic, branching, phi, call, and return examples.
- [x] 7.3 Run pnpm typecheck, pnpm exec biome check ., and pnpm test in that order and resolve all change-related failures.
- [x] 7.4 Run pnpm check and pnpm release:candidate and record the successful core-function handoff.
