## 1. Closed operator vocabulary and lexing

- [x] 1.1 Add the public `Operator` actor with the closed prefix/infix vocabulary, spellings, precedence, associativity, and canonical builtin mappings, then export its package namespace and subpath.
- [x] 1.2 Add longest-match token kinds and lexer recognition for arithmetic, comparison, equality, negation, and pipeline spellings without changing comment, arrow, assignment, or lossless byte coverage.
- [x] 1.3 Extend token, lexer, public-export, and deterministic encoding tests for every single- and double-byte operator spelling and invalid-prefix recovery.

## 2. Lossless precedence and pipeline syntax

- [x] 2.1 Add grouped, prefix, infix, and pipeline concrete node vocabulary with stable spans, token ownership, traversal, and syntax encoding coverage.
- [x] 2.2 Replace expression-kind dispatch with precedence climbing that preserves directly signed decimal literals, grouping, right-associative prefix parsing, left-associative arithmetic, and non-associative comparison/equality levels.
- [x] 2.3 Parse left-associative qualified pipelines with optional later-argument lists, including the no-parentheses unary form, without admitting bare callable or method lookup syntax.
- [x] 2.4 Add bounded recovery for missing operands, closing parentheses, pipeline qualifiers/members, malformed operator sequences, and comparison chains while preserving following arguments, statements, and declarations.
- [x] 2.5 Add accepted, trivia-heavy, precedence, associativity, grouping, pipeline, and malformed parser fixtures proving exact reconstruction and deterministic trees.

## 3. Operator and pipeline semantic facts

- [x] 3.1 Extend expression facts with immutable operator and pipeline outcomes reusing existing argument, mapping, contract, type, reference, provenance, and causal-unavailable vocabularies.
- [x] 3.2 Resolve arithmetic, relational, boolean negation, `I32`/`Bool` equality, directly signed literals, and trapping non-literal negation through the closed compiler-known actor table with existing `SEM0012` operand diagnostics.
- [x] 3.3 Analyze pipelines left-to-right, insert the completed left fact at argument zero, and resolve builtin or namespace-qualified public targets through the existing name-resolution and declaration-index authorities.
- [x] 3.4 Suppress diagnostic cascades for damaged, inaccessible, missing, conflicting, and mistyped dependencies while keeping unrelated nested facts and exact causes queryable.
- [x] 3.5 Extend elaboration, analysis-facade, multi-module, and fresh-process tests for resolved and unavailable operator/pipeline facts and effective argument mappings.

## 4. Canonical HIR and backend-neutral lowering

- [x] 4.1 Erase resolved prefix/infix facts into existing typed HIR builtin calls and resolved pipelines into ordinary builtin or canonical declaration calls, retaining complete surface spans and unavailable causes.
- [x] 4.2 Add `Negate` and `Bool.equals`/`Bool.notEquals` to the closed builtin contracts, encode them deterministically, and lower negation to a generated zero plus the existing trapping `Subtract` MIR operation.
- [x] 4.3 Update recursive HIR/fact consumers, ownership, instance discovery, lowering, and facade queries to consume normalized expressions without reconstructing surface semantics.
- [x] 4.4 Refresh HIR, MIR, LLVM IR, bitcode, and WebAssembly goldens/digests intentionally, proving operator and qualified-call programs share canonical downstream operations.

## 5. Runtime and backend parity

- [x] 5.1 Add an operator corpus covering precedence, grouping, all arithmetic/comparison/equality operators, both prefix operators, signed minimum, overflow, division traps, and nested expressions with interpreter/native parity.
- [x] 5.2 Run valid and trapping operator and pipeline programs through WebAssembly using the snapshot's existing target/layout/MIR, with deterministic text/binary output and interpreter parity.
- [x] 5.3 Add imported-pipeline and source-order-independent driver fixtures proving canonical symbols, calls, MIR, diagnostics, executable behavior, and no backend-specific operator path.

## 6. Facade-only inspection

- [x] 6.1 Extend syntax/HIR inspection presets and components for every precedence level, grouping, prefix/infix resolution, pipeline insertion/chaining, scalar equality, damaged syntax, type errors, and causal unavailable states.
- [x] 6.2 Extend MIR, evaluation, LLVM, WebAssembly, and pipeline views to connect operator provenance to canonical operations and identical result/trap outcomes.
- [x] 6.3 Preserve browser-only state, accessible text equivalents, exact source navigation, and the automated facade-only import boundary for every new inspection path.

## 7. Verification and handoff

- [x] 7.1 Run targeted compiler and docs suites while implementing and keep the existing qualified-call, module, branching, native, and WebAssembly corpora passing.
- [x] 7.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`, reporting any pre-existing diagnostics separately.
- [x] 7.3 Run `pnpm release:candidate` for the new public actor/export and packed contents, then run strict OpenSpec validation and review every scenario against retained test evidence.
