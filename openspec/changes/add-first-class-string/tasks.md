## 1. Semantic String Foundation

- [ ] 1.1 Add canonical non-scalar `string` identity, predicates, ordering, encoding, substitution, concreteness, and traversal to the compiler type actor with focused unit tests.
- [ ] 1.2 Resolve lowercase `string` in declared types, signatures, generic arguments, compatibility checks, presentations, and module semantic surfaces without adding it to the scalar catalog.
- [ ] 1.3 Add stable diagnostics and negative fixtures for direct string indexing, generic `length`, mutable byte exposure, implicit string/byte/`String` conversion, and duplicate `&string`-style view forms.
- [ ] 1.4 Update analysis, hover, inlay, completion, syntax-inspector, and deterministic semantic encodings to present canonical `string` distinctly from `&[u8]`.

## 2. Literal, HIR, and Ownership Split

- [ ] 2.1 Change text-literal elaboration to publish `string` while preserving existing UTF-8 decoding, static identity, and source provenance; keep byte-string literals on the immutable `u8` view path.
- [ ] 2.2 Add distinct HIR string literal/runtime-view facts, expression tree handling, encoders, and snapshots without reusing `Type.Slice`.
- [ ] 2.3 Extend ownership analysis so runtime `string` formation retains and ends the backing shared loan across bindings, calls, returns, and every structured exit.
- [ ] 2.4 Preserve string-backed loans nested inside generic nominal result data such as validated `Result<string, InvalidUtf8>`, with success, failure, move, escape, and owner-drop fixtures.

## 3. Sealed String Intrinsics

- [ ] 3.1 Add the unsafe unchecked UTF-8 formation, safe immutable UTF-8 bytes, safe byte length, and safe exact equality operations to the sealed intrinsic catalog and HIR builtin vocabulary.
- [ ] 3.2 Enforce exact intrinsic signatures, unsafe authorization, admission phases, lexical result loans, and rejection outside `unsafe` for unchecked formation.
- [ ] 3.3 Update the deterministic intrinsic inventory fixture and add catalog tests proving there is no allocation, normalization, traversal, or stdlib-`String` intrinsic.
- [ ] 3.4 Route `string == string` and `string != string` through exact intrinsic equality while rejecting mixed string/byte operands and preserving all existing scalar operator behavior.

## 4. Target Layout and MIR

- [ ] 4.1 Add a canonical string representation and calling shape to target layout planning, using distinct logical identity with address-provenance and target-sized byte-length lanes on current native/Wasm targets.
- [ ] 4.2 Extend layout verification and deterministic encoding to reject string/slice interchange even when their current physical lanes match.
- [ ] 4.3 Add MIR string type and operations for static formation, unchecked runtime formation, UTF-8 bytes, byte length, and exact equality, including operation trees, local/type discovery, and textual encoding.
- [ ] 4.4 Lower HIR literals, intrinsic calls, equality, parameters, returns, and lexical loan ends to the new MIR string paths without routing through ordinary slice operations.
- [ ] 4.5 Extend MIR verification with positive cross-call/static/runtime fixtures and negative forged-string, mutable-view, type-confusion, missing-loan-end, and calling-shape fixtures.

## 5. Evaluation

- [ ] 5.1 Add evaluator string values that retain static or runtime storage provenance, byte length, logical type identity, and lexical backing lifetime independently from host string identity.
- [ ] 5.2 Evaluate the four string operations with exact UTF-8 bytes and normalization-free equality, keeping malformed unchecked input outside recoverable safe behavior.
- [ ] 5.3 Add deterministic evaluator traces for static literals, validated runtime views, distinct backing storage equality, canonically equivalent inequality, calls/returns, and loan endings.

## 6. Standard-Library String Actor

- [ ] 6.1 Add canonical `silk/string` source with private `String` storage over ordinary `Bytes`, typed `InvalidUtf8` carrying the first invalid byte offset, and the required imports/exports/source-resolution fixtures.
- [ ] 6.2 Implement complete allocation-free UTF-8 validation in Silk, covering ASCII, multi-byte scalars, overlong forms, continuations, truncation, surrogates, and values above U+10FFFF before one unchecked intrinsic call.
- [ ] 6.3 Implement effectful `String` copy/construction and append from `string` with ordinary allocator failures, rollback, ownership, and Drop behavior inherited from `Bytes`/`Vector`.
- [ ] 6.4 Implement allocation-free `String.view`, explicit UTF-8 byte view, and explicit byte-length operations with lexical borrow fixtures.
- [ ] 6.5 Implement a scalar traversal cursor/step API over `string` with explicit byte offsets and tests for mixed-width Unicode without introducing another string storage type.
- [ ] 6.6 Add navigation, diagnostics, source archive, and stdlib behavior tests proving `String` remains ordinary Silk source and is never recognized by compiler spelling.

## 7. Native, Wasm, and Debug Tooling

- [ ] 7.1 Emit native LLVM storage/view formation, byte view, byte length, exact equality, parameters, and returns from the selected string layout, with native execution parity tests.
- [ ] 7.2 Emit equivalent direct Wasm string operations and calling paths, with static-memory, runtime-view, equality, and structured-exit parity tests.
- [ ] 7.3 Preserve logical `string` in compiler inspection artifacts, evaluator renderings, HIR/MIR text, and debug metadata while rendering byte slices as binary data.
- [ ] 7.4 Add debug/release fixtures proving UTF-8 string identification is present only in debug metadata and repeated native/Wasm artifacts remain deterministic.

## 8. Breaking Migration and Acceptance

- [ ] 8.1 Migrate stdlib, examples, pressure programs, fixtures, and tests that passed text literals as `&[u8]` to request explicit UTF-8 byte views; leave byte literals unchanged.
- [ ] 8.2 Add end-to-end programs covering literals, safe validation, unsafe authorization, owned copy/view/append, exact equality, scalar traversal, allocation failure, and invalid UTF-8 on evaluator/native/Wasm.
- [ ] 8.3 Update canonical syntax, semantic, ownership, HIR, instance, layout, MIR, evaluator, LLVM, Wasm, debug, and intrinsic-inventory snapshots for the breaking type distinction.
- [ ] 8.4 Document lowercase `string` versus uppercase `String`, explicit conversion costs, unsafe construction obligations, exact equality, and the absence of indexing or implicit normalization.

## 9. Verification

- [ ] 9.1 Run focused compiler, stdlib, ownership, evaluator, backend, debug-metadata, and language-tooling tests and resolve every string-specific failure.
- [ ] 9.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in the required order and record any pre-existing failure separately.
- [ ] 9.3 Run `pnpm check` and `pnpm release:candidate`, because shipped stdlib sources, compiler behavior, snapshots, and package contents change.
