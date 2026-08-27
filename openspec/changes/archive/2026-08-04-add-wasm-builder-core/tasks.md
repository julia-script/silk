# Tasks — add-wasm-builder-core

## 1. Package scaffolding

- [x] 1.1 Create `packages/wasm` with package.json (subpath exports, `effect` peer), tsconfig,
      tsconfig.test.json, LICENSE, and wire into pnpm workspace, turbo, and CI
- [x] 1.2 Add `WasmError` with reason variants (`InvalidInput`, `InvalidState`,
      `ValidationFailed`, `WrappedFailure`) and tests mirroring the repository error conventions
- [x] 1.3 Add `Builder.make` with options, serialized mutation gate, owner token, and
      internal state registry; cross-builder handle rejection tests

## 2. Types and declarations

- [x] 2.1 Add `ValType` as a tagged union (numeric, vector, reference variants) with text and
      binary projections
- [x] 2.2 Add `Type.func` with structural interning; duplicate-type dedup tests
- [x] 2.3 Add `Import` (func/table/memory/global) returning first-class handles
- [x] 2.4 Add `Func.declare`, `Table`, `Memory` (multiple), `Global` with initializer
      expressions, and optional names on all of them
- [x] 2.5 Add `Export` with duplicate-name rejection, and `Func.start` with signature check
- [x] 2.6 Add `Elem` (active/passive/declarative) and `Data` (active/passive) segments
- [x] 2.7 Add `ConstExpr` covering `*.const`, `ref.null`, `ref.func`, `global.get`, and
      extended constant expression operations, with its own validation

## 3. Instruction model

- [x] 3.1 Define the internal instruction table schema: mnemonic, opcode bytes (multi-byte
      capable), immediates shape, typing rule (uniform stack types or named procedure)
- [x] 3.2 Populate table rows for core 2.0 numeric, parametric, variable, memory (with
      multi-memory immediates), and control instructions
- [x] 3.3 Populate table rows for reference types, bulk memory/table operations, sign
      extension, saturating truncation, and tail calls
- [x] 3.4 Generate `Instr` constructors from the table; structured `block`/`loop`/`if`
      variants with nested arrays and block types; freeze all values

## 4. Validation

- [x] 4.1 Implement the spec validation algorithm: value stack, control-frame stack,
      per-frame unreachable mode, branch arity checking
- [x] 4.2 Implement the named typing procedures (calls and indirect calls, branches and
      br_table, select, locals, memory access, tail calls)
- [x] 4.3 Wire `Func.define(locals, body)` to run body validation atomically before commit;
      failed definitions leave no state
- [x] 4.4 Implement emit-time module validation (export uniqueness, segment offsets, start
      signature, limits) shared by both emitters
- [x] 4.5 Negative test corpus: one rejected case per validator rule

## 5. Emitters

- [x] 5.1 Implement emission-time index resolution over all index spaces (imports first,
      declaration order within kind)
- [x] 5.2 Implement `Binary.encode`: LEB128 writers, section framing, all baseline sections,
      `name` custom section
- [x] 5.3 Implement `WatText.render`: canonical text with `$name` identifiers (sanitized) and
      folded structured control flow
- [x] 5.4 Determinism tests: identical operation order ⇒ identical bytes and text

## 6. Parity and fixtures

- [x] 6.1 Pin `wasm-tools` version; write provenance docs (UPSTREAM.md equivalent) and a
      version-check guard in verification scripts
- [x] 6.2 Fixture generation script: representative modules per feature area, committed as
      expected bytes + expected text
- [x] 6.3 Verification script: byte/character-identical comparison, oracle validation, and
      text→binary round-trip equality
- [x] 6.4 Negative corpus verification: builder-rejected cases force-encoded are rejected by
      the oracle
- [x] 6.5 Wire fixture and parity verification into package scripts and CI

## 7. Documentation and release

- [x] 7.1 README with the add-two-numbers example (build → validate → both outputs) and scope
      statement listing deferred features and the Chrome-baseline destination
- [x] 7.2 JSDoc on all public modules per repository documentation conventions
- [x] 7.3 Changeset for the initial `@silklang/wasm` release
