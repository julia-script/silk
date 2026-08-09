## Why

The lexer and stack VM independently need stable names for byte classes, opcodes, status codes, and
fixed bounds, but Silk currently forces every use to repeat an untyped literal. Explicitly typed
compile-time constants are the smallest language addition that makes those real programs easier to
read without introducing global runtime state or hidden initialization cost.

## What Changes

- Add top-level `[pub] const <name>: <primitive-type> = <literal>` declarations for `bool`, integer,
  and floating-point values.
- Make constants participate in the existing local, selected-import, and namespace-qualified value
  scope with the same visibility, duplicate, and provenance rules as functions and structs.
- Resolve a constant reference to its declared primitive type and inline its canonical literal into
  HIR/MIR, evaluation, native LLVM, and direct WebAssembly with no addressable storage or runtime
  initialization.
- Expose constants coherently through formatting, semantic facts, hover, navigation, occurrences,
  inspector artifacts, and fresh-process determinism.
- Replace representative repeated opcode/status literals in the pressure corpus with named typed
  constants and record whether the constrained surface removes the observed pain.
- Keep aggregate values, inferred constant types, computed initializers, address-taking, mutation,
  runtime globals, and effectful initialization outside this change.

## Capabilities

### New Capabilities

- `bootstrap-typed-constants`: Explicit primitive constant contracts, semantic resolution,
  zero-runtime-cost lowering, diagnostics, tooling, and pressure-program evidence.

### Modified Capabilities

- `bootstrap-lexer`: Reserve and classify the `const` keyword.
- `bootstrap-syntax`: Parse and recover lossless top-level constant declarations.
- `bootstrap-declaration-index`: Collect constants as canonical module members with typed literal
  headers.
- `bootstrap-name-resolution`: Resolve local, selectively imported, and qualified public constants
  in value position.
- `silk-source-formatting`: Format constant declarations without losing source structure.

## Impact

This touches the lexer/token vocabulary, CST/parser/formatter, declaration and module scopes,
semantic/HIR/MIR presentation, evaluator and both backends, editor intelligence, tests, labs, and
the real-program pressure corpus. It adds no runtime dependency and deliberately emits no global
storage: accepted constant references lower as the same immediate values as their literal
initializers.
