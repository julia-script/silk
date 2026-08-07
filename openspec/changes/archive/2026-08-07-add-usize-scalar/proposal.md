## Why

Permanent allocation layout and capacity arithmetic must follow the selected target's pointer width,
but Silk currently exposes only `I32` and `Bool`. Adding `Usize` now prevents the allocation boundary
and future `Vector<T>` from fossilizing the runtime-slice ticket's intentionally temporary `I32`
length limit.

## What Changes

- Add `Usize` as a Copy, cleanup-free unsigned integer whose width is 64 bits on the three native
  bootstrap targets and 32 bits on `wasm32-unknown-unknown`.
- Context-type non-negative decimal literals as `Usize`, retain their exact magnitude through
  target-independent analysis, and range-check them after target selection without JavaScript-number
  precision loss.
- Add ordinary `Usize` arithmetic and comparisons using the existing operator surface. Addition,
  subtraction, multiplication, division, and remainder trap on overflow, underflow, or division by
  zero exactly as the bootstrap numeric model requires.
- Add compiler-owned target layouts and one word-width calling lane consumed uniformly by MIR,
  evaluator, native LLVM, and direct Wasm.
- Preserve the existing `I32` surface and byte-identical artifacts for programs that do not reach
  `Usize`; migrate slice length and add explicit integer conversion actors only when a concrete
  workload requires them.
- Add three-engine, determinism, and unified `/labs` coverage at both the Wasm-width boundary and
  values above 32 bits on native targets.

## Capabilities

### New Capabilities

- `bootstrap-usize`: Pointer-width unsigned values, contextual literals, checked arithmetic,
  comparisons, target representation, and runtime behavior.

### Modified Capabilities

- `bootstrap-syntax`: Accept `Usize` in type positions through the existing type-path grammar and
  preserve contextual decimal literals without adding literal suffix syntax.
- `bootstrap-semantic-facts`: Publish canonical `Usize` types, exact literal magnitudes, operator
  results, and target-dependent range availability.
- `bootstrap-diagnostics`: Report selected-target `Usize` range failures with exact magnitude and
  width while preserving deterministic phase ordering.
- `bootstrap-operator-semantics`: Resolve the ordinary arithmetic and comparison surface for
  same-typed `Usize` operands with unsigned checked behavior.
- `bootstrap-hir`: Retain exact `Usize` literals and typed operations without target or backend
  instruction vocabulary.
- `bootstrap-instances`: Discover `Usize` signatures and operations under ordinary canonical keys.
- `bootstrap-target-layout`: Plan target-width `Usize` size, alignment, scalar representation, and
  calling lanes before MIR lowering.
- `bootstrap-mir`: Represent monomorphic `Usize` literals and operations in the target-aware DAG and
  verify their selected word width and trap behavior.
- `bootstrap-evaluation`: Evaluate exact unsigned pointer-width values without JavaScript-number
  precision loss and with target-correct traps.
- `bootstrap-backend`: Lower `Usize` consistently to native LLVM word integers and Wasm `i32` while
  preserving unsigned operations and compiler-owned calling shapes.
- `bootstrap-compiler-driver`: Require evaluator/native/Wasm parity where targets overlap and native
  correctness for values above the Wasm range.
- `bootstrap-syntax-inspector`: Expose `Usize` syntax, facts, HIR, layout, MIR, and engine results in
  the unified workbench.

## Impact

- Compiler types, literal analysis, operator resolution, HIR, instance discovery, target layout,
  MIR, evaluator, native LLVM emission, direct Wasm emission, and public inspection projections under
  `packages/compiler`.
- Scalar and calling-lane APIs gain a word-width case; exact integer payloads become capable of
  retaining 64-bit unsigned values.
- Focused and three-engine fixtures plus `/labs` presets under `packages/compiler/test` and
  `apps/docs`.
- No `Isize`, fixed-width scalar family, bitwise/shift operations, implicit conversion, literal
  suffix, slice-length migration, allocation, raw pointer, or stable external ABI is added.
