## Why

The lexer pressure program exposed that an exact integer literal can be typed by intrinsic calls and result positions, but some ordinary user-function calls still default it to `i32` before checking a narrower parameter such as `u8`. This contradicts the existing exact-literal contract and forces low-level code to add noisy conversions at call boundaries.

## What Changes

- Make concrete integer parameter types an immediate context for exact integer literal arguments in ordinary direct calls.
- Preserve the same rule when a literal is supplied through the pipeline operator.
- Reject a literal that is outside the selected parameter type before MIR lowering.
- Keep non-literal integer arguments homogeneous and explicit; this change does not introduce implicit numeric conversion, promotion, or overload selection.
- Remove the lexer pressure program's `i32` byte-comparison workaround and use `u8` parameters and literals directly.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-integer-scalars`: Clarify that ordinary call parameters and pipeline-inserted parameters are immediate integer contexts for exact literals, while already-typed expressions still require explicit conversion.

## Impact

- Semantic expression and call-argument elaboration in `packages/compiler`.
- Integer-context regression coverage across analysis, HIR, MIR, evaluator, native LLVM, and direct WebAssembly.
- The visible lexer pressure example and its recorded findings.
- No syntax, runtime service, package export, or dependency change.
