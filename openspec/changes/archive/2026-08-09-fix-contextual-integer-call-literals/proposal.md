## Why

The lexer pressure program exposed noisy `i32` conversions around byte comparisons. Focused call tests show that concrete ordinary calls and pipelines already contextualize exact literals correctly; the live defect is that an enclosing `bool` result context suppresses operand-to-literal context in expressions such as `return byte == 13`.

## What Changes

- Lock in concrete ordinary-call and pipeline parameter contexts with focused regression coverage.
- Keep an enclosing operator-result expectation from replacing the operator's operand context; a known `u8` operand must contextualize the other exact literal even when the complete expression returns `bool`.
- Reject a literal that is outside the selected parameter type before MIR lowering.
- Keep non-literal integer arguments homogeneous and explicit; this change does not introduce implicit numeric conversion, promotion, or overload selection.
- Remove the lexer pressure program's `i32` byte-comparison workaround and use `u8` parameters and literals directly.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-integer-scalars`: Clarify ordinary call, pipeline, and homogeneous-operator contexts for exact literals, while already-typed expressions still require explicit conversion.

## Impact

- Semantic expression and call-argument elaboration in `packages/compiler`.
- Integer-context regression coverage across analysis, HIR, MIR, evaluator, native LLVM, and direct WebAssembly.
- The visible lexer pressure example and its recorded findings.
- No syntax, runtime service, package export, or dependency change.
