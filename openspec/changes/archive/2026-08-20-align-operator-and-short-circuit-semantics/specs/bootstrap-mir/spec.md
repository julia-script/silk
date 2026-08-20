## ADDED Requirements

### Requirement: MIR lowers marked dispatch and short-circuit control deterministically

MIR lowering SHALL resolve marked operator evidence to the same sealed intrinsic or source witness
used by ordinary interface specialization. Short-circuit lowering SHALL evaluate the left operand
first, enter the right region only when required, and join one Boolean result with path-correct
cleanup and typed Effect behavior. Evaluation, LLVM, and Wasm SHALL consume that same verified
structure.

#### Scenario: Lower a source operator witness

- **WHEN** a custom operator conformance maps its marked operation to an ordinary source function
- **THEN** MIR contains one statically selected call with the declared heterogeneous signature

#### Scenario: Skip an effectful right region

- **WHEN** the left Boolean decides a short-circuit expression
- **THEN** MIR execution skips every operation and cleanup local to the right region while producing the decided Boolean

#### Scenario: Emit engines consistently

- **WHEN** a valid operator and short-circuit corpus is evaluated and emitted for native and Wasm targets
- **THEN** every engine agrees on results, skipped work, traps, and cleanup order
