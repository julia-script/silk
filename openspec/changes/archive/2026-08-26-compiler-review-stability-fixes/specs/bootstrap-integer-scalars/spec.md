## ADDED Requirements

### Requirement: Signed remainder overflow semantics are identical across executors

Signed integer `%` with operands `MIN` and `-1` SHALL trap on every executor (interpreter, wasm, native), consistent with the existing rule that ordinary arithmetic traps on invalid division/remainder. The checked remainder of `MIN` and `-1` SHALL return `None` on every executor, and no executor SHALL evaluate it through an operation whose result is undefined for those operands.

#### Scenario: Ordinary remainder of MIN by -1 traps everywhere

- **WHEN** a program evaluates `i32::MIN % -1` (or the equivalent for any signed width) on any executor
- **THEN** execution traps, and the same program traps identically on the interpreter, the wasm backend, and the native backend

#### Scenario: Checked remainder of MIN by -1 is None everywhere

- **WHEN** a program evaluates the checked remainder of `i32::MIN` and `-1` on any executor
- **THEN** the result is `None`, identically on the interpreter, the wasm backend, and the native backend

### Requirement: Rotate counts wrap modulo lane width on every executor

Rotate-left and rotate-right SHALL interpret the count modulo the operand's bit width using an unsigned (Euclidean) reduction, so negative and out-of-range counts wrap instead of degenerating, identically on every executor.

#### Scenario: Rotate by a negative count wraps

- **WHEN** a program evaluates `rotate_left(x, -1)` on an odd `i32` value on any executor
- **THEN** the result equals `rotate_left(x, 31)` — the low bit wraps into bit 31 — identically on the interpreter, the wasm backend, and the native backend
