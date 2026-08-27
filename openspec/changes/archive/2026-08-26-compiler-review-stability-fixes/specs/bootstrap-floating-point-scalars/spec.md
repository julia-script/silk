## ADDED Requirements

### Requirement: Float remainder is exact IEEE fmod on every executor

Floating-point `%` SHALL produce the exact IEEE-754 remainder (fmod semantics: the result of `x - n*y` where `n` is `x/y` truncated toward zero, computed without intermediate rounding or overflow) for both `f32` and `f64`, identically on the interpreter, the wasm backend, and the native backend.

#### Scenario: Extreme-magnitude operands do not overflow
- **WHEN** a program evaluates `1e308 % 1e-308` as `f64` on any executor
- **THEN** the result is the exact finite fmod value in `[0, 1e-308)` — never infinity or NaN — identically on all three executors

#### Scenario: Ordinary operands agree bit-for-bit
- **WHEN** the same float remainder expression is evaluated on the interpreter, the wasm backend, and the native backend
- **THEN** all three produce the identical bit pattern
