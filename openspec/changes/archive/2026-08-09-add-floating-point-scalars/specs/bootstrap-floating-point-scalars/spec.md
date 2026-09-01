## Purpose

Define conservative `f32` and `f64` values, literals, operations, conversions, representation access, and deterministic cross-engine behavior for numerical Silk programs.

## ADDED Requirements

### Requirement: Floating types and literals are explicit

Silk SHALL provide distinct lowercase `f32` and `f64` types. Decimal fractions and exponent literals SHALL retain exact source value until contextual rounding; an unconstrained floating literal SHALL default to `f64`.

#### Scenario: Contextually type f32

- **WHEN** `1.25e2` appears where `f32` is required
- **THEN** it receives the correctly rounded binary32 value 125

#### Scenario: Default to f64

- **WHEN** a floating literal has no numeric context
- **THEN** it receives `f64`

### Requirement: Floating behavior is conservative IEEE

`f32` and `f64` SHALL use IEEE binary32/binary64 round-to-nearest ties-to-even for basic arithmetic. Ordinary comparisons SHALL keep NaN unordered; signed zero SHALL be preserved by representation operations. Classification, total order, same-width integer bit reinterpretation, and explicit numeric conversions SHALL be named operations. Compilation MUST NOT enable fast-math assumptions implicitly.

#### Scenario: Compare NaN

- **WHEN** an ordinary ordered comparison receives NaN
- **THEN** it returns `false` in evaluation and both backends

#### Scenario: Reinterpret signed zero

- **WHEN** positive and negative `f64` zero are reinterpreted as `u64`
- **THEN** their bits differ only by the sign bit

### Requirement: Floating operations have engine parity

HIR, MIR, layout, evaluator, LLVM, and direct WebAssembly SHALL support every admitted float operation. Exact bit parity SHALL apply where specified; NaN arithmetic SHALL compare specified classification/order behavior when payload is unspecified.

#### Scenario: Compare float engines

- **WHEN** an admitted finite operation or representation round trip executes
- **THEN** evaluator, native, and WebAssembly agree on the specified value or bits
