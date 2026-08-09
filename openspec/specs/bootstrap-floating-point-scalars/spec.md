# bootstrap-floating-point-scalars Specification

## Purpose
Define conservative `f32` and `f64` values, literals, operations, conversions, representation access, and deterministic cross-engine behavior for numerical Silk programs.
## Requirements
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

### Requirement: Floating scalars provide deterministic sine and cosine

`f32` and `f64` SHALL each provide width-preserving `sin` and `cos` operations backed by one
canonical approximation contract rather than target host `libm` behavior. For finite inputs the
result SHALL be within four units in the last place of the correctly rounded mathematical value,
and evaluator, native, and WebAssembly execution SHALL return identical result bits.

#### Scenario: Preserve trigonometric width

- **WHEN** `f32.sin`, `f32.cos`, `f64.sin`, or `f64.cos` receives a value of its declared width
- **THEN** it returns a value of the same width with the canonical result bits

#### Scenario: Evaluate signed zero

- **WHEN** sine receives positive or negative zero
- **THEN** it returns zero with the input sign, while cosine returns positive one

#### Scenario: Evaluate non-finite inputs

- **WHEN** sine or cosine receives NaN or positive or negative infinity
- **THEN** it returns the canonical quiet NaN for that width on every engine

#### Scenario: Compare representative finite values

- **WHEN** the conformance corpus evaluates small, large, positive, negative, and quadrant-boundary finite inputs
- **THEN** every result satisfies the accuracy bound and has identical bits across all engines
