## ADDED Requirements

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
