# bootstrap-floating-point-scalars Specification

## Purpose

Define conservative `f32` and `f64` values, literals, operations, conversions, representation access, and deterministic supported-target behavior for numerical Silk programs.

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
- **THEN** native and LLVM-generated WebAssembly artifacts return `false`

#### Scenario: Reinterpret signed zero

- **WHEN** positive and negative `f64` zero are reinterpreted as `u64`
- **THEN** their bits differ only by the sign bit

### Requirement: Floating operations are target-consistent

HIR, MIR, layout, LLVM native and WebAssembly artifacts SHALL support every admitted float operation. Exact bit parity SHALL apply where specified; NaN arithmetic SHALL compare specified classification/order behavior when payload is unspecified.

#### Scenario: Compare float engines

- **WHEN** an admitted finite operation or representation round trip executes
- **THEN** native and WebAssembly agree on the specified value or bits

### Requirement: Floating scalars provide deterministic sine and cosine

`f32` and `f64` SHALL each provide width-preserving `sin` and `cos` operations backed by one
canonical approximation contract rather than target host `libm` behavior. For finite inputs the
result SHALL be within four units in the last place of the correctly rounded mathematical value,
and native and WebAssembly execution SHALL return identical result bits.

#### Scenario: Preserve trigonometric width

- **WHEN** `f32.sin`, `f32.cos`, `f64.sin`, or `f64.cos` receives a value of its declared width
- **THEN** it returns a value of the same width with the canonical result bits

#### Scenario: Evaluate signed zero

- **WHEN** sine receives positive or negative zero
- **THEN** it returns zero with the input sign, while cosine returns positive one

#### Scenario: Evaluate non-finite inputs

- **WHEN** sine or cosine receives NaN or positive or negative infinity
- **THEN** it returns the canonical quiet NaN for that width on every supported target

#### Scenario: Compare representative finite values

- **WHEN** the conformance corpus evaluates small, large, positive, negative, and quadrant-boundary finite inputs
- **THEN** every result satisfies the accuracy bound and has identical bits across supported targets

### Requirement: Floating scalars provide exact sign, rounding, and ordering operations

`f32` and `f64` SHALL each provide width-preserving `abs`, `copysign`, `floor`, `ceil`, `round`,
`trunc`, `min`, and `max` operations. Each SHALL be defined by sign-bit and exponent manipulation,
or by comparison, over operations this specification already admits. Each is therefore exact by
construction, needs no approximation contract, and SHALL NOT reach the platform `libm` or any LLVM
math intrinsic. Native and WebAssembly execution SHALL return identical result bits.

`round` SHALL round a half away from zero, which is the reading the name carries in C and Rust; the
ties-to-even form is a distinct operation this specification does not yet admit. Signed zero SHALL
be preserved, so a value in `(-1.0, -0.0]` SHALL floor, round, and truncate to negative zero.
`min` SHALL order negative zero below positive zero and `max` SHALL order it above. `min` and `max`
SHALL return the canonical quiet NaN when either operand is NaN, which is the NaN-propagating
choice IEEE-754-2019 names `minimum` and `maximum`.

Every one of these operations SHALL return the canonical quiet NaN for that width when given a NaN
input, so no engine's NaN payload propagation rule is observable. For `copysign` this obligation
outranks sign transfer: a NaN magnitude gives the canonical quiet NaN, which is positive, rather
than a signed NaN.

#### Scenario: Preserve negative zero through rounding

- **WHEN** `abs`, `floor`, `ceil`, `round`, or `trunc` receives negative zero, or `trunc` receives a
  value in `(-1.0, -0.0)`
- **THEN** every supported target returns negative zero, except `abs`, which returns positive zero

#### Scenario: Round a half away from zero

- **WHEN** `round` receives `2.5`, `-2.5`, or the value one unit in the last place below `0.5`
- **THEN** it returns `3.0`, `-3.0`, and `0.0` respectively, with identical bits on every supported target

#### Scenario: Order signed zero through min and max

- **WHEN** `min` or `max` receives positive and negative zero in either argument order
- **THEN** `min` returns negative zero and `max` returns positive zero

#### Scenario: Propagate a NaN operand through min and max

- **WHEN** `min` or `max` receives a NaN in either operand
- **THEN** it returns the canonical quiet NaN for that width on every supported target

#### Scenario: Canonicalize a NaN input

- **WHEN** any of `abs`, `copysign`, `floor`, `ceil`, `round`, `trunc`, `min`, `max`, or `sqrt`
  receives a NaN
- **THEN** it returns the canonical quiet NaN bit pattern for that width on every supported target

### Requirement: Floating scalars provide a correctly rounded square root

`f32` and `f64` SHALL each provide a width-preserving `sqrt` operation returning the correctly
rounded mathematical square root, with identical result bits on native and WebAssembly
execution.

`sqrt` is the sole exception to the prohibition on lowering a floating operation to an LLVM math
intrinsic, and the exception rests on the standard rather than on convenience. IEEE-754 mandates
that a square root be correctly rounded, so for a given operand exactly one result is admissible
and every conforming implementation must produce it: `llvm.sqrt`, the WebAssembly `f64.sqrt` and
native and WebAssembly `f32.sqrt` opcodes agree because the standard requires
them to, not because the hosts happen to match. Target consistency is therefore preserved by
construction. No comparable mandate covers `pow` or `log`, whose results are implementation-defined
and which SHALL continue to reach neither an LLVM math intrinsic nor the platform `libm`.

That mandate governs numeric results only. IEEE-754 leaves unspecified the sign of a NaN produced
by an invalid operation, and hosts disagree: an x86 square root of a negative operand yields a NaN
with the sign bit set, while AArch64's default NaN clears it. `sqrt` SHALL therefore screen a NaN
operand and a negative operand before the primitive executes and return the canonical quiet NaN, so
the primitive is reached only on the domain where the standard makes it bit-exact. Negative zero is
not negative for this purpose and SHALL pass through, returning negative zero as IEEE-754 requires.

#### Scenario: Round a square root exactly

- **WHEN** `sqrt` receives an operand whose root is irrational, such as two, three, or ten
- **THEN** every supported target returns the correctly rounded root with identical bits

#### Scenario: Root a subnormal operand

- **WHEN** `sqrt` receives a subnormal operand
- **THEN** it returns the correctly rounded normal result, since no square root of a representable
  value is subnormal

#### Scenario: Root the special values

- **WHEN** `sqrt` receives positive zero, negative zero, or positive infinity
- **THEN** it returns that same value, preserving the sign of either zero

#### Scenario: Screen an invalid square root

- **WHEN** `sqrt` receives a negative operand other than negative zero
- **THEN** every supported target returns the canonical quiet NaN, rather than the host's unspecified NaN sign

#### Scenario: Lower a square root to the native instruction

- **WHEN** `sqrt` is compiled
- **THEN** LLVM emits `llvm.sqrt` for native artifacts and lowers it to `f64.sqrt` or `f32.sqrt`
  for WebAssembly, and neither artifact imports or calls a `libm` symbol

### Requirement: Concrete floating primitives use the Intrinsic namespace

Every concrete `f32` and `f64` arithmetic, comparison, classification, total-order, bit-conversion,
square-root, transcendental, and numeric-conversion primitive SHALL be a type-specific member of
`Intrinsic`.
Source-defined floating actor modules and numeric interfaces SHALL wrap those primitives without
changing their deterministic semantics or supported-target consistency.

#### Scenario: Specialize generic floating addition

- **WHEN** a generic numeric addition is instantiated with `f64`
- **THEN** its canonical conformance selects the concrete `f64` intrinsic with no runtime type dispatch

#### Scenario: Preserve bit conversion

- **WHEN** a source wrapper converts an `f32` to and from its bit representation
- **THEN** native LLVM and LLVM-generated WebAssembly execution preserve the same bits through the concrete intrinsics

### Requirement: Float remainder is exact IEEE fmod on every executor

Floating-point `%` SHALL produce the exact IEEE-754 remainder (fmod semantics: the result of `x - n*y` where `n` is `x/y` truncated toward zero, computed without intermediate rounding or overflow) for both `f32` and `f64`, identically in LLVM-generated native and WebAssembly artifacts.

#### Scenario: Extreme-magnitude operands do not overflow

- **WHEN** a program evaluates `1e308 % 1e-308` as `f64` on any executor
- **THEN** the result is the exact finite fmod value in `[0, 1e-308)` — never infinity or NaN — identically on all three executors

#### Scenario: Ordinary operands agree bit-for-bit

- **WHEN** the same float remainder expression runs in LLVM-generated native and WebAssembly artifacts
- **THEN** all three produce the identical bit pattern
