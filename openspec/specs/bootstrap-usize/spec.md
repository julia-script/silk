# bootstrap-usize Specification

## Purpose

Define the unsigned pointer-width scalar used for target-sized lengths, capacities, layout
arithmetic, and addresses without sacrificing exactness or cross-target determinism.

## Requirements

### Requirement: Usize follows the selected target pointer width

`Usize` SHALL be a Copy, cleanup-free unsigned integer with 64 value bits on
`aarch64-apple-darwin`, `x86_64-unknown-linux-gnu`, and `aarch64-unknown-linux-gnu`, and 32 value bits
on `wasm32-unknown-unknown`. Its range SHALL be zero through `2^width - 1`. These representation
facts are private compiler ABI decisions rather than a stable external ABI.

#### Scenario: Select native Usize

- **WHEN** a program using `Usize` targets any required native profile
- **THEN** its values and calling shape use the selected target's 64-bit unsigned range

#### Scenario: Select Wasm Usize

- **WHEN** the same logical program targets `wasm32-unknown-unknown`
- **THEN** its values and calling shape use the 32-bit unsigned range

### Requirement: Contextual decimal literals retain exact magnitude

A non-negative decimal literal with an immediate expected type of `Usize` SHALL receive that type
and retain its exact mathematical magnitude until the compilation target is selected. The selected
target SHALL reject a magnitude outside its `Usize` range before MIR lowering. A literal without a
`Usize` context SHALL retain the existing `I32` behavior, and unary negation MUST NOT produce a
`Usize` value.

#### Scenario: Accept a native-only magnitude

- **WHEN** a `Usize`-context literal is greater than `2^32 - 1` but no greater than `2^64 - 1` on a native target
- **THEN** compilation retains the exact value and accepts it without JavaScript-number rounding

#### Scenario: Reject the same magnitude on Wasm

- **WHEN** that source targets `wasm32-unknown-unknown`
- **THEN** target validation reports the out-of-range literal before MIR or backend emission

### Requirement: Usize arithmetic is checked and unsigned

The ordinary addition, subtraction, multiplication, division, and remainder operators SHALL accept
two `Usize` operands and return `Usize`. Overflow, subtraction below zero, and division or remainder
by zero SHALL trap on every target. Equality and ordered comparisons SHALL use unsigned ordering and
return `Bool`. Mixed `I32` and `Usize` operands MUST NOT convert implicitly.

#### Scenario: Trap unsigned underflow

- **WHEN** a `Usize` subtraction would produce a negative mathematical result
- **THEN** evaluation and generated code trap rather than wrap

#### Scenario: Compare above the signed boundary

- **WHEN** two valid `Usize` values straddle the target's signed maximum
- **THEN** ordered comparison uses unsigned magnitude and returns the same `Bool` on every engine

#### Scenario: Reject mixed arithmetic

- **WHEN** an arithmetic expression combines `I32` and `Usize` without an explicit conversion
- **THEN** semantic analysis rejects the operands without changing either type
