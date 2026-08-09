# bootstrap-usize Specification

## Purpose

Define the unsigned pointer-width scalar used for target-sized lengths, capacities, layout
arithmetic, and addresses without sacrificing exactness or cross-target determinism.

## Requirements

### Requirement: Usize follows the selected target pointer width

`usize` SHALL be a Copy, cleanup-free unsigned integer with 64 value bits on required native targets and 32 value bits on `wasm32-unknown-unknown`. Uppercase `Usize` MUST NOT remain an alias. These facts remain private compiler ABI decisions.

#### Scenario: Select native usize

- **WHEN** a native program uses `usize`
- **THEN** values and calling lanes use the selected 64-bit unsigned range

#### Scenario: Select Wasm usize

- **WHEN** the same program targets `wasm32-unknown-unknown`
- **THEN** values and calling lanes use the 32-bit unsigned range

### Requirement: Contextual decimal literals retain exact magnitude

A non-negative literal in immediate `usize` context SHALL retain exact magnitude until target selection. The target SHALL reject an out-of-range value before MIR. An unconstrained integer SHALL default to `i32`; unary negation MUST NOT produce `usize`.

#### Scenario: Preserve a native-only value

- **WHEN** a `usize` literal fits native width but not Wasm width
- **THEN** native accepts it exactly and Wasm rejects it before MIR

### Requirement: Usize arithmetic is checked and unsigned

`usize` SHALL use the complete integer contract: homogeneous operands, checked defaults, unsigned comparisons, named checked/wrapping/saturating variants, and explicit conversions.

#### Scenario: Trap underflow

- **WHEN** `usize` subtraction would be negative
- **THEN** ordinary execution traps rather than wrapping
