## ADDED Requirements

### Requirement: Unicode scalar conversion is explicit and checked

The concrete scalar surface SHALL expose one checked `u32`-to-`char` conversion returning
`Option<char>` and one total explicit `char`-to-`u32` conversion. The checked conversion SHALL
accept exactly `0...0xd7ff` and `0xe000...0x10ffff`; it SHALL return `None` for surrogates and larger
integers without truncating or trapping. Both operations SHALL be available through ordinary
standard-library source backed by the smallest concrete Intrinsic primitives.

#### Scenario: Convert the maximum scalar

- **WHEN** checked conversion receives `0x10ffff`
- **THEN** it returns `Some<char>` containing that scalar on every engine

#### Scenario: Reject the surrogate hole

- **WHEN** checked conversion receives a value from `0xd800` through `0xdfff`
- **THEN** it returns `None` on every engine

#### Scenario: Recover the scalar integer

- **WHEN** explicit conversion receives any valid `char`
- **THEN** it returns the exact corresponding `u32`
