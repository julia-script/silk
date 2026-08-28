## MODIFIED Requirements

### Requirement: Integer operations are homogeneous and explicit

Ordinary arithmetic SHALL accept one identical integer type and trap on overflow, invalid
division/remainder, or invalid shift counts. Comparisons SHALL return `bool`. Every integer SHALL
expose bitwise operations, shifts, and rotates. Named recoverable checked operations SHALL remain
ordinary standard-library wrappers returning direct nominal `Option<T>` values; their sealed scalar
primitives SHALL report only the low-level present-or-absent outcome through carrier-neutral inputs
and MUST NOT construct or recognize `Option` by spelling. Wrapping and saturating variants SHALL
return `T`. No numeric conversion SHALL be implicit.

#### Scenario: Trap ordinary byte overflow

- **WHEN** `u8.add(255, 1)` executes
- **THEN** evaluation, native, and WebAssembly trap at the same operation

#### Scenario: Recover checked overflow

- **WHEN** `u8.checkedAdd(255, 1)` executes
- **THEN** it returns `Option<u8>.None`, while representable addition returns `Option<u8>.Some`

#### Scenario: Reject mixed arithmetic

- **WHEN** an expression combines `i32` and `i64` without conversion
- **THEN** analysis rejects it without choosing a wider type

#### Scenario: Rename a checked wrapper and carrier

- **WHEN** ordinary source calls the same checked scalar primitive with equivalent present and absent constructors for another nominal union
- **THEN** the primitive reports the same arithmetic outcome without compiler registration of either carrier or variant spelling
