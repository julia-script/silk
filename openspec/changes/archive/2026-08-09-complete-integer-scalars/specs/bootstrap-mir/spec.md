## ADDED Requirements

### Requirement: MIR represents the complete integer family

MIR SHALL carry canonical integer logical types, exact constants, conversions, arithmetic modes, comparisons, bitwise operations, shifts, rotates, and recoverable checked outcomes. Verification SHALL reject mismatched types, widths, constants, modes, or layouts; encoding SHALL remain deterministic and backend-neutral.

#### Scenario: Verify wide addition

- **WHEN** MIR contains valid checked `u64` addition
- **THEN** verification accepts one exact backend-neutral operation

#### Scenario: Reject a malformed conversion

- **WHEN** a conversion operand disagrees with its declared source type
- **THEN** verification reports the mismatch before evaluation or emission

### Requirement: MIR represents unit and bottom without payloads

MIR SHALL use zero result lanes for unit and permit `never` only on non-returning paths.

#### Scenario: Lower bare return

- **WHEN** a unit function executes bare `return`
- **THEN** MIR terminates with no scalar result local
