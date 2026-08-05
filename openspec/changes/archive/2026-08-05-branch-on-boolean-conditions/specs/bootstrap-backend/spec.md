## ADDED Requirements

### Requirement: Booleans and comparisons emit natively

The backend SHALL represent `Bool` locals as `i32` zero-or-one values: comparisons emit `icmp`
plus a zero-extension into the destination local, and user branches reuse the existing
conditional-branch emission on the nonzero test. Emission SHALL remain deterministic, and the
compiled corpus SHALL agree with the interpreter on every branching program.

#### Scenario: Emit a comparison

- **WHEN** a program comparing two integers is emitted
- **THEN** the textual IR contains an `icmp` and a `zext` feeding the boolean local

#### Scenario: Branch natively arm by arm

- **WHEN** a branching corpus program compiles and runs
- **THEN** its native exit value equals the interpreter's result for the same condition
