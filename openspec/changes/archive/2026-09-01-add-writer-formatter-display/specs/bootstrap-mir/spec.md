## ADDED Requirements

### Requirement: MIR represents verified referent-place operations

MIR SHALL lower referent Copy reads, compatible reborrows, and exclusive replacements through the
canonical place model. Verification SHALL check the reference subject, target type, access,
provenance, and operation compatibility. MIR encoding SHALL be deterministic and SHALL introduce no
formatting or referent intrinsic.

#### Scenario: Lower a scalar referent read

- **WHEN** HIR reads `u32` through a shared referent place
- **THEN** MIR loads the canonical place into a Copy result without consuming its owner

#### Scenario: Lower an exclusive replacement

- **WHEN** HIR replaces an exclusive referent
- **THEN** MIR emits the ordinary cleanup and store operations for that place

#### Scenario: Reject a forged place mismatch

- **WHEN** MIR claims a referent target or access incompatible with its reference subject
- **THEN** MIR verification rejects the program
