## ADDED Requirements

### Requirement: MIR keeps strings logical and verifiable

MIR SHALL retain concrete `string` values, string calling paths, static string formation, lexical
runtime views, UTF-8 byte viewing, and unchecked construction as operations distinct from ordinary
slices. Verification SHALL enforce matching string types, complete provenance, valid loan endings,
and unsafe authorization while leaving UTF-8 validity of unchecked input as the source program's
unsafe obligation.

#### Scenario: Lower a static string through a call

- **WHEN** a static text literal crosses one internal `string` parameter and result boundary
- **THEN** MIR retains its logical string type, storage reference, byte count, and provenance at every operation

#### Scenario: Reject a forged safe string

- **WHEN** MIR attempts to construct `string` from a byte view without the accepted checked or unsafe formation path
- **THEN** verification rejects the program before evaluation or backend emission

#### Scenario: End an owned-string view loan

- **WHEN** a `string` view borrowed from `String` reaches its lexical end on success, failure, or control transfer
- **THEN** MIR ends the loan exactly once before the owner may move, mutate, or drop
