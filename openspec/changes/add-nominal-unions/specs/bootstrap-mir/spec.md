## ADDED Requirements

### Requirement: MIR verifies monomorphic nominal union operations

MIR SHALL contain only concrete nominal-union applications whose construction, tag selection,
payload fields, moves, projections, matches, and cleanup refer to one verified target-layout entry.
Verification SHALL reject a foreign variant, wrong parent application, duplicate or missing field,
invalid tag decision, payload-layout mismatch, incomplete hierarchical coverage, or cleanup path for
an inactive variant.

#### Scenario: Verify one concrete constructor

- **WHEN** lowering emits a `Result<i32, Problem>.Failure` value
- **THEN** MIR verifies the canonical parent and variant, the specialized `Problem` payload, and the exact planned representation before execution

#### Scenario: Reject incomplete nested coverage

- **WHEN** a match plan over `HttpError | OutOfMemoryError` omits one `HttpError` variant without a covering parent or wildcard decision
- **THEN** MIR verification rejects the region rather than allowing a backend default branch

### Requirement: Nominal union MIR encoding is deterministic

Equivalent concrete union programs SHALL encode parent, variant, field, hierarchical coverage,
layout, and cleanup identities in canonical order independent of discovery or source-map traversal.

#### Scenario: Repeat nominal union MIR

- **WHEN** equivalent generic union facts are lowered under distinct valid discovery traversals
- **THEN** their concrete instance ordering and committed MIR encoding are byte-identical
