## ADDED Requirements

### Requirement: Local shared MIR is verified before every execution engine

Target-neutral MIR SHALL represent local-shared layout planning, initialization, clone, callback
access, and opaque-core drop with the canonical core and element types, selected-target layout
provenance, consuming initialization inputs, take-once callback modes, callback result type, access
loan provenance, and source spans. Its deterministic inspection form MUST NOT contain public actor
names, raw addresses, backend field offsets, allocator implementations, or conflict-policy types.

MIR verification SHALL reject mismatched layout provenance, reused or unconsumed initialization
inputs, unavailable core or element types, malformed callback modes or result types, and any access
result or executable state that retains the callback-scoped loan. Each rejection SHALL retain a
stable diagnostic identity and the causative source provenance. No evaluator or backend SHALL enter
and no partial executable artifact SHALL exist when verification rejects the program.

#### Scenario: Verify a complete lifecycle program

- **WHEN** MIR initializes one core from matching consumed inputs, clones it, invokes callback access, and drops both handles
- **THEN** verification retains the exact target layout identity, ownership transitions, callback modes, access loan, and opaque cleanup operations for execution

#### Scenario: Reject mismatched or incomplete initialization

- **WHEN** initialization uses mismatched layout provenance, reuses an input, or leaves an allocation or value unconsumed
- **THEN** verification reports the stable diagnostic with source provenance and no execution engine or partial artifact is entered

#### Scenario: Reject malformed callback access

- **WHEN** callback modes or result types are incompatible or a result or executable state retains the restricted access loan
- **THEN** verification reports the stable diagnostic with access provenance before evaluation or backend lowering

#### Scenario: Inspect target-neutral local shared MIR

- **WHEN** verified local-shared MIR is encoded repeatedly
- **THEN** its bytes are deterministic and contain canonical operation, type, layout, ownership, callback, and source facts without actor names, addresses, or backend offsets
