## ADDED Requirements

### Requirement: MIR names and closes the selected entry explicitly

A MIR module SHALL retain an explicit entry descriptor independent of function order. An ordinary
entry descriptor SHALL identify the selected `I32` function. An effectful entry descriptor SHALL
identify the selected `Unit` Effect runner, its normalized failures, canonical report identities,
payload cleanup plans, and generated closing adapter. The verifier SHALL reject a missing,
ambiguous, signature-incompatible, open, or internally inconsistent descriptor before evaluation
or backend emission.

#### Scenario: Encode an ordinary entry explicitly

- **WHEN** lowering receives an ordinary `main() -> I32`
- **THEN** MIR names its canonical instance as the entry without relying on its function ordinal

#### Scenario: Encode an effectful entry adapter

- **WHEN** lowering receives an effectful `main() -> Unit ! SomeError`
- **THEN** MIR contains a generated scalar adapter that runs the effect and closes success and failure outcomes

#### Scenario: Verify failure cleanup metadata

- **WHEN** an effect entry descriptor's failure type, tag, payload local, and cleanup plan disagree
- **THEN** MIR verification reports deterministic entry-adapter violations

#### Scenario: Encode entry metadata deterministically

- **WHEN** equivalent programs are lowered repeatedly
- **THEN** their entry descriptors, generated adapter, failure ordering, and MIR text are identical
