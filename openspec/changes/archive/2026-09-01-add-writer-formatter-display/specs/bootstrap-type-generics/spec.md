## ADDED Requirements

### Requirement: Scalar source witnesses specialize through the ordinary static path

A concrete scalar satisfying an interface bound through a source-authored inline witness SHALL
select that witness by canonical declaration identity. Named bound-operation calls SHALL specialize
to finite monomorphic source calls with the interface's exact effect boundary, including any
declared failures and requirements. Scalar source selection SHALL introduce no runtime witness
dictionary, provider lookup, formatting intrinsic, or backend-specific dispatch.

#### Scenario: Specialize Display at i32

- **WHEN** a generic function bounded by `Display` applies its effectful display operation to `i32`
- **THEN** specialization selects the canonical inline `i32` witness and preserves its `WriterError` and mutable `Writer` contract

#### Scenario: Reach the scalar witness body

- **WHEN** no ordinary call names a selected inline scalar witness directly
- **THEN** instance discovery still retains the witness and every supported engine executes that source body

#### Scenario: Keep intrinsic and source scalar witnesses distinct

- **WHEN** one scalar conformance maps an operator to a sealed intrinsic and another interface-owned scalar conformance defines an inline source operation
- **THEN** each bound call selects its declared target without converting either witness kind into the other

#### Scenario: Reject an unavailable scalar witness before lowering

- **WHEN** a scalar conformance is incomplete, incompatible, non-local, or has no canonical inline target
- **THEN** specialization reports the phase-owned conformance failure and publishes no unlowerable call
