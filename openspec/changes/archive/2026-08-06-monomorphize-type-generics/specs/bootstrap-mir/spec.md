## ADDED Requirements

### Requirement: MIR contains only monomorphic generic instances

MIR lowering SHALL consume verified concrete instance keys and substitute every parameterized
logical type and operation before constructing the structured control DAG. Each function SHALL
retain provenance naming its generic declaration and concrete arguments, while the verifier MUST
reject open type parameters or missing concrete layout entries.

#### Scenario: Lower a concrete identity
- **WHEN** discovery supplies `identity<Token>`
- **THEN** MIR contains one concrete Token-typed function whose provenance names the generic declaration and `Token` argument

#### Scenario: Reject open generic MIR
- **WHEN** a malformed MIR function still contains type parameter `T`
- **THEN** verification rejects it before evaluation or backend emission

