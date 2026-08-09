## MODIFIED Requirements

### Requirement: Integer literals retain exact contextual magnitude

Integer literals SHALL retain exact magnitude until typed. An immediate integer context, including a concrete ordinary-call parameter or pipeline-inserted parameter, SHALL select a type only when representable; an unconstrained integer SHALL default to `i32`. Range rejection MUST occur before MIR lowering without JavaScript-number rounding. Contextual literal selection MUST NOT convert an already-typed integer expression or otherwise introduce implicit numeric conversion.

#### Scenario: Type a byte magnitude

- **WHEN** `255` appears where `u8` is required
- **THEN** it receives `u8` with exact value 255

#### Scenario: Contextualize an ordinary call literal

- **WHEN** an exact integer literal is passed directly to a concrete integer parameter of an ordinary function
- **THEN** the literal receives the parameter type when its value is representable

#### Scenario: Contextualize a pipeline literal

- **WHEN** an exact integer literal is inserted as a concrete integer parameter by the pipeline operator
- **THEN** the literal receives the parameter type when its value is representable

#### Scenario: Reject a wide out-of-range magnitude

- **WHEN** a literal exceeds its selected integer range
- **THEN** analysis reports the range before MIR lowering

#### Scenario: Reject an already-typed mismatch

- **WHEN** an integer expression with an established type is passed to a different integer parameter type without an explicit conversion
- **THEN** analysis rejects the call rather than converting the expression
