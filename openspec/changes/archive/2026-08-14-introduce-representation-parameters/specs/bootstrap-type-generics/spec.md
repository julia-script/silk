## MODIFIED Requirements

### Requirement: Declarations bind canonical type parameters

Struct and function declarations SHALL accept ordered ordinary type parameters, failure-row
parameters, requirement-row parameters, callable representation parameters, and Effect
representation parameters. Every parameter identity SHALL be local to its declaration and distinct
from nominal types and parameters with the same spelling elsewhere. A parameter SHALL be available
only in positions admitted by its kind, and duplicate or unbound parameters MUST produce
deterministic diagnostics.

#### Scenario: Bind one generic struct parameter
- **WHEN** `pub struct Box<T> { pub value: T }` is analyzed
- **THEN** the field type refers to the canonical `T` parameter owned by `Box`, not to a nominal type named `T`

#### Scenario: Bind a representation parameter
- **WHEN** `pub struct Mapper<A, B, F: fn(A) -> B> { transform: F }` is analyzed
- **THEN** `F` is canonical to `Mapper` and can appear only as a represented callable value

#### Scenario: Reject a duplicate parameter
- **WHEN** a declaration introduces `<T, T>`
- **THEN** analysis reports the second parameter as a deterministic duplicate without fabricating another identity

### Requirement: Generic applications are explicit canonical types

Applying a generic nominal declaration SHALL require exactly one kind-correct argument per declared
parameter and SHALL produce a canonical applied type identified by the declaration plus normalized
ordered arguments. Construction MAY infer concrete representation arguments from corresponding
field initializers. Applying arguments to a non-generic declaration, omitting arguments in a required
type position, supplying the wrong arity, or supplying the wrong kind MUST remain explicit semantic
failures.

#### Scenario: Reuse one applied type identity
- **WHEN** independent declarations refer to `Box<Token>`
- **THEN** both references resolve to the same canonical applied type identity

#### Scenario: Infer a construction representation
- **WHEN** `Mapper` construction supplies a named function for field `F`
- **THEN** the complete applied type includes that exact representation argument

#### Scenario: Reject the wrong arity
- **WHEN** `Pair<i32>` refers to a declaration with two parameters
- **THEN** analysis reports the expected and actual argument counts and produces no available applied type
