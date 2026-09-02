## ADDED Requirements

### Requirement: A bound records the interface contract it names

A type parameter's bound SHALL retain the spelling and syntax its declaration supplies and SHALL be
resolved, during header completion, in the bounded declaration's own module scope. A bound that
names an interface SHALL record that interface's canonical identity together with its ordered
operation names; a bound that names nothing, or names a declaration that is not an interface, SHALL
remain unresolved and be reported at the specialization that would have had to satisfy it. A bound
MUST NOT be restricted to interfaces declared by the bounded declaration's own module.

#### Scenario: Bound an interface another module declares

- **WHEN** a module imports `Integer` from `silk/numeric` and declares `fn twice<T: Integer>(value: T) -> T`
- **THEN** the bound resolves to the imported interface and the declaration specializes for every conforming scalar

#### Scenario: Record a two-operation contract

- **WHEN** a declaration is bounded by an interface declaring `add` and `subtract`
- **THEN** its type parameter records the interface's canonical identity and both operation names in declaration order

#### Scenario: Report a bound that names no interface

- **WHEN** a bound names a struct, or a name nothing declares, and a call specializes that parameter
- **THEN** the call reports an unknown interface constraint naming the bound's spelling

### Requirement: Every operation of a bound is callable at its declared contract

A generic body SHALL be able to call each operation its parameter's bound declares, over the
canonical parameter, checked once before any concrete argument exists. An operator on a bound-typed
operand SHALL select the bound's operation exactly when the bound's recorded contract names the
operation that operator spells, and SHALL take that operation's declared parameter and result
types — substituting the parameter only where the compiler-known operation carries its actor's own
type. An operator the bound does not declare MUST remain unavailable on that parameter.

#### Scenario: Call two operations of one bound

- **WHEN** a body bounded by an interface declaring `add` and `subtract` evaluates `(left + right) - left`
- **THEN** both operations are selected from the bound and the body checks once over the parameter

#### Scenario: Keep a bound comparison at its declared result

- **WHEN** a bound declares `lessThan(left: T, right: T) -> bool` and the body compares two bound-typed values
- **THEN** the comparison results in `bool` rather than the bounded parameter

#### Scenario: Refuse an undeclared operator

- **WHEN** a body multiplies two values bounded by an interface declaring only `add` and `subtract`
- **THEN** the declaration is rejected before any concrete specialization can make the operation appear valid

### Requirement: Specialization requires a complete witness

Specialization SHALL admit a type argument for a bounded parameter only when that argument's
conformance to the bound maps every operation the interface declares. Each operation the selected
conformance leaves unmapped SHALL be reported by name at the specialization, and a bound with more
than one operation MUST NOT be satisfied by a witness supplying only some of them.

#### Scenario: Reject a partial witness by operation name

- **WHEN** a bound declares `add` and `subtract` and the type argument's conformance maps only `add`
- **THEN** the specialization is rejected with a diagnostic naming `subtract` as the operation the argument does not implement

#### Scenario: Admit a complete witness

- **WHEN** the type argument's conformance maps every operation the bound declares
- **THEN** the specialization is admitted and selects each concrete operation without runtime dispatch
