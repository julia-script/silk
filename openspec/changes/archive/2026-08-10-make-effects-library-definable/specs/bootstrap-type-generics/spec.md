## ADDED Requirements

### Requirement: Generics distinguish value and contract-row kinds

Generic declarations SHALL bind ordinary type parameters, failure-row parameters, and requirement-
row parameters as distinct canonical kinds. Row binders SHALL use their channel identity in source,
such as `<A, !E, ?R>`, and MUST NOT be accepted where a value type is required or confused with a
nominal type of the same spelling.

#### Scenario: Bind all Effect channel kinds

- **WHEN** a generic Effect combinator declares value, failure-row, and requirement-row parameters
- **THEN** its body can use each parameter only in positions accepted by that parameter's kind

#### Scenario: Reject a row as a value

- **WHEN** a body uses failure-row parameter `E` as a field or ordinary parameter value type
- **THEN** analysis reports a deterministic kind mismatch before specialization

### Requirement: Contract rows support finite generic algebra

Failure and requirement rows SHALL support normalized union, membership, decomposition into a
selected entry plus an unknown remainder, and subtraction of the selected entry through generic
unification. Duplicate failure members SHALL normalize by canonical identity; duplicate
requirement entries SHALL normalize by capability, role, and strongest compatible access. The
compiler MUST reject ambiguous decompositions, absent selected entries, incompatible access, and
rows whose finite concrete specialization cannot be proven.

#### Scenario: Infer a selected failure remainder

- **WHEN** a generic declaration decomposes `Problem | Rest` by selecting `Problem`
- **THEN** inference binds `Rest` to the normalized unhandled failure members and composes the handler's failures

#### Scenario: Infer a requirement remainder for provide

- **WHEN** a generic provider selects `&Clock@Primary` from `&Clock@Primary | Rest`
- **THEN** inference binds `Rest` to every other normalized requirement and rejects a provider with incompatible access or role

### Requirement: Row specialization remains erased and deterministic

Generic row arguments SHALL be checked once, concretized through the reachable monomorphic
worklist, included in canonical specialization identity, and erased from runtime representation.
Equivalent normalized rows MUST produce identical instances and artifacts across fresh processes;
no row dictionary, type descriptor, capability name, or role string may be required at runtime.

#### Scenario: Reuse equivalent row specializations

- **WHEN** two calls infer the same failure and requirement members in different source orders
- **THEN** they reach the same canonical specialization and emit no runtime row object
