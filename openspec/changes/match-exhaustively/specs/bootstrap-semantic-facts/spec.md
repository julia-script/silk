## ADDED Requirements

### Requirement: Match facts retain source arms and canonical coverage

Semantic analysis SHALL publish the scrutinee type and access mode, source-ordered arms, resolved
nominal members, source and canonical field mappings, pattern bindings, guard outcomes, remaining
member set before and after each arm, reachability, result type, and complete-or-unavailable match
outcome. Failed lookups, damaged patterns, incompatible guards, and unavailable results SHALL retain
all independent facts with exact provenance and causal diagnostics.

#### Scenario: Inspect coverage arm by arm

- **WHEN** `Token` and `End` unguarded arms cover `Token | End`
- **THEN** facts show the canonical set before each arm and the empty remaining set after the second

#### Scenario: Retain an unknown member pattern

- **WHEN** one arm names an unresolved nominal type beside an independently valid arm
- **THEN** both arm facts remain queryable and only the dependent match outcome is unavailable

### Requirement: Pattern bindings are arm-local non-shadowing facts

Each accepted field binding SHALL have a stable identity under its arm, canonical field origin,
precise type, match access mode, declaration span, and lexical arm scope. A pattern binding SHALL
NOT shadow a parameter, body binding, or earlier binding in the same pattern, and it SHALL NOT be
visible in another arm, guard preceding its declaration, or code after the match.

#### Scenario: Resolve a field binding in its result

- **WHEN** `Token { kind, .. } => kind` is analyzed
- **THEN** the result reference resolves to the arm-local binding sourced from canonical `Token.kind`

#### Scenario: Reject pattern shadowing

- **WHEN** a pattern binds a name already declared in the enclosing function
- **THEN** the binding retains its source and field facts while analysis reports the original declaration as its conflict
