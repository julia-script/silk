## MODIFIED Requirements

### Requirement: First parameter reference fact

Every present bare-identifier expression SHALL resolve against the parameters of its enclosing
function and the binding statements that precede it in that function's body. Its reference fact
SHALL be `Resolved` with the exact parameter or binding identity and reference syntax when
exactly one declaration matches, `Missing` when none matches, `Ambiguous` with all matching
identities when multiple match, or unavailable when parser recovery did not supply usable
syntax. A resolved reference SHALL use its parameter's resolved declared type or its binding's
inferred type; all other reference or type states SHALL keep the expression type unavailable.
A binding SHALL NOT be referenced before its own statement completes.

#### Scenario: Resolve a returned parameter

- **WHEN** `identity(value: I32) -> I32` returns `value`
- **THEN** the returned expression resolves to parameter zero, has type `I32`, and the function return is compatible

#### Scenario: Resolve a parameter used as an argument

- **WHEN** a function passes its parameter `value` as a call argument
- **THEN** that argument's identifier reference resolves to the enclosing function's exact parameter declaration independently of the call target

#### Scenario: Preserve an ambiguous reference

- **WHEN** a bare identifier matches duplicate parameters in its enclosing function
- **THEN** the reference exposes every match, selects none, and its expression type remains unavailable

#### Scenario: Preserve parser ownership for a missing reference

- **WHEN** parser recovery inserts the identifier expression's token
- **THEN** the reference and type are unavailable without a semantic diagnostic

#### Scenario: Resolve a returned binding

- **WHEN** a body spells `let value = 42 return value`
- **THEN** the returned expression resolves to that binding with its inferred `I32` type

#### Scenario: Reject a use before the binding

- **WHEN** a body references a name whose `let` statement appears later
- **THEN** the reference is `Missing` at that span rather than resolving forward

### Requirement: Unknown parameter reference diagnostic

A present bare identifier with no matching local parameter or preceding binding SHALL retain a
`Missing` reference fact and produce one `SEM0006` diagnostic at the exact reference span.
Duplicate declarations SHALL rely on declaration-owned `SEM0005` diagnostics without adding a
second ambiguity diagnostic at the reference. Diagnostics SHALL remain deterministic and
phase-separated with existing lexical, parser, and semantic diagnostics.

#### Scenario: Diagnose an unknown value name

- **WHEN** a function returns `missing` without declaring a parameter named `missing`
- **THEN** the reference is missing and one `SEM0006` diagnostic identifies the exact identifier span

#### Scenario: Avoid duplicate ambiguity diagnostics

- **WHEN** a reference matches duplicate parameter declarations
- **THEN** only the later declarations carry `SEM0005` and no reference-site ambiguity diagnostic is added

#### Scenario: Repeat parameter analysis

- **WHEN** equivalent parameter declarations and references are analyzed repeatedly in fresh processes
- **THEN** parameter identities, lookup outcomes, reference facts, types, compatibility, and diagnostic ordering are identical

## ADDED Requirements

### Requirement: Local binding facts

Every complete binding statement SHALL publish one binding fact: its bound name, declaration
span, ordinal among the function's statements, and inferred type. The inferred type SHALL be its
initializer expression's type; an unavailable initializer type SHALL keep the binding's type
explicitly unavailable carrying the originating diagnostic's identity where one exists, and MUST
NOT default to `I32`. Name resolution SHALL remain flat and non-shadowing: a binding whose name
repeats an enclosing parameter or an earlier binding in the same function SHALL produce one
`SEM0008` diagnostic at the rebinding span carrying the original span as a related span, and
references to that name SHALL keep resolving to the original declaration.

#### Scenario: Infer a binding type from its initializer

- **WHEN** a body spells `let value = identity(42)` and `identity` resolves with an `I32` result
- **THEN** the binding fact carries inferred type `I32` and its exact declaration span

#### Scenario: Keep a damaged initializer explicit

- **WHEN** a binding's initializer contains an unresolved reference
- **THEN** the binding's inferred type is explicitly unavailable carrying the originating diagnostic's identity

#### Scenario: Reject shadowing a parameter

- **WHEN** `identity(value: I32)` declares `let value = 42`
- **THEN** one `SEM0008` diagnostic marks the rebinding with the parameter's span as a related span, and later `value` references still resolve to the parameter

#### Scenario: Reject rebinding a binding

- **WHEN** a body declares `let value = 1` and later `let value = 2`
- **THEN** the second binding carries one `SEM0008` diagnostic and references after it still resolve to the first binding

### Requirement: Move expressions are consuming references

A present `move <name>` expression SHALL resolve its name exactly like a bare-identifier
reference and SHALL carry the resolved declaration's type. The move itself SHALL remain an
explicit semantic fact distinct from an ordinary read so the ownership phase can treat it as a
consuming use. An unresolved moved name SHALL follow the same `Missing` fact and `SEM0006`
diagnostic as a bare identifier.

#### Scenario: Resolve a moved binding

- **WHEN** a body spells `let value = 42 return move value`
- **THEN** the returned expression is a move fact resolving to the binding with type `I32`

#### Scenario: Diagnose an unknown moved name

- **WHEN** a body moves a name with no matching parameter or binding
- **THEN** the move's reference fact is `Missing` with one `SEM0006` diagnostic at the name's span
