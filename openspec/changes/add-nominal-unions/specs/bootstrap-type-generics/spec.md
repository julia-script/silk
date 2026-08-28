## ADDED Requirements

### Requirement: Union variants specialize through parent-owned arguments

A generic union SHALL bind parameters once on its parent declaration. Variant field types, patterns,
ownership evidence, layouts, and constructors SHALL substitute the parent's canonical arguments;
variants SHALL NOT declare or infer an independent generic identity. Named-field construction MAY
supply a contiguous explicit prefix and infer the omitted suffix only from supplied fields under the
existing struct-construction rules. Unit construction and every variant pattern SHALL name a complete
parent application. Expected result types, scrutinee types, and later uses MUST NOT bind missing
arguments.

#### Scenario: Specialize two payload variants through one parent

- **WHEN** `Result<A, E>` is applied as `Result<i32, Problem>`
- **THEN** `Success.value` specializes to `i32`, `Failure.error` specializes to `Problem`, and both remain variants of the same applied parent

#### Scenario: Refuse expected-type inference for a unit variant

- **WHEN** `Option.None` omits `T` and is placed in a declaration expecting `Option<i32>`
- **THEN** construction reports `T` as uninferred and requires `Option<i32>.None`

### Requirement: Nominal variants never collapse during specialization

Every complete union application SHALL preserve the declaration's canonical ordered variant set
even when substitution makes two payload shapes equal, makes a payload uninhabited, or makes the
union representation coincide with another type. Structural unions nested in fields SHALL continue
to renormalize independently. An uninhabited payload SHALL NOT remove its variant from canonical
coverage, tag metadata, or layout; it remains unconstructible unless a valid value of that field type
is supplied.

#### Scenario: Preserve equal specialized payloads

- **WHEN** two variants carry `A` and `B` and the parent specializes both as `i32`
- **THEN** both variant identities and runtime alternatives remain distinct while any structural union inside a field follows ordinary normalization

#### Scenario: Preserve an uninhabited specialized variant

- **WHEN** `Result<A, E>.Failure` specializes its payload as `never`
- **THEN** `Failure` retains its canonical coverage leaf and private tag metadata, requires an arm in exhaustive matching, and cannot be constructed without a valid `never` expression
