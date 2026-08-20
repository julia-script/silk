## REMOVED Requirements

### Requirement: Generics distinguish value and contract-row kinds

**Reason**: Failure parameters are ordinary type parameters. Only requirement rows remain a
separate Effect-channel kind.

**Migration**: Replace `<A, !E, ?R>` with `<A, E, ?R>` and permit `E` in every ordinary type
position while retaining the specialized `?R` requirement-row kind.

### Requirement: Contract rows support finite generic algebra

**Reason**: This requirement combines two different domains: ordinary failure types and specialized
requirement rows.

**Migration**: Use ordinary structural-union relations and difference for failure types; retain
row-specific normalization, access labels, selection, and difference only for requirement rows.

## ADDED Requirements

### Requirement: Generics distinguish ordinary types from requirement rows

Generic declarations SHALL bind ordinary type parameters and requirement-row parameters as distinct
canonical kinds. A failure parameter `E` SHALL be an ordinary type parameter declared as `E`; the
`!` token SHALL appear only where an Effect contract labels its failure channel. A requirement-row
parameter SHALL remain declared as `?R` and SHALL be accepted only in requirement-row positions.

#### Scenario: Reuse a failure parameter as an ordinary value type

- **WHEN** a generic declaration binds `<E>` and returns `Effect<A ! E>`
- **THEN** the same canonical `E` may also type a parameter, local, field, handler input, or return value

#### Scenario: Reject a requirement row as a value

- **WHEN** a body uses requirement-row parameter `R` as a field or ordinary parameter value type
- **THEN** analysis reports a deterministic kind mismatch before specialization

### Requirement: Failure algebra is ordinary union algebra

Failure types SHALL use the same normalized finite structural unions, checked containment, and
`Without<E, S>` difference as ordinary value types. `never` SHALL be the empty type. A concrete
selected type or union `S` is contained in `E` only when every alternative in `S` belongs to `E`.
Difference SHALL remove those alternatives and SHALL be total as a type operation; operations that
promise to handle a selection MUST carry a separate checked containment constraint.

Open generic containment and difference SHALL remain static compiler facts, specialize
deterministically, and introduce no runtime dictionary. `Without` SHALL remain forward-computed:
expected result types MUST NOT infer `E` or `S` backwards.

Requirement rows SHALL retain their existing capability-role keys, access labels, provider
selection, row union, exact membership, subset, intersection, and difference semantics independently
of ordinary failure algebra.

#### Scenario: Subtract one ordinary failure alternative

- **WHEN** `Without<ProblemError | OtherError, ProblemError>` is specialized
- **THEN** it normalizes to ordinary type `OtherError`

#### Scenario: Subtract an ordinary failure union

- **WHEN** `Without<FirstError | SecondError | ThirdError, FirstError | ThirdError>` is specialized
- **THEN** it normalizes to ordinary type `SecondError`

#### Scenario: Preserve an open ordinary difference

- **WHEN** a generic declaration contains `Without<ProblemError | E, ProblemError>` and `E` remains open
- **THEN** analysis preserves the equivalent open ordinary type difference until specialization

#### Scenario: Reject inverse ordinary difference inference

- **WHEN** the only evidence for `E` is an expected type equivalent to `Without<E, ProblemError> = OtherError`
- **THEN** analysis reports `E` as underconstrained instead of choosing an inverse solution

#### Scenario: Check a selected union

- **WHEN** generic `S` is constrained by `S in E` and specializes to `FirstError | ThirdError` within concrete `E`
- **THEN** the common constraint solver accepts the complete selected subset without lifting `S` into a row kind
