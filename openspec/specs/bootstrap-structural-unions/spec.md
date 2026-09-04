# bootstrap-structural-unions Specification

## Purpose

Define canonical closed runtime alternatives and their contextual conversions before pattern matching adds narrowing and exhaustiveness.

## Requirements

### Requirement: Structural unions normalize to finite ordinary member sets

A value union SHALL be an unordered, duplicate-free set of canonical detached ordinary value
types. Nested unions SHALL flatten, spelling order and duplicate members SHALL not affect identity,
a one-member union SHALL normalize to that member, and `never` SHALL denote the empty uninhabited
union. Transparent alias spelling SHALL NOT affect normalized identity: a member that names an
alias contributes the alias's erased target, and a union alias contributes each of its members.
Scalars, arrays, `string`, nominal values, and exact or opaque executable values with a finite
representation MAY be direct members. Lexical borrows and bare callable or Effect contracts without
standalone storage SHALL NOT be direct members.

#### Scenario: Normalize order nesting and duplicates

- **WHEN** equivalent type positions spell `Token | i32`, `i32 | Token`, and `Token | (i32 | Token)`
- **THEN** all three produce one canonical type with the same deterministically ordered members

#### Scenario: Normalize the empty and singleton cases

- **WHEN** normalization receives `never | i32` or `i32 | i32`
- **THEN** each normalizes to the precise ordinary `i32` type

#### Scenario: Normalize through an alias

- **WHEN** `type Pair = Token | i32` is declared and a type position spells `Pair | Fault`
- **THEN** it produces the same canonical type as `Token | i32 | Fault`

#### Scenario: Reject a non-nominal member

- **WHEN** a union type directly includes a lexical borrow or bare executable contract without finite representation
- **THEN** analysis reports the exact invalid member and publishes no available union type

#### Scenario: Admit represented executable members

- **WHEN** a union includes an exact callable or opaque Effect value with a finite capture representation
- **THEN** the union retains its public contract and compiler-private represented member identity

### Requirement: Union conversion is immediate-contextual and monotonic

An ordinary value MAY inject into an immediate expected union containing its exact member. A union
value MAY widen to an immediate expected union containing every source member. These conversions
SHALL be monotonic, explicit in semantic facts and HIR, and SHALL NOT change the stored or inferred
type of the source expression or binding. A context requiring subtraction, member selection, or a
target union missing any source member SHALL be rejected until pattern narrowing applies.

Immediate expected contexts SHALL include declared return types, call parameters, struct fields,
contextual array elements, and assignment destinations. An expression without such a context SHALL
retain its precise inferred type; later uses MAY perform their own contextual conversion but MUST
NOT rewrite that earlier inference. A monomorphic generic specialization SHALL substitute and
renormalize its members, collapsing members that become identical and recomputing mappings without
retaining indistinguishable tags.

#### Scenario: Inject at a return boundary

- **WHEN** a function declared to return `i32 | Token` returns an `i32`
- **THEN** the return expression retains type `i32` and one conversion injects it into the declared union

#### Scenario: Preserve precise binding inference

- **WHEN** an unannotated binding is initialized with `i32` and later passed to an `i32 | Token` parameter
- **THEN** the binding remains `i32` and only the call argument receives a contextual injection

#### Scenario: Widen a smaller union

- **WHEN** an `i32 | Token` value enters an immediate `i32 | Token | Fault` context
- **THEN** one widening preserves its active ordinary member through the canonical target mapping

#### Scenario: Reject incompatible widening

- **WHEN** an `i32 | Fault` value enters an expected `i32 | Token` context
- **THEN** analysis identifies `Fault` as the non-contained member and publishes no executable conversion

#### Scenario: Collapse equal specialized members

- **WHEN** `A | B` is instantiated with both parameters equal to `i32`
- **THEN** the concrete instance carries `i32` and conversions recompute against that singleton without retaining two tags

### Requirement: Runtime union identity is internal and deterministic

Every runtime union value SHALL contain exactly one active ordinary member and its complete payload.
The compiler SHALL assign the active member's discriminant from canonical member identity and SHALL
preserve that member through injection, widening, calls, returns, aggregate storage, moves,
execution, and cleanup. Numeric tags and executable representation identities SHALL not be
source-observable and SHALL carry no stable external ABI or serialization promise.

#### Scenario: Transport a union through aggregates and calls

- **WHEN** injected scalar, array, nominal, and represented executable members are stored in aggregates, passed through functions, and returned in wider unions
- **THEN** every representation retains the same active ordinary identity and complete payload

#### Scenario: Repeat union construction

- **WHEN** equivalent union programs compile in fresh processes
- **THEN** their canonical member order, discriminants, conversions, layouts, and encoded facts are identical

### Requirement: Union values obey affine ownership

Moving an affine payload into an owned union SHALL consume that payload. A union SHALL be Copy only
when every normalized member has the compiler's sealed Copy property; copying that union SHALL
preserve exactly one canonical active member and its complete payload without consuming or mutating
the source. Otherwise the union SHALL remain one affine owner. Borrowed values SHALL NOT be stored
as union members, and cleanup SHALL act on exactly the active payload once.

#### Scenario: Consume an injected owner

- **WHEN** an affine `Token` is injected into `i32 | Token`
- **THEN** the original owner becomes unavailable and the union owns the complete `Token`

#### Scenario: Copy an explicitly all-Copy union

- **WHEN** every normalized member of `i32 | Array<i32, 2>` has sealed Copy evidence
- **THEN** the copy and source retain the same canonical active member and complete payload and neither acquires a cleanup obligation

#### Scenario: Reject structural Copy inference for a union member

- **WHEN** one nominal member contains only Copy fields but declares no `impl Copy`
- **THEN** the complete union remains affine

#### Scenario: Reject a partly affine union copy

- **WHEN** one member of a structural union owns an affine or Drop-bearing field
- **THEN** the complete union remains affine and a requested whole-value copy is rejected

#### Scenario: Reject a stored borrow

- **WHEN** a contextual conversion attempts to inject a shared or exclusive borrow into an owned union
- **THEN** ownership rejects the conversion without fabricating an owned payload

#### Scenario: Clean the active ordinary member

- **WHEN** a union holding a droppable non-nominal member leaves scope
- **THEN** every supported target cleans exactly that active payload once

### Requirement: Pattern selection uses exact normalized union members

Pattern analysis SHALL select ordinary structural-union roots by canonical normalized type identity.
Nominal values, scalars, arrays, strings, and finite represented executable members MAY be exact
whole-member selectors when they are valid members of the scrutinee. When an exact root is a nominal
union, a qualified variant pattern MAY additionally select one subordinate canonical variant leaf.
That selection SHALL retain a canonical path containing the structural root, applied nominal parent,
and variant; it SHALL NOT turn the variant into a structural-union member, invent a second membership
relation, or expose either numeric runtime tag.

#### Scenario: Select a scalar member

- **WHEN** an `i32 | string` value is matched by an `i32 number` pattern
- **THEN** the selected binding has exact type `i32` and coverage removes that canonical member

#### Scenario: Reject a foreign exact member

- **WHEN** a pattern selects `bool` from `i32 | string`
- **THEN** analysis identifies `bool` as absent from the normalized scrutinee members

#### Scenario: Select a subordinate nominal variant

- **WHEN** `HttpError.Dns { .. }` matches through structural root `HttpError` in `HttpError | OutOfMemoryError`
- **THEN** selection retains the complete root-parent-variant path while the normalized structural member set remains exactly `HttpError | OutOfMemoryError`

### Requirement: A nominal union is one atomic structural-union member

A complete represented nominal union MAY be one ordinary member of `A | B`. Structural
normalization SHALL use the applied parent union type as that member's identity and SHALL NOT flatten
its variants, merge a variant with another structural member, or expose its private tag. Ordinary
contextual injection and widening SHALL first construct or preserve the nominal union value and then
map that whole value as one structural member.

#### Scenario: Inject a nominal variant into a structural union

- **WHEN** `HttpError.Dns { ... }` enters an expected `HttpError | OutOfMemoryError`
- **THEN** construction first produces `HttpError` and structural conversion injects that complete nominal value as one member

#### Scenario: Preserve the nominal boundary during normalization

- **WHEN** one `HttpError` variant has the same payload shape as another structural member
- **THEN** normalization retains `HttpError` and the other member as distinct types and does not flatten the matching variant
