# bootstrap-structural-unions Specification

## Purpose

Define canonical closed runtime alternatives and their contextual conversions before pattern matching adds narrowing and exhaustiveness.

## Requirements

### Requirement: Structural unions normalize to finite nominal member sets

A value union SHALL be an unordered, duplicate-free set of canonical nominal types. Nested unions
SHALL flatten, spelling order and duplicate members SHALL not affect identity, a one-member union
SHALL normalize to that member, and `never` SHALL denote the empty uninhabited union. Transparent
alias spelling MUST NOT affect normalized identity when aliases become available. Scalars, arrays,
borrows, and other non-nominal types SHALL NOT be direct union members.

#### Scenario: Normalize order nesting and duplicates

- **WHEN** equivalent type positions spell `Token | End`, `End | Token`, and `Token | (End | Token)`
- **THEN** all three produce one canonical type with the same deterministically ordered members

#### Scenario: Normalize the empty and singleton cases

- **WHEN** normalization receives `never | Token` or `Token | Token`
- **THEN** each normalizes to the precise nominal `Token` type

#### Scenario: Reject a non-nominal member

- **WHEN** a union type directly includes an array or borrow type
- **THEN** analysis reports the exact invalid member and publishes no available union type

### Requirement: Union conversion is immediate-contextual and monotonic

A nominal value MAY inject into an immediate expected union containing that nominal member. A union
value MAY widen to an immediate expected union containing every source member. These conversions
SHALL be monotonic, explicit in semantic facts and HIR, and SHALL NOT change the stored or inferred
type of the source expression or binding. A context requiring subtraction, member selection, or a
target union missing any source member SHALL be rejected until pattern narrowing exists.

Immediate expected contexts SHALL include declared return types, call parameters, struct fields,
contextual array elements, and assignment destinations. An expression without such a context SHALL
retain its precise inferred type; later uses MAY perform their own contextual conversion but MUST NOT
rewrite that earlier inference.

#### Scenario: Inject at a return boundary

- **WHEN** a function declared to return `Token | End` returns a nominal `Token`
- **THEN** the return expression retains type `Token` and one conversion injects it into the declared union

#### Scenario: Preserve precise binding inference

- **WHEN** an unannotated binding is initialized with `Token` and later passed to a `Token | End` parameter
- **THEN** the binding remains `Token` and only the call argument receives a contextual injection

#### Scenario: Widen a smaller union

- **WHEN** a `Token | End` value enters an immediate `Token | End | Fault` context
- **THEN** one widening preserves its active nominal member through the canonical target mapping

#### Scenario: Reject incompatible widening

- **WHEN** a `Token | Fault` value enters an expected `Token | End` context
- **THEN** analysis identifies `Fault` as the non-contained member and publishes no executable conversion

### Requirement: Runtime union identity is internal and deterministic

Every runtime union value SHALL contain exactly one active nominal member and its complete payload.
The compiler SHALL assign the active member's discriminant from canonical member identity and SHALL
preserve that member through injection, widening, calls, returns, aggregate storage, moves, and
cleanup. Numeric tags SHALL not be source-observable and SHALL carry no stable external ABI or
serialization promise.

#### Scenario: Transport a union through aggregates and calls

- **WHEN** an injected move-only member is stored in a struct, passed through a function, and returned in a wider union
- **THEN** every representation retains the same active nominal identity and complete payload

#### Scenario: Repeat union construction

- **WHEN** equivalent union programs compile in fresh processes
- **THEN** their canonical member order, discriminants, conversions, layouts, and encoded facts are identical

### Requirement: Union values obey affine ownership

Moving a non-Copy nominal payload into an owned union SHALL consume that payload. A union SHALL be
Copy only when every member is recursively Copy and cleanup-free; copying that union SHALL preserve
exactly one canonical active member and its complete payload without consuming or mutating the
source. Otherwise the union SHALL remain one move-only owner. Borrowed values SHALL NOT be stored as
union members, and cleanup SHALL act on exactly the active payload once.

#### Scenario: Consume an injected owner

- **WHEN** a move-only `Token` is injected into `Token | End`
- **THEN** the original owner becomes unavailable and the union owns the complete `Token`

#### Scenario: Copy an all-Copy union

- **WHEN** a `Step | VmDiagnostic` value whose two nominal members contain only Copy fields is copied
- **THEN** the copy and source retain the same canonical active member and complete payload and neither acquires a cleanup obligation

#### Scenario: Reject a partly move-only union copy

- **WHEN** one member of a structural union owns a move-only or Drop-bearing field
- **THEN** the complete union remains move-only and a requested whole-value copy is rejected

#### Scenario: Reject a stored borrow

- **WHEN** a contextual conversion attempts to inject a shared or exclusive borrow into an owned union
- **THEN** ownership rejects the conversion without fabricating an owned payload
