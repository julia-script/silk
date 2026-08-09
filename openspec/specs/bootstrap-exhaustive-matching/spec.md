# Bootstrap Exhaustive Matching Specification

## Purpose

Define exhaustive, mode-aware inspection of nominal values and structural unions with precise narrowing, affine pattern bindings, guarded source-order arms, and deterministic result typing.

## Requirements

### Requirement: Match access mode is explicit and lexical

A match SHALL evaluate its scrutinee exactly once. Bare `match value` SHALL be accepted only for a
Copy scrutinee. `match move value` SHALL consume one complete owned value. `match &value` SHALL
create shared match-local bindings, and `match &mut value` SHALL require one mutable live place and
create exclusive match-local bindings. Shared and exclusive bindings MUST NOT escape their arm,
enter owned storage, or be moved, returned, or captured beyond the match.

#### Scenario: Borrow then reuse an owner

- **WHEN** a move-only union is matched through `&` and one arm reads a Copy field
- **THEN** the shared pattern binding ends with the selected arm and the original owner remains usable after the match

#### Scenario: Consume one union

- **WHEN** a move-only union is matched through `move`
- **THEN** the original source becomes unavailable and exactly one selected arm owns its active payload

#### Scenario: Reject a bare move-only match

- **WHEN** a bare match scrutinee is not Copy
- **THEN** analysis reports that an explicit consuming or borrowing mode is required

### Requirement: Nominal patterns bind complete member structure

A nominal pattern SHALL identify one canonical nominal type and destructure fields by canonical
field identity. A field may bind under its own name or an explicit local name, nested nominal
patterns SHALL recurse under the match access mode, and `..` SHALL explicitly acknowledge omitted
fields. A pattern without `..` SHALL name every field exactly once. Pattern bindings SHALL be flat,
arm-local, non-shadowing declarations with precise narrowed types and exact source provenance.

#### Scenario: Bind and omit fields while consuming

- **WHEN** `Token { kind, .. }` matches a consumed `Token` payload
- **THEN** `kind` becomes one owned or Copy binding as its field requires and every omitted field remains selected for branch-local cleanup

#### Scenario: Bind a nested field under another name

- **WHEN** a nested nominal pattern spells `span: Span { start: offset, .. }`
- **THEN** the selected arm binds `offset` to the canonical `Span.start` field with the outer match mode and exact nested provenance

#### Scenario: Reject an incomplete pattern

- **WHEN** a nominal pattern omits a declared field without `..`
- **THEN** analysis retains every supplied field fact and reports the missing field without creating an executable arm

### Requirement: Coverage uses canonical union subtraction

Match arms SHALL be considered in source order over the canonical member set of the scrutinee. An
unguarded nominal arm SHALL remove its member from the remaining set. A guarded arm SHALL NOT remove
its member. `_` SHALL cover every remaining member and MUST make every following arm unreachable. A
match SHALL be exhaustive only when no members remain or an explicit universal arm covers them.
Duplicate, unreachable, guard-after-exhaustive-member, and incomplete matches SHALL be rejected with
the exact relevant members and arm spans.

#### Scenario: Exhaust a two-member union

- **WHEN** a match over `Token | End` has unguarded `Token` and `End` arms
- **THEN** coverage reaches the empty set without a universal arm

#### Scenario: Guard does not prove coverage

- **WHEN** the only `Token` arm has a guard and the scrutinee is `Token | End`
- **THEN** both `Token` and `End` remain in the final uncovered-member diagnostic

#### Scenario: Reject an arm after universal coverage

- **WHEN** `_` is followed by another arm
- **THEN** the following arm is diagnosed as unreachable and contributes no binding or result fact

### Requirement: Matching narrows without changing the source type

Within a nominal arm, the selected payload SHALL have that precise canonical nominal type while the
scrutinee's binding and expression retain their original type outside the arm. Matching a precise
nominal value SHALL use the same coverage model as a one-member set. Matching SHALL NOT introduce
general subtyping, retroactive inference, numeric tag observability, or narrowing outside the
selected arm.

#### Scenario: Narrow one member locally

- **WHEN** the `Token` arm is selected from a `Token | End` binding
- **THEN** its pattern fields resolve against precise `Token` while the source binding remains `Token | End` after a borrowed match

### Requirement: Match results join reachable arm types canonically

The result of a match SHALL join only reachable arm expression types. Equal types SHALL remain that
precise type. Nominal and structural-union results SHALL normalize into one canonical union, with
`never` contributing no member. Distinct built-in scalars, arrays with different types or lengths,
or another mixture that cannot form a valid structural union SHALL make the match result
unavailable rather than introducing an implicit conversion or non-nominal union member.

#### Scenario: Join two nominal results

- **WHEN** reachable arms produce precise `Token` and `End` values
- **THEN** the match result is the canonical normalized type `Token | End`

#### Scenario: Keep one scalar result

- **WHEN** every reachable arm produces `i32`
- **THEN** the match result remains precise `i32`

#### Scenario: Reject incompatible scalar results

- **WHEN** one reachable arm produces `i32` and another produces `bool`
- **THEN** the match result is unavailable with a deterministic incompatible-arm diagnostic

### Requirement: Whole-member bindings extract union payloads

Match arms SHALL accept the whole-member binding form `Member name`, binding the entire matched
member payload as one value instead of destructuring its fields. The binding SHALL follow the
scrutinee's access mode, participate in coverage exactly like a field-destructuring pattern for
the same member, and leave nothing omitted: the binding owns the complete payload, so no
per-field cleanup is planned for the arm.

#### Scenario: Extract an affine member

- **WHEN** an arm binds `Full full` on a moved union scrutinee and the arm result moves the binding onward
- **THEN** the payload transfers exactly once, all three engines agree on the result, and no field of the member is separately released

#### Scenario: Extract an intrinsic result member

- **WHEN** an arm binds `Layout value` on the result of `Layout.repeat`
- **THEN** the binding is a usable `Layout` for allocation and the overflow arm still covers the remaining member
