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

Match arms SHALL be considered in source order over canonical selection paths rooted in the
scrutinee's normalized member set. An ordinary member contributes one root path. A nominal-union
member contributes one root-parent-variant leaf for every canonical variant, including variants with
uninhabited specialized payloads. An unguarded whole-member arm SHALL remove its root and every
remaining descendant; an unguarded qualified variant arm SHALL remove exactly its leaf. A guarded
arm SHALL NOT remove any path. `_` SHALL cover every remaining path and MUST make every following arm
unreachable. A match SHALL be exhaustive only when no paths remain or an explicit universal arm
covers them. Duplicate, unreachable, guard-after-exhaustive-path, and incomplete matches SHALL be
rejected with fully qualified remaining paths and exact arm spans. Variant leaves SHALL never become
members of the structural-union type.

#### Scenario: Exhaust a two-member union

- **WHEN** a match over `Token | End` has unguarded `Token` and `End` arms
- **THEN** coverage reaches the empty set without a universal arm

#### Scenario: Guard does not prove coverage

- **WHEN** the only `Token` arm has a guard and the scrutinee is `Token | End`
- **THEN** both `Token` and `End` remain in the final uncovered-member diagnostic

#### Scenario: Reject an arm after universal coverage

- **WHEN** `_` is followed by another arm
- **THEN** the following arm is diagnosed as unreachable and contributes no binding or result fact

#### Scenario: Match variants directly through a structural union

- **WHEN** a match over `HttpError | OutOfMemoryError` has unguarded arms for every `HttpError` variant and `OutOfMemoryError {}`
- **THEN** coverage is exhaustive without requiring an intermediate whole-`HttpError` arm or nested match

#### Scenario: Cover the remaining nominal subtree

- **WHEN** one direct `HttpError.Timeout` arm is followed by `HttpError remaining`
- **THEN** the whole-parent arm binds `remaining` as `HttpError` and covers every other `HttpError` variant

#### Scenario: Reject a leaf after whole-parent coverage

- **WHEN** `HttpError remaining` is followed by `HttpError.Dns { ... }`
- **THEN** the later variant arm is unreachable because its parent subtree was already removed

#### Scenario: Keep a guarded affine variant available

- **WHEN** a guarded direct variant arm inspects an affine payload and its guard is false before a later arm can select the same path
- **THEN** coverage retains the complete path and ownership retains the tags and payload for the later arm without early movement or cleanup

#### Scenario: Diagnose a missing generic variant path

- **WHEN** a match over `Option<i32> | Option<bool>` omits only `Option<bool>.Some`
- **THEN** the incomplete-match diagnostic names that fully applied root-parent-variant path without collapsing either Option application

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

### Requirement: Match let and if-let share one pattern model

`match` arms, `let` bindings, and statement-form `if let` SHALL use one typed compiler-defined pattern representation. Patterns SHALL be non-executable and SHALL resolve recursive structure, exact union membership, bindings, access mode, irrefutability, and source spans before HIR lowering.

#### Scenario: Reuse one struct pattern

- **WHEN** the same struct pattern shape appears in a match arm, an irrefutable let, and an if-let condition
- **THEN** all three positions bind the same fields with the same type and ownership rules

### Requirement: Local destructuring requires irrefutability

A `let` pattern SHALL be accepted only when it matches every value of the initializer's type. Move, shared-borrow, exclusive-borrow, and Copy bindings SHALL apply recursively. A standalone wildcard SHALL NOT consume a non-unit result in place of explicit `drop`.

#### Scenario: Reject a refutable local union pattern

- **WHEN** a let pattern selects only one member of a multi-member union
- **THEN** analysis reports that the pattern is refutable and recommends conditional or exhaustive matching

#### Scenario: Reject wildcard discard bypass

- **WHEN** `let _ = operation()` would discard a non-unit result
- **THEN** analysis requires `drop operation()` under the ordinary expression-statement rule

### Requirement: If-let owns both outcomes explicitly

Statement-form `if let` SHALL run a success body with bindings and MAY run a mismatch body. A move pattern SHALL consume the scrutinee on both outcomes; borrow patterns SHALL create loans scoped to the corresponding body; cleanup and post-statement ownership SHALL join deterministically.

#### Scenario: Borrow one exact union member

- **WHEN** `if let` borrows one exact normalized union member
- **THEN** the success body receives the narrowed borrow and the mismatch body retains the complementary union without consuming the owner

#### Scenario: Accept an irrefutable conditional

- **WHEN** an if-let pattern is statically irrefutable
- **THEN** the compiler accepts it and any simplification notice is an optional LSP warning

### Requirement: Generic selectors renormalize at complete applications

A generic pattern body SHALL be checked once against its symbolic normalized member set. Every
complete application SHALL substitute and renormalize the selected members before MIR lowering. If
distinct symbolic selectors become the same concrete member, the first source-ordered matching arm
SHALL select that member and later equivalent arms SHALL emit no additional source diagnostic.

#### Scenario: Collapse two generic selectors

- **WHEN** a generic match over `A | B` has source-ordered `A` and `B` arms and specializes with both parameters equal to `i32`
- **THEN** the concrete match tests one `i32` member, selects the first arm, and retains no duplicate runtime tag

#### Scenario: Preserve distinct generic selectors

- **WHEN** the same match specializes with `A = i32` and `B = string`
- **THEN** both concrete members retain their source-ordered arms and exhaust the normalized union

### Requirement: Exhaustive matching covers scalar enum members

The existing source-ordered coverage model SHALL accept a scalar enum's canonical member set as a
closed coverage domain. Qualified member arms SHALL remove only that exact member, `_` SHALL remove
all remaining members, and guarded member arms SHALL NOT prove coverage. Enum patterns SHALL bind no
payload and SHALL leave the scrutinee's source type unchanged. Duplicate, post-wildcard, incomplete,
foreign-enum, and integer enum patterns SHALL receive deterministic enum-specific diagnostics.

#### Scenario: Exhaust one enum without a wildcard

- **WHEN** an enum match contains one unguarded qualified arm for each canonical member
- **THEN** coverage reaches the empty set and the match is exhaustive

#### Scenario: Keep a guarded member uncovered

- **WHEN** an enum member appears only in a guarded arm and no wildcard follows
- **THEN** that member remains in the final uncovered-member diagnostic

### Requirement: Variant patterns bind struct-like fields

A named-field variant pattern SHALL bind, rename, nest, borrow, move, omit with `..`, and validate
fields under the same rules as a nominal struct pattern. A unit variant SHALL bind no fields. Pattern
selection SHALL retain the applied parent type and canonical variant identity without introducing a
variant subtype.

#### Scenario: Move fields from one selected variant

- **WHEN** `Result<A, E>.Success { value }` matches a moved `Result<A, E>`
- **THEN** `value` receives the specialized `A` payload and cleanup remains restricted to that selected variant

#### Scenario: Reject an incomplete field pattern

- **WHEN** a variant pattern omits a declared field without `..`
- **THEN** analysis reports the same missing-field condition as struct destructuring and creates no executable arm
