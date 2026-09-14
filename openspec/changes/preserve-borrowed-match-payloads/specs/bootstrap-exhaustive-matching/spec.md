## MODIFIED Requirements

### Requirement: Match access mode is explicit and lexical

A match SHALL evaluate its scrutinee exactly once. Bare `match value` SHALL be accepted only for a
Copy scrutinee. `match move value` SHALL consume one complete owned value. `match &value` SHALL
create shared match-local bindings, and `match &mut value` SHALL require one mutable live place and
create exclusive match-local bindings. Shared and exclusive bindings MUST NOT escape their arm,
enter owned storage, or be moved, returned, or captured beyond the match. Borrowed bindings SHALL
refer to the selected original payload, including when the scrutinee is projected through a reference.
A scoped operation or capture using an exclusive binding SHALL mutate that original payload, and
suspension SHALL preserve its alias identity until the match loan ends.

#### Scenario: Borrow then reuse an owner

- **WHEN** a move-only union is matched through `&` and one arm reads a Copy field
- **THEN** the shared pattern binding ends with the selected arm and the original owner remains usable after the match

#### Scenario: Consume one union

- **WHEN** a move-only union is matched through `move`
- **THEN** the original source becomes unavailable and exactly one selected arm owns its active payload

#### Scenario: Reject a bare move-only match

- **WHEN** a bare match scrutinee is not Copy
- **THEN** analysis reports that an explicit consuming or borrowing mode is required

#### Scenario: Publish an owner after a borrowed operation

- **WHEN** an exclusive match lends its selected provider to an operation and the enclosing owner is moved after the arm
- **THEN** the published owner contains the operation's mutations rather than the payload's pre-match state

### Requirement: Variant patterns bind struct-like fields

A named-field variant pattern SHALL bind, rename, nest, borrow, move, omit with `..`, and validate
fields under the same rules as a nominal struct pattern. A unit variant SHALL bind no fields. Pattern
selection SHALL retain the applied parent type and canonical variant identity without introducing a
variant subtype. Nested variant patterns SHALL test every selected inner variant before binding
its fields or evaluating the arm guard. An inner mismatch SHALL continue to the next source arm,
and coverage SHALL retain the outer variant until all of its nested alternatives are covered.

#### Scenario: Move fields from one selected variant

- **WHEN** `Result<A, E>.Success { value }` matches a moved `Result<A, E>`
- **THEN** `value` receives the specialized `A` payload and cleanup remains restricted to that selected variant

#### Scenario: Reject an incomplete field pattern

- **WHEN** a variant pattern omits a declared field without `..`
- **THEN** analysis reports the same missing-field condition as struct destructuring and creates no executable arm

#### Scenario: Distinguish nested variant alternatives

- **WHEN** an outer `Io` variant contains an inner error with `Closed` and `Open` alternatives
- **THEN** an `Io { error: Closed { operation } }` arm binds `operation` only for `Closed`, and `Open` remains available to a later arm

#### Scenario: Exhaust all nested alternatives

- **WHEN** unguarded arms cover every inner alternative of an outer variant and every other outer variant
- **THEN** the match is exhaustive without a redundant whole-parent fallback
