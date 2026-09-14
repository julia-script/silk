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
