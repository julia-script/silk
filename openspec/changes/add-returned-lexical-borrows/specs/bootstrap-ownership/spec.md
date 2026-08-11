## ADDED Requirements

### Requirement: Returned views preserve source provenance through their live range

Ownership facts SHALL identify the single source owner of every accepted returned view and carry
that provenance through assignments and compatible reborrows. While a shared returned view is live,
the owner MUST NOT be mutated, moved, or dropped. While an exclusive returned view is live, the owner
MUST NOT be otherwise read, mutated, moved, or dropped. Conflicting access MAY resume after the
view's last use.

#### Scenario: Suspend mutation for a shared returned view

- **WHEN** a caller keeps a shared returned view live and attempts to mutate its source owner
- **THEN** ownership rejects the mutation and relates it to the live view's origin

#### Scenario: Suspend every competing access for an exclusive returned view

- **WHEN** a caller keeps an exclusive returned view live and attempts a second access to its source owner
- **THEN** ownership rejects the competing access until the exclusive view's last use

#### Scenario: Move the owner after the view ends

- **WHEN** a returned view's last use precedes a whole-owner move
- **THEN** the view no longer suspends the owner and the ordinary move succeeds

#### Scenario: Reject dropping a borrowed owner

- **WHEN** a structured exit would drop an owner while a returned view derived from it remains live
- **THEN** ownership rejects the exit rather than emitting cleanup that invalidates the view

