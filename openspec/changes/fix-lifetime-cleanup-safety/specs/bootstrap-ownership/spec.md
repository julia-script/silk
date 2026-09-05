## MODIFIED Requirements

### Requirement: Dependent replacement preserves both cleanup obligations

Replacement SHALL check the unchanged destination type before a non-suspending cleanup/install commit. Incoming evaluation can commit moves or writes before failure; exits clean the actual initialized remainder without rollback. Missing destinations skip displaced cleanup and maybe-initialized destinations clean conditionally. A complete Drop-bearing field can move from a plain outer owner, but no move or consuming destructuring can cross a whole-value user Drop ancestor.

#### Scenario: Displaced storage cannot supply its replacement

- **WHEN** an incoming reference points into storage displaced by replacement
- **THEN** the replacement is rejected even through generic replacement or a shortened outer exclusive borrow

#### Scenario: Cleanup follows a failing incoming expression

- **WHEN** an incoming expression moves a disjoint field and then propagates a typed failure before installation
- **THEN** the moved value cleans at its new owner and the destination's still-initialized remainder cleans once without rollback

#### Scenario: Extract a complete dependent Drop child

- **WHEN** a complete initialized Drop-bearing child moves out of a plain outer owner
- **THEN** its new owner retains the child's dependencies and cleanup; moving a subfield across the child's own Drop boundary remains rejected

#### Scenario: Installed referents survive dependent cleanup

- **WHEN** a field of a dependent Drop owner is replaced with a borrow of storage released before that owner, including storage inside a nested branch
- **THEN** analysis SHALL reject the installation; a referent acquired before the owner and surviving its cleanup SHALL remain accepted

## ADDED Requirements

### Requirement: Reacquired pattern owners receive fresh cleanup state

Every successful pattern acquisition SHALL start with the initialized state of the newly acquired value. A previous loop iteration's conditional partial move or drop MUST NOT suppress cleanup of the new value.

#### Scenario: Match reacquires an owner after conditional partial drop

- **WHEN** a loop move-matches a fresh aggregate into a binding on each iteration and drops one field only on the first iteration
- **THEN** cleanup SHALL release each remaining initialized field exactly once on every iteration

#### Scenario: Statement pattern reacquires an owner

- **WHEN** repeated statement-pattern selection acquires fresh owned storage after an earlier iteration conditionally drops a field
- **THEN** the newly acquired storage SHALL receive fresh cleanup state before its body executes
