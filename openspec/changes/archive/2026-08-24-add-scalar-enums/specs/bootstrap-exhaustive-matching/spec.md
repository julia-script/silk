## ADDED Requirements

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
