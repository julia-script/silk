## ADDED Requirements

### Requirement: Value-reference parameters support compatible call-scoped reborrows

Borrowing a value-reference parameter for a nested call SHALL form a call-scoped reborrow. A shared
parent SHALL yield only a shared child. An exclusive parent SHALL yield a shared or exclusive child,
suspend conflicting use of the parent for the complete call, and restore it afterward. Reborrowing
SHALL preserve the original backing identity and SHALL NOT strengthen access.

#### Scenario: Reborrow an exclusive Formatter repeatedly

- **WHEN** an exclusive Formatter parameter is passed by exclusive reborrow to sequential helpers
- **THEN** each child borrow ends with its call
- **AND** the parent Formatter is available for the next helper

#### Scenario: Share an exclusive parent temporarily

- **WHEN** an exclusive reference parameter is shared-reborrowed for a nested call
- **THEN** the parent is suspended during that call and restored afterward

#### Scenario: Reject access strengthening

- **WHEN** source requests an exclusive child from a shared reference parameter
- **THEN** ownership analysis rejects the reborrow
