## ADDED Requirements

### Requirement: Pattern-context diagnostics retain stable intent

Invalid statement patterns SHALL use structured diagnostics for the failed language rule. A
refutable unconditional binding SHALL report `SEM0133` with the initializer type and uncovered
members and recommend `if let` or `match`. A standalone wildcard binding that would discard a
non-unit result SHALL report the ordinary explicit-discard diagnostic `SEM0087`. Ownership,
unknown-name, member, field, and recovery failures SHALL retain their existing phase-owned codes.

#### Scenario: Reject refutable let

- **WHEN** a local pattern covers only `Token` from `Token | End`
- **THEN** `SEM0133` identifies `End` as uncovered and recommends conditional or exhaustive matching

#### Scenario: Reject wildcard discard

- **WHEN** `let _ = operation()` would ignore a non-unit result
- **THEN** `SEM0087` requires an explicit `drop operation()`
