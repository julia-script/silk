## ADDED Requirements

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
