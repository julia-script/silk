## ADDED Requirements

### Requirement: Slice loans attach to stable owner roots

Every available slice borrow SHALL create a compiler-only loan identity attached to the complete
source owner root. Any number of shared loans MAY coexist, while an exclusive loan MUST conflict
with every other live loan. A shared loan SHALL prevent mutation, replacement, movement, or cleanup
of its root; an exclusive loan SHALL prevent every direct use of its root. Loan identity and access
MUST NOT become runtime fields.

#### Scenario: Permit shared aliases

- **WHEN** one call supplies two shared borrows of the same live array root
- **THEN** ownership accepts both loans for the complete invocation

#### Scenario: Reject conflicting call arguments

- **WHEN** one invocation supplies shared and exclusive borrows or two exclusive borrows of the same root
- **THEN** ownership rejects the conflict because every argument loan overlaps all later argument evaluation and the complete callee execution

#### Scenario: Reject owner use during a loan

- **WHEN** source attempts to move, replace, mutate, or clean an owner while a conflicting slice loan is live
- **THEN** ownership diagnoses the owner operation and preserves the original loan and cleanup state

### Requirement: Slice loans remain call-scoped and non-escaping

An explicit borrow argument SHALL begin before its argument value is supplied and end only after the
ordinary callee returns. A function slice parameter SHALL remain borrowed for the complete function
body. Slice types MUST be rejected recursively from return types, struct or union fields, fixed
arrays, owned generic wrappers, lazy flow environments, and other escaping captures. Standalone
slice local bindings and borrows of temporaries or subplaces MUST be rejected in this bootstrap
capability.

#### Scenario: End a temporary loan after an ordinary call

- **WHEN** an exclusive whole-array borrow is passed to an ordinary function and that function returns
- **THEN** the call loan ends and subsequent caller access to the mutable owner is permitted

#### Scenario: Reject recursive storage of a slice

- **WHEN** a slice type appears directly or transitively inside an owned struct, union, array, or generic application
- **THEN** ownership rejects the containing type at the escaping boundary

#### Scenario: Reject a captured slice

- **WHEN** a lazy computation or callback would retain a slice after call construction
- **THEN** ownership rejects the capture rather than ending the source loan prematurely

### Requirement: Structured exits end loans before owner cleanup

Every successful return, typed failure, early return, loop `break`, loop `continue`, and lexical
fallthrough SHALL end loans belonging to exited regions before scheduling cleanup of their backing
owners. Cleanup SHALL remain exactly once and element-derived. A trap SHALL retain the existing
trap semantics and MUST NOT pretend that normal cleanup ran.

#### Scenario: Exit a loop containing a slice call

- **WHEN** `break` or `continue` leaves a region after a call-scoped borrow completes
- **THEN** the loan ends before the structured outcome and the backing owner remains valid for its eventual single cleanup

#### Scenario: Return after an exclusive write

- **WHEN** a callee replaces a move-only element through an exclusive slice and returns early
- **THEN** the displaced element and eventual backing array elements are each cleaned exactly once after the relevant loans end
