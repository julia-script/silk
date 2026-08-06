## Purpose

Define explicit mutable owners and structured repetition for safe, deterministic algorithms over compiler-shaped Silk values.

## ADDED Requirements

### Requirement: Mutation is explicit and owner-scoped

Bindings SHALL be immutable unless declared `let mut`. A writable place SHALL be rooted in one live
mutable owner, and every field or index projection SHALL preserve that root identity. Assignment to
an immutable, moved, unavailable, or non-writable place SHALL be rejected without changing any value.

#### Scenario: Update a mutable scalar

- **WHEN** `let mut count = 0` is followed by `count = count + 1`
- **THEN** the assignment replaces the live `I32` value and later reads observe `1`

#### Scenario: Reject an immutable update

- **WHEN** an ordinary `let count = 0` binding is assigned again
- **THEN** analysis reports the immutable root and no successful write fact is produced

### Requirement: Assignment replaces one complete value

Assignment SHALL first resolve and check the destination place, including every dynamic bound, then
evaluate the right-hand expression exactly once, and only then commit one exact-type replacement.
Until commit, the old destination SHALL remain initialized. Commit SHALL clean a replaced non-Copy
value exactly once before installing the new complete value, while a failed place check or failed
right-hand evaluation SHALL leave the old value live and unchanged.

#### Scenario: Replace a checked array element

- **WHEN** `values[index] = next()` targets an in-bounds element of a mutable array
- **THEN** the index is checked once, `next()` runs once, and exactly that element is replaced

#### Scenario: Preserve the old value after a trap

- **WHEN** a right-hand expression traps before a replacement commits
- **THEN** the destination's previous complete value remains the live value for cleanup

#### Scenario: Clean a replaced owner

- **WHEN** assignment replaces one non-Copy struct or array element with another complete value
- **THEN** the old value is cleaned exactly once and the new value assumes the place's ownership obligation

### Requirement: While repeats under a strict boolean condition

A `while` statement SHALL evaluate its condition before every iteration and enter the body only when
the result is `Bool.true`. A false initial condition SHALL execute zero iterations. Body fallthrough
and `continue` SHALL begin the next condition evaluation; `break` SHALL continue after the loop.

#### Scenario: Mutate across several iterations

- **WHEN** a loop increments a mutable counter while it is less than three
- **THEN** the condition is evaluated four times, the body executes three times, and the following statement observes three

#### Scenario: Skip a false loop

- **WHEN** a loop's first condition is false
- **THEN** its body performs no reads, writes, calls, or cleanup acquisitions

### Requirement: Loop transfers are lexical and cleanup-safe

`break` and `continue` SHALL target the innermost enclosing loop and SHALL be invalid outside a loop.
Neither form carries a value during bootstrap. Before any `continue`, `break`, or `return` leaves its
current lexical regions, every live owner acquired in those regions SHALL be cleaned exactly once in
the established reverse acquisition order.

#### Scenario: Continue cleans one iteration

- **WHEN** an iteration creates a move-only local and then continues
- **THEN** that local is cleaned before the next condition evaluation and is not cleaned again

#### Scenario: Break from a nested conditional

- **WHEN** a conditional inside a loop executes `break`
- **THEN** its arm-local and iteration-local owners are cleaned before control reaches the statement after the loop

#### Scenario: Reject a transfer outside a loop

- **WHEN** a function body contains `break` or `continue` without an enclosing loop
- **THEN** analysis reports the invalid transfer while preserving unrelated function facts
