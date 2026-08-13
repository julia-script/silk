## ADDED Requirements

### Requirement: Bulk raw-storage evaluation reads its whole range before writing

The evaluator SHALL read every element of a bulk copy's source range before it writes any element of
the destination range, so an overlapping copy produces the intermediate-buffer result. It SHALL
record one bulk event per copy and per set in the allocation trace, and it SHALL block a bulk
operation whose storage is released or whose source range is not fully initialized.

#### Scenario: Trace one bulk move

- **WHEN** a program copies a range of elements
- **THEN** the trace holds exactly one copy event carrying the destination ticket, offset, and count

#### Scenario: Block an uninitialized source range

- **WHEN** a copy's source range holds a slot that no write has initialized
- **THEN** evaluation blocks with a trap rather than inventing a value
