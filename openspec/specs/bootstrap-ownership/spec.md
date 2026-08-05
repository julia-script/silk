# bootstrap-ownership Specification

## Purpose
The ownership and scope phase over typed HIR: per-declaration ownership facts (bindings,
ownership categories, live ranges, verdicts) and the target-neutral cleanup plan that MIR
lowering will consume to insert drops — established as a producer phase with its fact table and
artifact before any lowering exists to need them.
## Requirements
### Requirement: Ownership facts are produced once per declaration

The ownership phase SHALL run once per declaration over typed HIR and SHALL publish one immutable
ownership fact per function: its bindings with their ownership category and live range over
source spans, and a closed verdict. In the frozen slice every binding is a copyable `I32`
parameter live from its declaration through the function body, and the verdict is satisfied. A
function whose HIR body or contract is unavailable SHALL carry an explicitly unavailable verdict
retaining the originating diagnostic identity where one exists, and MUST NOT report a satisfied
check it could not perform.

#### Scenario: Check a copyable parameter

- **WHEN** `pub fn identity(value: I32) -> I32 { return value }` is checked
- **THEN** its ownership fact lists one copyable binding for `value` live from its declaration span through the function body, with a satisfied verdict

#### Scenario: Keep unavailable bodies explicit

- **WHEN** a function's HIR body is unavailable after recovery or an unresolved reference
- **THEN** its ownership verdict is explicitly unavailable, carrying the originating diagnostic's identity where one exists

### Requirement: The cleanup plan is a target-neutral artifact

The phase SHALL produce one cleanup plan per function: every structured exit path with its
ordered releases in last-acquired, first-released order. The plan SHALL be target-neutral —
it MUST NOT insert target-specific drops — and in the frozen slice every exit releases nothing
because every value is copyable. The plan SHALL expose a deterministic textual encoding gated by
committed golden files.

#### Scenario: Plan the single return exit

- **WHEN** a frozen-slice function with parameters is checked
- **THEN** its cleanup plan contains one return exit at the returned expression's span with an empty release list

#### Scenario: Match the cleanup golden encoding

- **WHEN** a committed fixture is checked and its plan encoded
- **THEN** the encoding equals the committed golden text byte-for-byte, naming every binding, exit, and release order

### Requirement: Ownership output is deterministic

Checking the same elaborated module repeatedly in fresh processes SHALL produce identical
ownership facts, cleanup plans, and encodings.

#### Scenario: Repeat the ownership phase

- **WHEN** equivalent modules are checked repeatedly in fresh processes
- **THEN** the ownership facts, plans, and encoded texts are identical

