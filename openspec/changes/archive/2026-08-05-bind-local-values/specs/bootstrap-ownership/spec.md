## MODIFIED Requirements

### Requirement: Ownership facts are produced once per declaration

The ownership phase SHALL run once per declaration over typed HIR and SHALL publish one immutable
ownership fact per function: its bindings with their ownership category and live range over
source spans, and a closed verdict. Bindings SHALL cover parameters and `let` statements alike:
a parameter is live from its declaration through the function body, and a `let` binding is live
from its statement through its last use — its consuming move where one exists, otherwise the end
of the function body. A function whose HIR body or contract is unavailable SHALL carry an
explicitly unavailable verdict retaining the originating diagnostic identity where one exists,
and MUST NOT report a satisfied check it could not perform.

#### Scenario: Check a copyable parameter

- **WHEN** `pub fn identity(value: I32) -> I32 { return value }` is checked
- **THEN** its ownership fact lists one copyable binding for `value` live from its declaration span through the function body, with a satisfied verdict

#### Scenario: Keep unavailable bodies explicit

- **WHEN** a function's HIR body is unavailable after recovery or an unresolved reference
- **THEN** its ownership verdict is explicitly unavailable, carrying the originating diagnostic's identity where one exists

#### Scenario: Range a let binding's liveness

- **WHEN** a body binds `let value = 42` and returns `value`
- **THEN** the ownership fact lists the binding live from its statement span through the end of the function body, with a satisfied verdict

#### Scenario: End liveness at a consuming move

- **WHEN** a body binds a value, moves it into a call argument, and returns the call's result
- **THEN** the binding's live range ends at the move's span rather than the end of the body

### Requirement: The cleanup plan is a target-neutral artifact

The phase SHALL produce one cleanup plan per function: every structured exit path with its
ordered releases in last-acquired, first-released order. A release SHALL record the end of one
binding's ownership at that exit; bindings already consumed by a move before the exit MUST NOT
be released again. The plan SHALL be target-neutral — it MUST NOT insert target-specific drops —
and it SHALL record releases uniformly whether or not the released type carries cleanup
behavior, so lowering and later cleanup-bearing types consume one shape. The plan SHALL expose a
deterministic textual encoding gated by committed golden files.

#### Scenario: Plan the single return exit

- **WHEN** a frozen-slice function with parameters is checked
- **THEN** its cleanup plan contains one return exit at the returned expression's span with an empty release list

#### Scenario: Match the cleanup golden encoding

- **WHEN** a committed fixture is checked and its plan encoded
- **THEN** the encoding equals the committed golden text byte-for-byte, naming every binding, exit, and release order

#### Scenario: Release bindings in reverse binding order

- **WHEN** a body declares `let first = 1` then `let second = 2` and returns a literal
- **THEN** the return exit releases `second` before `first`

#### Scenario: Skip a moved binding at the exit

- **WHEN** a body moves its only binding before the return
- **THEN** the return exit's release list omits that binding

## ADDED Requirements

### Requirement: Moves consume bindings

The ownership phase SHALL treat each move expression as the consuming use of its resolved
binding, including bindings of copyable types: after a move, the binding is no longer live. Any
later use — read or move — of a consumed binding SHALL produce one `OWN0001` ownership
diagnostic at the later use's span carrying the consuming move's span as a related span, and the
function's verdict SHALL be an explicit violation retaining that diagnostic's identity. A
violated function's facts SHALL remain published so inspection can present the timeline that
produced the violation.

#### Scenario: Diagnose a use after move

- **WHEN** a body moves a binding and then reads it
- **THEN** one `OWN0001` diagnostic marks the later read with the move's span related, and the function's verdict is a violation carrying that diagnostic's identity

#### Scenario: Diagnose a double move

- **WHEN** a body moves the same binding twice
- **THEN** the second move carries the `OWN0001` diagnostic and the first move's span as its related span

#### Scenario: Accept an ordinary read before a move

- **WHEN** a body reads a copyable binding and moves it afterwards
- **THEN** the verdict is satisfied because reads before the consuming move copy rather than consume
