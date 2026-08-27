## ADDED Requirements

### Requirement: Effect-block result typing accounts for every terminal

An effect block's success and failure types SHALL be derived from every `return` and `fail` terminal reachable in the block, including terminals nested inside `unsafe` blocks. Return sites with differing types SHALL combine through the language's canonical result join — never by silently adopting one site's type: joinable types form their union, and a join with no representable form is reported as a diagnostic at the offending return. A `fail` whose failure type is a value-kind type parameter SHALL contribute that parameter to the block's failure row exactly as a nominal failure would.

#### Scenario: Terminals inside unsafe blocks are collected
- **WHEN** an effect block's only `fail` (or only `return`) sits inside an `unsafe { }` statement
- **THEN** the block's failure row (or success type) includes it, and running the effect requires handling the failure

#### Scenario: Disagreeing branch returns cannot pass silently
- **WHEN** an effect block returns `bool` on one branch and `i32` on another inside a context expecting `Effect<i32>`
- **THEN** the block types as the canonical join (`Effect<bool | i32>`) and the context rejects it with a type-mismatch diagnostic — the block is never typed from the lexically last return alone

#### Scenario: Generic failures survive into the failure row
- **WHEN** a generic function's effect block fails with a value of type parameter `E`
- **THEN** the block types as an effect whose failure row contains `E`, and after specialization the concrete failure must be handled at `run`

### Requirement: Effect-block captures include enum-value arguments

Capture analysis for effect blocks SHALL register a capture for every binding referenced anywhere in the block body, including bindings referenced as the argument of an enum value construction.

#### Scenario: Enum.value argument is captured
- **WHEN** an effect block's body evaluates `Color.value(c)` for an outer binding `c`
- **THEN** `c` appears in the effect's capture environment and the deferred runner reads the captured value
