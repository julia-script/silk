## ADDED Requirements

### Requirement: Indirected values are released at runtime by their holder's hook

The target-neutral cleanup plan is statically unrolled to constant offsets, so it SHALL NOT be
required to represent an unbounded chain of owners. A struct that owns a value only through an
indirection SHALL release that value through its own `Drop` hook, which the plan invokes as one
call rather than by inlining the indirected value's cleanup. Cleanup of a compiler-owned
indirection SHALL release the storage only and MUST NOT descend into the indirected element type,
so the plan stays finite; the hook SHALL therefore drop the element explicitly before the storage
releases. Recursion depth SHALL be consumed by the runtime call stack, so an owner reachable
through any number of indirections SHALL be released exactly once, and exhausting the stack on a
deep chain SHALL NOT leak.

A cleanup plan MUST NOT reach its recursion guard on a cycle that passes through an indirection: no
owner in such a cycle may be planned as having no cleanup.

#### Scenario: Release a recursive tree through its hooks

- **WHEN** a multi-level tree whose nodes hold their children behind indirections leaves scope
- **THEN** every level's storage is released, and the release count equals the acquire count

#### Scenario: Invoke a hook rather than inline an indirected owner

- **WHEN** a struct's cleanup plan reaches a field that owns a value through an indirection
- **THEN** the plan records one hook call at a constant offset rather than the indirected value's own recursive cleanup

#### Scenario: Keep releasing a value the storage release would abandon

- **WHEN** an indirection's storage cleanup releases the block that holds an owned element
- **THEN** the element has already been dropped by the holder's hook, so no owner below the first level is abandoned

#### Scenario: Preserve identical release counts across engines

- **WHEN** the same recursive owner is released under the evaluator, the Wasm backend, and the native backend
- **THEN** all three report the same number of releases
