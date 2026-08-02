## Why

Expression-valued conditionals are the point where learners encounter real control flow and SSA merging. The tutorial needs a deliberate explanation of predecessors, beginner-level dominance, and PHI selection before implementing the `abs` function.

## What Changes

- Add a control-flow and PHI lesson centered on lowering `if/then/else`.
- Convert language `i32` truthiness to an LLVM `i1` branch condition.
- Create true, false, and merge blocks with unique nested names.
- Terminate both paths and seal an `i32` PHI with one incoming value per predecessor.
- Add a CFG diagram, nested-conditional test, and recovery guidance for invalid body drafts.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Extends tutorial lowering and tests for branching control flow. It uses existing block, branch, comparison, and PHI APIs.

