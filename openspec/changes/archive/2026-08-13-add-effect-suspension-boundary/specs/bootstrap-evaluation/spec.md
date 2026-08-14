## ADDED Requirements

### Requirement: Evaluation executes suspension through logical activations

Evaluation SHALL execute explicit suspension by retaining the parent logical activation in its heap
activation machine, evaluating the deferred child, and resuming the parent with the child's exact
typed outcome. It SHALL model continuation allocation, initialization, ownership transfer, and
release through the selected allocator with the same logical boundary identities, request order,
success or failure ordinals, ownership transfers, and release count as the native and Wasm engines.
Each engine MAY use a different validated physical frame size, alignment, and private header.
Evaluation MUST NOT recurse on the JavaScript stack or expose a pending value as a source result.

#### Scenario: Complete deep suspension under raised limits

- **WHEN** a terminating suspended Effect recursion is evaluated with step and call-depth limits above its logical work
- **THEN** evaluation completes with the same result, logical allocation order and outcomes, ownership and release counts, and cleanup trace as native and Wasm without requiring identical physical frame bytes or depending on the JavaScript call stack

#### Scenario: Sweep continuation allocation failure

- **WHEN** deterministic allocation failure rejects each continuation allocation ordinal in turn
- **THEN** evaluation returns `OutOfMemory` at that boundary, creates no owner for the rejected request, and cleans every previously created logical continuation exactly once

## MODIFIED Requirements

### Requirement: Evaluation limits are deterministic blocked data

Evaluation SHALL accept positive maximum-step and maximum-call-depth limits with stable defaults.
Each executed MIR operation consumes one step and each active source-logical invocation consumes one
depth unit, including an invocation retained as a suspended heap continuation. Compiler-generated
driver, resume, and storage helpers MUST NOT consume additional source-logical depth units.
Exhaustion SHALL produce an `EvaluationLimit` blocked outcome naming `Steps` or `CallDepth`, the
configured limit, the active function, the source span that attempted further work, and the complete
active source-logical call identities. Evaluation MUST NOT depend on JavaScript stack overflow or
wall-clock time.

#### Scenario: Bound direct non-termination

- **WHEN** a function recursively calls itself without reaching a base case
- **THEN** evaluation blocks at the configured call-depth limit with a deterministic active call path

#### Scenario: Bound suspended logical recursion

- **WHEN** an Effect crosses suspension boundaries until another source-logical invocation would exceed `maxCallDepth`
- **THEN** evaluation blocks with `CallDepth` at that exact boundary and reports every retained suspended invocation in the active call path

#### Scenario: Bound an infinite loop

- **WHEN** a reachable loop executes beyond the configured step limit
- **THEN** evaluation blocks at the next operation with the `Steps` limit and exact operation span

#### Scenario: Repeat a limited evaluation

- **WHEN** an equivalent program is evaluated repeatedly under equal limits
- **THEN** its blocked reason, active frames, source provenance, and trace are identical
