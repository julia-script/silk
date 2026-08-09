## REMOVED Requirements

### Requirement: Recursive call cycles are bounded data

**Reason**: Rejecting any active-function re-entry prevents terminating recursive programs from
executing and treats function identity as if it proved non-termination.

**Migration**: Recursive calls now create distinct activation frames. Tooling that consumed
`RecursiveCycle` SHALL consume `EvaluationLimit` and its active-frame provenance instead.

## ADDED Requirements

### Requirement: Recursive calls execute in distinct activation frames

Evaluation SHALL permit direct and mutual recursive calls by creating one independent activation
frame per invocation. Parameters, locals, borrows, cleanup, returns, and trace events MUST belong to
the correct frame even when several active frames share one canonical function identity.

#### Scenario: Complete terminating recursion

- **WHEN** a recursive function reduces its input to a base case and returns through several active invocations
- **THEN** evaluation completes with the same result and caller-visible mutations as native and WebAssembly execution

#### Scenario: Keep recursive mutable slices distinct

- **WHEN** quicksort recursively passes one mutable slice with different low and high bounds
- **THEN** every activation observes its own scalar bounds while mutations remain visible through the shared slice

### Requirement: Evaluation limits are deterministic blocked data

Evaluation SHALL accept positive maximum-step and maximum-call-depth limits with stable defaults.
Each executed MIR operation consumes one step and each active invocation consumes one depth unit.
Exhaustion SHALL produce an `EvaluationLimit` blocked outcome naming `Steps` or `CallDepth`, the
configured limit, the active function, the source span that attempted further work, and the complete
active call identities. Evaluation MUST NOT depend on JavaScript stack overflow or wall-clock time.

#### Scenario: Bound direct non-termination

- **WHEN** a function recursively calls itself without reaching a base case
- **THEN** evaluation blocks at the configured call-depth limit with a deterministic active call path

#### Scenario: Bound an infinite loop

- **WHEN** a reachable loop executes beyond the configured step limit
- **THEN** evaluation blocks at the next operation with the `Steps` limit and exact operation span

#### Scenario: Repeat a limited evaluation

- **WHEN** an equivalent program is evaluated repeatedly under equal limits
- **THEN** its blocked reason, active frames, source provenance, and trace are identical
