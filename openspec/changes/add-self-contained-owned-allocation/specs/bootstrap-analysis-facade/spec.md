## ADDED Requirements

### Requirement: Allocation inspection crosses one analysis boundary

The public analysis facade SHALL expose source-correlated allocation requirements, validated layout
facts, affine owner and loan facts, restricted Drop and cleanup plans, HIR, reachable instances,
target layout, verified MIR, evaluation events, and backend artifacts through immutable phase-owned
projections. It MUST NOT expose mutable evaluator storage, host pointers, backend-private heap state,
reclaim function addresses, or allocator implementation branching to clients.

#### Scenario: Inspect one allocation identity across phases

- **WHEN** a client selects a successful allocation call
- **THEN** the facade correlates its source span, semantic owner, HIR operation, cleanup obligation, layout, MIR identity, evaluation events, and backend realization without exposing private addresses

#### Scenario: Preserve an unavailable allocation path

- **WHEN** semantic analysis rejects an invalid unsafe storage operation
- **THEN** the facade exposes its diagnostic and unavailable downstream projections rather than fabricating MIR, evaluation, or backend state
