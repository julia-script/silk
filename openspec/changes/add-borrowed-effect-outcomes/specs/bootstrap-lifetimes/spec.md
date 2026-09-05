## MODIFIED Requirements

### Requirement: Exclusive storage retains lifetime and access authority

Stored exclusive references SHALL remain affine through fields, generic wrappers, arrays, extraction, restoration and ordinary results. Their outer access lifetime is covariant and their payload type invariant. Nominal variance SHALL derive from declared storage over a finite lattice; opaque unsafe storage defaults invariant. Dependent Effect outcomes and partial suspension SHALL be admitted only under complete outcome, stable-storage and suspended initialization/cleanup proofs.

#### Scenario: Shared descendants retain exclusivity

- **WHEN** a shared child of an exclusive stored view is copied and one child ends
- **THEN** conflicting parent access remains rejected until every dependent ends; parent access is accepted afterward

## ADDED Requirements

### Requirement: Effect environment and outcome validity remain independent

`Effect<'env; A ! E ? R>` and `effect<'env> fn` SHALL preserve environment validity separately from complete success/failure types, required services, exact representation and execution access. External outcome sources SHALL survive producing temporaries, handlers, retry and cancellation. Run-local or consumed environment-owned referents SHALL NOT escape.

#### Scenario: Release a temporary receiver

- **WHEN** an Effect captures `&'call mut Holder<'data>` and returns an external `&'data A`
- **THEN** ending the temporary Effect releases the receiver borrow while the result retains the external source obligation

#### Scenario: Reject consumed scratch

- **WHEN** a consuming Effect returns a reference into its own captured or run-local storage
- **THEN** the compiler rejects the escaping outcome before lowering

### Requirement: Outcome analysis preserves finite canonical reuse

Lifetime checking SHALL consume resolved operations/providers and canonical binder-context comparisons without discovering alternatives, conversions, handlers or speculative static computations. Exported environment and cleanup changes SHALL invalidate actual semantic consumers; private body edits preserving their complete consumed surface SHALL reuse downstream body proofs. Runtime lifetime erasure SHALL introduce no owner-specific instances or layouts. Workloads SHALL expose actual query, generic, residual, constraint, initialization, cleanup and resolution-initiator work without hidden union or partial-state Cartesian expansion.

#### Scenario: Change a consumed bound

- **WHEN** an exported environment validity bound changes
- **THEN** dependent checks are invalidated while an alpha-rename retains the canonical semantic identity

#### Scenario: Add another owner

- **WHEN** another caller supplies different local source owners to an unchanged generic Effect
- **THEN** obligations are instantiated locally, the generic body proof is reused and runtime identities erase the owner and region distinction

#### Scenario: Measure failing composition

- **WHEN** opt-in workloads vary composition, callbacks/binder width, provider forwarding, module fan-out and partial suspension independently
- **THEN** accepted and failing samples report attributable work and development/optimized paths agree on lifetime validity
