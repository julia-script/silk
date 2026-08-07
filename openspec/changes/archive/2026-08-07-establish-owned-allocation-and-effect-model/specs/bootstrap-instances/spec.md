## ADDED Requirements

### Requirement: Instance discovery follows Effect and storage reachability

Instance discovery SHALL reach concrete Effect bodies, handlers, retry policies, allocator witnesses,
layouts, raw-buffer operations, Drop hooks, and Silk Vector specializations from executable roots.
Equivalent concrete uses SHALL reuse canonical instances, and unused allocator implementations or
container specializations MUST NOT enter the plan.

Every reachable Effect construction site SHALL create one canonical hidden instance per enclosing
monomorphized function instance. Distinct sites MUST remain distinct even when their public Effect
contracts are structurally equal.

#### Scenario: Discover one Vector specialization

- **WHEN** several effects append `Token` values through the same `Vector<Token>` operations
- **THEN** discovery records one canonical Vector specialization, its Drop behavior, allocation witness calls, and required Token cleanup
