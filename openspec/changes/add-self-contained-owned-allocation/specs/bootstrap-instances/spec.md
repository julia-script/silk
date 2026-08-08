## ADDED Requirements

### Requirement: Allocation reachability remains finite and type-directed

Instance discovery SHALL follow reachable allocator conformance witnesses, allocation/reclaim
operations, concrete `RawBuffer<T>` and `Slot<T>` operations, restricted Drop hooks, and every
transitively cleaned field. Instance keys SHALL include canonical concrete types, roles, targets,
and callable contracts where already required, but MUST NOT include runtime counts, allocation
ordinals, provider object identities, logical addresses, or cleanup-event identities.

#### Scenario: Reuse one typed-storage instance across counts

- **WHEN** one generic raw-buffer helper is called for the same canonical `T` with several runtime counts
- **THEN** discovery records one concrete helper instance and retains each count only as runtime data

#### Scenario: Discover cleanup through an uncalled path

- **WHEN** a reachable owner type has a restricted Drop hook but one execution path never constructs it
- **THEN** discovery still includes the statically reachable hook exactly once without inventing a runtime owner
