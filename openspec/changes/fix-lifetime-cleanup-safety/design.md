## Context

See proposal.md for motivation. LifetimeFlow solves activated bounds over source points before ordered synthetic cleanup points exist. CleanupEmission provides sparse initialization flags, but pattern lowering does not reset them at runtime acquisition.

## Goals / Non-Goals

Goals: preserve installed bounds through actual cleanup, and initialize each fresh pattern owner before its body can move or release fields.
Non-goals: other lifetime review findings, cleanup reordering, runtime lifetime metadata.

## Decisions

- Extend each activated constraint to synthetic release points when it is active at that exit's source point. Keep per-exit release ordering and referent availability; globally activating bounds would reject unrelated paths.
- Emit existing initializeBinding operations inside selected pattern execution, before body operations. Allocation alone is compile-time bookkeeping and cannot reset flags on loop re-entry. Cover match expressions and statement selections.
- Use frontend diagnostics for dependent lifetime rejection and structural MIR assertions for flag placement. Keep paired valid controls; do not add per-feature native compilation.

## Risks / Trade-offs

- Exit mapping can overactivate constraints → cover branch-local installation and valid earlier referents.
- Reset placement can affect rejected guards or sibling arms → keep resets in selected execution and test both pattern paths.
