## Context

Effect construction identity currently stands in for representation compatibility. Finite control-flow alternatives can instead be represented directly without making Effect dynamically boxed.

## Goals / Non-Goals

**Goals:** admit finite compatible joins; preserve exact channels and ownership; remain lazy, deterministic, allocation-free, and cross-engine equivalent.

**Non-goals:** open existential Effects, arbitrary runtime polymorphism, implicit run/flatten, or heap boxing.

## Decisions

1. Join semantic contracts first, independently from construction identity.
2. Represent the result as a closed tagged set of concrete Effect realizations.
3. Capture and clean only the selected alternative while retaining a maximum static layout where required.
4. Lower selection into ordinary HIR/MIR control and backend tags; no new source-visible primitive is introduced.
5. Reject joins whose alternatives are not finite or whose access/ownership contracts have no safe join.

## Risks / Trade-offs

- Composite layouts can be larger than either individual branch, accepted in exchange for allocation freedom.
- Care is required to avoid constructing or dropping the unselected Effect.

## Migration Plan

Replace `SEM0069` construction-identity checks with contract joining, add composite HIR/MIR, implement evaluation and both backends, add ownership verification, migrate diagnostics/tests, and delete legacy identity rejection.

## Open Questions

None for finite joins. Open runtime-polymorphic Effect storage remains outside the stabilized core.
