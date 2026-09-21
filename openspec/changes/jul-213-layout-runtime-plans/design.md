# Context

Non-generic declaration layouts and target word constants do not depend on executable reachability.
Concrete generic instantiations, callable/effect environments, execution packages, storage, and
literal verdicts do.

# Decisions

- `computeTypes(target, index, presentation, opaqueRealizations)` cannot accept an instance graph.
- `computeRuntime(catalog, instances, index, opaqueRealizations)` completes layouts for reached
  concrete specializations, then produces the runtime plan.
- The runtime plan retains only reached entries even though its catalog may contain more declared
  types.
- The old `catalog` and `plan` entry points are deleted rather than aliased.

# Risks

Completing concrete specializations repeats a bounded declaration-layout walk. This is preferable to
polluting the pre-reachability artifact; later persistent caching can reuse the catalog by identity.
