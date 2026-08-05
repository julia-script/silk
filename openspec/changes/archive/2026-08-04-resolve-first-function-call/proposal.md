## Why

A preserved call becomes meaningful only when its callee can be connected to exactly one collected
declaration and its result type can flow back to the caller. This final milestone change proves the
first real semantic relationship without introducing a general scope graph or intermediate IR.

## What Changes

- Resolve a present zero-argument callee name against the collected top-level declarations.
- Publish resolved, missing, ambiguous, and syntax-unavailable call-reference states with exact
  source and declaration provenance.
- Emit a stable unknown-function diagnostic for a present name with no declaration while relying on
  the existing duplicate-name diagnostic for ambiguity.
- Propagate a uniquely resolved callee's available `I32` result type into caller return compatibility.
- Visualize `caller → callee` edges, unknown names, and ambiguous names in the hidden inspector.
- Defer parameters, arguments, recursion policy, dependency scheduling, AST, HIR, and lowering.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-semantic-facts`: Resolve the first call reference, diagnose missing targets, preserve
  ambiguity, and compute call-based return compatibility.
- `bootstrap-syntax-inspector`: Show the first declaration relationship as a navigable visual edge
  with separate semantic diagnostics.

## Impact

This depends on the synced `parse-first-function-call` change. It affects `SemanticAnalysis`,
semantic diagnostics, lookup tests, README/release validation, and the hidden inspector. It completes
the checkable two-function milestone but still produces no AST, HIR, MIR, or executable output.
