## Context

See `proposal.md` for motivation. This change assumes multi-function declaration collection and the
unresolved call-expression fact are synced. All declarations are top-level, parameterless, and have
explicit return-type syntax, so reference and type facts can be computed without execution or a
general scope graph.

## Goals / Non-Goals

**Goals:**

- Connect each present call name to zero, one, or many collected declarations deterministically.
- Propagate a uniquely resolved target's declared return type to the call expression.
- Preserve exact caller, call-site, and target provenance.
- Complete the milestone with an inspectable `caller → callee` relationship.

**Non-Goals:**

- Arguments, overload selection, nested scopes, imports, recursion validation, call ordering,
  evaluation, AST, HIR, MIR, or lowering.

## Decisions

### Analysis separates declaration collection from expression resolution

The analyzer will first build every declaration header and name group, then analyze returned
expressions using the completed collection. This makes backward, forward, and self references
equivalent and avoids source-order-dependent results. It is a bounded two-pass analysis, not a
general query engine.

### Reference states reuse declaration lookup semantics

A call reference records `Resolved`, `Missing`, `Ambiguous`, or syntax-unavailable. Resolved holds
the target declaration identity and both call/target provenance; ambiguous retains all matches in
source order. The resolver does not pick the first duplicate.

The temporary `Unresolved` staging state from the parser change is removed rather than retained as
a compatibility branch.

### Missing targets use `SEM0004`; ambiguity stays declaration-owned

A present callee with no match produces `SEM0004` on the callee span. Duplicate declarations already
produce `SEM0003` at the source of ambiguity, so an ambiguous call adds no redundant diagnostic.
Parser-damaged callees remain unavailable with parser-owned diagnostics.

### Call type comes from the target's declared return type

A uniquely resolved call has the target declaration's resolved return type. It does not depend on
whether the target body's own compatibility check succeeded; that body reports its own diagnostics.
If the target return type is unresolved or unavailable, the reference remains resolved while the
call type and caller compatibility are unavailable.

### Self references resolve without executing

Self-calls are ordinary declaration relationships in this phase. Recursion legality, termination,
stack behavior, and lowering are later concerns. No dependency scheduling or cycle walk is needed to
record a reference to an explicit header type.

### The inspector renders a compact relation, not a graph framework

Each call fact will show a small `caller → target` row or a missing/ambiguous state with clickable or
clearly labeled provenance spans. Presets cover resolved, forward, unknown, and ambiguous calls. A
general graph visualization is unnecessary for this two-function milestone.

## Risks / Trade-offs

- **Two-pass analysis could drift from declaration lookup** → Make call resolution consume the same
  closed lookup operation and test backward, forward, self, missing, and ambiguous cases.
- **A resolved target with a broken body may look successful** → Display reference resolution and
  target-body compatibility as separate facts and keep all diagnostics visible.
- **The inspector relation may imply execution** → Label it as semantic resolution and retain the
  explicit statement that no HIR or code generation exists.

## Migration Plan

Land only after call syntax is synced. Replace the temporary unresolved state across the public
union, tests, README, and inspector in one prerelease breaking change. Rollback restores unresolved
call facts; no persisted state or compatibility layer is required.
