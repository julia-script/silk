## Context

The hidden Syntax Inspector already derives one flat value-flow path from semantic facts and can
render ordered evaluator traces. Nested expressions now make both sources recursive in meaning:
one call result can feed an enclosing argument, sibling arguments have observable order, and an
inner blocked reason can terminate the enclosing path. The compiler data already carries stable
identities, source provenance, contracts, values, and deterministic event order.

See `proposal.md` for motivation and the delta spec for observable behavior.

## Goals / Non-Goals

**Goals:**

- Project nested semantic and evaluated value flow without changing compiler-owned data.
- Keep completed and incomplete flow equally legible, navigable, deterministic, and accessible.
- Stay small enough to remain a developer inspector rather than a general graph product.

**Non-Goals:**

- Build control-flow, dataflow analysis, SSA, HIR, MIR, a runtime debugger, or a general-purpose
  graph layout engine.
- Persist source, selection, evaluation, or layout state.
- Change evaluator trace semantics or compiler package exports.

## Decisions

### Build one pure browser-side projection

The inspector will derive a presentation model from the current semantic result and optional
evaluation outcome. Groups are keyed by expression and call-site identities; nodes and edges refer
back to existing argument, parameter, reference, function, trace, and span data. Recomputing the
projection after every analysis or evaluation keeps it disposable and prevents stale relationships.

Adding a compiler-side visualization model was rejected because these grouping and label choices
are documentation concerns. Reading the concrete tree independently was rejected because it would
reimplement semantic resolution and could draw relationships the analyzer did not establish.

### Use nested lanes rather than arbitrary graph layout

Each call site forms a bounded group. Its argument branches flow through any nested call groups,
then converge on the enclosing positional bindings, body reference, and return. Sibling groups stay
in argument order. Wide layouts can place call depth in adjacent lanes; narrow layouts stack the
same ordered groups vertically.

This structure directly matches the current expression slice and can be rendered with ordinary
document layout. A force-directed graph or canvas was rejected because it would add nondeterminism,
accessibility work, and a dependency without improving this small tree-shaped flow.

### Keep semantic flow and evaluated flow visibly distinct

Semantic facts define possible relationships and compatibility; only an explicit evaluation
outcome defines reachable order, exact runtime values, and the point where execution stopped. The
base view always renders semantic flow. When an outcome exists, trace-backed badges and ordered
states overlay the same identities rather than replacing the semantic view.

Inferring runtime order or values from compatible contracts was rejected because it would turn a
static relationship into a false execution claim. Building separate unrelated diagrams was also
rejected because selection and identity correspondence would become harder to follow.

### Terminate incomplete paths at the authoritative boundary

Missing and ambiguous references branch or stop from semantic data. Incompatible and unavailable
contracts stop before a successful result edge. Evaluator blockage retains the successful trace
prefix but suppresses any enclosing binding or return absent from the trace. Recursive cycles render
the finite cycle data and closing call site as a terminal group.

The projection never repairs or fills gaps. This keeps visual feedback trustworthy even for the
malformed and blocked presets that are most useful during compiler work.

### Make the text structure the canonical reading order

The visual and accessible renderings consume the same ordered projection. Each group has a heading,
each relationship has a textual source and target, and state, value, ordinal, depth, and byte range
are exposed without relying on geometry or color. Activating either rendering uses the same stable
selection key to emphasize source and detail panels.

Maintaining a hand-authored alternate summary was rejected because it could silently omit branches
or drift from the visual view.

## Risks / Trade-offs

- **Deep nesting can make the page horizontally unwieldy** → Cap visual indentation, preserve depth
  labels, and switch to ordered vertical groups at narrow widths without hiding data.
- **Static and evaluated overlays can be mistaken for one state** → Use explicit labels and a legend,
  and never show trace order or exact runtime values before evaluation.
- **Repeated calls to the same declaration can look identical** → Key and label groups by call-site
  identity and span, not callee name alone.
- **Blocked paths can accidentally imply work that never ran** → Derive evaluated edges only from
  trace events and assert the absence of unperformed bindings and returns.
- **Large fixtures can create dense markup** → Keep presets intentionally small and use bounded
  group disclosure without changing the accessible ordered data.

## Migration Plan

First extract the pure nested projection with unit fixtures, then render it in the existing flow
panel, add source synchronization and accessible structure, and finally add presets and responsive
visual verification. The change has no compiler migration. Rollback restores the flat projection
without affecting syntax, semantic facts, evaluation, or saved user data because none is persisted.
