## Context

The preceding parser change makes call expressions valid inside argument lists but deliberately
publishes them as unavailable semantic placeholders. The current semantic model already represents
integer literals, parameter references, top-level call facts, positional argument contracts, stable
source-local identities, and exact syntax provenance. The evaluator and hidden inspector consume
those facts directly, so changing the expression boundary affects all three surfaces.

See `proposal.md` for motivation and the delta specs for observable behavior.

## Goals / Non-Goals

**Goals:**

- Make the existing semantic expression vocabulary recursive over the lossless concrete tree.
- Preserve deterministic source order, stable identity, exact provenance, and phase-owned errors at
  every nesting depth.
- Leave the repository correct and inspectable between semantic analysis and recursive evaluation.

**Non-Goals:**

- Introduce a semantic AST, HIR, MIR, lowering pass, operator precedence, or general expression
  framework.
- Evaluate nested calls in this change.
- Define a general recursion-depth policy for the language.

## Decisions

### Use one recursive semantic expression fact

The semantic expression union will represent integer literals, parameter references, and call
expressions uniformly. An argument fact points at that union, and a call-expression fact owns its
ordered argument facts and contract. This matches the recursive grammar while preserving the
current actor-oriented fact model.

Keeping a separate one-off `NestedCallArgumentFact` was rejected because it would duplicate the
call contract and make the next expression position require another special case. Introducing an
AST or HIR was rejected because the concrete tree already supplies the structure and provenance
needed for this milestone.

### Analyze leaves before dependent contracts

The analyzer will walk a returned expression recursively in concrete source order. Each call target
is resolved using the existing top-level declaration collection, each argument expression is
analyzed in the caller's local parameter environment, and the call contract is computed only after
its arguments have type states. The enclosing expression then consumes that call's result-type
state.

This produces inside-out semantic dependencies without changing diagnostic order: diagnostics are
collected deterministically by concrete traversal position, not by incidental object construction.
A global expression graph was rejected as unnecessary for this first recursive slice.

### Reuse call spans to distinguish nested identities

Existing source-local function identities and owner-qualified, half-open call spans are sufficient
to distinguish nested call expressions. Argument identities continue to combine their owning call
site with a zero-based ordinal, so identical spellings at different depths cannot collide. No
process-global counters or opaque runtime identities will be added.

### Add an explicit evaluator boundary

The evaluator will recognize a reachable recursive call expression and return an
`UnsupportedNestedExpression` blocked reason containing the nested expression identity and span.
It will preserve events emitted before the argument was required. This closed transitional case is
preferable to coercing the expression back to unavailable data, which would erase the fact that
analysis succeeded, or partially evaluating it before trace semantics are specified.

The following `evaluate-nested-expressions` change removes this boundary by implementing recursive
evaluation. Because the project is prerelease, the blocked-reason union can change directly rather
than carrying a compatibility alias afterward.

### Render the same recursive facts in the inspector

The inspector will recursively render semantic expression cards beneath their owning argument and
will link each card to its concrete span. The evaluation panel will render the explicit temporary
blocked reason beside those facts. The visualization remains a browser-only projection of compiler
data and does not introduce a second semantic model.

## Risks / Trade-offs

- **Deep input can exhaust the host stack** → Add a representative deep fixture and a deterministic
  implementation guard if testing shows the straightforward recursion is unsafe; do not define a
  language limit accidentally through an undocumented crash.
- **Recursive public types can become awkward to consume** → Keep the union discriminated and keep
  expression-specific data owned by the corresponding actor instead of exposing cyclic object
  references.
- **The evaluator is temporarily less capable than the analyzer** → Expose a precise closed reason
  and inspector feedback, then remove that boundary in the immediately following change.
- **Outer failures can obscure useful inner facts** → Retain every analyzed inner fact and attach
  unavailability only to the dependent outer type or contract.

## Migration Plan

Implement and verify the recursive semantic fact shape first, update evaluator exhaustiveness with
the closed transitional boundary, then update the inspector and documentation. This is a deliberate
prerelease breaking change with no compatibility layer. If the change must be rolled back, revert
the semantic shape and evaluator case together so the parser's explicit unavailable placeholders
remain the truthful boundary.
