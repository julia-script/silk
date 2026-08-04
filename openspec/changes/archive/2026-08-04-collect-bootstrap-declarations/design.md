## Context

See `proposal.md` for motivation. This change assumes `parse-multiple-bootstrap-functions` has been
synced, so a source-file tree can contain multiple direct function declarations. The current semantic
result exposes one declaration, integer expression, and compatibility at its root; that shape cannot
preserve per-function ownership.

## Goals / Non-Goals

**Goals:**

- Publish complete ordered facts for every parsed function.
- Make declaration identity and name lookup deterministic in the presence of recovery and duplicates.
- Keep body facts attached to the function that owns them.
- Provide direct visual evidence through declaration cards and ambiguity states.

**Non-Goals:**

- Calls, reference facts, nested scopes, parameters, a generic symbol table, AST, or HIR.
- Rejecting or removing duplicate declarations from the fact collection.

## Decisions

### The result owns ordered `FunctionFact` values

`SemanticAnalysis.Result` will replace its singular declaration/expression/compatibility fields with
a readonly `functions` collection. Each `FunctionFact` groups a `DeclarationFact`, the returned
integer fact, and compatibility. Grouping by owning function keeps the module concept-oriented and
avoids parallel arrays or a generic fact database.

The previous singular API is removed rather than preserved because the package is unreleased and a
compatibility alias would create two competing result models.

### Declaration ordinals are concrete source indexes

Every direct function node receives ordinal `0..n-1` in source order, paired with the existing
source identity. Missing names do not alter ordering. The identity is deterministic for one source
snapshot without promising stability across source edits.

### Lookup has closed resolved, missing, and ambiguous outcomes

Name lookup will return data that distinguishes one match, no match, and multiple matches. All
matching declaration identities remain available in the ambiguous case. This is more honest than
returning the first duplicate or collapsing ambiguity into `None`, and it becomes the exact input
needed by the later resolver without introducing scopes.

### Later duplicates produce `SEM0003`

The first present spelling establishes the original declaration; every later present match produces
one duplicate-name diagnostic on its own name span. All declarations remain analyzable. Missing names
do not enter lookup and do not produce semantic duplicates of parser diagnostics.

### Analysis maps the existing bounded function traversal

The current function-local traversal and exact integer/type analysis will be reused independently for
each direct function node. Diagnostics from every function and duplicate collection are merged only
within the semantic phase and sorted by span/code after collection.

### The inspector mirrors ownership instead of flattening facts

One compact card per `FunctionFact` will show its identity, name, declared type, integer fact,
compatibility, and spans. A duplicate preset will show both cards plus the ambiguous lookup outcome
and `SEM0003`; the concrete tree remains alongside the facts.

## Risks / Trade-offs

- **The public result shape breaks immediately after its introduction** → Embrace the prerelease
  correction and update every consumer, README example, test, and packed export in one change.
- **Duplicate diagnostics and lookup could disagree** → Derive both from the same spelling groups
  and test two and three duplicate declarations.
- **Fact cards may overwhelm the inspector** → Use a compact ordered list with expandable provenance
  while keeping diagnostics and the concrete tree visible.

## Migration Plan

Land only after the parser dependency is synced. Replace singular consumers atomically; do not add
deprecated aliases. Rollback restores the previous singular result and inspector panel because no
persisted representation exists.
