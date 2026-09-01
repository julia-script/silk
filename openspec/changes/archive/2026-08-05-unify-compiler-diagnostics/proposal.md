## Why

The spike carries three per-phase diagnostic shapes (`LexicalDiagnostic`, `ParseDiagnostic`,
`SemanticDiagnostic`) with no stable codes, no causal links, and no single ordering authority.
The pinned pipeline requires one structured diagnostic model that every phase publishes into,
so this change comes first: every later realignment proposal produces diagnostics in this shape.

## What Changes

- **BREAKING**: Replace the three per-phase diagnostic types with one `Diagnostic` model carrying
  a stable code, severity, concise message, one primary source span, optional labeled related
  spans and notes, optional unambiguous machine-applicable edits, the originating phase and
  semantic entity, and an optional causal diagnostic ID.
- Introduce error sentinels that preserve diagnostic provenance so dependent cascades can be
  suppressed or attached to the primary error.
- Move ordering into one driver-side sort: canonical module identity, primary span, code, and a
  stable tie-breaker. Phases return diagnostics as data and never print.
- Rework the inspector's diagnostic presentation to consume the unified stream, including phase
  origin and causal chains.

## Capabilities

### New Capabilities

- `bootstrap-diagnostics`: The single structured diagnostic model, error sentinels, and the
  deterministic driver-side ordering that all compiler phases publish into.

### Modified Capabilities

- `bootstrap-lexer`: Emit unified diagnostics with stable codes.
- `bootstrap-syntax`: Emit unified diagnostics with stable codes.
- `bootstrap-semantic-facts`: Emit unified diagnostics with stable codes and causal links from
  unavailable facts to their originating diagnostics.
- `bootstrap-syntax-inspector`: Render the unified diagnostic stream with phase and cause.

## Impact

Every phase result type, all diagnostic fixtures and tests, and the inspector's diagnostic
panels. No language surface changes. Existing diagnostic _content_ is preserved; only its shape,
codes, and ordering authority change.

## Plan References

- [Roadmap — Track 1, proposal 1](../../../roadmaps/compiler-realignment.md)
- [Issue 06 — Design the bootstrap compiler pipeline](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  diagnostics paragraph: "Diagnostics are structured ordinary data. Each diagnostic has a stable
  code, severity, concise message, one primary source span, optional labeled related spans and
  notes, optional unambiguous machine-applicable edits, its originating phase and semantic
  entity, and an optional causal diagnostic ID. … Phases never print diagnostics themselves."
- Same ticket, recovery paragraph: "Source mistakes are ordinary diagnostic data rather than
  fail-fast phase errors."
