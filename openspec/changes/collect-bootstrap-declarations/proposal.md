## Why

Once source files contain multiple functions, a single declaration fact can no longer represent the
program or support trustworthy lookup. The milestone needs ordered identities and explicit duplicate
handling before any call can be resolved.

## What Changes

- **BREAKING**: Replace the single-function semantic result shape with ordered per-function facts.
- Assign deterministic source-local declaration ordinals in concrete source order.
- Add closed name-lookup outcomes for one match, no match, and ambiguous duplicate matches.
- Diagnose duplicate present names without inventing names for recovered syntax.
- Render declaration cards and duplicate-name feedback in the hidden inspector.
- Defer call syntax and reference resolution.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-semantic-facts`: Collect and query every parsed function declaration, including stable
  ordering, identity, body facts, and duplicate-name behavior.
- `bootstrap-syntax-inspector`: Show the complete declaration collection and ambiguity state as the
  ticket's visual checkpoint.

## Impact

This depends on the synced `parse-multiple-bootstrap-functions` change. It affects
`SemanticAnalysis`, semantic diagnostics, compiler exports/tests/README, release validation, and the
inspector. No AST, generic symbol table, scope graph, or reference model is introduced.
