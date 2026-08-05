## Why

After nested calls become valid concrete syntax, semantic analysis still treats them as explicitly
unavailable placeholders. The next baby step is to make those parsed expressions meaningful by
analyzing the same existing semantic facts recursively, without introducing an AST, HIR, or MIR.

## What Changes

- **BREAKING**: Replace the flat argument-expression fact boundary with a recursive semantic
  expression fact that can represent integer literals, parameter references, and resolved or
  unresolved calls at any argument depth.
- Resolve nested call targets, analyze their arguments in source order, and compute each nested
  positional contract and result type from the inside out.
- Keep missing, ambiguous, damaged, and type-unavailable inner expressions explicit, propagating
  unavailability to dependent outer contracts without inventing bindings or duplicate diagnostics.
- Make the evaluator return a closed, provenance-rich unsupported-nested-expression outcome when a
  reachable nested call is analyzed but recursive evaluation is not available yet.
- Add nested semantic presets and relationship feedback to the hidden Syntax Inspector, including
  the deliberately blocked evaluation state.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-semantic-facts`: Analyze call expressions recursively within argument facts and retain
  their nested identities, contracts, types, resolution states, and provenance.
- `bootstrap-evaluation`: Define the honest transitional outcome for a reachable nested expression
  before recursive evaluation is implemented.
- `bootstrap-syntax-inspector`: Show nested semantic relationships and their temporary evaluation
  boundary directly.

## Impact

This changes the prerelease semantic expression model and analyzer, evaluator blocked-reason data,
fixtures and tests, and the hidden inspector's semantic and evaluation panels. Existing flat
integer and parameter-reference programs keep their behavior. Recursive evaluation remains the
next separate change.
