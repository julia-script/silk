# Proposal

## Why

Checked bodies, compile-time evaluation, residual construction, and ownership still maintain independent reuse and validity rules. That split can preserve stale facts, charge reused work incorrectly, and prevents the compiler from explaining one coherent dependency path across revisions.

## What Changes

- Add reconstructible revision queries for complete checked units with stable body and application identities.
- Separate compile-time evaluation from residual-program construction while recording the actual semantic inputs each operation reads.
- Derive ownership through the shared query validator so unchanged ownership results reuse under the same dependency rules as name and type queries.
- Preserve editor presentation against the current syntax revision while reused results remain position-independent.
- Make evaluation reuse budget-aware: validation is free, a hit charges its recorded work cost exactly once, and policy participates in validity.
- Preserve abort, cycle, parser-recovery, and forced-fresh behavior across the new query families.
- **BREAKING**: delete the body-specific validity scanner and evaluator-local cache authority; all semantic reuse is admitted by the shared validator.

## Capabilities

### New Capabilities

- `semantic-body-revision-validation`: Complete checked-body, evaluation, residualization, and ownership queries validated from recorded semantic dependencies across adjacent revisions.

### Modified Capabilities

None.

## Impact

This changes compiler-internal semantic query descriptors, checked-unit construction and presentation, static evaluation, residualization, ownership analysis, incremental project state, and their focused compiler tests. It does not add a compatibility path for the superseded body cache.
