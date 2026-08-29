## Why

Silk source can drive recursive expression parsing past JavaScript's host-stack limit, causing `Parser.parse` and `Analysis.ofSource` to leak a nondeterministic `RangeError` instead of returning syntax and diagnostics. The parser needs one host-independent nesting contract and lossless recovery before more recursive expression forms deepen this exposure.

## What Changes

- Define a maximum of 256 nested expression edges, with an outer expression at depth 0, every recursive expression edge incrementing depth once, and siblings measured independently.
- Diagnose the first edge beyond that limit with a stable parser code and the exact offending syntax-edge span.
- Recover the over-budget region as explicit lossless syntax, preserve every original token exactly once, and resume at the owning expression boundary so later statements and declarations remain parseable.
- Apply the same budget to grouping, arrays, call/container arguments, direct prefixes, and every other recursive expression entry rather than guarding one syntax form.
- Preserve parser invariant failures as defects; do not translate arbitrary `RangeError` values into source diagnostics.
- Document the supported expression boundary and verify both direct parsing and analysis-facade behavior.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Bound recursive expression parsing and define lossless over-budget recovery.
- `bootstrap-syntax-file`: Preserve deterministic, byte-reconstructable syntax artifacts for over-budget source.
- `bootstrap-analysis-facade`: Return queryable snapshots and diagnostics for source that exceeds the expression budget.
- `bootstrap-diagnostics`: Add the stable parser diagnostic contract, cardinality, span, and defect-isolation rules for excessive nesting.

## Impact

- Compiler parser state, recursive expression entry points, error-node construction, and parser diagnostic generation.
- `SyntaxFile` tree/diagnostic encoding and `Analysis.ofSource` snapshot construction.
- Parser and analysis tests covering boundary counts, independent expressions, lossless recovery, determinism, and invariant defects.
- The prescriptive expression reference in `apps/docs/content/reference/expressions-and-operators.md`.
- No dependency, package export, or source-language compiler-intrinsic changes.
