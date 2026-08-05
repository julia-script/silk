## Why

Once parameters exist in concrete syntax, a function body needs to know whether a bare name refers to one of its own parameters. A function-local lookup is the smallest honest scope model and creates usable value flow without introducing nested scopes or a general symbol graph.

## What Changes

- Collect ordered parameter declaration facts with source-local identities, names, declared `I32` types, and exact syntax provenance.
- Resolve bare identifier expressions against the enclosing function's parameters, including references used as call arguments.
- Diagnose unknown and duplicate parameter names deterministically while preserving every declaration and damaged-syntax state.
- Propagate a uniquely resolved parameter's type to its expression fact and return compatibility.
- Add inspector presets and links for resolved, missing, duplicate, and recovered parameter references.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-semantic-facts`: Add function-local parameter declarations, lookup, reference facts, types, and diagnostics.
- `bootstrap-syntax-inspector`: Visualize parameter declarations and reference relationships beside their concrete syntax and diagnostics.

## Impact

This changes semantic analysis and diagnostic public data, related tests and fixtures, compiler documentation, release-candidate validation, and the hidden syntax inspector. Lookup remains limited to parameters of the current top-level function; locals, globals, nested scopes, shadowing policy, and general name resolution remain deferred.
