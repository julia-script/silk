## Why

After syntax, resolution, and call checking can describe a complete value path, Silk is ready for its first source-to-result vertical slice. A tiny direct evaluator proves the frontend facts compose into behavior without pretending that HIR, MIR, LLVM lowering, or native compilation already exist.

## What Changes

- Introduce a bootstrap evaluator for a uniquely named, zero-parameter `main` function returning `I32`.
- Evaluate the existing closed expression slice: decimal integers, resolved parameter references, and checked function calls.
- Bind evaluated arguments positionally to target parameters and return an exact `I32` result.
- Return explicit typed outcomes for missing or invalid entry points, semantically unavailable programs, and recursive call cycles.
- Preserve deterministic evaluation traces that identify calls, bindings, references, and returned values by existing semantic identities.
- Add inspector controls and output for evaluating the canonical program and seeing its result or blocked reason.

## Capabilities

### New Capabilities

- `bootstrap-evaluation`: Define the first deterministic, direct evaluation contract over analyzed bootstrap programs.

### Modified Capabilities

- `bootstrap-syntax-inspector`: Let developers evaluate a valid in-memory fixture and inspect its result, trace, or blocked reason.

## Impact

This adds a public compiler evaluation actor and exports, tests, documentation, package changeset, release-candidate validation, and inspector output. It does not emit machine code, invoke LLVM, perform I/O, persist state, optimize, or define the eventual runtime. Recursion is detected and reported rather than executed indefinitely.
