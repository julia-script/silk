## Why

Silk has no source-level declaration marker, deterministic inventory, or safe per-test invocation
boundary, so neither the standard library nor user packages can build a runner in Silk.

Source: [SLP-0004, revision 36](../../../proposals/0004-silk-native-testing/proposal.md),
SHA-256 `0a39823f15178c075870f85c54ee86c2a8be5cd873c3d4139696500c07331808`,
realization slice 1 of 4.

## What Changes

- Add the contextual `test` marker for private, named, top-level, non-generic, zero-parameter
  closed Effect functions returning unit.
- Build one canonical ordered inventory over explicit test-root closures while keeping the
  separately designated executable runner role distinct from inventory membership.
- Expose opaque Copy test handles, canonical IDs, borrowed inventory access, and one uniform
  per-handle invocation primitive without general erased callable values.
- Convert normal return and unhandled typed failure into closed outcomes; a failed outcome owns the
  complete evaluator logical `StackPath`, while traps remain fatal.
- Gate representation work on proving owned path capture and cleanup at the individual invocation
  boundary; failure returns SLP-0004 to Candidate instead of weakening its contract.

## Capabilities

### New Capabilities

- `bootstrap-test-inventory`: define test eligibility, deterministic inventory metadata, opaque
  handles, invocation outcomes, and complete owned logical paths.

### Modified Capabilities

- `bootstrap-syntax`: parse and preserve the contextual `test` declaration marker.
- `bootstrap-declaration-index`: retain the marker on the canonical function header independently
  of body resolution.
- `bootstrap-module-closure`: compose explicit test roots with a distinct runner root without
  scanning or duplicate module identities.
- `bootstrap-evaluation`: invoke one closed marked Effect through the evaluator and capture its
  complete failure path while preserving cleanup and fatal traps.
- `bootstrap-diagnostics`: publish stable declaration-local diagnostics for every invalid marked
  declaration shape.
- `bootstrap-intrinsic-boundary`: admit only the smallest target-neutral inventory and invocation
  primitives and forbid privilege by standard-library spelling.
- `bootstrap-intrinsic-target-availability`: admit the testing operations only to evaluation in the
  initial slice and reject reachable artifact-emission use before lowering.

## Impact

This affects syntax, declaration facts, project closure composition, evaluator failure handling,
cleanup, semantic inspection, diagnostics, and the sealed intrinsic and availability catalogs. It
is a prerequisite for `add-silk-test-standard-library`.
