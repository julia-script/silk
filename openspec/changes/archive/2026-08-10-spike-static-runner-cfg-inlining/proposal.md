## Why

Shared constructor/static-dispatch normalization leaves one statically selected runner call in most
direct-Wasm entries. Removing it may require cloning structured runner control flow and remapping
typed exits and cleanup, so Silk needs evidence about the real MIR shapes and proof burden before a
production inliner is proposed.

## What Changes

- Add an evidence-only classifier for the runner graphs reached by the synchronous cost corpus.
- Record region, match, call, outcome, loan, cleanup, ownership, recursion, and growth facts for
  each remaining static runner.
- Prototype deterministic region/local/exit remapping over the smallest safe synthetic shape
  without enabling compiler behavior.
- Decide whether production work is justified now, must wait for affine-capture repair, or should
  remain delegated to target optimizers/linkers.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None. This is an evidence spike and changes no normative compiler behavior.

## Impact

The spike affects research artifacts and test-only MIR inspection. It does not alter lowering,
evaluation, backend input, Wasm emission, Effect semantics, ownership, or public APIs.
