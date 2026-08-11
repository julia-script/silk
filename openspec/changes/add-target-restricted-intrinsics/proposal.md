## Why

Some irreducible platform primitives are valid only on selected execution targets, but their mere
presence in the packaged standard library must not contaminate portable programs. Silk needs one
generic reachable-only availability rule before adding native-only providers.

## What Changes

- Make the intrinsic catalog's supported-target metadata enforceable rather than documentary.
- Diagnose a target-restricted intrinsic only when it survives reachable-program closure for the
  selected evaluator/backend request.
- Allow unreachable standard-library and application declarations to mention another target's
  intrinsic without adding runtime symbols or host imports.
- Add a stable target-unavailable diagnostic naming the intrinsic and requested target.
- Keep the mechanism generic: no filesystem, standard-stream, service, or library actor name
  receives special treatment.
- Verify deterministic behavior and artifact pay-for-use across evaluator, LLVM, and direct Wasm.

## Capabilities

### New Capabilities

- `bootstrap-intrinsic-target-availability`: Reachable-only target validation, diagnostics, and
  artifact pay-for-use for sealed intrinsic operations.

### Modified Capabilities

- `bootstrap-intrinsic-boundary`: Make each catalog operation's supported-target set an enforced
  part of its auditable contract.
- `bootstrap-module-closure`: Separate source-module reachability from executable operation
  reachability so unused target-specific declarations remain inert.
- `bootstrap-backend`: Reject reachable unsupported operations before emission and omit their
  runtime symbols and imports when unreachable.

## Impact

The change affects intrinsic catalog metadata, executable closure planning, diagnostics, HIR/MIR
verification boundaries, evaluator requests, native and Wasm emission plans, artifact inventories,
tooling presentation, and focused pay-for-use tests. Existing all-target intrinsics retain their
current behavior.
