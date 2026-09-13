## Why

MIR runner discovery currently loses a semantically valid provided service call when an ordinary
generic bracket callback obtains its owned provider through a nominal-union pattern binding. The
program analyzes cleanly, but lowering reports an `InvalidEffectOperation` because the specialized
callback runner was not retained, blocking the owned TLS prerequisite on an otherwise ordinary
`Effect.useReleaseNonParking` composition.

## What Changes

- Preserve exact callable and provided-runner reachability through specialized generic bracket
  callbacks, including providers reached through an active nominal-union pattern arm.
- Carry the callback's substituted provider selection and pattern-rooted borrow through the
  structural forwarding path into final MIR without inventing a dynamic Effect representation or
  globally retaining unrelated runners.
- Add a permanent, target-neutral compiler regression that distinguishes this case from the already
  working direct-field generic provider, pure nominal-union pattern, and borrowed-provider cases.
- Keep MIR verification strict: the correction must produce the required runner rather than silence
  an `InvalidEffectOperation`, approximate the call as synchronous, or accept an incomplete module.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-flow-functions`: Clarify that generic source-defined provision remains valid when the
  provider is selected through an owned aggregate or nominal-union pattern inside an ordinary
  bracket callback.
- `bootstrap-mir`: Require complete, deterministic runner discovery and realization for that
  specialized callback and provider shape.

## Impact

- Expected implementation scope is the compiler's structural forwarded-provider lowering seam.
  Executable discovery, provisional MIR, and layout already retain the callback and its exact
  environment; final lowering must accept the same active pattern-rooted provider borrow. No Silk
  syntax, Effect API, public runtime ABI, backend-specific rule, or standard-library actor changes.
- The existing verifier remains the acceptance boundary, and the focused regression belongs in the
  existing suspendability/compiler-ownership suite rather than a new backend pass.
- This unblocks the portable lowering and runtime evidence for
  `add-owned-tls-connections`. That change's native/TLS design and public API remain unchanged while
  this compiler prerequisite is addressed.
