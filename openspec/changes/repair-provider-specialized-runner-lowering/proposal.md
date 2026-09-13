## Why

A second realized caller of the owned TLS connection can make final MIR reference a valid
provider-specialized Effect runner that lowering silently failed to publish. The verifier correctly
rejects the partial module as `InvalidEffectOperation`, blocking native evidence for owned TLS even
though semantic analysis and the same single-caller composition are valid.

## What Changes

- Preserve precise internal provenance when a generated Effect runner cannot lower, including its
  canonical runner/base/owner identities and the first statement or expression boundary that made
  the body unavailable; never silently omit it while retaining callers.
- Repair the target-neutral lowering path so the same generic provided operation can be realized
  from a second concrete caller/callback specialization with its exact captures, provider witness,
  rows, loans, cleanup, and result contract.
- Add one permanent structured regression derived from the measured two-caller failure. It will
  lower and verify MIR without starting LLVM, WebAssembly, or a native process, and will be reduced
  from the current approximately 116-second diagnostic source before becoming default-suite
  evidence.
- Keep MIR verification strict and retain only reachable canonical runners. This change does not
  add a runtime ABI, globally retain generated runners, force a suspension classification, or
  recognize TLS, `OwnedConnection`, `ByteDuplex`, `Effect.map`, or any other source spelling.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-mir`: Require generated-runner lowering to either publish every referenced reachable
  runner or preserve a precise causative lowering failure, including when a second concrete caller
  specializes the same generic provided Effect body.

## Impact

- Compiler scope is the generated-runner construction boundary in `EntryAssembly`, exact/runtime
  call-shape selection in `FunctionLowering`, callable-section recontextualization and direct-run
  emission in `ValueType`/`LowerExpression`, canonical captured-Effect identity metadata in
  `Layout`, and post-worklist validation in `Lower`. `ValueType.ensureProvidedRunner` remains
  unchanged evidence input rather than the failing seam.
- Permanent evidence belongs in an existing target-neutral compiler test file and reuses one
  analysis/lowering snapshot. The 116-second TLS-derived diagnostic remains investigation evidence,
  not an acceptable new backend or default-suite pass.
- No Silk syntax, public Effect or TLS API, standard-library implementation, runtime component,
  backend ABI, verifier rule, or generated catalog changes are intended.
- The correction unblocks native acceptance for `add-owned-tls-connections`; completing that
  separate change remains out of scope here.
