# Compiler Review Stability Fixes

## Why

A high-effort compiler review confirmed 10 defects: cross-executor arithmetic divergence (including one LLVM undefined-behavior path), three soundness holes in effect-block result typing, a missed capture, a borrow-checker blind spot, native-only lane/frame-keying bugs, and environment-lane logic re-derived outside its authority that has already desynced once. All are reachable from legal source or from a routine future change; fixing them now converges the three executors and closes the silent-miscompile paths.

## What Changes

Minimal edits per finding — behavior converges on what the specs already require; no new features.

- **Integer remainder MIN/-1** (findings 1, 6): the intended semantics is the spec's "trap on invalid division/remainder" — native already traps and the wasm backend's comment shows trap was assumed. Fix the two executors that silently return 0: wasm emits an explicit MIN/-1 guard → `unreachable` before `rem_s`; bootstrap traps in its remainder range check. `CheckedRemainder(MIN, -1)` returns `None` on all three executors (native's existing CheckedDivide guard extends to CheckedRemainder, eliminating the `srem MIN,-1` UB; wasm and bootstrap add the same overflow condition).
- **Float remainder on wasm** (finding 7): replace the inexact `left - trunc(left/right) * right` expansion (two sites) with an exact fmod helper function emitted into the module, matching native `frem` and bootstrap `%`.
- **Bootstrap rotate counts** (finding 8): mask the rotate count unsigned modulo lane width (Euclidean mod) before shifting, matching wasm `rotl`/`rotr` and native `fshl`/`fshr`.
- **Effect-block result typing** (finding 2): `collectTerminals` descends into `UnsafeStatement`; all collected returns are joined with a diagnostic on incompatible success types instead of last-return-wins; value-kind type-parameter failures survive into the failure row instead of being dropped by the nominal-only filter.
- **Effect-block captures** (finding 3): `effectCaptureFacts` visits the `EnumValue` fact's argument so `Enum.value(x)` registers `x` as a capture.
- **Ownership loan ends** (finding 4): `scanRunEnds` gains cases for `PlaceReplace`, `EffectResult`, and `EffectBindRequirement` so identifier/callable occurrences inside their operands extend loan ends and invalidate `callableEnds`.
- **Native EffectComposite lanes** (finding 5): `NativeType.lanesFor` resolves `EffectComposite` through the registered `Layout.callingShape` (overlapped MAX-payload lanes) the way `WasmLanes.laneKindsOf` does, instead of concatenating alternatives.
- **Coroutine frame lookup** (finding 9): `CoroutineFrame.stateLayout` matches frame entries by the full suspension key including `contractRow`, aligning with `pointKey`/`Instances.keyText`.
- **Environment-lane single source of truth** (finding 10): `WasmBackend.hookReleaseInstructions` and `MirVerification.effectFieldLaneCount` consume `Layout.effectFieldLanes` instead of re-deriving the field walk (one prior desync in history; the verifier's copy is count-only and blesses divergence).
- Fix the false `rem_s` trap comment at `WasmBackend.ts:134`.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-integer-scalars`: pin `MIN % -1` (trap) and `CheckedRemainder(MIN, -1)` (`None`) semantics, and rotate-count wrapping modulo lane width, identically across executors.
- `bootstrap-floating-point-scalars`: pin float remainder to exact IEEE fmod results on every executor.
- `bootstrap-flow-functions`: effect-block result typing accounts for terminals inside `unsafe` blocks, rejects incompatible success types across return sites with a diagnostic, keeps generic (type-parameter) failures in the failure row, and captures bindings referenced through enum-value construction.
- `bootstrap-ownership`: loan live-ranges account for uses nested in place-replace, effect-result, and requirement-binding expressions.

### Implementation-only (no spec delta)

Findings 5, 9, 10 make the native backend and verifier conform to already-specified parity/layout behavior; no requirement changes.

## Impact

- `packages/compiler/src`: `NativeScalarOperation.ts`, `NativeArith.ts`, `WasmBackend.ts`, `BootstrapArithmetic.ts`, `ExpressionAnalysis.ts`, `Ownership.ts`, `NativeType.ts` (+ `NativeEffectOperation.ts` if placement follows lanes), `CoroutineFrame.ts`, `MirVerification.ts`, `Layout.ts` (export of the lane-walk helper if not already public).
- Observable behavior: programs relying on `MIN % -1 == 0` on wasm/bootstrap now trap (they already trapped on native); effect blocks with mismatched branch returns now get a diagnostic. Silk is unreleased — no compatibility concerns.
- Tests: one focused check per fix, run via `node scripts/turbo.mjs run test`; cross-executor cases land in the existing engine-parity suites.
