# Design

## Context

See proposal.md — Why. Ten confirmed review findings across three executor backends (bootstrap interpreter, wasm, native/LLVM), semantic analysis, ownership, and layout plumbing. Line numbers below are from the review at commit 60c01ed and must be re-anchored after merging latest main.

## Goals / Non-Goals

**Goals:** minimal, surgical edits per finding; three-executor convergence; one focused test per fix.

**Non-Goals:** no refactors beyond what a fix requires (no WasmBackend split, no Mir operand-visitor API, no broader dedup of layout math — those stay tech debt); no new diagnostics infrastructure; no changes to the other 30 confirmed review items.

## Decisions

1. **`MIN % -1` traps; checked returns `None`** (findings 1, 6). The existing spec text ("trap on ... invalid division/remainder"), native's current behavior, and the wasm backend's comment (which wrongly assumed `rem_s` traps) all indicate trap was the intended semantics; checked ops return `None` exactly where ordinary ops trap (matching CheckedDivide). Alternative — converge on 0 (wasm `rem_s` spec behavior) — rejected: it contradicts the written spec and the checked/ordinary correspondence.
   - `NativeScalarOperation.ts` (~552): widen the `CheckedDivide` MIN/-1 guard condition to also cover `CheckedRemainder`, so `invalid` includes overflow and `safeRight` is substituted — result `None`, no `srem MIN,-1` ever emitted.
   - `WasmBackend.ts` checked path (~6807): widen the `signedOverflow` guard to `CheckedRemainder` the same way.
   - `WasmBackend.ts` ordinary signed remainder (~141/169 emit sites): emit an explicit `if (left == MIN && right == -1) unreachable` guard before `rem_s`, mirroring how other invalid-arith traps are emitted; fix the false comment at ~134.
   - `BootstrapArithmetic.ts` (~87/119): make the ordinary remainder path trap on MIN/-1 (the range check currently passes 0); checked path returns `None` for the same condition (~155).

2. **Exact fmod on wasm via emitted helper** (finding 7). Wasm has no `frem` instruction; the div-trunc-mul-sub expansion is numerically wrong. Emit one synthetic module-local helper per float width implementing exact fmod with the standard exponent-aligned iterative algorithm (musl-style, integer bit manipulation on i32/i64 reinterpretations), and route both Remainder emit sites (~6261, ~6900) through calls to it. Alternative — import a host function — rejected: breaks self-contained wasm output. Alternative — keep expansion but clamp — rejected: still inexact on ordinary operands.

3. **Bootstrap rotate count: Euclidean mod** (finding 8). One-line fix at `BootstrapArithmetic.ts:57`: `Number(((right % w) + w) % w)`. No changes to wasm/native (already mask correctly).

4. **Effect-block typing** (finding 2), three edits in `ExpressionAnalysis.ts`:
   - `collectTerminals` (~5008): add an `UnsafeStatement` arm recursing into its nested statements, mirroring `returnFlowOf` (StatementAnalysis.ts:1066-1071).
   - Success type (~5024): join every collected return through `Match.join` — the language's one canonical result-join rule (match arms already use it), so effect blocks and match expressions cannot disagree. Joinable-but-different types form the canonical union (the surrounding context then rejects a mismatched use); a join with no representable form emits the new SEM0163 `effectBlockReturnMismatch` at the first disagreeing return. Alternative — first-return-wins with per-site equality checks — rejected in implementation: it would invent a second join rule beside `Match.join`.
   - Failure filter (~5013): drop the `Type.isNominal` filter entirely — `FailStatement.failure` is only ever set after StatementAnalysis.ts:950-955 validated it (runtime-concrete or value-kind parameter), so re-filtering at collection can only lose information; `Type.effect` already partitions concrete vs symbolic failures.

5. **EnumValue capture** (finding 3): add an `EnumValue` arm to the `effectCaptureFacts` switch (~4768) that visits `fact.argument`. Also add an exhaustiveness guard (`satisfies never` on the fallthrough) so the next missing fact kind is a compile error, not a silent non-capture — this is the root-cause fix for the bug class, at one line of cost.

6. **Ownership scanRunEnds** (finding 4): add `PlaceReplace`, `EffectResult`, and `EffectBindRequirement` arms (~1445) that recurse into their operand facts the same way the existing composite arms do, so nested occurrences reach the Identifier case (1528-1558). Include the same `satisfies never` exhaustiveness guard if the fact union allows it; otherwise mirror the existing default handling.

7. **Native EffectComposite lanes via callingShape** (finding 5): in `NativeType.lanesFor` (~24), resolve a registered `Layout.callingShape` for the composite first and derive lanes from it — exactly the `WasmLanes.laneKindsOf` pattern (WasmLanes.ts:34-36) — falling back to the current computation only when no shape is registered. Audit `NativeEffectOperation.ts` Pack/RunEffectComposite (~105-118, ~388-403): with overlapped MAX-payload lanes their slot-0 placement becomes consistent; adjust lane-type coercion to the unified payload types from the shape.

8. **Coroutine frame keyed by full suspension key** (finding 9): change `CoroutineFrame.stateLayout` (~150) to match entries with `contractRow` included, using the same canonicalization as `pointKey`/`Instances.keyText` rather than a hand-rolled triple — one comparison via the existing key text kills the drift risk.

9. **Environment lanes from Layout only** (finding 10):
   - `MirVerification.effectFieldLaneCount` (~1108): delete the re-derivation; return `Layout.effectFieldLanes(...).length` (the exact pattern CleanupEmission.ts:126 already uses).
   - `WasmBackend.hookReleaseInstructions` `environmentOffsets` (~1767): consume `Layout.effectFieldLanes`/`callableFieldLanePlacements` to enumerate field offsets and hook-bearing lanes instead of re-walking representations. If the Layout helpers don't currently expose per-field offset + hook info, add the minimal accessor to `Layout.ts` next to `effectFieldLanes` rather than widening the backend walk.

10. **Discovered during apply — two additional native/runner bugs fixed** (exposed by the new tests, same bug families as findings 3 and 5):
   - `EntryAssembly.lowerEffectRunner` lowered capture parameter types via `mirType` without the layout, so a captured scalar enum lost its `Enum` representation and every enum operation in a generated runner body silently failed to lower (the runner was then dropped and `Mir.verify` rejected the module). Fix: pass the layout through.
   - `MirLinearization.opensRuntimeContinuation` omitted `RunEffectComposite` while listing every sibling `Run*` operation, so locals defined after a composite run in the same linear block leaked as raw SSA values from the synthesized `following` block into later blocks — "non-phi forward value reference" at bitcode encoding. Fix: add the missing tag.

## Risks / Trade-offs

- [Wasm MIN/-1 guard adds a branch to every signed `%`] → only signed integer remainder; two extra instructions; acceptable. Constant-fold cases unaffected.
- [Emitted fmod helper is the largest new code surface] → gate with parity tests against native/bootstrap on a value sweep (subnormals, extreme exponents, NaN/inf propagation, both widths).
- [New diagnostic on mismatched effect-block returns may flag existing corpus code] → it flags real unsoundness; fix corpus programs if any trip it.
- [Finding 5's lanesFor change can shift native frame layouts] → run the full native acceptance corpus; the change only affects EffectComposite types with a registered shape, where current behavior is already wrong.
- [Line numbers drift after merging main] → tasks reference symbols, not lines; re-locate by symbol.

## Deferred to a follow-up PR

Review findings intentionally left out of this change (tracked as a spawned follow-up task): loan-end handling for effect-bind provider references (Ownership.scanRunEnds); joining only Available returns instead of degrading the block on any Unavailable return, and never-aware offender-span selection; SEM0163 test coverage; subword/i64 remainder and rotate corpus cases; EnumValue/EffectResult loan-scan tests; the dedicated hook-release test (task 10.3); corpus program rename, parity-test tag guard, and an operation-based needsFloatRemainder gate.

## Migration Plan

Land as one change; each fix is an independent commit-sized edit; tests run via `node scripts/turbo.mjs run test`. No data or deployment migration (unreleased project). Rollback = revert.
