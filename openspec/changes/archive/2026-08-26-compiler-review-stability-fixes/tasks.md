## 1. Sync

- [x] 1.1 Fetch and merge latest main into the working branch; re-anchor all finding sites by symbol (line numbers in design.md are from commit 60c01ed)

## 2. Integer remainder MIN/-1 (findings 1, 6)

- [x] 2.1 NativeScalarOperation: widen the CheckedDivide MIN/-1 guard to CheckedRemainder so the result is None and no `srem MIN,-1` is emitted
- [x] 2.2 WasmBackend checked path: widen the signedOverflow guard to CheckedRemainder (result None)
- [x] 2.3 WasmBackend ordinary signed remainder: emit MIN/-1 guard → unreachable before rem_s; correct the false rem_s-traps comment
- [x] 2.4 BootstrapArithmetic: ordinary remainder traps on MIN/-1; checked remainder returns None
- [x] 2.5 Cross-executor parity test: `MIN % -1` traps and checked remainder is None on all three executors, all signed widths

## 3. Float remainder on wasm (finding 7)

- [x] 3.1 Implement synthetic exact-fmod helper funcs (f32/f64, musl-style bit algorithm) emitted into the wasm module
- [x] 3.2 Route both float Remainder emit sites through the helper; delete the div-trunc-mul-sub expansion
- [x] 3.3 Parity sweep test vs native/bootstrap: extreme exponents, subnormals, NaN/inf propagation, sign of zero, both widths, bit-exact

## 4. Bootstrap rotate counts (finding 8)

- [x] 4.1 Euclidean-mod the rotate count in BootstrapArithmetic
- [x] 4.2 Parity test: rotate by negative and >width counts matches wasm/native

## 5. Effect-block typing (finding 2)

- [x] 5.1 collectTerminals: descend into UnsafeStatement (mirror returnFlowOf)
- [x] 5.2 Success type: join all returns via Match.join (canonical rule); joinable types form the union, non-representable joins emit new SEM0163 at the first disagreeing return
- [x] 5.3 Failure filter: accept value-kind type parameters via the same predicate StatementAnalysis uses for fail statements
- [x] 5.4 Tests: unsafe-nested fail/return typed correctly; mismatched branch returns diagnosed; generic failure row survives specialization and must be handled at run

## 6. Effect-block captures (finding 3)

- [x] 6.1 effectCaptureFacts: add EnumValue arm visiting the argument; add exhaustiveness guard (`satisfies never`) on the switch
- [x] 6.2 Test: Enum.value(c) inside an effect block captures c and the runner reads it

## 7. Ownership loan ends (finding 4)

- [x] 7.1 scanRunEnds: add PlaceReplace, EffectResult, EffectBindRequirement arms recursing into operand facts; exhaustiveness guard if the union permits
- [x] 7.2 Test: view loan extends through a use nested in Intrinsic.replace (OWN0011). Note: the callable-capture variant and the EffectResult/EffectBindRequirement arms have no dedicated test yet — flagged in review

## 8. Native EffectComposite lanes (finding 5)

- [x] 8.1 NativeType.lanesFor: resolve registered Layout.callingShape for EffectComposite first (WasmLanes.laneKindsOf pattern), fallback to current computation
- [x] 8.2 Audit/align NativeEffectOperation Pack/RunEffectComposite placement and coercion with the unified payload lanes
- [x] 8.3 Native test: EffectComposite with different-arity alternative captures round-trips correctly; full native acceptance corpus passes

## 9. Coroutine frame keying (finding 9)

- [x] 9.1 CoroutineFrame.stateLayout: match entries by the full suspension key including contractRow via the existing key canonicalization
- [x] 9.2 Native test: added contract-row-suspension-frames corpus program (two provider-bound suspendable specializations). Note: runner naming ($provided$N suffix) already disambiguates same-name frames today, so a failing pre-fix repro is not constructible; the fix is full-key hardening

## 10. Environment-lane single source of truth (finding 10)

- [x] 10.1 MirVerification.effectFieldLaneCount → Layout.effectFieldLanes(...).length
- [x] 10.2 WasmBackend hookReleaseInstructions environmentOffsets → consume Layout lane/placement helpers (add minimal Layout accessor if per-field offset+hook info isn't exposed)
- [ ] 10.3 Test: environment with borrow + callableIdentity + effectIdentity fields releases exactly the layout-enumerated hooks (not written; behavior-preservation covered indirectly by DropHookExecution + StoredEffectEngineParity suites)

## 10b. Discovered during apply

- [x] 10b.1 EntryAssembly.lowerEffectRunner: pass layout to mirType so captured scalar enums keep their Enum representation (generated runners with enum captures previously failed to lower)
- [x] 10b.2 MirLinearization.opensRuntimeContinuation: add missing RunEffectComposite so post-composite locals get stack storage (fixes native "non-phi forward value reference" on composite runs consumed across blocks)

## 11. Verification

- [x] 11.1 Full suite via `node scripts/turbo.mjs run test`; fix fallout (corpus programs newly diagnosed by 5.2 are fixed, not suppressed)
- [x] 11.2 Re-report the 10 findings via ReportFindings with outcomes
