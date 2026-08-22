## Context

See proposal.md. `MirVerification.verify` and `MirEncoding.encode` are the final actor surfaces; `Mir` retains the IR vocabulary without forwarding those operations. The ProvisionalMir -> Mir staging is deliberate (per-monomorphic control that backends must never consume); only its shared vocabulary needs a single owner.

## Decisions

- **MirVerification.ts**: verify (3630–6208) plus its private per-operation validators; RangeError guards stay as internal invariant defects. **MirEncoding.ts**: encode + the *Text/*Lines helpers (6213–6514). **Suspension.ts**: the 1043–1262 data block.
- **Shared suspension vocabulary**: move Classification/Runner/Completion/Provider to one owner (a Suspension.ts data module, or import Mir.SuspensionClassification into ProvisionalMir); SuspensionMir field mappers consume it instead of re-typing. Fix SuspensionMir.operationArguments to plain operation.arguments.
- **BootstrapArithmetic.ts**: integralBinary(op, scalar, pointerBits, left, right), compare, and checkedOp, used by both the callable path (invokeCallableTarget) and the MIR Binary/checked arms.
- **BootstrapPlace.ts**: walkPlace(root, selectors, indexes) returning selected + traceSelectors, plus replacePlaceByIndexes; resolvePlace, ReadPlace, WritePlace, referenced, and selectStoredPlace all delegate.

## Risks / Trade-offs

- [Evaluator fidelity] -> the corpus differential suite (evaluator vs wasm/native) is the gate; extract one block at a time and run it after each.
- [Large code motion] -> one actor per commit.

## Validation

pnpm typecheck, pnpm exec biome check ., pnpm test (bootstrap-evaluation + bootstrap-mir differential suites).
