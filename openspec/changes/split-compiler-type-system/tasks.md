## 1. Type inference extraction

- [x] 1.1 Extract `TypeInference.ts` from `Type.ts:2806-3367` and re-export `infer`/`inferOpenGenericArguments`
- [x] 1.2 Verify generic-inference and type tests pass

## 2. Instances split

- [x] 2.1 Extract `InstanceDiagnostics.ts` (the five violation checks)
- [x] 2.2 Extract `ExecutableOrigin.ts` (origin, callables, effects, suspension, reachability)
- [x] 2.3 Verify instance/discovery and conformance tests pass

## 3. Pipeline split and realize/prepare dedup

- [x] 3.1 Extract `discoverAndLower` and make `realize`/`prepare` thin mappers over it
- [x] 3.2 Split `Pipeline.ts` into `Frontend`, incremental-reuse, and `Realization` modules
- [x] 3.3 Verify the full driver/analysis pipeline tests pass, including the gate semantics

## 4. Verification

- [x] 4.1 Run `pnpm typecheck` and verify clean
- [x] 4.2 Run `pnpm exec biome check .` and verify clean
- [x] 4.3 Run `pnpm test`

## 5. Convergence findings

- [x] 5.1 Extract `IncrementalReuse.ts` with `ProjectReuseBasis`, invalidation, structural sharing, and checkpointing; keep `Frontend` focused on phase orchestration
- [x] 5.2 Verify incremental reuse and full frontend/project gate behavior
