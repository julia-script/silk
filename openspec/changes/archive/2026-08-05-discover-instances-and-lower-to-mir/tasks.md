## 1. Instance discovery

- [x] 1.1 Create `packages/compiler/src/Instances.ts`: entry resolution (unique zero-parameter
      `I32` root `main`) as explicit `Resolved`/`Unavailable` states; canonical instance keys with
      empty type/contract-row arguments
- [x] 1.2 Record-before-follow worklist over resolved HIR calls, deterministic discovery order,
      unreachable declarations excluded
- [x] 1.3 Tests: call-chain discovery, direct and mutual recursion termination, unreachable
      exclusion, unavailable entries, determinism

## 2. Lowering

- [x] 2.1 Create `packages/compiler/src/Lower.ts`: lower discovered instances to one MIR program
      module in discovery order — evaluation-order linearization, parameters pre-bound, cleanup
      insertion directed by the ownership plan, unavailable bodies as generated traps
- [x] 2.2 Lowered programs verify clean; committed lowered goldens with byte-identical and
      repeat-determinism tests

## 3. Facade

- [x] 3.1 Compute discovery and the lowered program in `Analysis.make`; add `instancesOf` and
      `loweredMir` queries; facade tests

## 4. Package surface

- [x] 4.1 Export `Instances` and `Lower` from the index and exports map; update release-candidate
      surface

## 5. Inspector

- [x] 5.1 Create the direct-link `/docs/labs/instances` lab: entry state, discovery-ordered
      instances with canonical keys; tests
- [x] 5.2 CFG lab program mode: edit source, render lowered blocks/edges, provenance hover with
      exact source slice; tests

## 6. Verification

- [x] 6.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 6.2 `openspec validate discover-instances-and-lower-to-mir --type change --strict` passes
