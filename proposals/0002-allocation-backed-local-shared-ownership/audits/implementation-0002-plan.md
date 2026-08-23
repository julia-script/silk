# SLP-0002 implementation plan and outcome

- SLP: `0002-allocation-backed-local-shared-ownership`
- Accepted-direction revision: 6
- Accepted-direction digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
- Integration branch: `julia/slp-0002-implement`
- Audited handoff baseline: `987ce26`
- Run dates: 2026-08-22 through 2026-08-23
- Outcome: Complete; all six DAG layers implemented and integrated

## Dependency plan and final state

The handoff DAG had width one, so each change started only after its dependency crossed the root
integration barrier.

| Layer | OpenSpec change | Dependency | State | Attempts | Stop reason | Unresolved C/H findings | Commit or range |
| --- | --- | --- | --- | --- | --- | ---: | --- |
| 1 | `establish-local-shared-ownership` | none | Done | 3 hard-gate attempts; 1 conformance fix pass | none | 0 | `987ce26..07a5977`; merge `3d5d745` |
| 2 | `add-local-shared-control-block-allocation` | layer 1 | Done after bounded resume | 3 attempts in the parked run; 0 resumed gate fixes; 1 conformance fix pass | none | 0 | `3d5d745..dcc3b3d`; merge `9d3d788` |
| 3 | `add-local-shared-lifecycle-operations` | layers 1-2 | Done | 0 gate-fix attempts; 1 conformance fix pass | none | 0 | `9d3d788..e41e3bf`; merge `3cb428d` |
| 4 | `add-local-shared-standard-library` | layers 2-3 | Done | 0 gate-fix attempts; no C/H fix required | none | 0 | `3cb428d..f18a984`; merge `7cb2dc3` |
| 5 | `add-local-shared-engine-parity` | layers 1-4 | Done | 0 gate-fix attempts; 1 conformance fix pass | none | 0 | `7cb2dc3..1b1f4c1`; merge `4da4475` |
| 6 | `prove-local-shared-slp1-sufficiency` | layers 4-5 | Done | 0 pre-conformance gate fixes; 1 conformance fix pass; 1 test-only rerun repair | none | 0 | `4da4475..de2a915`; merge `8462754` |

Every implementation worktree was isolated from the integration branch. Done changes were merged in
DAG order, and each merge crossed the full root barrier before its dependent worktree was created.

## Handoff audit

All six OpenSpec changes passed strict validation and had `Result: Ready` audit records before
implementation. Audit repairs were committed at `987ce26` before the first implementation worktree
was created.

## Layer outcomes

### Layer 1: ownership semantics

OpenSpec state: 14/14 tasks complete.

Three hard-gate attempts were used within the cap: the first repaired polymorphically recursive
affinity traversal, the second repaired release-candidate export-manifest evidence, and the third
passed completely. The single three-lens pass verified five High findings and eight Medium
findings. The one permitted
High fix pass repaired sealed `SharedCore` provenance, absence of ordinary construction/layout
privilege, retained borrow-root affinity, concrete callable/Effect environment facts, logical
zero-lane frame obligations, and ownership fabrication through slots and raw buffers. No Critical
or High finding remained. The integrated root barrier passed all required commands.

### Layer 2: control-block allocation

OpenSpec state: 8/8 tasks complete.

The first bounded run parked after its third distinct gate failure, when generated diagnostics were
stale. The later explicit resume regenerated the prescribed evidence, completed the ordered gates,
and ran the required fresh conformance lenses. The consolidated fix preserved exact allocation
provenance through semantic facts, HIR, MIR, ordinary providers, and backend initialization, closing
the verified cross-provider authorization gap. Commits `fd6b981`, `ab9dc8a`, and `dcc3b3d` preserve
the implementation, completed evidence, and conformance repair respectively. The resumed worktree
and root integration barriers passed in full.

### Layer 3: lifecycle operations

OpenSpec state: 12/12 tasks complete.

The implementation added sealed clone and access operations, checked bigint reference-count
transitions, stable ownership diagnostics including `OWN0016`, opaque recursive cleanup plans,
evaluator/native/direct-Wasm lifecycle behavior, and differential corpus coverage. The single
conformance repair closed all verified Critical/High lifecycle and cleanup gaps. Worktree and root
barriers passed in full.

### Layer 4: standard library

OpenSpec state: 11/11 tasks complete.

The canonical ordinary-Silk `silk/shared` actor now provides `make`, `clone`, `withMut`, and `with`,
with generated manifest/documentation coverage and the public behavior matrix. Generic compiler
defects exposed by the wrappers were repaired without recognizing the module or actor spelling.
The three-lens pass left no Critical/High repair, and all worktree and root barriers passed.

### Layer 5: engine parity

OpenSpec state: 12/12 tasks complete.

Initial and post-conformance gates both passed on their first attempt. The fresh three-lens pass
verified and the single consolidated pass repaired self-attested initialization provenance and
consumption, retained-loan metadata, opaque-core `NoCleanup` acceptance, non-falsifiable cycle
retention, missing clone/access cost probes, and duplicate-loan provenance. The final evidence
includes deterministic MIR encoding/golden coverage, structural native/Wasm ordering and forbidden
runtime-helper checks, cycle witnesses, expanded differential cases, and a 10,000-operation bounded
Wasm heap probe. No Critical/High finding remained, and the root barrier passed.

### Layer 6: SLP-0001 pressure sufficiency

OpenSpec state: 10/10 tasks complete.

The ordinary-Silk pressure slice contains canonical and fully renamed fixtures, a fixed-capacity
ready inbox, shared Deferred-style state, real once-callable storage/extraction/invocation after
access restoration, shared-borrow payload observation, coexisting dormant Effects, affine cleanup
fingerprints, deterministic quota recovery, native corpus cases, and a classified findings report
with an acceptance-evidence matrix.

The single three-lens pass found four deduplicated High proof/correctness categories: record dispatch
instead of real callable extraction, backend-invisible lifecycle/failure evidence, masked quota
recovery, and stale Wasm callable lanes after Drop hooks. The consolidated pass repaired all four
plus directly related Medium evidence gaps: callable cleanup type identity, simultaneous dormant
Effects, normalized rename/MIR evidence, findings citations, and duplicate snapshot work. Focused
regressions also repaired generic Wasm cleanup of borrowed Effect payloads without inventing an owner
frame root. One final-rerun typecheck failure was limited to two test typings; after that one repair,
the ordered rerun passed completely. No Critical/High finding remained.

## Final verification

Every change is strict-valid and complete: 14/14, 8/8, 12/12, 11/11, 12/12, and 10/10 tasks.
The layer-six integrated barrier at merge `8462754` passed. After current `origin/main` introduced
the seeded-random standard-library slice, the generated-integrity conflict was resolved by
regenerating the digest from both merged source sets. Merge `ae863ee` then passed the full barrier,
in repository order:

1. `pnpm typecheck`
2. `pnpm exec biome check .`
3. `pnpm test`
4. `pnpm check`
5. `pnpm release:candidate`

The synchronized run passed 216 compiler files and 2,077 compiler tests, native differential
acceptance, all 28 repository test tasks, and release-candidate validation 9/9.

The completed implementation is ready for the separately invoked `$slp-6-audit-implementation`
step. Archival remains out of scope until that audit passes.
