## Context

The first four changes define verified semantic and MIR obligations for local shared ownership. The
evaluator is the logical oracle; native LLVM and direct Wasm consume the same target-aware plan.
Existing allocation and cleanup infrastructure already distinguishes logical ownership parity from
physical heap policy. See `proposal.md` and both delta specs.

## Goals / Non-Goals

**Goals:**

- Execute one observable transition system on all three engines.
- Allow private target representation without observable divergence.
- Prove exact cleanup, conflict, and overflow ordering at the cheapest falsifying test tier.

**Non-Goals:**

- Standardize byte offsets, allocator headers, pointer identity, or post-trap cleanup.
- Add atomics, locking, scheduler behavior, or a collector.

## Decisions

### Carry explicit target-neutral local-shared MIR operations

Verified MIR identifies layout planning, initialization, clone, callback access, and opaque-core
drop with concrete `T`, target layout provenance, callback shapes, and source provenance. The
verifier rejects mismatched layout identities, unconsumed initialization inputs, escaping access
results, malformed callback modes, and any operation whose core type is unavailable. MIR does not
publish field offsets, raw addresses, actor names, or conflict policy types.

Alternatives rejected: lowering source wrappers directly to allocation loads would duplicate
ownership logic per backend; a runtime library recognized by `Shared` spelling would violate the
sealed boundary; one universal byte ABI would unnecessarily freeze target representation.

### Model logical state explicitly in evaluation

The evaluator stores a stable logical block identity, target layout identity, strong count, access
state, initialized `T`, and active reclaim authority without relying on JavaScript object identity or
garbage collection. Its bounded trace records acquisition/initialization, clone transition, access
acquire/conflict/release, non-last drop, value cleanup, and allocation release in deterministic
order. Conflict records no state transition. Strong cycles remain retained logical blocks after
external drops.

### Use target-local non-atomic storage in both backends

Native and Wasm realize the planned header using target-sized count storage, one local access state,
the private reclaim representation, and aligned payload storage. Loads and stores are non-atomic
because semantic affinity prevents cross-thread transfer. Clone emits compare/trap before increment;
access emits select-before-callback and restore-after-normal-return; last drop calls canonical `T`
cleanup before allocation release. Wasm returns storage through its existing reclaim path; native
uses its existing self-contained allocation authority.

### Verify claims at the cheapest tier that can falsify them

Semantic rejection and transition ordering use analysis/evaluator tests. Native behavior is added to
the designated differential acceptance corpus rather than a per-feature compile test. Wasm is used
where codegen and physical release matter. Overflow uses a private transition unit with a reduced
maximum plus structural backend assertions that the compare and trap dominate the count store;
tests never attempt billions of public clones. One source snapshot is shared per program.

The parity corpus covers sequential access, the four conflict combinations, clone and non-last drop
during access, two-frame typed failure, affine `T` moved in and out under access, last cleanup order,
allocation exhaustion, and strong-cycle non-release.

## Risks / Trade-offs

- **Risk: target representations diverge semantically** → compare logical results and ordered
  ownership events across all engines while separately testing Wasm physical reclamation.
- **Risk: callback control flow restores access too early or twice** → make normal restoration a
  verified continuation edge and pin nested conflict after callback entry and later success after return.
- **Risk: traces grow without bound** → use existing bounded deterministic trace policy and compact
  block identities rather than payload dumps.
