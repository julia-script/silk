# OpenSpec audit o001: add-local-shared-control-block-allocation

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `add-local-shared-control-block-allocation`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `0727d8e676b299e17ed889628070a6e8afcd1fe7d25372a65c93b81f2a596a06`
- `design.md`: `43920bb665ca32584bc219a97d17ea7282cb9460b07fe4b589238bdec17f292e`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `bd116266438d2725592ad2115a7af54847997e30dd024147cb0276e49293f338`
- `specs/bootstrap-owned-allocation/spec.md`: `2c62571fbcf8c4a5c2a3275d384ba2d9e72d8959f579c2ca37500cce4535482f`
- `tasks.md`: `711509b26782f51d522f1aa37d834019a8b6f11e05e704e948ee876fe4ae32fe`

Canonical spec baselines:

- `openspec/specs/bootstrap-intrinsic-boundary/spec.md`: `201a6ae4f28b556bbec4fa098d678a9d2b1ca7fd023bab45204bc9e860d75224`
- `openspec/specs/bootstrap-owned-allocation/spec.md`: `00bfee8a21aa1008b0a346cc72a25a547a170e4ac6800c572fdf4848ccbfc881`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec status --change add-local-shared-control-block-allocation --json` reported all four
  planning artifacts complete.
- `openspec instructions apply --change add-local-shared-control-block-allocation --json` reported
  state `ready`, eight tasks total, zero complete.
- `openspec validate add-local-shared-control-block-allocation --strict --json --no-interactive`
  passed after one bounded OpenSpec fix pass: one valid change, zero issues.
- Three fresh reviewers read the accepted SLP, raw change artifacts, and both canonical specs through
  the SLP-fidelity, normative-completeness, and realization-coverage lenses.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Construction exposes allocation rather than hiding it | Exact caller-funded allocation; ordinary exhaustion cleanup | Allocation occurs before the unsafe initializer | 3.1 exhaustion fixture | Covered |
| The compiler exposes only layout and from-allocation construction | Two sealed construction primitives; construction inventory | One opaque target layout and one consuming initialization transition | 1.1 catalog, availability, and unsafe admission | Covered |
| The exact layout is target-aware while physical lanes remain private | Exact requested layout; zero-sized element; unrepresentable layout | Canonical provenance identity and compile-time target-layout rejection | 2.1 layout, overflow, ZST, and no-effect fixture | Covered |
| Initialization consumes one allocation and one `T` without partial state | Initialize exact layout; reject mismatch | Atomic count-one/available transition retaining reclaim authority | 1.2, 2.2, 2.3 positive and negative MIR/ownership tests | Covered |
| Allocation failure creates no core and leaves cleanup with ordinary source | Preserve ordinary exhaustion cleanup | Initializer is unreachable on allocator failure | 3.1 failure cleanup fixture | Covered |
| The resulting core outlives allocator access through self-contained reclaim authority | End allocator access before shared lifetime | Reclaim authority moves into the opaque block | 2.3 provider-loan and source-binding facts | Covered |
| Public actors and policy receive no name-based privilege | Keep policy actors ordinary; same-spelled operation scenario | No compiler-known allocator, error, Shared, or witness actor | 3.2 branch audit and renamed-operation fixtures | Covered |

## Completeness findings

### Missing normative behavior

The fidelity and completeness lenses independently found that control-block layout overflow had no
normative outcome. The accepted SLP delegates this realization question while retaining the total
`sharedLayout<T>() -> Layout` signature. The fix pass now makes an unrepresentable concrete block an
unavailable target-layout specialization with stable call-site diagnostic before MIR; it is neither
runtime `LayoutOverflow`, `OutOfMemoryError`, nor a trap. Closed.

### Missing boundary or failure scenarios

The realization lens found no direct safe-syntax rejection for unsafe initialization and no
same-spelling source test for the two new operations. The fix pass added both normative scenarios and
task coverage. Closed.

The completeness lens noted that mismatch diagnostics and target availability were implicit. The fix
pass now requires stable initializer/provenance diagnostics and normalized evaluator, supported-native,
and direct-Wasm availability. Closed.

### Missing implementation or verification work

The realization lens found that successful initialization and allocation-free layout planning were
only indirectly tested. Tasks now require the complete positive count-one/available ownership
transition and a source/evaluator no-allocation/no-cleanup fixture. Closed.

## Divergence findings

### OpenSpec contradictions or inventions

None remain. Compile-time target-layout unavailability follows the canonical layout phase's existing
handling of unrepresentable concrete layouts while preserving the SLP's total source signature and
ordinary allocation boundary.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. The compiler plans one opaque target layout and verifies one unsafe consuming transition.
Allocator choice, `OutOfMemoryError`, construction sequencing, public wrappers, and policy remain
ordinary Silk. Same-spelled ordinary declarations receive no catalog identity or lowering behavior.

## Required revisions

Completed in one fix pass:

1. define unrepresentable control-block layout as compile-time target-layout unavailability;
2. pin target availability and diagnostic provenance;
3. add unsafe-call and same-spelling negative scenarios; and
4. add positive initialization and allocation-free behavior verification.

## Next state

Ready. Implement after `establish-local-shared-ownership`; implementation must complete every task
and pass the SLP-5 hard gates before the lifecycle slice starts.
