# OpenSpec audit o001: add-local-shared-standard-library

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `add-local-shared-standard-library`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `4bad7979764e8750f2484fc179a4f1ab04e9c0d4e898f096c3d2b10843e5d2ba`
- `design.md`: `5c89cd87fde34bebd445934bd586bb5169a059963cca58c916647b8481523fe8`
- `specs/bootstrap-silk-stdlib/spec.md`: `b285c49a802af5931b6385d1c082ceca7535fa289cf42a37d30aa8344a493581`
- `tasks.md`: `2b4eeadd88d01910fd1ec84a33ee35422b9808bc34908e8747f239073cb82583`

Canonical spec baseline:

- `openspec/specs/bootstrap-silk-stdlib/spec.md`: `26465e7b27e2b490393e393d2b40802a2d5dc72b23d49664ace0ee418dad35eb`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec status --change add-local-shared-standard-library --json` reported complete planning.
- `openspec instructions apply --change add-local-shared-standard-library --json` reported state
  `ready`, eleven tasks total, zero complete.
- Strict OpenSpec validation passed after one bounded fix pass: one valid change, zero issues.
- Three fresh reviewers read the accepted SLP, raw change, and canonical stdlib spec. The realization
  lens found no orphan scenario or task; fidelity and completeness gaps were repaired below.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Public `Shared<T>` is ordinary source over one opaque core | Canonical module; renamed wrapper | One private field, no source Drop hook | 1.1, 3.2, 3.3 | Covered |
| Construction is the only allocation boundary | Success and failure construction scenarios | `Allocator.allocate` before unsafe initialization | 1.2, 4.1 | Covered |
| Clone borrows receiver, allocates nothing, and traps before overflow mutation | Clone/access and overflow scenarios | Direct wrapper over `sharedClone` | 1.3, 4.1 | Covered |
| Public access is callback-scoped and all-exclusive | Four-way conflict, escape, and suspension scenarios | `withMut` over intrinsic; `with` through `withMut` | 2.1–2.3, 4.1 | Covered |
| Affine `T` moves through ordinary state without becoming Copy | Affine move-in/move-out scenario | Ordinary callback operations over `&mut T` | 2.1 | Covered |
| Non-last drop preserves `T`; last drop cleans then releases once | Public exact-drop scenario | Recursively derived core cleanup | 1.1, 4.1 | Covered |
| Handles and containing values remain local | Recursive local-affinity scenario | Core field propagation, no transfer surface | 1.1, 4.1 | Covered |
| The canonical source is importable and navigable | `silk/shared` import scenario | Manifest and generated table | 3.1, 4.1–4.2 | Covered |

## Completeness findings

### Missing normative behavior

The fidelity/completeness lenses found clone overflow, exact public drop, successful payload
transfer, affine payload movement, and three of four reentrant combinations absent from normative
scenarios. The fix pass added exact public contracts and scenarios for each. Closed.

### Missing boundary or failure scenarios

Direct, recursive-container, executable, and suspension borrow escapes were only implied by the
prerequisite lifecycle slice. Public `with` and `withMut` now each carry explicit rejection scenarios
using the stable lifecycle diagnostic and both provenance spans. Closed.

The requested current thread-transfer rejection was not adopted because this SLP slice deliberately
adds no transfer syntax or diagnostic. The verified gap was recursive local affinity; its new
scenario publishes `LocalExecution` for the wrapper, aggregates, and captures without inventing a
future transfer operation. Closed.

### Missing implementation or verification work

No orphan work remains. Tasks now cover exact construction transfer, clone channels and overflow,
affine movement, every escape form, recursive affinity, last cleanup order, and canonical import.

## Divergence findings

### OpenSpec contradictions or inventions

The design incorrectly had `Shared.with` call the intrinsic independently. It now follows the
accepted SLP exactly: public `with` delegates through public `withMut` and narrows its borrow. Closed.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. `Shared`, allocation policy, conflict trap policy, imports, docs, and every wrapper remain
ordinary source. Only the explicit `Intrinsic.SharedCore` operations receive compiler behavior, and
renamed equivalent source receives the same general semantic and ownership behavior.

## Required revisions

Completed in one fix pass: exact clone/construction/drop contracts; all conflict, affine-movement,
escape, suspension, affinity, and import scenarios; corrected `with` layering; and matching task gates.

## Next state

Ready after the semantic, allocation, and lifecycle slices. Package-content changes require the
repository gates and `pnpm release:candidate` during implementation.
