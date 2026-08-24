# OpenSpec audit o002: establish-silk-test-inventory

SLP: `proposals/0004-silk-native-testing/proposal.md`
SLP revision: 36
SLP digest: `0a39823f15178c075870f85c54ee86c2a8be5cd873c3d4139696500c07331808`
OpenSpec change: `establish-silk-test-inventory`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `cef97c66dca8a4822d16808d0c827486f612c60b5fa247f48cdf6eb25f768e94`
- `proposal.md`: `b5626a0ffe8ae2501503aeabe8af6292ad82fb6ca1ad52bfba365008367310ca`
- `design.md`: `55f25a43edc52741db3fb8a5fb3bf674f5982669560480c62862f4215beb271a`
- `specs/bootstrap-declaration-index/spec.md`: `b636a8e93224fa60d01cc41a4a44395d70347d7ea7b750cb58838f922f13f0be`
- `specs/bootstrap-diagnostics/spec.md`: `d72de370d604466c2ac6de4d58d4d35c39296b4a4f94c4d1a3dfbb4555cb65c5`
- `specs/bootstrap-evaluation/spec.md`: `985beeb7c3a5986b2b6cb6b6a3cb3cf80a31de685841693dd2e48ee307ce4a64`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `5f2b48ec212848a4be54ae03c7f607cd861812b8cd1eef113e92023a10c0a468`
- `specs/bootstrap-intrinsic-target-availability/spec.md`: `5373555e3398ff8eea5683fb7c88516ada717b0fe84bca4c724093858ee60de5`
- `specs/bootstrap-module-closure/spec.md`: `fed61b812078b5d94bea0e9e7f360bebcbe7f996a40609be2a59acd5580f0a01`
- `specs/bootstrap-syntax/spec.md`: `9acb8b40d224ca62f4edd40109b8866bb066edddbf8225cf23ce6a9e453964c8`
- `specs/bootstrap-test-inventory/spec.md`: `0962e4374c8d7edc288747336811305376e4e8efc3766ed9ff63d678bbb5b3b8`
- `tasks.md`: `c489d846ba05cee3d66f42402a60fea2fec3d36bccc7fc3aa674a3310dc901ec`

Date: 2026-08-23
Result: Ready

## Validation evidence

- `openspec status --change establish-silk-test-inventory --json` reported every required planning artifact complete.
- `openspec instructions apply --change establish-silk-test-inventory --json` reported state `ready`, sixteen tasks before the audit repair, and zero complete.
- Strict validation passed after fix pass 1: one valid change, zero issues.
- Three fresh reviewers independently audited revision-36 raw artifacts for SLP fidelity,
  normative completeness, and realization coverage. No SLP decision remains open.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Contextual marker with unchanged function semantics | Syntax, declaration-index, and private-closed-Effects requirements | Canonical marker fact and ordinary header analysis | 2.1–2.3 | Covered |
| Explicit test roots plus separate runner role | Module-closure and root-scoped inventory requirements | Role-aware ProjectRequest composition | 3.1–3.2 | Covered |
| Opaque Copy handles and canonical borrowed IDs | Handle requirement and exact ID scenarios | Inventory-local ordinal and borrowed metadata | 3.3, 5.2 | Covered |
| Closed invocation over heterogeneous failures | Evaluation and per-test Outcome requirements | Direct per-entry adapter | 4.1–4.2 | Covered |
| Complete owned logical StackPath and cleanup gate | Owned-path requirements and lifecycle scenarios | Existing snapshot transfer characterization | 1.1–1.3, 4.2–4.3 | Covered; explicit SLP revisit gate retained |
| Evaluator-only initial operations | Intrinsic availability requirement | Canonical availability catalog | 4.4 | Covered |
| Minimal privilege and pay-for-use | Intrinsic boundary and ordinary-build scenarios | No actor spelling; test-only rooting | 3.2, 5.1–5.3 | Covered |

## Completeness findings

### Missing normative behavior

Fix pass 1 added the canonical declaration-index marker fact, invalid-body compilation gate,
successful-path affine cleanup, checked invalid StackPath lookup, metadata-only availability, and
borrowed allocation-free ID access.

### Missing boundary or failure scenarios

Fix pass 1 specified that evaluator terminations other than normal return and unhandled typed
failure retain their existing classification outside Outcome; runtime traps remain fatal. It also
added marked-versus-unmarked semantic parity and invalid semantic/ownership body scenarios.

### Missing implementation or verification work

Tasks now verify canonical marker publication, unchanged ordinary function semantics, invalid-body
gating, successful and recovered cleanup, checked path absence, borrowed metadata cost, and the
complete evaluator termination boundary.

## Divergence findings

### OpenSpec contradictions or inventions

The initial design invented explicit suite behavior for evaluator exhaustion and all internal
blocked states. Fix pass 1 removed that policy and retained each existing evaluator classification
outside the two-case Outcome boundary.

### SLP decisions requiring reconsideration

None. The reviewer-labelled evaluator-termination fork was resolved as removable OpenSpec scope
drift rather than a new language decision.

## Compiler–standard library boundary

Closed. Compiler privilege is limited to the contextual marker fact, root-scoped inventory,
canonical borrowed metadata, opaque invocation, and owned logical-path inspection. No source actor
name, runner policy, Reporter identity, assertion behavior, filtering, or presentation enters a
compiler phase.

## Required revisions

None.

## Next state

Ready. `add-silk-test-standard-library` may depend on this frozen inventory contract.
