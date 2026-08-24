# OpenSpec audit o001: establish-silk-test-inventory

SLP: `proposals/0004-silk-native-testing/proposal.md`
SLP revision: 35
SLP digest: `a23040d98a7c4028e759f000bc586ebbc539366dbca75a987886753eaf627205`
OpenSpec change: `establish-silk-test-inventory`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `cef97c66dca8a4822d16808d0c827486f612c60b5fa247f48cdf6eb25f768e94`
- `proposal.md`: `d8ca463fd085f578dcb14ece8daa9c8d7cd84deec7467934ffd93e400d992cd2`
- `design.md`: `4cbed603a88b85593c58c1154eb83602f8c8d75866ea1955c5c348c039e79cbb`
- `specs/bootstrap-diagnostics/spec.md`: `d72de370d604466c2ac6de4d58d4d35c39296b4a4f94c4d1a3dfbb4555cb65c5`
- `specs/bootstrap-evaluation/spec.md`: `f5ac7bec92ba03e6ef1b5b7c0a26514ba5fe0fd40e03c4d19ebfdaf462fe61c9`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `4daaeeb94df63397fd818a054438b6d3fcdcda3e7143d19177b17eccdd4b5965`
- `specs/bootstrap-intrinsic-target-availability/spec.md`: `428412d0469b68e20ffd0fd0e68b9244d6a1623482b3730c2113cb05bdc7811e`
- `specs/bootstrap-module-closure/spec.md`: `fed61b812078b5d94bea0e9e7f360bebcbe7f996a40609be2a59acd5580f0a01`
- `specs/bootstrap-syntax/spec.md`: `9acb8b40d224ca62f4edd40109b8866bb066edddbf8225cf23ce6a9e453964c8`
- `specs/bootstrap-test-inventory/spec.md`: `2bc4bef019c15dc367d077fe83b365ac01a43df62eef62450ce4500e2d290af2`
- `tasks.md`: `2df1625b2ba9694b7dd2477c18bd7f7aa414a974e923b3afd40128eadecbdb83`

Canonical spec baseline:

- `openspec/specs/bootstrap-syntax/spec.md`: `1478d3027e2b7d6154f6cca1ff4761afd5ff4e7e440d570dc3671e9df11a6803`
- `openspec/specs/bootstrap-module-closure/spec.md`: `1e76d1e0570cdc97f162a6086ded6c451ea7b27cfbdc822678da722b328de2c3`
- `openspec/specs/bootstrap-evaluation/spec.md`: `35c61d8e9b53b91e64eec8d3cc428db47aab8ca4a8bae2a9b2ae72f74bff1632`
- `openspec/specs/bootstrap-diagnostics/spec.md`: `7b338bc3753dc1ce3fd3a7dd34f71a3066bb13c2390020928aa23c0780ddc8ef`
- `openspec/specs/bootstrap-intrinsic-boundary/spec.md`: `201a6ae4f28b556bbec4fa098d678a9d2b1ca7fd023bab45204bc9e860d75224`
- `openspec/specs/bootstrap-intrinsic-target-availability/spec.md`: `a584031459a0772e7a049733356b85f3b7a401ddaab7d82cf44ff8c0a1e5ed57`

Date: 2026-08-23
Result: Ready

## Validation evidence

- `openspec status --change establish-silk-test-inventory --json` reported complete planning.
- `openspec instructions apply --change establish-silk-test-inventory --json` reported state
  `ready`, sixteen tasks total, zero complete.
- Strict OpenSpec validation passed after bounded fixes: one valid change, zero issues.
- Three fresh reviewers read the accepted SLP, raw change, and canonical touched specs through
  fidelity, normative-completeness, and realization-coverage lenses.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| `test` marks only private closed top-level Effects | Test eligibility; public/ordinary exact syntax; invalid diagnostics | Canonical header marker and post-normalization validation | 2.1–2.3 | Covered |
| Explicit test roots and separately designated runner role | Root-scoped inventory; modified closure request; overlapping roles | One-root loads composed by role-aware ProjectRequest | 3.1–3.2 | Covered |
| Canonical ordered opaque Copy handles and IDs | Inventory order; exact ID; forged/callable refusal | Inventory-local ordinal token and borrowed metadata | 3.2–3.3 | Covered |
| Uniform invocation without general erased pointers | Closed Outcome and heterogeneous failure scenarios | Direct per-entry adapters | 4.1–4.2 | Covered |
| Complete existing logical path with sound cleanup | Path boundary, affine payload, snapshot lifecycle, rejection gate | Transfer existing outer-to-inner snapshot after cleanup | 1.1–1.3, 4.2–4.3 | Covered |
| Traps remain fatal and Reporter is outside eligibility | Invocation trap/recovery scenarios | Typed-failure-only interception | 4.2 | Covered |
| Normal builds pay no test rooting/code cost | Ordinary-build exclusion and unreachable availability | Test-only inventory materialization and pay-for-use catalog | 3.2, 4.1, 4.4 | Covered |
| Initial execution is evaluator-only but identities remain target-neutral | Evaluator-only intrinsic availability scenarios | Existing supported-engine catalog, pre-lowering rejection | 4.4 | Covered |
| No public actor spelling has privilege | Minimal intrinsic and same-named actor scenarios | Sealed operations only | 5.1–5.2 | Covered |

## Completeness findings

### Missing normative behavior

Initial review found the evaluator frame order reversed, exact test-ID and invalid-marker syntax
oracles absent, causal-unavailable suppression implicit, ordinary-build adapter exclusion missing,
and evaluator-only availability left unspecified. Bounded fixes added exact outer-to-inner order,
metadata and syntax cases, normal-build exclusion, and a deterministic evaluator-only supported
set with native/direct-Wasm pre-lowering rejection. Closed.

### Missing boundary or failure scenarios

Initial review found only frame-owner cleanup covered. The final evidence gate also requires the
erased affine failure payload and owned path snapshot to clean exactly once across drop, move,
downstream failure, and repeated invocation. Partial invalid root/import closure evidence is also
retained without a runnable inventory. Closed.

### Missing implementation or verification work

Tasks now cover exact path order and lifecycle, ordinary and role-aware root requests, partial
closure causes, same-named behavioral fixtures, deterministic catalog availability, and test-only
adapter rooting. No orphan scenario or task remains.

## Divergence findings

### OpenSpec contradictions or inventions

The initial added multi-root closure requirement contradicted the canonical one-root requirement;
it now modifies that requirement by preserving ordinary CompilationRequest semantics and composing
them through ProjectRequest. A transient cross-engine invocation obligation exceeded the SLP and
was removed; compiled test modes remain deferred. Closed.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Compiler privilege is limited to the marker, root-scoped inventory, canonical metadata,
opaque invocation, and immutable logical-path inspection. Eligibility, reporting, assertions,
filtering, presentation, and runner policy remain ordinary source concerns. Testing operations are
pay-for-use and evaluator-only in this slice.

## Required revisions

Completed in bounded fixes: path order and full cleanup lifecycle; exact ID and invalid syntax
oracles; canonical one-root/project-union reconciliation; ordinary-build exclusion; runner/test role
overlap; behavioral privilege fixtures; and explicit evaluator-only intrinsic availability.

## Next state

Ready as the prerequisite for `add-silk-test-standard-library`, subject to the later SLP-level
standard-library naming decision recorded in that change's audit.
