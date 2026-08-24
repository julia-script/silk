# OpenSpec audit o001: prove-silk-native-testing-sufficiency

SLP: `proposals/0004-silk-native-testing/proposal.md`
SLP revision: 36
SLP digest: `0a39823f15178c075870f85c54ee86c2a8be5cd873c3d4139696500c07331808`
OpenSpec change: `prove-silk-native-testing-sufficiency`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `cef97c66dca8a4822d16808d0c827486f612c60b5fa247f48cdf6eb25f768e94`
- `proposal.md`: `84fef76a179675f694d38ff0ba21a706821406d4a4212b4cb44f07f7a2021f76`
- `design.md`: `db81d89e5b7599c77c9de5e23894877b36a2335e84b8fc5b42e9229f748afd25`
- `specs/bootstrap-language-pressure-programs/spec.md`: `a611f42fd948fb67fa9e641bce3910ab9b69446c4ece70b0f40da287d385c4a6`
- `tasks.md`: `d46463de7d387586a5a73f986e527ee884f66e19d0becc64f637869168bb6094`

Date: 2026-08-23
Result: Ready

## Validation evidence

- `openspec instructions apply --change prove-silk-native-testing-sufficiency --json` reported
  state `ready`, thirteen tasks, and zero complete.
- Strict validation passed after fix pass 1: one valid change, zero issues.
- Three fresh reviewers independently audited revision-36 raw artifacts and every frozen
  prerequisite for SLP fidelity, normative completeness, and realization coverage.
- No SLP decision remains open.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Current stdlib behavior is directly testable in Silk | Dual-placement Random requirement | Seeded scalar and fillBytes vector | 1.1–1.3 | Covered |
| Reporter is replaceable ordinary policy | Custom Reporter and role-composition scenarios | Counting and no-report runners | 2.1–2.3 | Covered |
| Basic filters preserve exact byte rules | Filter evidence scenarios | CLI plus post-parser injection | 2.4 | Covered |
| Status, cleanup, and traps retain distinct semantics | Connected termination scenarios | Compact evaluator witnesses | 2.5 | Covered |
| Complete path differs from presentation | Nested-helper scenario | Standard/custom comparison | 2.6 | Covered |
| Compiler privilege is minimal and actor-agnostic | Privilege and non-passing requirements | Final artifact/falsifier report | 3.1–3.3 | Covered |
| Initial execution remains evaluator-only | Narrow-boundary scenario | Cheapest-tier evidence reuse | 4.1 | Covered |

## Completeness findings

### Missing normative behavior

Fix pass 1 made both user and standard-library placements execute both Random witnesses, qualified
exactly-once execution around early termination, required a connected runner/test-root-role
fixture, added reporter-derived custom status forwarding, and made the final findings gate
non-passing for any unproven falsifier or failed prerequisite.

### Missing boundary or failure scenarios

Fix pass 1 added exact ReportError cleanup and stopping, source rejection before execution, fatal
trap behavior, a non-ASCII positive/near-miss pair, invalid bytes ORed with an ASCII match, and a
failure scenario for an incomplete sufficiency report.

### Missing implementation or verification work

Tasks now include backend artifacts, both logical-path capture and checked inspection, the complete
deferred feature list, an exact reporter-derived status, and explicit reuse of frozen prerequisite
evidence to avoid duplicate evaluator/CLI work without a distinct falsifier.

## Divergence findings

### OpenSpec contradictions or inventions

The initial unconditional exactly-once sentence contradicted accepted mid-suite ReportError and
fatal-trap early termination. The final privilege allowlist also omitted checked path inspection,
which the frozen primitive explicitly requires. Both contradictions are removed.

### SLP decisions requiring reconsideration

None. Owned-path and admitted-platform-byte failures use already accepted revisit gates; this
change does not invent a new resolution path.

## Compiler–standard library boundary

Closed. The final audit permits privileged branches only for marker, inventory, metadata, opaque
invocation, owned path capture, and checked path inspection. Syntax through backend and command
artifacts must show no ordinary Test, Reporter, assertion, equality, filter, presentation, or
runner spelling privilege.

## Required revisions

None.

## Next state

Ready. All four SLP-0004 realization slices are ready for DAG-ordered implementation after the
documented SLP-0001 dependency is available.
