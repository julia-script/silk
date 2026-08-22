# OpenSpec audit o005: establish-local-shared-ownership

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `establish-local-shared-ownership`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `d159dc49a67e1a01316723a030c57c0a52c6ae76467f6fc545e2b876a5812ec1`
- `design.md`: `11794cd589b6332f337144cd7f53d58b3f4acc25c76e6b61bb23d06c022c3f73`
- `specs/bootstrap-semantic-facts/spec.md`: `d1bdff4999867969d4d526709a21ff9b6f1212d0dcf993c9d2db9f4371756d20`
- `specs/bootstrap-ownership/spec.md`: `332d02debccf6715d5cb71b69c51d21ecbe7c8359c800e1f6e330fe5bffd771c`
- `tasks.md`: `c5f306d1dee589c6b37cfbc977451691cd47ce19969e6535727a42896d9a39e7`

Canonical spec baselines:

- `openspec/specs/bootstrap-semantic-facts/spec.md`: `2d6edde8271b03c17d104f1545371ce02c80a06b27116f67b953a7eab503685b`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec validate establish-local-shared-ownership --strict --json --no-interactive`: passed,
  one valid change, zero issues.
- SLP revision and digest match the change proposal.
- Three fresh o005 reviewers read the raw accepted SLP, complete change, and both canonical specs.
  SLP fidelity, normative completeness, and realization coverage each reported clean: zero Critical,
  High, Medium, or other non-editorial findings.
- The realization lens mapped all semantic and ownership scenarios through design and explicit tasks,
  found no orphan task, and confirmed exact privilege, deterministic-golden, and repository-gate
  coverage.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| One sealed `Intrinsic.SharedCore<T>` with canonical element identity and no representation lanes | Inspect core; specialize core; malformed specialization | Sealed role rather than implementation lanes | 1.1, 1.3, 1.4, 1.6 semantic facts and inspection | Covered |
| Every available core handle is affine and carries exactly one `LocalSharedStrong` obligation independently of `T` | Move, Copy-element, generic ownership, aggregate cardinality | Affine core independent of element ownership | 1.3, 2.1, 2.3 focused ownership tests | Covered |
| Non-consuming duplication and invalid Copy conformance remain phase-owned | `OWN0003` read; `SEM0083` conformance scenarios | Ownership versus conformance split | 2.1–2.2 code/span/witness assertions | Covered |
| Local non-transferability is a semantic type fact that composes through all relevant forms | Recursive joins, arrays/unions, captures, retained borrows, same-domain movement | Four-outcome total affinity algebra | 1.2, 1.4–1.6 and 2.4 | Covered |
| Open generics remain sound before specialization | Parameter-dependent aggregate and concrete substitutions | `ParameterDependent` with canonical ids and re-normalization | 1.2, 1.4 | Covered |
| Recovery never fabricates unrestricted, Copy, satisfied, or obligation evidence | Malformed core, unavailable-plus-local, multiple causes, unavailable ownership | Deterministic unavailable precedence and causes | 1.2, 1.4, 2.5 | Covered |
| Parking and movement within one same-thread local execution domain are legal without instance confinement | Future-consumer and same-domain frame scenarios | No execution/fiber/Scheduler identity | 1.6, 2.4 | Covered |
| No standard-library actor gains privilege by spelling | Five-name ordinary nominal matrix | Canonical sealed intrinsic boundary | 3.1 semantic and ownership assertions | Covered |
| Parallel transfer, Scheduler policy, allocation, lifecycle, and backend representation stay outside this slice | Future-consumer negative scenario and non-goals | Outcome-only prerequisite | 1.6 and 2.4 negative scope checks | Covered |
| Facts and ownership encodings are deterministic and implementation is gated by repository policy | Ordered causes and all normative scenarios | Canonical joins and narrow artifact surface | 3.2 committed goldens; 3.3 focused and full gates | Covered |

## Completeness findings

### Missing normative behavior

None.

### Missing boundary or failure scenarios

None.

### Missing implementation or verification work

None.

## Divergence findings

### OpenSpec contradictions or inventions

None. `ParameterDependent`, deterministic cause ordering, and the domain-level `LocalExecution`
outcome are realization refinements of the SLP's delegated non-transferability representation; they
do not add transfer or Scheduler policy.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. The compiler-owned surface in this slice is limited to the sealed core identity, exact
ownership role, affinity outcome, and affine obligation facts. Ordinary names—including `Shared`,
`SharedCore`, `Deferred`, `Scheduler`, and `LocalRuntimeHandle`—receive no privilege. Allocation,
clone/access/drop mechanics, public wrappers, Scheduler policy, parallel transfer, and backend
representation remain in their later slices or ordinary Silk.

## Required revisions

None.

## Next state

The change is implementation-ready. Implement via `$openspec-apply-change`, complete every task and
verification gate, then audit the implementation with `$slp-6-audit-implementation` before archive.
