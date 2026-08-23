# OpenSpec audit o001: add-silk-test-command

SLP: `proposals/0004-silk-native-testing/proposal.md`
SLP revision: 36
SLP digest: `0a39823f15178c075870f85c54ee86c2a8be5cd873c3d4139696500c07331808`
OpenSpec change: `add-silk-test-command`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `cef97c66dca8a4822d16808d0c827486f612c60b5fa247f48cdf6eb25f768e94`
- `proposal.md`: `e14f2672a142311c31ab33633f74d7007d737fb57f64d633ea123bcccbf7460d`
- `design.md`: `e03f385fba66d30434991cd25fe6649eae877902405860c10f0e3966c4b8e30e`
- `specs/silk-cli-workflows/spec.md`: `5682b20c5df62ea386dd8eb04591447c5404da1b612e89b3e72e0fe3ce92ec01`
- `specs/silk-project-manifest/spec.md`: `e6f7d796a9af162e25939139743def385e701f7584fe2e768dd37a3259a774b8`
- `tasks.md`: `4236eff42c222e48fbcd67ff0953483cfa28241f807c896fbae9b982f6727ecd`

Date: 2026-08-23
Result: Ready

## Validation evidence

- `openspec status --change add-silk-test-command --json` reported every planning artifact complete.
- `openspec instructions apply --change add-silk-test-command --json` reported state `ready`,
  thirteen tasks, and zero complete.
- Strict validation passed after fix pass 1: one valid change, zero issues.
- Three fresh reviewers independently audited revision-36 raw artifacts for SLP fidelity,
  normative completeness, and realization coverage.
- No SLP decision remains open.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| User roots are manifest-relative and source-root-contained | Test configuration and resolution scenarios | Manifest-directory resolution then containment | 1.1–1.2 | Covered |
| Standard-library roots come from a deterministic catalog | Catalog requirement and standard-library command scenario | `--standard-library` toolchain mode | 1.3, 2.1–2.2, 3.2 | Covered |
| Runner role is distinct from test roots | Manifest and command runner scenarios | Separate source-entry roles and shipped default | 1.2, 2.2, 3.1 | Covered |
| Filters use unchanged HostInput bytes | Raw boundary requirement | Platform evidence gate and byte script | 0.1, 2.3 | Covered with explicit SLP revisit gate |
| One evaluator-only host execution | One-suite requirement | TestWorkflow boundary | 2.2 | Covered |
| Output and status remain runner-owned | Output and status requirements | Scoped StandardStreams plus entry forwarding | 2.4–2.5 | Covered |
| Ordinary commands do not pay for testing | Test configuration isolation | Lazy TestConfiguration | 3.3 | Covered |

## Completeness findings

### Missing normative behavior

Fix pass 1 corrected the manifest resolution base, defined repeated roots, made the shipped runner
unshadowable, added a concrete standard-library selector, closed CLI option grammar, specified
program-name bytes at HostInput index zero, connected evaluator output to command stdout, separated
standard-runner statuses from custom entry termination, and preserved every existing non-entry
evaluator classification.

### Missing boundary or failure scenarios

Fix pass 1 added absent/unreadable configured entries, absent imports, catalog drift, stdout write
failure, option-looking filters after `--`, arbitrary custom status, mixed source/operational
failure, exact help, and non-entry evaluator termination scenarios.

### Missing implementation or verification work

Tasks now begin with the real platform-byte evidence gate, jointly validate catalog and shipped
sources, exercise both user and standard-library workflows, wire the evaluator StandardStreams
provider, validate all canonical custom entry shapes, and guard build/check/run from test-config
leakage.

## Divergence findings

### OpenSpec contradictions or inventions

The initial artifacts incorrectly assigned standard-runner 0/1/2 meanings to every custom runner
and resolved manifest-relative paths from the source root. Both contradictions are removed.

### SLP decisions requiring reconsideration

None. HostInput index zero reuses the existing platform-derived program-name contract and gains no
new stable spelling. Evaluator states outside ordinary entry completion retain their existing
classification instead of acquiring new test-command policy. These are canonical-mechanism reuse,
not new SLP decisions.

## Compiler–standard library boundary

Closed. Tooling selects roots, seeds the existing HostInput script, and provides the evaluator's
ordinary stream boundary. It does not inspect filters, synthesize a test entry ABI, reinterpret
custom runner status, or move Test/Reporter policy into compiler phases.

## Required revisions

None.

## Next state

Ready. `prove-silk-native-testing-sufficiency` may depend on this frozen command contract.
