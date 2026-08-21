# OpenSpec audit o001: rename-effect-module

SLP: `proposals/0012-reserved-module-path-segments/proposal.md`
SLP revision: 1
SLP digest: `e3f251c6776d745ee4b4629adb1fead08f2aba2d56274057fa5cbd0eb18bb4ff`
OpenSpec change: `rename-effect-module`
Schema: `spec-driven`
Artifact digests:

- `proposal.md`: `8986917bf6e72a9065d7597ccb210dee39abca1e34381615cafa11e245c8168a`
- `design.md`: `b652c1ac1b921a104aa139b2c09d0aecd494bd3381f8dd1a5b3b23d3750b2075`
- `tasks.md`: `685d4c2554a96669316833caa9a6489a8c46692e368dd49d9a68da03bb369acb`
- `specs/bootstrap-module-closure/spec.md`: `c248c955aecb77dae607488c360fa3d33ac531b749eec95c541c5e3dd866216c`
- `specs/bootstrap-name-resolution/spec.md`: `5d310e2f81861a9b3500d1aca457ecf542ad77716de1a9b0e9d8cae5e4c667cc`
- `specs/bootstrap-silk-stdlib/spec.md`: `e934648b001f15279a7a7ee897eebfd76c7814b45b37c2a48a7b1b772a803d71`
- `specs/bootstrap-syntax/spec.md`: `5f66de48bd9f6e8c6a5f13e3e89e8fde27361a64201fb5cf5330a3f51615d1aa`
- `specs/language-server-auto-import/spec.md`: `0d4c445e697f72d8bc616b46da239090dfc4de9b9860a6cfea92db4053b86356`
- `specs/language-server-completion/spec.md`: `be06f040408c06c2fb535c55a55703dedfe0a4874462e6993dd8b7e1b3ddc407`

Date: 2026-08-21
Result: Ready

## Validation evidence

`openspec validate rename-effect-module --strict` completed successfully. The proposal declares six
modified capabilities and each has one syntactically valid delta specification. Planning status
reports proposal, specs, design, and tasks complete.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Reserved tokens are contextual import-path segments | `bootstrap-syntax`: reserved segment and binding-boundary scenarios | Syntax-owned path query; lexer remains unchanged | 1.1, parser tests | Covered |
| `silk.effect` maps to `silk/effect` | `bootstrap-module-closure`: follow a reserved path segment | All consumers use ordered contextual segments | 1.3, closure and artifact tests | Covered |
| A reserved final segment cannot form an implicit binding | `bootstrap-syntax`: reject unusable binding; `bootstrap-name-resolution`: explicit binding forms | Parser diagnostic plus no implicit semantic binding | 1.2 and 1.3 | Covered |
| Canonical Effect module is singular with no compatibility alias | `bootstrap-silk-stdlib`: import singular and reject plural | Single breaking distribution migration | 3.1–3.3 | Covered |
| Catalog metadata is discovery, not scope | `bootstrap-name-resolution`: catalog namespaces out of scope and removed prelude requirement | Remove catalog seeding; inventory is tooling-only | 2.1 | Covered |
| Partial non-type `Eff` offers namespace import | `language-server-completion`: partial and complete namespace scenarios | LSP merges revision-bound catalog inventory into compiler non-type context | 2.2 | Covered |
| `Effect<...>` remains import-free | `language-server-completion`: type scenario | Compiler context gates namespace enrichment | 2.2 | Covered |
| Completion inserts `import silk.effect as Effect` | `language-server-auto-import`: insert, reuse, collision, wrong-shape scenarios | Namespace request variant in shared import planner | 2.3 | Covered |
| `Effect.` exposes ordinary source members | `language-server-completion`: imported members scenario | Ordinary namespace lookup and compiler completion | 2.4 | Covered |
| No runtime or compiler-known library privilege | proposal impact and stdlib requirement | No intrinsic; generic syntax/catalog/import machinery only | 4.4 boundary audit | Covered |

## Completeness findings

### Missing normative behavior

None found. The delta specs cover syntax, canonical identity, binding eligibility, distribution
identity, completion context, partial spelling, import shape, reuse, and collision behavior.

### Missing boundary or failure scenarios

None found. The plan includes reserved words outside paths, reserved interior segments, alias-less
final segments, removed plural resolution, type-context suppression, duplicate imports, wrong
selected-import shape, and collision aliasing.

### Missing implementation or verification work

None found. Tasks cover every affected compiler/tooling consumer identified by the SLP, repository
migration, generators, full checks, package validation, and final reconciliation.

## Divergence findings

### OpenSpec contradictions or inventions

None found. Removing catalog namespace seeding is a direct realization of the SLP's explicit-import
model and also reconciles the existing module-semantics requirement that catalog membership does
not create scope.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

The plan adds no source-callable compiler operation and no Effect-specific semantic recognition.
The compiler owns only contextual import grammar, canonical path extraction, semantic import
bindings, completion context, and generic catalog inventory. The standard library continues to own
all Effect operation policy and implementation in ordinary source under `silk/effect`.

## Required revisions

None.

## Next state

Implementation may begin from the validated `rename-effect-module` task list. Recompute artifact
digests and perform the implementation audit after all generated outputs and validation gates are
complete.
