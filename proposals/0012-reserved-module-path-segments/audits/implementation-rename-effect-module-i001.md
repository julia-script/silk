# Implementation audit i001: rename-effect-module

SLP: `proposals/0012-reserved-module-path-segments/proposal.md`
SLP revision: 1
SLP digest: `e3f251c6776d745ee4b4629adb1fead08f2aba2d56274057fa5cbd0eb18bb4ff`
OpenSpec change: `rename-effect-module`
OpenSpec artifact digests:

- `proposal.md`: `8986917bf6e72a9065d7597ccb210dee39abca1e34381615cafa11e245c8168a`
- `design.md`: `b652c1ac1b921a104aa139b2c09d0aecd494bd3381f8dd1a5b3b23d3750b2075`
- `tasks.md`: `7020a05626d86ec6e4db0c7cc135082bf1afe6893394d72684e9ea017322fc86`
- `specs/bootstrap-module-closure/spec.md`: `c248c955aecb77dae607488c360fa3d33ac531b749eec95c541c5e3dd866216c`
- `specs/bootstrap-name-resolution/spec.md`: `5d310e2f81861a9b3500d1aca457ecf542ad77716de1a9b0e9d8cae5e4c667cc`
- `specs/bootstrap-silk-stdlib/spec.md`: `e934648b001f15279a7a7ee897eebfd76c7814b45b37c2a48a7b1b772a803d71`
- `specs/bootstrap-syntax/spec.md`: `5f66de48bd9f6e8c6a5f13e3e89e8fde27361a64201fb5cf5330a3f51615d1aa`
- `specs/language-server-auto-import/spec.md`: `0d4c445e697f72d8bc616b46da239090dfc4de9b9860a6cfea92db4053b86356`
- `specs/language-server-completion/spec.md`: `be06f040408c06c2fb535c55a55703dedfe0a4874462e6993dd8b7e1b3ddc407`

Implementation fixed point: working-tree implementation and generated artifacts named in this audit,
after completion of `openspec/changes/rename-effect-module/tasks.md` on 2026-08-21
Date: 2026-08-21
Result: Conformant

## Evidence inspected

- Contextual path ownership and parsing in `packages/compiler/src/ImportPath.ts`,
  `packages/compiler/src/Parser.ts`, and `packages/compiler/src/Diagnostic.ts`.
- Canonical path consumption in `ModuleClosure.ts`, `ModuleSummary.ts`, `ImportPlan.ts`,
  `NameResolution.ts`, and the LSP document actor.
- Completion and edit planning in `packages/compiler/src/Completion.ts`,
  `packages/compiler/src/ImportPlan.ts`, and `packages/lsp/src/Document.ts`.
- The singular manifest/source pair, generated stdlib embedding and integrity inventory, generated
  language documentation, package export map, and release-candidate inventory.
- Focused parser, module-closure, name-resolution, import-planning, stdlib-resolution, and LSP
  completion tests, including complete and partial namespace spellings.
- `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
  `pnpm release:candidate`, all successful at the audited fixed point.
- `openspec validate rename-effect-module --strict`, successful at the audited fixed point.
- A current-tree search for `silk.effects`, `silk/effects`, and `effects.silk`; remaining matches are
  confined to planning statements that identify the removed spelling and the negative resolution
  test that proves it is unavailable.

## Contract-to-implementation traceability

| SLP/OpenSpec contract | Implementation location and behavior | Verification evidence | Disposition |
| --- | --- | --- | --- |
| Reserved words are contextual import-path segments only | `ImportPath.isSegmentKind` owns the contextual token set; `Parser` uses it only while parsing import paths and retains the original tokens | Parser tests accept reserved interior segments, preserve keyword token kinds, and reject reserved words in ordinary identifier positions | Conformant |
| A reserved final segment cannot become an implicit binding | `Parser` emits `PAR0004`; `NameResolution` does not synthesize an implicit namespace from a reserved final token | Parser recovery and name-resolution tests | Conformant |
| `silk.effect` maps canonically to `silk/effect` through every consumer | Module closure, summaries, import planning, resolution, and LSP inspection use `ImportPath.segments` | Module-closure, module-summary, import-plan, name-resolution, and LSP tests | Conformant |
| The only distributed Effect operations module is singular | Manifest and source are `silk/effect` and `silk/effect.silk`; the plural source and generated page are removed | Stdlib resolution rejects `silk/effects`; generation checks and release-candidate validation pass | Conformant |
| Catalog metadata discovers namespaces but does not inject them into scope | Name resolution remains import-driven; catalog namespace inventory is consumed only by completion | Unimported `Effect` operation namespace is missing while explicit namespace import resolves | Conformant |
| Partial `Eff` and complete non-type `Effect` offer a namespace auto-import | The LSP merges matching catalog namespaces only in non-type expression completion and plans a namespace request | LSP tests cover `Eff`, `Effect`, source edit shape, and partial replacement range | Conformant |
| `Effect<...>` remains a built-in, import-free type completion | Compiler completion publishes the built-in `Effect` type candidate; LSP catalog enrichment is excluded from type contexts | LSP type-completion test asserts no additional import edit | Conformant |
| Namespace import planning inserts, reuses, extends, and resolves collisions deterministically | `ImportPlan.namespace` emits `import silk.effect as Effect`, reuses equivalent bindings, extends selected imports to hybrid form, and returns a deterministic alias when occupied | Import-plan and LSP completion tests, including `SilkEffect` collision alias | Conformant |
| `Effect.` exposes ordinary source members without spelling-specific semantic privilege | Explicit namespace resolution feeds the ordinary compiler member-completion path | Existing and focused qualified-completion tests | Conformant |
| No compatibility alias or compiler-known Effect actor is introduced | Distribution has no plural manifest entry or source; no intrinsic or semantic spelling dispatch was added | Negative stdlib resolution test, intrinsic inventory tests, release-candidate package inspection | Conformant |

## Divergence analysis

### Realization refinements

- The built-in `Effect` type candidate was made explicit in compiler completion so the type-context
  half of the contract is positively testable, not merely the absence of an import edit.
- The existing catalog-to-scope boundary already satisfied the requested explicit-import model, so
  task 2.1 required strengthening its regression evidence rather than deleting a compatibility path.
- Namespace collision planning selects the deterministic module-prefixed spelling `SilkEffect`.
  This is the concrete realization of the SLP's deterministic-alias requirement.

### OpenSpec gaps or divergences

None.

### Justified SLP divergences

None.

### Unjustified implementation divergences

None.

### Author decision forks

None.

## Compiler–standard library boundary

The implementation adds no source-callable intrinsic, runtime operation, or compiler-known
standard-library actor. The compiler owns contextual import syntax, canonical segment extraction,
ordinary import binding, generic catalog discovery, and generic namespace edit planning. All Effect
operations and policy remain ordinary Silk declarations in `silk/effect`.

## Required actions

None.

## SLP amendment response

No amendment is required.

## Next state

The implementation conforms to accepted SLP-0012 and the validated OpenSpec change. The change is
ready for author review and may be archived after acceptance.
