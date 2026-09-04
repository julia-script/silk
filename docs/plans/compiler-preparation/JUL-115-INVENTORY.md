# JUL-115 removal inventory

Implementation began at `ee6ebb3d41a55fee65f1119b9c495c16d8dfec09`.

The baseline inventory was generated with:

```sh
rg --hidden -n 'Analysis\.evaluate|BootstrapEvaluation\.evaluate' --glob '!.git/**' --glob '!**/node_modules/**'
rg --hidden -n '@silklang/wasm|WasmBackend|WasmCleanup|WasmEmitContext|WasmLanes|WasmMemory' --glob '!.git/**' --glob '!**/node_modules/**'
git grep -n 'BootstrapEvaluation' ee6ebb3d41a55fee65f1119b9c495c16d8dfec09 -- .
git grep -n -E 'codegenWasm|WebAssemblyModuleArtifact|corpusOutcomeShard' ee6ebb3d41a55fee65f1119b9c495c16d8dfec09 -- .
git grep -n -E 'direct[- ]Wasm|direct WebAssembly' ee6ebb3d41a55fee65f1119b9c495c16d8dfec09 -- .
```

It found 671 runtime-evaluator references in 174 files, and 274 direct-Wasm references in 86 files.
The supplemental whole-actor evaluator search found 141 lines in 56 files, including 118 lines that
did not spell the `.evaluate` entry point. The supplemental direct-backend API/shard search found
384 lines in 133 files; the prose/identifier search found 672 lines in 360 files. These supplemental
sets overlap the primary inventories and deliberately include tracked hidden files and archived
documents so consumers described without an actor import are not omitted.
The [assertion ledger](JUL-115-ASSERTION-DISPOSITIONS.md) records the disposition and surviving
evidence for every affected test registration.

## Final package graph

The lockfile contained these 13 importers at implementation start: `.`, `apps/docs`, `apps/vscode`,
`examples/tiny-language`, `packages/cli`, `packages/compiler`, `packages/docgen`,
`packages/editor-support`, `packages/formatter`, `packages/llvm`, `packages/lsp`,
`packages/platform-webcontainer`, and `packages/wasm`. It now contains the same list except for
`packages/wasm`: 12 importers including the root, or 11 non-root importers.

The removed dependency edges are `apps/docs -> @silklang/wasm`,
`@silklang/compiler -> @silklang/wasm`, `apps/docs -> @silklang/formatter`, and
`apps/docs -> @silklang/lsp`; the removed external Labs-only dependencies are `dockview` and
`dockview-react`, and the removed workspace node is `packages/wasm`. The package exports, workspace
scripts, release validation, and lockfile entries for the deleted package are absent.

## Deleted surfaces

- Runtime execution: `Analysis.evaluate`, `BootstrapEvaluation`, runtime values, outcomes, traces,
  limits, evaluator hosts/adapters, provider dispatch, evaluator fixtures, and evaluator corpus
  shards.
- Independent WebAssembly: compiler selection and lowering actors, `WasmMain`, direct-emitter
  tests/fixtures/scripts, and the complete `packages/wasm` package.
- Product/tooling: the current Labs route, navigation, workbench/state, presets, panes, worker
  protocol, tests, fixtures, examples, documentation, and evaluator inspection/LSP fields.
- Historical implementation records: archived changes devoted solely to the removed evaluator,
  direct backend, engine parity, foreign host, evaluator test-inventory implementations, and the
  deleted Labs-only bootstrap/nested-value flow visualizations.

The Labs-only archives removed in the final review were
`2026-08-04-visualize-bootstrap-data-flow` and `2026-08-05-visualize-nested-value-flow`.
The archive audit also removed 47 `bootstrap-syntax-inspector` delta specifications and 49
`bootstrap-evaluation` delta specifications whose only subject was the deleted inspection or
runtime-evaluator implementation. Mixed archived changes retain only their still-relevant
language and LLVM-facing specifications.

## Surviving authoritative evidence

- Compile-time execution: `packages/compiler/test/StaticText.test.ts` exercises
  `StaticEvaluation` directly, alongside focused static reflection and constant suites.
- Target-neutral runtime behavior: `packages/compiler/test/support/corpus.ts`, whose independently
  pinned `expected` results are asserted by `DriverNativeAcceptance.test.ts`.
- WebAssembly behavior: LLVM-to-Wasm compiler and driver suites.
- Lowering and ABI behavior: LLVM IR/object inspection and separately compiled C fixtures in the
  compiler native acceptance suites.
- Static tooling: inspector and language-server suites asserting HIR, MIR, ownership, target,
  roots, requirements, diagnostics, and serialization without execution.

## Final absence searches

The following source/package search returns no matches:

```sh
rg --hidden -n '@silklang/wasm|WasmBackend|WasmCleanup|WasmEmitContext|WasmLanes|WasmMemory|Analysis\.evaluate|BootstrapEvaluation|codegenWasm|WebAssemblyModuleArtifact|corpusOutcomeShard' packages apps scripts .github .changeset package.json pnpm-lock.yaml pnpm-workspace.yaml turbo.json --glob '!.git/**' --glob '!node_modules/**' --glob '!dist/**'
```

`packages/wasm` and `apps/docs/app/labs` do not exist. Repository-wide matches are permitted only
in this inventory and assertion ledger, negative release-candidate assertions that prove removed
exports stay absent, and mixed archived OpenSpec change records retained solely as historical
context for still-surviving language work. Active specifications contain no such references.

## Completion gates

- `pnpm typecheck`: passed after the final migration changes (18/18 Turbo tasks).
- `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and
  `pnpm release:candidate`: pending final recorded runs.
