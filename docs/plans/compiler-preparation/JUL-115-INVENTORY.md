# JUL-115 removal inventory

Implementation began at `ee6ebb3d41a55fee65f1119b9c495c16d8dfec09`.

The baseline inventory was generated with:

```sh
rg -n 'Analysis\.evaluate|BootstrapEvaluation\.evaluate' --glob '!**/node_modules/**'
rg -n '@silklang/wasm|WasmBackend|WasmCleanup|WasmEmitContext|WasmLanes|WasmMemory' --glob '!**/node_modules/**'
```

It found 671 runtime-evaluator references in 174 files, and 266 direct-Wasm references in 82 files.
The [assertion ledger](JUL-115-ASSERTION-DISPOSITIONS.md) records the disposition and surviving
evidence for every affected test registration.

## Final package graph

The lockfile contained 11 workspace importers at implementation start and contains 10 after this
change. The only removed importer is `packages/wasm`. The compiler dependency on
`@silklang/wasm`, the docs-app dependency, package exports, workspace scripts, release validation,
and lockfile entries are absent.

## Deleted surfaces

- Runtime execution: `Analysis.evaluate`, `BootstrapEvaluation`, runtime values, outcomes, traces,
  limits, evaluator hosts/adapters, provider dispatch, evaluator fixtures, and evaluator corpus
  shards.
- Independent WebAssembly: compiler selection and lowering actors, `WasmMain`, direct-emitter
  tests/fixtures/scripts, and the complete `packages/wasm` package.
- Product/tooling: the current Labs route, navigation, workbench/state, presets, panes, worker
  protocol, tests, fixtures, examples, documentation, and evaluator inspection/LSP fields.
- Historical implementation records: archived changes devoted solely to the removed evaluator,
  direct backend, engine parity, foreign host, and evaluator test-inventory implementations.

## Surviving authoritative evidence

- Compile-time execution: `packages/compiler/test/StaticEvaluation.test.ts` and focused static
  reflection/constant suites.
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
rg -n '@silklang/wasm|WasmBackend|WasmCleanup|WasmEmitContext|WasmLanes|WasmMemory|Analysis\.evaluate|BootstrapEvaluation|codegenWasm|WebAssemblyModuleArtifact|corpusOutcomeShard' packages apps scripts .github package.json pnpm-lock.yaml pnpm-workspace.yaml turbo.json --glob '!node_modules/**' --glob '!dist/**'
```

`packages/wasm` and `apps/docs/app/labs` do not exist. Repository-wide matches are permitted only
in this inventory and assertion ledger, negative release-candidate assertions that prove removed
exports stay absent, and mixed archived OpenSpec change records retained solely as historical
context for still-surviving language work. Active specifications contain no such references.

## Completion gates

- `pnpm typecheck`: passed (18/18 Turbo tasks).
- `pnpm format:check`: passed (3,170 files checked).
- `pnpm lint`: passed.
- `pnpm test`: passed (22/22 Turbo tasks), including the complete shared native acceptance corpus
  and the production documentation build.
- `pnpm check`: passed (33/33 Turbo tasks and 17/17 repository script-policy tests).
- `pnpm release:candidate`: passed (10/10 package archive, manifest, export, and consumer-install
  validations).
