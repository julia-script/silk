## Why

Silk projects can be discovered and built only after users hand-write a manifest and source tree, and the current build model collapses backend selection into a single native target. Adding safe project initialization and treating backend and target as independent build inputs gives new projects a reliable starting point while allowing both LLVM and the direct WebAssembly backend to produce `wasm32-unknown-unknown` artifacts without collisions.

## What Changes

- Add `silk init [path] [--name <name>]` for creating an executable project in a new or existing directory without overwriting project-owned files.
- Generate a sparse Rust-like manifest, `/build/` ignore rule, and minimal `src/main.silk` executable; reserve library initialization until library builds exist.
- **BREAKING** Require project manifests to carry package version metadata and move the default artifact root from `.silk/build` to visible `build`.
- Add optional `[build]` defaults for one backend, an ordered target array, and a manifest-relative output directory.
- Introduce stable `llvm` and `wasm` backend selectors plus a portable `host` target selector, with repeatable `--target` overrides.
- Make backend and target independent: LLVM supports native targets and `wasm32-unknown-unknown`, while the direct WebAssembly backend supports `wasm32-unknown-unknown`.
- **BREAKING** Include backend identity in deterministic artifact paths: `<output-dir>/<backend>/<target>/<profile>/<package>[.wasm]`.
- Build and check every selected target deterministically, validate the complete batch before work begins, retain independently committed successes, summarize every target, and return the most severe existing exit class.
- Keep `silk run` host-only and executable-only; do not add `init --lib` or library artifact production yet.

## Capabilities

### New Capabilities

- `silk-project-initialization`: Safe project scaffolding, generated file contents, package-name selection, collision handling, and cleanup of partial initialization.

### Modified Capabilities

- `silk-project-manifest`: Add version and build defaults, ordered target selectors, backend selection, visible output roots, and backend-qualified artifact paths.
- `silk-cli-workflows`: Add `init`, backend and repeatable target options, multi-target build/check behavior, host-only run selection, summaries, and aggregate exit status.
- `bootstrap-backend`: Allow canonical backend selection independent of target and extend LLVM emission to `wasm32-unknown-unknown` while retaining the direct WebAssembly backend.
- `bootstrap-compiler-driver`: Generalize final artifact production beyond native executables so compatible LLVM and direct WebAssembly requests can produce durable Wasm modules.
- `bootstrap-native-toolchain`: Extend the external-tool boundary with deterministic LLVM-to-Wasm finalization while keeping direct WebAssembly output free of Clang.

## Impact

- `packages/compiler-cli`: command registration, project decoding, initialization, option resolution, build planning, workflows, reporting, tests, README, and exports.
- `packages/compiler`: target/backend compatibility, driver artifact outcomes, LLVM Wasm emission/finalization, toolchain planning, tests, and public exports where actor seams change.
- Existing `silk.toml` files and assertions using `.silk/build/...` require migration to the new manifest version field and visible backend-qualified layout.
- LSP project discovery continues to use `silk.toml`, but its shared project decoding must accept and preserve the evolved schema.
- No dependency management, library production, workspace support, build parallelism, or runtime execution of Wasm artifacts is added.
