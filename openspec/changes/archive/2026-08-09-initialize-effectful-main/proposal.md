## Why

`silk init` currently teaches an ordinary integer-returning entry even though effectful `main` is
the canonical application boundary for running effects and reporting typed failures. New projects
should begin with the model most applications are expected to grow into.

## What Changes

- Generate `src/main.silk` with a public zero-argument effectful `main` returning `()`.
- Preserve the generated program's zero-status behavior across check, native build/run, LLVM-Wasm,
  and direct WebAssembly workflows.
- Update initializer and CLI acceptance tests to pin the new canonical source.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `silk-project-initialization`: The generated executable entry becomes an effectful `main`
  returning `()` instead of an ordinary `main` returning `i32`.

## Impact

The change affects the CLI project initializer, its generated-source fixture, CLI
end-to-end initialization coverage, and the project-initialization specification. It adds no new
dependencies and does not remove support for ordinary `main() -> i32` programs.
