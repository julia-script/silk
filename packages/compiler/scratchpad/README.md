# Compiler profiling scratchpad

Build the local dependencies from the repository root:

```sh
pnpm --filter @silklang/llvm build
pnpm --filter @silklang/compiler build
```

Start Jaeger and compile the Silk compiler sources under `compiler/src`:

```sh
docker compose -f packages/compiler/scratchpad/compose.yaml up -d
cd packages/compiler
bun run ./scratchpad/compile.ts
```

`compile.ts` runs the TypeScript compiler sources directly. The executable and
link plan are written to `scratchpad/dist/hello-world`. Compilation caching is
disabled. `saveTemps: true` retains intermediate files in the system temporary
directory; the link plan records their paths.

## Compare Chrome traces

Every run saves a new `scratchpad/dist/comp.chrome-trace.<datetime>.json`, including
failed compilations. The filename uses the run's start time in UTC, for example
`comp.chrome-trace.2026-09-15T23-15-42.123Z.json`. Earlier runs are preserved for
comparison, and the start time is also stored in the trace's `otherData` field.

Drag a trace into Chrome DevTools' **Performance** panel to inspect the Effect
spans. Export captures spans directly and works when Jaeger is unavailable.
These are elapsed span durations, without CPU samples or GC events.

## Jaeger

Open [Jaeger](http://localhost:16686), select the `comp` service, and click
**Find Traces**. Each compilation has a `Scratchpad.compile` root span. Buffered
spans flush when the script exits.

The scratchpad exports OTLP/HTTP to `http://127.0.0.1:4318/v1/traces`. Set
`OTEL_EXPORTER_OTLP_TRACES_ENDPOINT` to use another collector. Jaeger stores traces
in memory, so restarting its container clears them. The dated Chrome exports
remain on disk. Stop Jaeger from the repository root with:

```sh
docker compose -f packages/compiler/scratchpad/compose.yaml down
```

The native toolchain must be installed. The scratchpad uses Homebrew LLVM on Apple
Silicon when available, otherwise `clang` from `PATH`. Set `SILK_CLANG` to select
another executable and `SILK_LLVM_AR` to override the sibling `llvm-ar` selection.
