# Upstream provenance

`@silk-lang/wasm` implements the WebAssembly specification directly; it ports no upstream
implementation. Its correctness is anchored to one pinned oracle release used by development-time
verification only — the published runtime never invokes it.

## Pinned oracle

| Tool | Version | Role |
| --- | --- | --- |
| `wasm-tools` | `1.255.0` | binary validation, text→binary assembly for round-trip comparison |

Install the exact pin with:

```sh
cargo install wasm-tools@1.255.0 --locked
```

`scripts/oracle.mjs` refuses to run against any other version. Point `WASM_TOOLS` at the binary
if it is not on `PATH`.

## Feature baseline

Validation runs with exactly these `wasm-tools` features, matching the package's supported
surface: `mutable-global`, `saturating-float-to-int`, `sign-extension`, `multi-value`,
`bulk-memory`, `reference-types`, `tail-call`, `extended-const`, `multi-memory`, `simd`,
`relaxed-simd`, `threads`, `memory64`, `exceptions`, `gc`, `function-references`. Branch hints are custom-section content
validated through text round-trips rather than a feature flag.

## Verification layers

1. `pnpm --filter @silk-lang/wasm test` — unit tests plus committed-fixture comparison
   (byte-identical binary, character-identical text). No external tools; runs in CI.
2. `pnpm --filter @silk-lang/wasm parity:oracle` — oracle validation of every fixture binary,
   text→binary round-trip equality, and negative-corpus agreement (constructs the builder
   rejects must be rejected by the oracle). Requires the pinned `wasm-tools`.
3. `pnpm --filter @silk-lang/wasm fixtures:regenerate` — rebuilds committed fixtures after an
   intentional output change; rerun the oracle afterwards.

## Name section note

The builder emits the standard `name` custom section subsections (module, function, local) plus
the extended subsections for tables, memories, globals, element segments, and data segments,
mirroring what the pinned `wasm-tools` generates from text identifiers so text round-trips stay
byte-identical. Extended subsections are custom-section content; they do not affect the module's
validation feature baseline.
