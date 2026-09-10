## Why

Silk programs cannot currently compute standardized cryptographic digests without duplicating
correctness-sensitive code or depending on target-specific native libraries. Portable source-owned
SHA actors are needed now for protocol work such as WebSocket handshakes and for direct WebAssembly
and no-libc programs that cannot rely on an operating-system crypto provider.

## What Changes

- Add ordinary Silk implementations of legacy SHA-1, all six fixed-output SHA-2 variants, and all
  four fixed-output SHA-3 variants.
- Give every algorithm a streaming state with `make`, inherent mutable `update`, consuming
  `finish`, and an equivalent one-shot `hash` helper that returns an exact fixed-array digest.
- Preserve complete standard length accounting, including a carry-correct two-`u64` bit count for
  the SHA-512 family, and reject algorithmically overlong messages instead of wrapping.
- Ship the modules through the deterministic standard-library manifest and generated embedding,
  and publish complete source-authored/generated API documentation.
- Verify every variant against independent known-answer vectors and consolidate chunking and
  padding-boundary behavior into the shared native acceptance corpus.

## Capabilities

### New Capabilities

- `bootstrap-cryptographic-hashes`: Defines the portable fixed-output SHA modules, actor surface,
  streaming semantics, digest sizes, length limits, validation corpus, and absence of compiler or
  runtime privilege.

### Modified Capabilities

None.

## Impact

- Adds canonical modules under `packages/compiler/stdlib/silk/` and entries in
  `packages/compiler/stdlib/manifest.json`.
- Regenerates `packages/compiler/src/Stdlib.generated.ts` and the standard-library reference under
  `apps/docs/content/language/stdlib/`.
- Extends the shared compiler native acceptance corpus with SHA vectors and incremental cases.
- Adds no external dependency, compiler intrinsic, runtime provider, service, Effect requirement,
  or target-specific binding.
