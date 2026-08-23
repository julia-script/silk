## Why

Silk has the arithmetic, mutation, service, and provider machinery needed for reproducible
pseudorandom generation, but the standard library offers no reusable random contract or seeded
generator. Adding one in ordinary Silk provides an expected library feature while exercising those
language facilities across every execution engine without expanding compiler privilege.

## What Changes

- Add a portable `Random` service whose single provider operation produces the next `u64` value.
- Add a public algorithm-named Xoshiro256** provider, seeded deterministically through SplitMix64,
  with a stable sequence on every engine for every seed.
- Add provider-independent source operations for a random boolean, unbiased selection below a
  positive exclusive upper bound, and deterministic filling of a mutable byte slice.
- Ship the implementation as canonical documented `silk/random` source and include it in the
  standard-library manifest, generated embedding, reference documentation, and doctest inventory.
- Keep operating-system entropy, cryptographic security, ambient global generators, floating-point
  distributions, ranges beyond `below`, shuffling, and sampling distributions out of this change.
- Add no intrinsic, compiler-known random operation, host import, or target-specific provider.

## Capabilities

### New Capabilities

- `bootstrap-random`: Defines the portable Random service, the reproducible Xoshiro256** provider,
  derived sampling operations, edge cases, and cross-engine determinism.

### Modified Capabilities

- `bootstrap-silk-stdlib`: Ships `silk/random` as canonical ordinary Silk source with no compiler
  privilege or platform dependency.

## Impact

The change adds one standard-library source module and updates its deterministic manifest, generated
embedding, generated reference documentation, doctest inventory, and compiler acceptance corpus.
It exercises existing `u64` wrapping and bit operations, fixed arrays, exclusive mutation, services,
and lexical provider replacement. It does not change parsing, analysis, HIR, MIR, evaluation,
backend semantics, runtime ABIs, host configuration, intrinsic availability, or direct-Wasm imports.
