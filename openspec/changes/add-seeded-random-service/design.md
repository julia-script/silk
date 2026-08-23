## Context

See `proposal.md` for motivation. Silk already exposes portable `u64` wrapping arithmetic,
bitwise operations, shifts, rotations, fixed arrays, exclusive borrows, service requirements, and
lexical provider replacement. `silk/hash` demonstrates cross-engine deterministic mixing, while
the existing service modules demonstrate that reusable policy belongs in ordinary source and only
irreducible host behavior belongs behind `Intrinsic`.

This change has no irreducible host behavior. Its whole implementation can therefore live in one
canonical portable module and exercise the same source pipeline as user code. The public sequence
must nevertheless be designed as a compatibility contract: reproducibility is useful only when an
implementation update cannot silently change the words produced by a seed.

## Goals / Non-Goals

**Goals:**

- Keep the provider contract minimal enough for scripted user implementations.
- Publish one high-quality, small-state, algorithm-named non-cryptographic provider using only
  existing portable operations.
- Make every derived operation's provider consumption and edge behavior deterministic.
- Prove behavior at the cheapest test tier while retaining one shared three-engine parity witness.

**Non-Goals:**

- Provider splitting or jumping, state serialization, implicit cloning, thread-safe sharing, or
  parallel stream assignment.
- Statistical benchmarking or a statistical test suite in the correctness path.
- A default or process-global provider, automatic seeding, OS entropy, or cryptographic security.

## Decisions

### The service owns one primitive: `nextU64`

`silk/random` will declare an exclusive `Random` service with one infallible operation:

```silk
pub service Random {
  effect fn nextU64() -> u64 ? &mut Random
}
```

The module will expose an ordinary `nextU64` wrapper plus `nextBool`, `below`, and `fillBytes`, all
with the same exclusive requirement. A provider implements only word generation; range policy and
byte presentation remain one canonical source implementation.

An alternative service with `nextBool`, bounded selection, and filling as provider operations would
duplicate policy in every provider and make scripted tests less precise. A concrete-generator-only
API would be simpler, but it would prevent application-level replacement and would make algorithms
instead of capabilities flow through program signatures.

### The concrete provider is named `Xoshiro256StarStar`

The module will expose a non-`Copy` provider containing four private `u64` state words and a
`seeded(seed: u64)` constructor. Naming the provider after xoshiro256** makes its exact sequence an
honest public contract and leaves room for future provider types without changing `Random`.

The transition follows the authors' public-domain reference implementation:

1. Output `rotateLeft(wrappingMultiply(state[1], 5), 7) * 9`, using wrapping multiplication for
   both products.
2. Save `state[1] << 17`.
3. Apply the XOR sequence `s2 ^= s0`, `s3 ^= s1`, `s1 ^= s2`, `s0 ^= s3`, then `s2 ^= saved`.
4. Rotate `s3` left by 45.

The [reference xoshiro256** implementation](https://prng.di.unimi.it/xoshiro256starstar.c) describes
it as a 256-bit all-purpose generator and recommends SplitMix64 expansion for a single 64-bit seed.
SplitMix64 advances by `0x9e3779b97f4a7c15`, then applies its two XOR-shift/multiply rounds with
`0xbf58476d1ce4e5b9` and `0x94d049bb133111eb`. Four successive outputs initialize the four provider
words. Seed zero is valid because it is an input to SplitMix64 rather than the raw xoshiro state.

The first four outputs for seed zero are fixed known answers:

```text
99ec5f36cb75f2b4
bf6e1f784956452a
1a5f849d4933e6e0
6aa594f1262d2d2c
```

SplitMix64 alone was considered because it needs one word of state, but xoshiro256** is the stronger
general-purpose provider and gives the standard library an algorithm whose state size and period are
comfortable for long-running applications. A cryptographic stream cipher was rejected for this
change because it would change both the security contract and the implementation scope.

### Bounded selection uses remainder rejection without `u128`

`below(upperExclusive)` returns `None` immediately for zero. For a positive bound it computes:

```text
threshold = wrappingSubtract(0, upperExclusive) % upperExclusive
```

It discards provider words smaller than `threshold`, then returns the accepted word modulo the
bound. The accepted cardinality is divisible by the bound, so a uniform provider gives each result
the same number of source words. This formulation needs only existing `u64` operations; a
multiply-high reduction would require a 128-bit product the language does not currently expose.

An unconditional remainder was rejected because it biases every bound that does not divide
`2^64`. Trapping on a zero bound was rejected because `Option` expresses the empty output domain
without adding a failure channel or advancing the provider.

### Boolean and byte mappings favor explicit reproducibility

`nextBool` consumes one word and tests bit 63. It does not cache the other 63 bits: caching would
add hidden state above the provider and make the next provider call depend on which derived
operation ran previously.

`fillBytes` consumes consecutive words and writes each least-significant byte first. It repeatedly
masks the low eight bits and shifts right by eight. A partial final chunk consumes one whole word;
an empty slice consumes none. Little-endian presentation is specified by the API rather than chosen
from the target, so native and Wasm output agree byte-for-byte.

### Tests use known answers and scripted providers, not statistics

The reference algorithm is proven with committed known-answer words for at least seeds zero and 42.
A scripted ordinary-source provider proves exclusive service replacement, exact consumption,
zero-bound behavior, rejection, boolean selection, and partial byte filling. These semantic claims
belong in existing evaluator-oriented standard-library and user-service tests.

One fixed-seed fingerprint program is shared by logical evaluation and direct WebAssembly and added
to `packages/compiler/test/support/corpus.ts` for the existing native differential. It is not given
a feature-local native compile/link loop or a timing assertion. Namespace, source navigation,
manifest, generated documentation, and absence of intrinsic privilege extend their existing global
tests rather than introducing duplicate harnesses.

### Canonical source and documentation remain the only editable implementation

`packages/compiler/stdlib/silk/random.silk` owns the service, provider, derived functions, and public
documentation. `packages/compiler/stdlib/manifest.json` registers `silk/random` as portable with
`Random` as its namespace and `Xoshiro256StarStar` as an alias. Generated TypeScript embedding and
reference documentation are rebuilt from that source; neither is edited as an independent
implementation.

No compiler catalog, semantic phase, HIR/MIR operation, evaluator host, backend branch, runtime ABI,
or target-availability entry changes. If implementation discovers that existing ordinary Silk
cannot express the design, that is a scope break requiring a separate language/compiler decision,
not permission to add an intrinsic here.

## Risks / Trade-offs

- [The stable sequence prevents silently replacing the algorithm] → Keep the provider
  algorithm-named; add a differently named provider in a future change instead of changing this
  sequence.
- [A malicious or broken provider can make rejection sampling run forever] → Document that bounded
  progress assumes the provider eventually returns an accepted word; use finite scripted rejection
  cases in correctness tests.
- [Users may mistake a statistically strong PRNG for cryptographic randomness] → Put the
  non-cryptographic warning in module, service, provider, constructor, and generated reference
  documentation, and expose no security-oriented operation names.
- [Service dispatch and source-level word expansion may be slower than a compiler primitive] → Keep
  correctness tests structural and functional; measure only in an opt-in benchmark before proposing
  any optimization.
- [Known-answer tests do not establish statistical quality] → Treat them as conformance tests for
  the published reference algorithm; statistical claims come from the selected algorithm, not from
  an unstable repository timing or randomness test.

## Migration Plan

This is additive. Generate the standard-library embedding and documentation after adding the source
and manifest entry, then land the module and its tests together. Rollback removes the new manifest
entry, canonical source, regenerated artifacts, and feature-specific tests; no existing source or
stored data requires migration.
