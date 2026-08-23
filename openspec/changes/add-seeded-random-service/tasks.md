## 1. Canonical Random Surface

- [x] 1.1 Add documented canonical `packages/compiler/stdlib/silk/random.silk` declarations for the exclusive `Random.nextU64` service operation, its public wrapper, the non-`Copy` `Xoshiro256StarStar` provider, and `seeded`; verify the compiler resolves and analyzes a minimal import with no diagnostics.
- [x] 1.2 Register `silk/random` as a portable manifest module with `Random` as its namespace and `Xoshiro256StarStar` as an alias; run `pnpm --filter @silk-effect/compiler stdlib:generate` and verify `pnpm --filter @silk-effect/compiler stdlib:check` succeeds.

## 2. Deterministic Provider

- [x] 2.1 Implement private SplitMix64 seed expansion with the specified wrapping constants and four state words; extend an existing evaluator-oriented standard-library test with seeds zero and 42 and verify the committed expanded-state or output vectors match the reference.
- [x] 2.2 Implement the xoshiro256** output and state transition plus the ordinary `Random` conformance; verify successive `nextU64` calls match the committed known-answer words and two providers constructed from one seed remain position-for-position equal.
- [x] 2.3 Extend existing user-service coverage with a renamed scripted provider and lexical `provideMut`; verify `Random.nextU64` advances only the supplied exclusive provider and no intrinsic or concrete-provider selection appears in the analyzed source path.

## 3. Derived Sampling Operations

- [x] 3.1 Implement `nextBool` from bit 63 of exactly one provider word; verify a scripted two-word provider returns `false` then `true` and records exactly two advances.
- [x] 3.2 Implement `below` with `None` for zero and the specified remainder-rejection threshold for positive bounds; verify zero consumes no word, a scripted biased-prefix word is discarded, accepted results stay below the exclusive bound, and no unconditional-remainder implementation passes the rejection case.
- [x] 3.3 Implement allocation-free little-endian `fillBytes`; verify empty, eight-byte, and partial-final-word cases write the specified bytes and consume zero, one, and the minimum covering number of provider words respectively.

## 4. Engine, Source, and Documentation Integration

- [x] 4.1 Add one fixed-seed fingerprint program to the existing evaluator/direct-Wasm parity coverage and to `packages/compiler/test/support/corpus.ts`; verify the targeted parity tests and existing native differential return the same fingerprint without a feature-local native compile loop.
- [x] 4.2 Extend existing standard-library namespace, source-resolution, and privilege coverage for `silk/random`; verify navigation reaches canonical `.silk` declarations, copied renamed source behaves equivalently, and intrinsic inventory and target imports remain unchanged.
- [x] 4.3 Complete module, service, provider, constructor, and operation doc comments with examples and explicit non-cryptographic warnings; run `pnpm --filter @silk-effect/compiler documentation:generate`, then verify `documentation:policy`, `documentation:check`, and `documentation:examples` succeed.

## 5. Repository Verification

- [x] 5.1 Run `pnpm typecheck` and resolve every failure introduced by the change, reporting any unrelated pre-existing failure separately.
- [x] 5.2 Run `pnpm exec biome check .` after type checking and resolve every formatting or lint failure introduced by the change.
- [x] 5.3 Run `pnpm test` after Biome and resolve every failing correctness test introduced by the change.
- [x] 5.4 Run `pnpm check` as the full repository handoff gate and record its exact result.
- [x] 5.5 Run `pnpm release:candidate` because the compiler package contents and shipped standard-library manifest change, and record its exact result.
