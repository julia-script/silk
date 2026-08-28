## Why

Silk currently gives the unqualified `Random` name to a deterministic non-cryptographic stream and
has no capability for unpredictable system randomness or the one-time seed used to harden language
hash tables. The public names should communicate their security contract, and the runtime should
offer the same three-way distinction as WASI random without making any standard-library actor
compiler-known.

## What Changes

- **BREAKING** Rename the existing `silk/random.Random` service and its xoshiro256** provider module
  to `silk/insecure_random.InsecureRandom`; update every caller, test, generated artifact, and
  document with no alias, forwarding module, or compatibility provider.
- Add portable `silk/random` with an infallible, provider-replaceable `Random` service whose one
  primitive exactly fills a borrowed byte slice with fresh unpredictable CSPRNG data; derive `u64`,
  boolean, and unbiased bounded sampling in ordinary Silk source.
- Add native `silk/os_random` with a stateless `OsRandom` provider backed by one unsafe native-only
  exact-fill intrinsic and a reachability-selected C runtime symbol.
- Add portable `silk/insecure_seed` with a 128-bit `Seed`, a shared `InsecureSeed` service that
  returns one provider-stable seed, fixed construction for deterministic environments, and an
  ordinary constructor that samples a seed once through `Random`.
- Support the current Linux GNU and macOS native targets and injected evaluator hosts. Reject a
  reachable OS-random operation for direct WebAssembly; do not add WASI, browser, or Windows
  support in this version.
- Keep all providers explicit. Importing a module installs no ambient random capability, and an
  environment unable to uphold the secure contract omits `Random` rather than returning
  deterministic bytes under that name.

## Capabilities

### Modified Capabilities

- `bootstrap-random`: Replace the former seeded `Random` contract with final public contracts for
  secure `Random`, deterministic `InsecureRandom`, one-time `InsecureSeed`, their derived
  operations, provider replacement, and supported-target behavior.
- `bootstrap-intrinsic-boundary`: Admit one target-neutral native exact-fill primitive and no
  compiler-known random distribution, generator, service, or seed abstraction.
- `bootstrap-evaluation`: Accept an explicitly injected random-byte host, preserve deterministic
  scripted evaluation, and block reachable OS-random calls when the host is absent.
- `bootstrap-silk-stdlib`: Ship the renamed insecure module, secure portable module, native secure
  provider, and insecure-seed module as canonical documented ordinary source.

## Impact

The change affects standard-library source identities and imports, the manifest and generated
embedding/documentation, intrinsic/HIR/MIR inventories, evaluator options and blocked reasons, the
native LLVM declaration and reachability-selected C shim, native artifact tests, user-service tests,
and the global evaluator/Wasm/native differential corpus. It adds no package dependency and no new
target triple. Programs using the current `silk/random` API must move to `silk/insecure_random`.
