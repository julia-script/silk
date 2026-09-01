## Context

See [proposal.md](proposal.md) for motivation and the delta specs for the final behavioral
requirements. The repository now has the architectural seams established by the system and
monotonic-clock work:

- canonical standard-library modules and generated embeddings selected by a deterministic manifest;
- ordinary source services with lexical shared or exclusive provider replacement;
- a sealed `Intrinsic.Os` catalog lowered through the generic native-only `OsCall` representation;
- independently injected evaluator hosts with explicit missing-capability blockage;
- reachable-symbol validation for evaluator, LLVM, and direct WebAssembly; and
- reachability-selected C runtime fragments for the current 64-bit GNU/Linux and macOS targets.

The existing `silk/random` module is an ordinary, portable xoshiro256** implementation whose
provider primitive is `nextU64`. Its name is the only reason it appears secure; its implementation
and deterministic test vectors are worth preserving under an explicit insecure identity. The
existing native filesystem shim also has a private best-effort `silk_entropy` helper for unique
temporary-directory suffixes. That helper deliberately has a deterministic fallback because
directory exclusivity comes from `mkdirat`; it is not a CSPRNG and cannot implement this proposal.

The most important constraint is failure semantics. Once a provider claims the secure `Random`
contract, returning predictable or partially initialized output is worse than terminating. The
public service consequently has no recoverable error channel, while the one compiler-owned
primitive reports only success or failure to its canonical source provider.

## Goals / Non-Goals

**Goals:**

- Keep the public security distinction visible in module, service, provider, and documentation
  names.
- Make the secure service easy to replace with deterministic scripted providers in ordinary Silk
  tests without weakening the production-provider contract.
- Cross the compiler/runtime boundary exactly once per secure byte-fill request and derive every
  scalar/distribution operation in source.
- Preserve deterministic evaluation unless a random host is explicitly injected, and preserve
  reachable-only target validation and native runtime cost.
- Give `InsecureSeed` immutable shared-provider semantics so reading it cannot accidentally become
  a general random stream.

**Non-Goals:**

- Integrating the new two-word `Seed` into `HashSeed`, `HashMap`, `HashSet`, or the scheduler. Their
  current one-word hashing design requires a separate keyed-hashing proposal before a meaningful
  hash-flood-resistance claim can be made.
- Exposing entropy-pool state, native error codes, short reads, provider algorithms, or seeding
  controls through the portable API.
- Statistical test suites, entropy estimation, deterministic record/replay of production bytes, or
  an ambient process-global provider.
- Windows, direct-WebAssembly, WASI Preview 2/3, browser, or a new Unix target triple.

## Decisions

### Split the API into four ordinary source modules

The standard-library manifest contains these canonical modules:

| Module                 | Provider mode                   | Responsibility                                  |
| ---------------------- | ------------------------------- | ----------------------------------------------- |
| `silk/random`          | exclusive `&mut Random`         | secure exact fill and source-derived operations |
| `silk/os_random`       | stateless exclusive provider    | official evaluator/native secure boundary       |
| `silk/insecure_random` | exclusive `&mut InsecureRandom` | renamed deterministic xoshiro256** stream       |
| `silk/insecure_seed`   | shared `&InsecureSeed`          | one immutable 128-bit seed                      |

`silk/random` defines one service operation:

```silk
pub service Random {
  effect fn fillBytes(output: &mut [u8]) -> () ? &mut Random
}
```

The module exposes same-named `fillBytes`, plus `nextU64`, `nextBool`, and `below` wrappers. A
provider receives exclusive authority because filling caller storage is an exclusive operation and
scripted providers normally advance private state. `OsRandom` remains stateless but implements the
same shape uniformly. Importing or constructing it performs no host call and installs no provider.

`silk/insecure_random` is the current source moved as one unit, with `Random` renamed to
`InsecureRandom` and documentation/imports updated. Its `nextU64` primitive, byte consumption,
bounded selection, xoshiro sequence, and `Xoshiro256StarStar` name remain otherwise unchanged.
There is no forwarding `silk/random.seeded`, alias service, or deprecated module because this is a
green-field breaking migration.

`silk/insecure_seed` defines a private-field, explicitly `Copy` `Seed` with two `u64` accessors and
a shared service:

```silk
pub service InsecureSeed {
  effect fn get() -> Seed ? &InsecureSeed
}
```

`impl Copy for Seed {}` makes returning a stored value through a shared provider legal without
exposing its fields or inventing mutation. An immutable `FixedInsecureSeed` provider backs
`fixed(first, second)`. `fromRandom` obtains two
successive `Random.nextU64` values once and returns an immutable provider containing that pair.
Subsequent `get` calls are pure shared reads and require no `Random`. Keeping `InsecureSeed` shared
communicates that it is configuration, not advancing state. A single combined random service and a
mode flag were rejected because they make security a runtime convention instead of a type-level
capability distinction.

### Exact fill is the secure primitive; scalar operations have a canonical source encoding

The secure service uses exact fill instead of WASI's short-read list because Silk already has
borrowed mutable slices and callers overwhelmingly need initialized storage. Exact fill avoids an
allocation and prevents every caller from having to write a security-sensitive accumulation loop.
It also makes malformed scripted/evaluator output observable at one boundary. The public wrapper
returns before service dispatch for an empty slice, and every conforming provider must independently
leave an empty request untouched because callers can dispatch the public service operation directly.

`nextU64` fills a local eight-byte array and folds it in least-significant-byte-first order. This
encoding is specified independently of machine endianness and matches the existing insecure byte
mapping. `nextBool` tests bit 63 of one such word. `below` retains the existing complete-word
rejection algorithm, including no consumption for a zero bound. There is no second native `u64`
operation: it would duplicate policy and make scripted byte consumption disagree between APIs.

Making `nextU64` the secure provider primitive was considered because it would reuse the insecure
service shape, but filling large buffers would then perform one effect dispatch per eight bytes and
would make byte-oriented host APIs unnatural. Returning an owned list with a short-read length was
rejected because it adds allocation and partial-progress policy without adding security.

### Admit one native-only intrinsic with a success-commit Boolean ABI

Add one unsafe `Intrinsic.osRandomFill` operation with the conceptual signature:

```silk
Intrinsic.osRandomFill(output: &mut [u8]) -> Effect<bool>
```

It is admitted for evaluator and LLVM, not direct WebAssembly, and has the sole canonical consumer
`silk/os_random.fillBytes`. The existing slice lowering passes its data pointer and length to the
native symbol:

```text
silk_os_random_fill_v1(unsigned char *output, size_t length) -> int32_t
```

The four-byte `int32_t` result is the canonical Silk Boolean lane. A true result guarantees the
entire output has been initialized with CSPRNG bytes. A false result exposes no error code and the
source provider immediately enters the existing fatal arithmetic trap. The native implementation
may leave the output clobbered, including writing a prefix, before failure; no continuing Silk code
can observe or rely on that state because the provider traps instead of returning. Evaluator hosts
stage and validate their complete result before copying it into Silk memory. Zero length returns
true without consulting the OS.

The intrinsic owns only memory-safe boundary transport and target availability. Service identity,
cryptographic policy, seed construction, scalar decoding, distribution selection, and fatal policy
remain ordinary source. A native error enum or byte-count result was rejected because the public
contract cannot safely recover from either, while a compiler-known `Random` actor would violate the
minimal-privilege boundary.

`BootstrapOsIntrinsics.execute` dispatches this compact operation before the filesystem protocol's
reason/native-code output assumption, just as clock operations do. HIR/MIR, intrinsic inventory,
LLVM declarations, and runtime-symbol selection gain only the one operation/symbol.

### GNU/Linux uses nonblocking `getrandom`; macOS uses `arc4random_buf`

The selected runtime fragment includes only the headers required by random support. On GNU/Linux it
fills the destination with `getrandom(..., GRND_NONBLOCK)`. Each call is capped at the platform's
documented maximum transferable count, advances by every positive short read, retries `EINTR`, and
returns false on `EAGAIN`, zero, or any other error. `GRND_NONBLOCK` means the provider never waits
for kernel entropy initialization: a system that cannot yet uphold the unpredictability contract
terminates rather than blocking or falling back. Computation and copying remain proportional to the
requested length; “non-waiting” specifically forbids suspension for external entropy readiness, not
ordinary synchronous CSPRNG work. The libc wrapper pins the supported GNU/Linux baseline at glibc
2.25 and Linux 3.17 or later. Raising that baseline is explicitly preferable to a private raw-syscall
ABI or opening `/dev/urandom` with weaker early-boot semantics.

On macOS the fragment calls `arc4random_buf` once for the complete slice and returns true. Apple
documents it as a fast, automatically seeded and fork-reseeded CSPRNG that is always successful,
which matches the infallible exact-fill surface better than looping the kernel seeding primitive
`getentropy`. The void platform API is not hiding a recoverable failure: catastrophic initialization
failure is fatal below the ABI, and the shim's success result records completion of the call. This
path needs no extra framework link and is available on the currently supported macOS target.

Linux `arc4random_buf` was considered but would raise the glibc baseline to 2.36 or add a dependency.
`/dev/urandom` was rejected for the secure provider because its early-boot behavior and descriptor
lifecycle make the non-waiting/unpredictable contract harder to state. Reusing filesystem
`silk_entropy` was rejected because its deterministic fallback is intentionally non-cryptographic.
Windows APIs and WASI imports are separate provider implementations for later proposals, not
branches hidden behind this symbol.

The random C fragment stays reachability-selected and independent of filesystem support. A program
that uses only `InsecureRandom` or a fixed `InsecureSeed` neither emits nor links the symbol.

### The evaluator receives bytes only through an injected RandomHost actor

Add a public `RandomHost.ts` boundary actor whose host operation accepts a requested `usize` length
and returns either an immutable byte sequence of exactly that length or one member of a closed
failure category (`ExplicitFailure`, `Exhausted`, `Underfill`, `Overfill`, `InvalidByte`, or
`HostThrew`). `scripted` receives one immutable byte chunk per expected host call, so exhaustion and
per-call length mismatches are unambiguous. Construction rejects out-of-range scripted values;
execution stages the returned chunk, validates its exact length and byte range, and only then commits
the caller buffer. Production callers that deliberately want host randomness must adapt it outside
the compiler and inject the actor through `BootstrapEvaluation.Options.randomHost`. The actor is
exported from the compiler barrel and a package subpath like the other reusable host boundaries.

The evaluator never falls back to `Math.random`, Web Crypto, Node crypto, a clock, object identity,
or process state. If `OsRandom` is reachable and no host was injected, planning/evaluation records a
new `MissingRandomHost` blocked reason. The reason is threaded through `BootstrapTrace`, inspector
presentation, and flow views consistently with the existing independent clock and filesystem hosts.

An `OsCall` trace records operation identity, requested length, success/failure, call-site source
provenance, and only the closed failure category. Returned messages, arbitrary failure payloads, and
thrown values are intentionally discarded at this security boundary because any could contain the
generated bytes. The same redaction holds in raw trace data, inspector presentation, flow models,
and serialized snapshots. A convenience ambient crypto host was rejected because it would make
identical evaluator inputs nondeterministic and would silently grant authority.

### Keep direct WebAssembly rejection reachable-only

The intrinsic target set excludes `Wasm`, so the existing planner emits the stable target-unavailable
diagnostic when `OsRandom.fillBytes` remains reachable. Merely resolving, analyzing, importing, or
dead-code-eliminating the provider does not reject a direct-Wasm artifact. Portable secure code with
an ordinary source provider remains valid on direct Wasm; only this official OS provider is absent.

Adding a WASI import now was rejected because the direct-Wasm backend has no general component-model
host integration and the requested first version explicitly excludes it. This separation lets a
future `WasiRandom` provider implement the same portable `Random` service without changing the
service or intrinsic catalog.

### Tests separate deterministic semantics from native smoke coverage

The current random service tests and differential corpus move to `InsecureRandom`, preserving their
known-answer vectors and evaluator/Wasm coverage. One shared random-capabilities source snapshot
proves secure scripted providers, exact empty/nonempty fills, little-endian `u64`, Boolean mapping,
rejection sampling, fixed and randomly initialized insecure seeds, shared provider replacement, and
the absence of extra reads.

Compiler-level tests prove exactly one admitted intrinsic, evaluator host injection, missing-host
blockage, unchanged sentinel storage after every staged evaluator failure, trace redaction against a
secret-bearing canary, direct-Wasm portable/unused/reachable cases, public package exports, and the
smallest runtime-symbol inventory. A deterministic C harness forces and compiles both platform
branches: Linux proves the count cap, `GRND_NONBLOCK` flag, short-read pointer/count advancement,
`EINTR` retry, `EAGAIN` failure, zero length, and incomplete-output failure; macOS proves one exact
`arc4random_buf` call and unconditional success. Native acceptance uses a deterministic ordinary
provider in the shared evaluator `source` and an `OsRandom`-backed `nativeSource` with the same
status result. It does not assert two outputs differ, measure entropy, set timing bounds, or add
per-feature fresh-process tests.

Generated standard-library embeddings, navigation/docs, diagnostic catalogs, and intrinsic/runtime
inventories are regenerated through their owning commands. Focused test files run after each module
or boundary milestone; typecheck and Biome run after the portable source migration, and the complete
required repository gates run after native/evaluator integration. The macOS native CI job is
generalized beyond clocks to compile/run the random harness and native acceptance case as well.

## Risks / Trade-offs

- [GNU/Linux baseline rises from glibc 2.17 to 2.25 for programs that reach `OsRandom`] → Document
  the provider-specific baseline, retain reachable-only linkage, and keep portable/insecure programs
  on the existing baseline.
- [A nonblocking Linux request can fail during very early boot] → Treat `EAGAIN` as fatal and never
  substitute weak bytes; ordinary deterministic providers remain available where secure authority is
  intentionally absent.
- [The infallible public surface terminates on host or OS failure] → Keep the boundary Boolean and
  tests explicit, document the policy, and provide no misleading recovery that could downgrade
  security.
- [A failing native call may mutate an unobservable slice prefix before trapping] → Guarantee only
  success-commit semantics, stage evaluator results, and test that no failure path returns to source.
- [A scripted provider can satisfy the `Random` type without cryptographic security] → Document that
  deterministic providers are test-only; explicit lexical injection is necessary for testability,
  and the official production provider is the only implementation making an OS-backed claim.
- [One-shot intent cannot prevent repeated service calls at the type level] → Make the provider
  immutable/shared and stable so repeated reads cannot turn the capability into a stream; document
  initialization-only use.
- [The new `Seed` is not yet consumed by current hash collections] → Keep integration explicitly out
  of scope and avoid claiming protection until keyed hashing and collection construction are designed
  together.

## Migration Plan

1. Move the existing source and every internal consumer from `silk/random.Random` to
   `silk/insecure_random.InsecureRandom`; update the manifest, generated artifacts, docs, corpus, and
   deterministic tests atomically. No obsolete identity remains.
2. Add and verify the portable secure service and insecure-seed source modules using ordinary
   scripted/fixed providers before introducing platform code.
3. Add the single intrinsic, evaluator host, diagnostics/inspection, native declaration, and
   reachability-selected runtime fragment; then add `silk/os_random` as its sole source consumer.
4. Regenerate owned artifacts and run focused suites at each boundary, followed by the repository's
   full typecheck, format/lint, test, check, and release-candidate gates required for changed package
   contents.

Because the repository is green-field, rollback is a source-level revert of the atomic change, not
a compatibility alias or dual API. A platform defect may be fixed inside `OsRandom` without changing
the portable service; it must not be mitigated by restoring deterministic output under `Random`.
