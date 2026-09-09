## Context

See `proposal.md` for motivation and
`specs/bootstrap-cryptographic-hashes/spec.md` for the behavioral contract. The compiler already
ships ordinary Silk modules from a deterministic manifest, and the language already provides the
fixed arrays, slices, mutable receivers, consuming receivers, `u32`/`u64` wrapping arithmetic,
rotations, loops, and static sequences needed by the FIPS algorithms. It does not provide `u128`,
so SHA-512-family length accounting must use two words without changing the scalar model.

The closest implementation reference is Zig's source-only SHA family at commit
`e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`. It validates that the algorithms need no host service,
but Silk's public API and implementation must follow Silk ownership, documentation, and testing
conventions rather than reproduce Zig's generic API.

## Goals / Non-Goals

**Goals:**

- Keep every public algorithm state concrete, stack-storable, allocation-free, and directly
  navigable to canonical Silk source.
- Share compression or permutation machinery inside each family while retaining exact concrete
  public digest types.
- Make incremental operation the primitive behavior and implement one-shot hashing through the
  same state lifecycle.
- Keep runtime verification in one economical native acceptance surface with independent expected
  bytes.

**Non-Goals:**

- A cross-family interface, algorithm erasure, dynamic digest sizes, or caller-selected truncation.
- A block-at-a-time public API, state cloning, non-consuming digest inspection, or reusable
  finalized states.
- Target-specific acceleration or compiler changes made solely to optimize these algorithms.

## Decisions

### Use three canonical modules and one concrete actor per standardized digest

`sha1.silk`, `sha2.silk`, and `sha3.silk` mirror the standards families and receive manifest
identities `silk/sha1`, `silk/sha2`, and `silk/sha3`. Every public actor owns its state and exposes
the same four inherent members. Public wrappers use private family state and helpers where sharing
does not weaken the concrete digest type.

This is preferable to one generic `Sha` actor because output length and initialization parameters
are compile-time API facts, while Silk has no need for runtime algorithm selection here. Separate
files per variant would make common compression and padding logic harder to audit and increase
documentation/catalog surface without adding a capability.

### Make byte-wise absorption the shared streaming substrate

`update` advances through the borrowed slice once, fills the family block buffer (or the SHA-3
rate portion of the lane state), and invokes the compression/permutation exactly when the active
block becomes full. Empty slices naturally perform no transition. `hash` constructs a state,
updates it once, and finishes it, so it cannot diverge from streaming behavior.

A bulk-copy fast path is deliberately deferred. The byte-wise path is simpler to express with the
current slice API, covers arbitrary alignment and chunking uniformly, and keeps the first portable
implementation auditable. Direct block processing can be introduced later without changing the
contract if benchmarks justify it.

### Use circular message schedules for SHA-1 and SHA-2

SHA-1 and both SHA-2 word families keep a 16-word circular schedule, load source words in big-endian
order, and extend the schedule in place during compression. Round constants are represented through
private static sequences so the compiler can generate finite ordinary operations without a runtime
allocation or a compiler-known hash operation.

Full 80-word or 64-word schedule arrays would be easier to compare line-for-line with some
references but consume more state/stack space and do not improve the public contract. The circular
form is standardized, common, and keeps the implementation compact.

### Represent bit lengths explicitly and reject overflow

SHA-1 and the 32-bit SHA-2 family store one `u64` bit count. Each update converts its slice length,
checks that multiplying by eight and adding it remain representable, and traps on overflow. The
64-bit SHA-2 family stores `bitLengthHigh` and `bitLengthLow`; it adds `byteLength << 3` to the low
word, adds `byteLength >> 61` plus the low-word carry to the high word, and traps if the high word
would overflow. Finalization writes the count in big-endian order without changing the logical
message length.

Counting bytes and multiplying only at finalization was considered, but a two-word bit count maps
directly to the encoded 128-bit field and makes overflow behavior explicit at each update.

### Absorb SHA-3 directly into a 25-lane Keccak state

SHA-3 actors keep 25 `u64` lanes and a byte offset within their variant's rate. Each input byte is
XORed into the appropriate little-endian lane position. A full rate triggers Keccak-f[1600].
Finalization applies domain byte `0x06`, sets the final rate bit `0x80`, permutes once, and extracts
the fixed digest bytes from the rate in little-endian lane order. Every admitted digest is shorter
than its rate, so no public multi-squeeze machinery is required.

Keeping a second rate-sized byte buffer was rejected because direct lane absorption represents the
sponge state faithfully and avoids redundant storage and copies.

### Validate public behavior through generated docs and one shared runtime corpus

Source comments own the public teaching material and contain a concise complete-module example;
the repository's existing generators update the embedded standard-library table and module-based
reference. Runtime cases hardcode independently obtained empty and `abc` expected bytes for all
eleven actors, then add consolidated family representatives for empty chunks, repeated small
chunks, and every distinct padding/rate boundary. Structured analysis tests are added only if they
prove a compile-time ownership or method-resolution claim that runtime compilation cannot
distinguish cheaply.

Per-variant test files and fresh-process tests were rejected because they repeat expensive compiler
pipelines. One shared corpus entry can return a compact pass/fail value while exercising all
runtime claims.

## Risks / Trade-offs

- **[Large correctness-sensitive source change]** → Keep family cores local, derive constants from
  published standards/reviewed source, validate every variant against independent vectors, and use
  boundary cases that force both one-block and two-block finalization paths.
- **[Byte-wise updates may be slower than bulk block processing]** → Treat this release as the
  portable correctness baseline and optimize only behind the unchanged API after measurement.
- **[Two-word SHA-512 length arithmetic is easy to get subtly wrong]** → Isolate the transition,
  check both carry sites, expose no unchecked path, and add a focused structural test of the carry
  calculation in addition to digest vectors where feasible.
- **[Generated artifacts can drift from canonical source]** → Use the existing generators and run
  normal documentation, manifest-integrity, package, and release-candidate checks.
- **[SHA-1 availability may encourage new insecure use]** → Put the legacy warning on the module,
  actor, and generated reference while keeping SHA-2 and SHA-3 equally discoverable.

## Migration Plan

This is additive: no existing module or public declaration changes. Add and validate canonical
sources first, regenerate the deterministic embedding and documentation, then ship all artifacts in
one package revision. Rollback removes the three manifest entries, canonical files, generated
artifacts, tests, and this change together; no stored data or compatibility migration is involved.
