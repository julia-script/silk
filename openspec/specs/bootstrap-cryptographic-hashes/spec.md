# bootstrap-cryptographic-hashes Specification

## Purpose

Define portable, fixed-output SHA hashing in the compiler-shipped Silk standard library, including
the public streaming lifecycle, standardized variants, security guidance, and cross-target behavior.

## Requirements

### Requirement: The standard library exposes the standardized fixed-output SHA families

The standard library SHALL ship canonical modules `silk/sha1`, `silk/sha2`, and `silk/sha3`.
`silk/sha1` SHALL expose `Sha1` with a `[u8; 20]` digest. `silk/sha2` SHALL expose `Sha224`,
`Sha256`, `Sha384`, `Sha512`, `Sha512_224`, and `Sha512_256` with `[u8; 28]`, `[u8; 32]`,
`[u8; 48]`, `[u8; 64]`, `[u8; 28]`, and `[u8; 32]` digests respectively. `silk/sha3` SHALL
expose `Sha3_224`, `Sha3_256`, `Sha3_384`, and `Sha3_512` with `[u8; 28]`, `[u8; 32]`,
`[u8; 48]`, and `[u8; 64]` digests respectively.

The returned byte arrays SHALL use each algorithm's standardized digest byte order and SHALL match
the fixed-output algorithms defined by FIPS 180-4 and FIPS 202. SHAKE and other extendable-output
functions MUST NOT be exposed by these actors.

#### Scenario: Import every SHA-2 variant

- **WHEN** a program imports each named actor from `silk/sha2` and hashes one byte sequence
- **THEN** every call resolves through the canonical module and returns its exact fixed-array digest type

#### Scenario: Distinguish fixed-output SHA-3 from SHAKE

- **WHEN** a program imports the four named actors from `silk/sha3`
- **THEN** each actor produces its specified fixed-size digest and no SHAKE actor is present

### Requirement: Every SHA actor supports one streaming lifecycle

Every SHA actor SHALL expose a pure `make() -> Self`, an inherent
`update(self: &mut Self, bytes: &[u8])` method, a consuming `finish(self: Self)` method returning
the actor's exact digest type, and a one-shot `hash(bytes: &[u8])` member returning the same digest
type. `update` SHALL absorb bytes in call order, SHALL accept an empty slice without changing the
digest, and SHALL permit any chunking that represents the same byte sequence. `finish` SHALL consume
the state so a finalized state cannot be updated or finalized again.

#### Scenario: Hash incrementally through the inherent update method

- **WHEN** a mutable state receives a message through multiple `state.update(bytes)` calls and is finished
- **THEN** its digest equals a one-shot hash of the concatenated bytes in the same order

#### Scenario: Ignore empty updates

- **WHEN** empty updates occur before, between, or after non-empty updates
- **THEN** the resulting digest equals the digest produced without those empty updates

#### Scenario: Consume state at finalization

- **WHEN** source finishes an owned state and then attempts to use that state again
- **THEN** ownership analysis rejects the later use

### Requirement: Streaming padding and length accounting cover the complete admitted domains

SHA-1, SHA-224, and SHA-256 states SHALL preserve and encode the complete unsigned 64-bit message
bit length required by their standards. SHA-384, SHA-512, SHA-512/224, and SHA-512/256 states SHALL
preserve and encode the complete unsigned 128-bit message bit length required by their standards.
An update that would exceed an algorithm's representable bit length MUST trap rather than silently
wrap. Finalization SHALL apply the standardized padding correctly for every message length,
including lengths immediately before, at, and after a padding spill boundary.

#### Scenario: Carry a SHA-512-family length across the low word

- **WHEN** cumulative SHA-512-family length accounting carries out of the low 64-bit word
- **THEN** the high word advances and finalization encodes the resulting 128-bit bit length

#### Scenario: Reject an algorithmically overlong message

- **WHEN** an update would make a SHA-1 or SHA-2 state exceed its standardized bit-length domain
- **THEN** execution traps instead of producing a digest for a wrapped length

#### Scenario: Finalize at padding boundaries

- **WHEN** a message ends immediately before, at, or after a family-specific padding spill boundary
- **THEN** finalization produces the standardized digest for that exact message

### Requirement: SHA implementations remain portable ordinary Silk source

The SHA modules SHALL be allocation-free ordinary Silk source built from existing scalar, fixed
array, slice, mutation, loop, and static-evaluation semantics. They MUST NOT add or depend on a
SHA-specific intrinsic, compiler-known standard-library actor, runtime provider, service, Effect
requirement, operating-system API, native crypto library, or target-specific implementation.

#### Scenario: Navigate to a SHA operation

- **WHEN** tooling resolves a SHA constructor, update, finish, or one-shot hash operation
- **THEN** go-to-definition opens its canonical `.silk` declaration and ordinary Silk implementation

#### Scenario: Compile one program for portable targets

- **WHEN** the same SHA program is analyzed and emitted for LLVM-generated native and direct WebAssembly targets
- **THEN** both targets use the same ordinary Silk implementation and the direct-WebAssembly artifact has no external crypto import

### Requirement: Validation covers every actor and incremental boundary class

Repository verification SHALL compare every admitted actor against independently sourced
known-answer digests for empty and non-empty inputs. The shared native acceptance corpus SHALL also
cover empty updates, repeated small updates, one-shot versus segmented equivalence, SHA-1/SHA-2
block padding boundaries, and each distinct SHA-3 rate boundary without duplicating compiler
pipelines for equivalent claims.

#### Scenario: Verify all admitted variants

- **WHEN** the standard-library acceptance suite runs
- **THEN** empty and non-empty known-answer cases pass for all eleven actors

#### Scenario: Verify incremental boundaries economically

- **WHEN** the shared native corpus runs its consolidated SHA cases
- **THEN** the distinct chunking, padding, and rate-boundary claims are exercised without a separate test process per variant

### Requirement: SHA documentation teaches selection and lifecycle constraints

Public source and generated reference documentation SHALL list every digest size, demonstrate
streaming and one-shot calls, explain that `finish` consumes state, distinguish cryptographic SHA
actors from the collection-key hashing in `silk/hash`, and identify SHA-1 as legacy interoperability
only and unsuitable for new collision-resistant uses.

#### Scenario: Choose an algorithm from the generated reference

- **WHEN** a reader opens a generated SHA module page
- **THEN** the page identifies the variants and output sizes and shows a compilable streaming or one-shot example

#### Scenario: Inspect SHA-1 guidance

- **WHEN** a reader opens the `Sha1` documentation
- **THEN** the documentation clearly limits it to legacy interoperability and directs new collision-resistant uses to SHA-2 or SHA-3
