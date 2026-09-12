# owned-trust-snapshots Specification

## Purpose

Define bounded independently owned trust snapshots and lexical replaceable trust sources without implicit native acquisition, deduplication, or authentication policy.

## Requirements

### Requirement: Snapshot limits are finite, inclusive, and checked

The standard library SHALL expose `SnapshotLimits` with anchor-count and aggregate encoded-byte bounds and `TrustLoadLimits` with strict certificate-decoder plus snapshot bounds. Defaults SHALL permit 1024 anchors, 8 MiB aggregate encoded bytes, and the existing strict certificate decoder defaults. Every bound SHALL be inclusive; zero SHALL mean zero; count, addition, layout, and growth arithmetic MUST be checked before allocation or mutation. Aggregate accounting SHALL use each `TrustAnchor.encodedBytes` value.

#### Scenario: Exact aggregate boundary

- **WHEN** an input has exactly the configured anchor count and aggregate encoded-byte length
- **THEN** snapshot construction succeeds without weakening either inclusive bound

#### Scenario: Overflow cannot bypass a budget

- **WHEN** aggregate encoded-byte accounting cannot be represented or exceeds its configured limit
- **THEN** construction returns the corresponding structured limit error and publishes no snapshot

### Requirement: Snapshots preserve every independently constrained authority

`TrustSnapshot.fromAnchors` SHALL consume one `Vector<TrustAnchor>` without allocation, preserve every entry and duplicate in order, and accept an explicit empty vector as “trust nobody.” A successful snapshot SHALL be opaque and immutable; its borrowed anchors MUST NOT outlive it. The operation SHALL not inspect certificate profiles, validate paths or identities, deduplicate, acquire host roots, or perform I/O.

#### Scenario: Identical certificates remain distinct

- **WHEN** two anchors contain identical certificate DER but different configured restrictions
- **THEN** both anchors remain in input order with their independent restrictions intact

#### Scenario: Empty explicit trust

- **WHEN** the caller constructs a snapshot from an empty anchor vector under zero limits
- **THEN** construction succeeds with a zero-length anchor view and does not select another trust source

### Requirement: PEM import transfers strict decoded certificate ownership atomically

`CertificateBundle.intoCertificates` SHALL consume a complete bundle and return its owned certificate vector without reparsing or DER copying. `TrustSnapshot.fromPem` SHALL use the existing strict `CertificateBundle.decodePem`, move each certificate into `TrustAnchor.fromCertificate`, and then apply the same snapshot constructor. It SHALL preserve order and duplicates, attach no configured restrictions, expose `DecodeError` through the semantic trust-source error, and expose allocator refusal separately. Empty or whitespace-only PEM SHALL remain invalid.

#### Scenario: Malformed later PEM block rolls back

- **WHEN** a valid certificate block is followed by a malformed or unsupported block
- **THEN** PEM import returns the structured decoder failure, releases accumulated owners, and publishes no partial snapshot

#### Scenario: Allocator refusal stays distinct

- **WHEN** strict decoding or anchor-vector construction cannot allocate
- **THEN** the operation reports `OutOfMemoryError` instead of classifying the input as malformed

### Requirement: Copy and combination create independent owners

`TrustSnapshot.copy` SHALL clone every anchor into a new owner. `TrustSnapshot.combine` SHALL append independent copies of every additional anchor after every primary anchor. Both operations SHALL preserve order, duplicates, certificate bytes, configured path lengths, and configured NameConstraints. Both SHALL validate their complete result against the supplied limits before publishing it, expose allocation refusal separately, leave borrowed inputs unchanged, and release partial output on every failure.

#### Scenario: Combined alternatives preserve restrictions

- **WHEN** primary and additional snapshots include duplicate certificate DER with distinct restrictions
- **THEN** the combined snapshot preserves each authority as a separate ordered alternative without merging its restrictions

#### Scenario: Independent lifetime

- **WHEN** a copied or combined snapshot remains after every input snapshot is dropped
- **THEN** its complete anchors and configured restrictions remain valid from independent storage

### Requirement: Trust-source errors preserve semantic recovery information

The public `TrustSourceError` SHALL distinguish decoder failures, anchor-count/encoded-byte/native-input limits, invalid native configuration, and native open/read/close failures. Decoder locations and `FileError` codes SHALL remain intact. Portable snapshot and memory-source operations SHALL use only the variants applicable to their scope and MUST NOT collapse allocator refusal into `TrustSourceError`.

#### Scenario: Count limit is recoverable by kind

- **WHEN** snapshot construction receives more anchors than its inclusive limit
- **THEN** it returns `LimitExceeded` with `AnchorCount` and the configured bound

#### Scenario: Decoder location survives translation

- **WHEN** PEM import rejects malformed certificate data at a known decoder location
- **THEN** `TrustSourceError.Decode` retains the complete `DecodeError` value

### Requirement: TrustSource is a lexical replaceable capability

The standard library SHALL expose a `TrustSource` service whose mutable `load` operation receives `TrustLoadLimits`, uses the caller-supplied allocator, and returns one independently owned `TrustSnapshot`. Nested lexical providers SHALL replace and restore the active source through normal Effect composition. A load SHALL not observe time, environment variables, global state, caches, watchers, or implicit platform roots.

#### Scenario: Nested provider restoration

- **WHEN** a load runs under a nested provider and a later load runs after that lexical scope ends
- **THEN** the nested load uses only the nested source and the later load uses the restored outer source

#### Scenario: Failed load leaves held owners valid

- **WHEN** a provider load fails after the caller already holds a prior successful snapshot
- **THEN** the prior snapshot remains unchanged and valid

### Requirement: MemoryTrustSource replaces only future loads

`MemoryTrustSource.make` SHALL consume one complete snapshot without allocation. Its `TrustSource.load` implementation SHALL serialize through the mutable service receiver and copy the current snapshot under caller limits. `MemoryTrustSource.replace` SHALL atomically consume a complete next snapshot and return the complete old snapshot without allocation, I/O, or failure. A successful replacement SHALL affect only later loads; failed loads MUST NOT change provider state; earlier loaded snapshots SHALL survive replacement and provider drop.

#### Scenario: Successful replacement affects later load only

- **WHEN** a caller retains a loaded snapshot, replaces the provider snapshot, and loads again
- **THEN** the first owner keeps the old anchors and the second owner receives the new anchors

#### Scenario: Load limit failure is non-mutating

- **WHEN** the active memory snapshot exceeds the requested load limits
- **THEN** load returns the structured limit error and the provider keeps its complete current snapshot

### Requirement: Delivery evidence remains portable and economical

The implementation SHALL remain ordinary Silk source with no compiler-known trust actor. Repository evidence SHALL use one shared native corpus program for target-neutral behavior and distinguishing allocator failures, one combined structured ownership/requirement test, one compact LLVM-to-Wasm portability witness, deterministic existing certificate fixtures, and exact pull-request CI native-smoke selection. Generated documentation SHALL distinguish explicit trust storage from certificate-path, identity, revocation, and OS-policy authentication.

#### Scenario: Cross-target portable behavior

- **WHEN** the focused native corpus case and compact Wasm witness evaluate the public actors
- **THEN** bounded construction, ownership, replacement, and error behavior agree with their fixed expected outcomes

#### Scenario: Documentation avoids platform equivalence

- **WHEN** a reader consults the public module comments or generated reference
- **THEN** it does not describe explicit PEM membership or a memory snapshot as equivalent to host trust policy
