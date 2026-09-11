## Purpose

Define explicit bounded native PEM-file trust acquisition as an owned replaceable `TrustSource` without implying operating-system trust-policy equivalence.

## ADDED Requirements

### Requirement: Native file trust configuration is explicit, validated, and owned

The native file trust provider SHALL be constructed from an explicit native root byte sequence and an existing normalized portable path. Construction SHALL perform no file I/O, SHALL independently own both values, and SHALL reject an empty, non-absolute, or NUL-containing root and a combined root/path length greater than 4096 bytes with the corresponding `TrustSourceError.InvalidConfiguration` reason rather than trapping. Allocation refusal SHALL remain `OutOfMemoryError`.

#### Scenario: Construct an independent provider

- **WHEN** a caller supplies a valid absolute root and normalized path and later releases or changes its inputs
- **THEN** the provider retains unchanged independent configuration and construction has not accessed the file

#### Scenario: Reject invalid configuration without trapping

- **WHEN** the root is empty, relative, contains NUL, or the combined configured byte length exceeds 4096
- **THEN** construction fails with `InvalidConfiguration` naming `EmptyRoot`, `NonAbsoluteRoot`, `NulRoot`, or `PathBytes` respectively and publishes no provider

### Requirement: Acquisition is bounded before materialization

Every load SHALL open the configured regular file anew and enforce `limits.decode.inputBytes` while reading rather than checking after an unbounded whole-file read. It SHALL advance after every positive short read, treat zero as EOF, use bounded append growth, and at the exact input limit SHALL perform a separate one-byte read to distinguish exact EOF from excess input. It SHALL never retain input beyond the inclusive budget. Any nonempty input SHALL exceed a zero budget.

#### Scenario: Accept exact-budget EOF

- **WHEN** the file contains exactly the configured input-byte limit and the one-byte probe returns EOF
- **THEN** acquisition succeeds and passes exactly those bytes to strict snapshot decoding

#### Scenario: Reject one excess byte

- **WHEN** the file has at least one byte beyond the configured input-byte limit, including a nonempty file under limit zero
- **THEN** load fails immediately with `LimitExceeded(InputBytes, limit)` and retains no over-limit input

#### Scenario: Advance through short reads

- **WHEN** the native source returns multiple positive reads shorter than the offered buffer before EOF
- **THEN** load preserves every byte in order and does not interpret a short positive count as EOF

### Requirement: Cleanup and file failures preserve their exact stage

An opened descriptor SHALL be consumed by one close attempt before decoding or publishing a successful snapshot. Missing, inaccessible, wrong-kind, symlink, read, and close failures SHALL become `TrustSourceError.File` with the `Open`, `Read`, or `Close` stage and the complete underlying `FileError`, including its reason, native provider code, and operation context. A read, limit, or caller-allocation failure SHALL remain primary if close also fails. A close failure after successful acquisition SHALL replace success. Explicit close SHALL not retry after EINTR; structured unwinding SHALL retain the native handle's affine exact-once cleanup policy.

#### Scenario: Preserve read failure over close failure

- **WHEN** reading fails and the consuming close attempt also fails
- **THEN** load reports the translated `File(Read)` failure and the descriptor receives no second close attempt

#### Scenario: Preserve allocation or limit failure over close failure

- **WHEN** bounded acquisition fails from caller allocation refusal or excess input and close also fails
- **THEN** load reports the original `OutOfMemoryError` or `LimitExceeded(InputBytes)` outcome

#### Scenario: Surface successful-path close failure

- **WHEN** acquisition reaches EOF within budget but consuming close fails
- **THEN** load reports `File(Close)` and does not decode or publish a snapshot

### Requirement: Publication is strict, atomic, and independently owned

After complete bounded acquisition and successful close, load SHALL pass the complete bytes through `TrustSnapshot.fromPem` with the caller's unchanged decode and snapshot limits. Only strict whitespace and `CERTIFICATE` blocks SHALL be accepted. Empty input, unsupported labels, malformed later blocks, trailing junk, and snapshot/decode limit failures SHALL publish no snapshot. Success SHALL preserve input order and duplicate certificates. Every returned snapshot SHALL remain valid after provider drop, a later load, or a failed later load.

#### Scenario: Reject a malformed later block atomically

- **WHEN** an earlier certificate is valid and a later block is malformed
- **THEN** load returns the structured decode failure and publishes neither certificate

#### Scenario: Preserve duplicate authorities

- **WHEN** the selected file contains the same certificate twice
- **THEN** the returned snapshot contains two anchors in the same positions

#### Scenario: Keep an old snapshot after failed reload

- **WHEN** a caller holds a successful snapshot and a later load fails during acquisition or decoding
- **THEN** the held snapshot remains valid and unchanged

### Requirement: Reload observes explicit file generations without an implicit cache

Each load SHALL reopen the configured path. A producer replacement by atomic rename SHALL allow existing readers and snapshots to retain the prior opened generation while a later load observes the replacement. The provider SHALL make no point-in-time consistency promise for in-place modification and SHALL perform no implicit caching or replacement of caller-owned snapshots.

#### Scenario: Observe a renamed replacement

- **WHEN** one load completes, the producer atomically renames a different valid PEM file onto the configured path, and a second load begins
- **THEN** the two returned snapshots independently retain their respective file generations

### Requirement: Native availability is restricted to verified libc supplies

The provider SHALL expose selected members only for Darwin ARM64 with Apple ABI and system libc and GNU Linux ARM64 or x86-64 with GNU libc. It SHALL be absent on LLVM-generated WebAssembly and raw Linux no-libc profiles. Portable snapshots and memory trust sources SHALL remain available on those unsupported native-file targets.

#### Scenario: Select a verified native supply

- **WHEN** a program imports and reaches the provider on one of the three verified native target/libc profiles
- **THEN** ordinary selected-source analysis admits the provider using the existing native filesystem boundary

#### Scenario: Reject unsupported provider reachability

- **WHEN** a program reaches a native file trust member on WebAssembly or a no-libc profile
- **THEN** ordinary selected-source availability reports the member as unavailable before emission while portable trust actors remain usable

### Requirement: Explicit file trust does not claim platform policy equivalence

The provider SHALL interpret the chosen PEM file only as an explicit sequence of trust-anchor certificates. It SHALL NOT choose default paths, inspect environment variables, search or scan directories, execute commands, extract a keychain, evaluate OS trust settings, apply revocation, or claim OS per-purpose trust-policy equivalence. The configured root and its ancestors SHALL be trusted application configuration; the root itself and every component beneath it SHALL use the native filesystem boundary's no-follow validation.

#### Scenario: Load the Debian generated bundle explicitly

- **WHEN** an application configures root `/etc/ssl/certs` and portable path `/ca-certificates.crt`
- **THEN** the provider reads that named generated bundle without scanning the sibling certificate links or treating the path as a universal Linux default

#### Scenario: Reject a symlink below the configured root

- **WHEN** the final configured root or any selected component below it is a symlink
- **THEN** native file acquisition fails through the preserved `File(Open)` error and never follows the link

### Requirement: Conformance evidence is deterministic and synthetic

Acceptance evidence SHALL use committed or generated synthetic certificate fixtures rather than live host trust stores. It SHALL record fixture generation commands, source and DER hashes, and expected results; cover short reads, exact/over/zero budgets, empty and malformed content, file-kind and symlink failures, duplicates, error precedence, allocation refusal, reload ownership, and target availability; and execute the selected provider on each supported target in debug and optimized modes when a runner exists. Missing target runners SHALL be reported honestly. No per-feature fresh-process determinism suite SHALL be added.

#### Scenario: Reproduce fixture identity

- **WHEN** the fixture generation and verification commands run in a clean checkout
- **THEN** the recorded source/DER hashes and expected certificate results match without consulting a machine trust store
