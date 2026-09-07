## ADDED Requirements

### Requirement: Selected source owns the descriptor ABI

Darwin/GNU standard-stream operations SHALL use ordinary selected read/write/error declarations with exact target-width signatures, constants, header provenance and independent conformance evidence. Unsupported or no-libc profiles SHALL omit native operations while preserving portable services. Library names MUST NOT receive compiler privilege.

#### Scenario: Select a GNU provider

- **WHEN** a supported GNU profile reaches a native stream operation
- **THEN** only the GNU descriptor/error declarations and their verified supply are used.

#### Scenario: Portable provider replacement

- **WHEN** a Wasm program uses an ordinary replacement Writer or StandardInput
- **THEN** it requires no native descriptor import or stream helper.

### Requirement: Complete writes preserve actual progress

Source providers MUST complete partial writes without duplication, retry EINTR without advancing, fail on zero progress, and capture errno only immediately after a failed foreign call. Empty writes and flush MUST perform no foreign operation. Failure after a committed prefix MUST NOT imply rollback.

#### Scenario: Interrupted partial write

- **WHEN** write commits a prefix, returns EINTR, then commits the remainder
- **THEN** each input byte is written once in order and errno is read only for the failed call.

#### Scenario: Zero progress

- **WHEN** a nonempty write returns zero
- **THEN** the operation fails without retrying or reading stale errno.

### Requirement: Reads commit exact prefixes and establish EOF honestly

A nonempty source read MUST preserve its actual short count and untouched initialized tail, retry EINTR, and translate real native failures. A zero-capacity request MUST return Filled(0) without calling read or claiming EOF. A real nonempty zero transfer MUST latch EOF in that provider; later nonempty reads MUST return EOF without a foreign call.

#### Scenario: Empty read before and after EOF

- **WHEN** the caller supplies a zero-capacity buffer
- **THEN** the provider returns Filled(0), changes no bytes and makes no foreign call.

#### Scenario: Short read followed by EOF

- **WHEN** read commits a short prefix and a later nonempty read returns zero
- **THEN** the prefix count and tail remain exact, and that provider permanently reports EOF for later nonempty requests.

### Requirement: Standard descriptors remain borrowed

Stream providers MUST NOT close, transfer ownership of, or release process descriptors. Construction MUST perform no foreign call. Lexical replacement and suspension MUST preserve this behavior.

#### Scenario: Destroy a provider

- **WHEN** a provider is dropped after success or failure
- **THEN** its process descriptor remains open.

### Requirement: Obsolete stream privilege is deleted

The implementation MUST remove both stream intrinsics, HostWrite operations/lowering, native reservations/imports, generated stream adapters and forced inclusion. The separate hosted reporting loop SHALL remain without referencing the removed helper until its own migration.

#### Scenario: Inspect compiled stream code

- **WHEN** a native stream consumer is compiled and inspected
- **THEN** its object refers to ordinary selected foreign descriptor/error calls and contains no obsolete stream-helper dependency.
