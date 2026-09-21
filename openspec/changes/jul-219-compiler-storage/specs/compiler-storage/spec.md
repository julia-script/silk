# compiler-storage Specification

## Purpose

Define provider-independent bounded storage for opaque compiler cache records and the migration of
existing native cache owners onto that capability.

## ADDED Requirements

### Requirement: Storage uses logical validated addresses

The compiler SHALL expose an Effect Storage service addressed by a namespace and key whose
validation and filesystem encoding are unambiguous, bounded, and incapable of path traversal.
Provider roots SHALL be supplied by configuration and SHALL NOT be encoded in logical keys.

#### Scenario: Namespace and key isolate records

- **WHEN** callers publish different records under the same key in two namespaces
- **THEN** each logical address reads its own record
- **AND** neither provider interprets a namespace or key as a raw filesystem path

#### Scenario: Invalid address is rejected

- **WHEN** a namespace or key is empty, oversized, contains a separator, or names a dot segment
- **THEN** the operation fails with an invalid-address StorageError before provider I/O

### Requirement: Reads and publication are bounded

Every read and publication SHALL receive a caller-owned positive safe-integer maximum complete
record size. Invalid limits and oversize records SHALL be distinct typed failures. A filesystem
provider SHALL reject a known oversize record before allocating its contents.

#### Scenario: Oversize read does not allocate the record

- **WHEN** stored metadata reports a record larger than the caller limit
- **THEN** the read fails with an oversize StorageError before reading the record body

#### Scenario: Invalid limit is configuration failure

- **WHEN** a caller supplies a non-positive, fractional, or unsafe integer limit
- **THEN** the operation fails with an invalid-limit StorageError rather than returning missing

### Requirement: Byte ownership crosses each boundary

Publication SHALL retain no writable alias of caller input after completion, and every successful
read SHALL return caller-owned bytes. Providers SHOULD avoid copies that do not establish an
ownership boundary.

#### Scenario: Caller mutation cannot change a record

- **WHEN** a caller mutates its publication input or a prior read result
- **THEN** a later read returns the originally published record unchanged

### Requirement: Publication is atomically visible

A read SHALL observe a whole old record, a whole new record, or missing. The filesystem provider
SHALL publish through a scoped unique same-directory temporary and atomic replacement. Cleanup
SHALL run after success, typed failure, defect, and interruption without replacing the original
exit. Concurrent same-key writers MAY leave either complete winner.

#### Scenario: Interrupted publication cleans temporary state

- **WHEN** filesystem publication fails or is interrupted after acquiring its temporary file
- **THEN** the temporary resource is removed when possible
- **AND** readers never observe its partial contents as the addressed record
- **AND** the original failure or interruption remains the operation exit

### Requirement: Storage failures preserve recovery semantics

Missing SHALL be a normal optional result. Invalid address, invalid limit, oversize, external read,
and external publication failures SHALL be distinguishable StorageError reasons with operation and
logical-resource context. Interruption SHALL remain interruption and unexpected defects SHALL
remain defects.

#### Scenario: External provider failure is typed

- **WHEN** a provider filesystem read or publication fails
- **THEN** Storage fails with the matching external reason and preserves the dependency failure as
  causal ancestry

### Requirement: Native cache owners use Storage without surrendering policy

Artifact-cache and runtime-object-cache actors SHALL use Storage for opaque record transport and
SHALL retain their own namespaces, maximum sizes, envelopes, checksums and versions, target and
complete-plan admission, cache policy, native error translation, and runtime statistics. The old
byte-store interfaces and implementations SHALL be removed without a compatibility facade.

#### Scenario: Native admission survives migration

- **WHEN** Storage returns a corrupt, wrong-target, or incomplete-plan artifact record
- **THEN** the owning native cache rejects it under its existing policy
- **AND** Storage does not interpret or bless its contents

#### Scenario: Runtime counters remain actor-owned

- **WHEN** runtime-object records hit or miss through either provider
- **THEN** the runtime cache actor reports its existing entry, hit, and miss accounting
- **AND** the generic Storage service exposes no cache-policy counters
