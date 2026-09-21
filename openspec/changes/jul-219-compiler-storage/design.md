# Design

## Context

See proposal.md for motivation and the compiler-storage spec for behavior. `ArtifactCache` and
`RuntimeObjectCache` currently expose equivalent `get`/`set` byte protocols from
`NativeToolchain.ts`; one is disk-capable and the other adds process-local statistics. Their
owners already validate cache envelopes, complete link plans, targets, and admitted artifacts.
Storage must replace only the byte transport so JUL-221 can reuse it without depending on native
toolchain errors or filesystem layout.

## Goals / Non-Goals

**Goals:**

- Expose one Effect service with named operations and precise storage failures.
- Give memory and filesystem providers identical logical addressing, byte ownership, size-bound,
  missing-record, and overwrite semantics.
- Make filesystem publication atomic for readers and interruption-safe for temporary resources.
- Migrate existing native cache owners without moving their envelopes, policy, validation, or
  statistics into Storage.

**Non-Goals:**

- Listing, deletion, eviction, compare-and-swap, transactions, durability/fsync, or orphan cleanup.
- HTTP, credentials, project/global routing, replication, promotion, or distributed locking.
- Moving executable publication, chmod, subprocess files, or toolchain scratch directories.
- Interpreting checksums, versions, targets, semantic validity, or other record contents.

## Decisions

### Storage is a logical record service

`Storage` is a `Context.Service` whose operations accept a validated logical namespace/key address
and a caller-supplied maximum complete-record size. Public actor functions validate raw address and
limit inputs before delegating to the service. Missing is an ordinary optional result; invalid
addresses, invalid limits, oversize records, and external failures are typed `StorageError`
reasons.

The alternative—exposing provider paths to callers—would make filesystem layout part of every
cache owner and block a future non-filesystem provider.

### Filesystem encoding is injective and rooted

Namespaces and keys use a deliberately small logical alphabet and bounded length. The filesystem
provider encodes each component as a complete filename segment below its configured root and never
joins unvalidated input. Namespace and key boundaries remain explicit directories/files, so two
logical addresses cannot alias or traverse the root.

The alternative—sanitizing arbitrary paths—cannot distinguish collisions such as separators,
dot-segments, or replacement characters.

### Limits apply before unbounded allocation

Every read and publication receives a positive safe-integer byte limit. Memory records are checked
before copying. Filesystem reads inspect metadata before reading and reject an oversized record
without allocating its contents; a post-read length check protects against concurrent replacement.
Publication rejects the caller bytes before any provider work. The limit belongs to the cache
owner so existing artifact capacity is preserved without a small global default.

### Publication owns a scoped same-directory temporary

The filesystem provider creates a unique temporary in the destination directory, writes the whole
record, then atomically renames it over the destination. The temporary is acquired and released
with an Effect scope so success, typed failure, defect, and interruption all attempt cleanup without
replacing the original exit. Concurrent writers may leave either complete winner; readers observe
old, new, or missing, never a partial published record. No power-loss durability is promised.

### Cache actors own meaning and policy

Artifact and runtime-object caches become actors that select a namespace, maximum record size, and
Storage layer. They continue to encode/decode envelopes, count hits and misses, validate link plans
and targets, decide whether caching is enabled, and translate `StorageError` once into
`ToolchainError`. Storage never interprets corrupt bytes.

The alternative—teaching Storage about native envelopes or statistics—would recreate the existing
coupling under a new name and make JUL-221 depend on native semantics.

## Risks / Trade-offs

- [Risk] Atomic rename behavior differs by provider. → Define the contract at whole-record
  visibility, use same-directory replacement, and run the same fixture against memory and real
  filesystem layers.
- [Risk] Cleanup failure can obscure the publication failure or interruption. → Use scoped release
  and a release effect that does not replace the original exit.
- [Risk] Migration can accidentally weaken native admission. → Keep decoding and admission in the
  native cache actors and retain the existing corruption/plan/target fixtures.
- [Risk] Copying every disk buffer wastes memory. → Copy only at ownership boundaries; the
  filesystem read already returns a fresh owned buffer and publication writes directly from the
  caller without retaining it.

## Migration Plan

1. Add the Storage actor, error family, address validation, and memory layer.
2. Add the filesystem layer with bounded reads and scoped atomic publication.
3. Replace runtime-object cache byte storage with a Storage-backed actor while retaining counters.
4. Replace artifact cache memory/disk implementations and migrate Driver/Linker injection.
5. Delete obsolete interfaces and raw cache filesystem code; update exports, docs, and focused
   tests in the same change.
