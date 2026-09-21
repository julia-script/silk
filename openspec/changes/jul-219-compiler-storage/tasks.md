# Tasks

## 1. Storage contract

- [ ] 1.1 Add the Storage service, logical address validation, caller-supplied size limits, and
      precise typed errors, verified by focused invalid-address, invalid-limit, missing, overwrite,
      and oversize tests.
- [ ] 1.2 Add the memory layer with namespace/key isolation and caller-owned publication/read bytes,
      verified by the shared provider contract fixture.

## 2. Atomic filesystem provider

- [ ] 2.1 Add the rooted filesystem layer with injective path encoding and metadata-first bounded
      reads, verified by path-safety, missing, overwrite, and preallocation-limit tests.
- [ ] 2.2 Publish through scoped unique same-directory temporaries and atomic replacement, verified
      by focused concurrent-writer, injected-failure, interruption, and cleanup fixtures.

## 3. Native cache migration

- [ ] 3.1 Move runtime-object byte storage onto Storage while retaining cache-owned statistics and
      admission behavior, then delete its obsolete byte-store contract and implementation.
- [ ] 3.2 Move artifact-cache memory and explicit-directory providers onto Storage, preserving
      process-memory defaults, native error translation, cache policy, and record capacity.
- [ ] 3.3 Update Driver, Linker, package exports, and tests, then delete superseded ArtifactCache and
      raw cache filesystem publication paths without a compatibility facade.

## 4. Coherence and documentation

- [ ] 4.1 Preserve existing envelope corruption, complete-plan admission, target validation, and
      runtime-counter fixtures through the new capability boundary.
- [ ] 4.2 Document logical addressing, ownership, bounded-read, atomic-publication, and error-policy
      boundaries, verified by strict OpenSpec validation and focused architecture review.
