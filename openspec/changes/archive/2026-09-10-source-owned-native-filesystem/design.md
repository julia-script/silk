## Context

See proposal.md. The remaining filesystem bridge owns eleven operations and an opaque compiler handle. Existing Pointer requalification deliberately preserves the pointee type; readdir records cannot safely be read by loading a nominal whole dirent. Independently compiled SDK fixtures establish distinct Darwin, GNU x86-64 and GNU ARM64 record layouts and flag constants.

## Goals / Non-Goals

Own calls, resource lifetimes, error translation and path policy in selected source. Preserve the portable seven-operation service and byte identities. No seek/fcntl surface, syscall ABI, transactional writes, cryptographic unique names or guarantees against privileged mount changes and hostile cross-boundary directory renames.

## Decisions

### Selected ABI declarations

Select exactly Darwin ARM64 with system libc and GNU x86-64/ARM64 with GNU libc. Declare open/openat with their actual C variadic tail, using target mode_t values and C promotions. Keep per-target stat and dirent descriptions and constants: GNU architectures differ in stat layout and O_DIRECTORY/O_NOFOLLOW. Independently compiled C checks establish size, alignment, offsets, function signatures and flag values before execution fixtures.

### Lossless path bytes

OsFileSystem.make takes an absolute byte slice and copies it without I/O. Path.joinBytes becomes the public normalized byte-fragment operation; text helpers delegate to it. OS paths use Path.rawBytes, never Path.view or UTF-8 validation. Reject NUL, empty interior components, dot and dot-dot. Open the configured root per operation and traverse relative descriptors with directory/no-follow/close-on-exec flags. The configured root and ancestors are trusted configuration; each component below it rejects symlinks. Descriptor anchoring does not promise Linux openat2-style containment under hostile directory renames or mounts.

### Ordinary affine resources

A source descriptor owns a signed fd and Drop closes it once. Explicit consuming close disarms before calling close, never retries, and reports failure. File handles wrap checked regular-file descriptors. Directory handles own DIR pointers, a borrowed dirfd, EOF state and an optional owned pending name. fdopendir transfers the descriptor only on success. closedir disarms first and runs once. Explicit success paths observe cleanup errors; unwinding and cancellation Drop preserve the primary failure. Fatal traps have no cleanup guarantee.

Use nonblocking file open so FIFO inspection cannot hang. For writes, open/create without truncation, fstat and reject non-regular kinds, then ftruncate. Successful wrong-kind checks produce logical code zero without reading errno. Capture errno immediately on failed native calls, before allocation releases or resource cleanup. Temporary C strings use the ordinary system allocator; translate its allocation failure to FileError.NoSpace while preserving existing public requirements. Output bytes and lists retain the caller allocator.

### Variable-sized directory records

Add only unsafe Intrinsic.pointerBytes<T> and Pointer.bytes<T>: nullable single const T pointer to nullable many const u8 pointer, preserving address and address space, with no load, ownership or extent claim. Do not relax pointerRequalify's same-pointee rule. The caller proves each byte access readable and initialized. Read the valid dirent prefix, validate reclen, scan a bounded name (and Darwin namlen), then copy it before another readdir. Never load sizeof(dirent). Clear errno before readdir to distinguish null EOF from failure; read it only for null. Skip dot entries and inspect child kind with no-follow fstatat. Own a pending name across short-buffer retries, so retry does not call readdir or lose an entry.

### Completion and uniqueness

Read and write loop over checked partial counts and retry EINTR. Preserve primary errors over cleanup; otherwise return close failure. List builds full byte paths and sorts by bytes. Unique directories use a provider-local u64 counter rendered as sixteen hex digits, wrapping explicitly, and at most 128 exclusive mkdirat attempts with mode 0700. Collision advances the counter; other failures stop. Exhaustion is AlreadyExists with logical code zero. This is predictable namespace allocation, independent of Random, clocks and process IDs.

## Risks / Trade-offs

- Variable-sized native records → bounded prefix reads and independently compiled short-record fixtures.
- Resource transfer mistakes → affine source ownership plus fault-injected close/closedir counts, cancellation and primary/cleanup failure cases.
- Filesystem races → descriptor-relative no-follow traversal and explicit trusted-root threat boundary; no unsupported stronger claim.
- Predictable unique names → document non-secret names and rely on exclusive creation, with bounded collision failure.
- Removal touches shared machinery → audit every OsHandle consumer and remove only after migrating all uses, then run complete compiler and native acceptance gates.

## Migration Plan

Land one stack layer with source, callers, tests, docs and deletion together. Verify ABI fixtures and actual executions on all six target/optimization lanes. Run required workspace checks and release candidate before publication. No compatibility implementation remains.

## Generic Effect lowering corrections

The filesystem ABI exposed two target-neutral lowering defects: widened Effect outcome lanes must be coerced back to each success field’s native width, and propagation cleanup must retain the same partial-initialization tree as ordinary Drop. Correct all native propagation forms; cover narrow fields and a consumed owned field in the shared native acceptance corpus. Neither correction recognizes filesystem declarations.
