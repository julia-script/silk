## Context

The previous stack supplies selected source, pointer layout, ordered foreign contracts and physical platform inputs. The initial hosted subset is Darwin ARM64 (SDK 15.5, deployment 11.0) and GNU x86-64/ARM64 (glibc 2.36, GCC 12), compiled by LLVM/LLD 22.1.8. PlatformCatalog records describe provenance only; ordinary static selection determines availability.

## Goals / Non-Goals

Move all admitted standard-stream behavior into source; prove actual partial transfers and ordered errno capture; delete every obsolete stream path. Preserve lexical and suspended provider replacement. Do not add file ownership, process spawning, terminal behavior, failure reporting policy or new Wasm host imports.

## Decisions

### Selected descriptor catalog

Hand-author a small source catalog for read/write, signed ssize_t, unsigned size_t, stdin/stdout/stderr constants and target-specific error accessors. Both initial native ABIs use signed/unsigned pointer-width transfer counts and 32-bit descriptors/errors. Darwin uses __error; GNU uses __errno_location. Verify types, C symbol spellings and constants against the pinned headers, with immediate error capture through conservative foreign memory effects and no compiler recognition of these names. These synchronous read/write calls do not retain the buffer pointer; noCapture is the only added pointer promise. Unsupported/no-libc selections omit native declarations and providers.

Record declarations, authority versions, headers, scope, per-claim fixtures, tools and review/drift procedure using PlatformCatalog. Planned evidence becomes Verified only after actual results exist. Updating a header or tool requires reviewing source declarations, repeating the full ABI/boundary matrix, and publishing changed hashes and evidence together.

### Source transfer policy

Expose honest low-level descriptor calls and implement bounded transfer policy in a source descriptor module. Use a conservative maximum request of 0x7ffff000 bytes, below both admitted platforms' signed-count limits; this is library request policy, not a claim that both kernels share a maximum.

Writes loop over the remaining slice. Advance only by a positive returned count, retry EINTR without advancing, fail on zero progress, and capture the native error immediately after a negative result. A count beyond the requested range is an invalid transfer. Empty writes return success without a syscall or errno access. Unbuffered flush is a no-op. A failure can follow a committed prefix; the provider never retries the whole message or claims rollback.

Reads perform one nonempty transfer, retrying EINTR. Return its exact positive count and preserve the initialized tail. A zero-capacity read returns Filled(0) without foreign calls. A real nonempty zero result proves EOF. OsStandardInput stores an ended bit after that observation and returns EOF on later nonempty calls without touching the descriptor, preserving StandardInput's permanent-end promise. Empty reads still return Filled(0), including after EOF. Construction creates only this local state and performs no read.

Descriptors 0/1/2 are borrowed process resources: no close operation, ownership transfer, release callback or destructor is introduced. WriterError and StreamReadError remain public service failures; detailed native errors are translated in source, not synthesized by the backend.

### Consumer and compiler deletion

Keep `silk.writer`, `silk.logger` and `silk.standard_input` free of native provider imports and module-level platform conditions. Put `StdoutWriter.make` and `StderrWriter.make` in `silk.os_writer`, and `StdoutLogger.make` in `silk.os_logger`; those provider modules select their declarations from target/libc facts. This keeps ordinary portable frontend analysis independent of a platform profile while explicit native consumers select one. Writer.failure and Logger.failure let ordinary source providers construct their service errors without compiler-known error shapes. Move the writer stream-selection field into ordinary source descriptors or remove it where the type fixes the descriptor. Migrate direct intrinsic users and suspended formatting/logging cases. Delete StandardStreamWrite and OsStandardInputRead catalog entries, HostWrite HIR/MIR/verification/encoding/native paths, both reserved symbols, generated standardStreamsShimSource and its forced inclusion, and the OsRuntime standard-input fragment. The distinct silk_write_all/silk_write_text reporting loop remains accounted for under JUL-130.

### Verification

Structured analysis proves selected availability, absent inactive imports and absence of old operations. Existing shared corpus proves actual stdout/stderr and portable lexical/suspended replacements. One independent C boundary fixture scripts short writes, interruption, zero progress, failure, short reads, exact prefixes/tails, zero capacity and EOF latching; object/LLVM checks prove actual read/write/error symbols and ordered error access. Run debug and optimized lanes on all three targets, using the existing real GNU ARM64 runner. Reject LTO. No timing oracle or duplicate per-feature native Driver loop.

### Prior art review

Pinned Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa posix.read caps requests, returns immediately for empty input and retries EINTR. Its pipe test covers a real short read, not the full interruption/failure matrix. Io.zig establishes the separate I/O interfaces but does not prove these descriptor boundaries. Pinned Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84 fd/unix.rs caps reads and advances initialized cursors by actual transfer counts; its tests.rs only covers vectored-write count limiting. Neither prior-art suite was executed. Silk uses its own deterministic boundary fixtures and the selected platform headers as ABI authority; its EOF latch is explicit source service policy.

## Risks / Trade-offs

- Error state can be overwritten by another foreign call: capture it in the failure branch before translation or retry, and verify the ordering in IR and scripted fixtures.
- Permanent EOF differs from a descriptor that later receives appended data: latch it per StandardInput provider; a fresh provider can observe a later descriptor state.
- Source imports can accidentally make portable services native-only: select provider declarations while keeping the service and scripted replacements portable.
- Generated report code must remain until JUL-130, but no longer retain the removed stream helper.

## Migration Plan

Pin headers and provenance, implement selected source calls and transfer policy, migrate all consumers, remove obsolete compiler/runtime paths, update generated docs/inventories, execute conformance and required gates, and publish above JUL-127 through gh stack. No compatibility path remains.
