## Why

Silk HTTP and file consumers cannot decode DEFLATE content without a host dependency. JUL-163 requires portable, bounded decoding that can suspend across input and output boundaries.

## What Changes

- Add `silk.inflate` with one resumable DEFLATE core and explicit raw, zlib, and gzip formats.
- Validate block coding, wrapper headers and checksums, and concatenated gzip members.
- Expose caller-owned output, exact progress, explicit final input, typed failures, and cumulative resource limits.
- Add independently sourced fixtures, boundary coverage in the shared native acceptance corpus, generated stdlib surfaces, and prescriptive documentation.
- Exclude encoders, preset dictionaries, zstd, and new compiler privileges.

## Capabilities

### New Capabilities

- `bootstrap-streaming-inflate`: Bounded streaming raw DEFLATE, zlib, and gzip decoding in ordinary Silk.

### Modified Capabilities

None.

## Impact

The change adds a standard-library actor under `packages/compiler/stdlib/silk`, its manifest entry and generated surfaces, tests and fixtures under `packages/compiler/test`, and documentation in `apps/docs/content/reference/runtime-and-standard-library.md`. It uses existing byte storage, integer, borrow, and Result facilities. It adds no runtime dependency or compiler intrinsic.
