## Why

Silk has owned numeric endpoints and a scoped reliable-byte abstraction, but no ordinary-source
transport that can connect those endpoints to native TCP or Unix sockets. JUL-146 supplies that
missing boundary now that JUL-190 has fixed endpoint ownership, while preserving cooperative
single-threaded scheduling, exact transfer ownership, and target-selected libc policy.

## What Changes

- Add selected `silk.native_socket` source for an affine owned native connection that implements
  `ByteDuplex` over nonblocking TCP and pathname Unix sockets on admitted Darwin and GNU targets,
  plus scoped convenience over the same acquisition path.
- Add bounded connect options, one overall absolute monotonic deadline, ordered sequential endpoint
  attempts, cooperative readiness polling, typed native error translation, half-close, EOF, and
  exact-once terminal cleanup.
- Pin Darwin and GNU socket layouts, constants, signals policy, connect completion rules, and close
  behavior with independent native C witnesses and deterministic scripted evidence.
- Extend the shared foreign-symbol spelling rule so quoted native aliases may contain `$`, enabling
  Darwin's `close$NOCANCEL`, while continuing to reject empty, leading-invalid, whitespace, NUL,
  non-ASCII, and compiler-reserved spellings.
- Publish reference documentation for supported profiles, deadline and polling trade-offs, transfer
  guarantees, error mapping, Unix-path limits, and cleanup semantics.

## Capabilities

### New Capabilities

- `native-socket-connections`: Native TCP and pathname-Unix owned connection acquisition, scoped
  ByteDuplex convenience, readiness/deadline behavior, transfer semantics, and platform cleanup.

### Modified Capabilities

- `bootstrap-foreign-functions`: Admit `$` after the first character of a quoted native symbol and
  preserve that spelling through analysis, ABI metadata, LLVM emission, objects, and linking.

## Impact

The change adds one target-selected standard-library actor, its reference page, focused analysis
and ownership cases, target C fixtures, and shared native-acceptance support. It narrowly changes
`ForeignSymbol.isValidSpelling` and its declaration/ABI/object-linking coverage; no socket-specific
compiler recognition or intrinsic is introduced. The integration coordinator must register the
new actor in the shared manifest/generated catalog and wire the ticket-local program and fixtures
into the shared corpus without adding availability on Wasm, no-libc, musl, or Windows profiles.
