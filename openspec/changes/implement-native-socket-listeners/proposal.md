## Why

Silk now owns outbound native byte connections, but it still cannot admit inbound TCP or pathname-
Unix connections without each application rebuilding descriptor setup, peer-address validation,
cooperative waiting, and cancellation cleanup. JUL-191 supplies that missing listener boundary so the
streaming HTTP server can accept one independently owned connection at a time without leaking or
coupling it to the listener lifetime.

## What Changes

- Extend the selected ordinary-source native socket actor with affine TCP and pathname-Unix
  listeners, bounded listen options, actual bound-address discovery, terminal idempotent close, and
  no pathname unlink ownership.
- Add exclusive nonblocking accept with one unchanged optional absolute deadline, positive
  timer-assisted retries, structured cancellation, owned peer metadata, and an independently scoped
  accepted `Connection` using the existing ByteDuplex and close policy.
- Add a compile-time-only `AcceptedContext` adapter so server handlers can consume owned or
  once-callable state without capturing it in a runtime callback value.
- Validate every returned native address before reading it; represent unnamed Unix peers explicitly,
  preserve pathname peer spelling, and reject unsupported abstract or malformed peers after closing
  only the provisional accepted descriptor.
- Pin Darwin/system-libc and GNU/Linux/glibc socket ABIs with independent C witnesses and consolidate
  real loopback, scripted failure, ownership, target-selection, and documentation evidence into the
  existing focused/shared acceptance boundaries.

## Capabilities

### New Capabilities

- `native-socket-listeners`: Owned TCP and pathname-Unix listener creation, bounded serial admission,
  peer-address publication, cooperative waiting, and independent accepted-connection cleanup on the
  native profiles already admitted by `silk.native_socket`.

### Modified Capabilities

None.

## Impact

- Extends `packages/compiler/stdlib/silk/native_socket.silk` and its public manifest/generated
  surface; adds focused listener support, shared native-corpus coverage, and Darwin/GNU ABI fixtures.
- Adds the native listener reference page and index entry, including explicit ownership of Unix
  pathname cleanup and the polling/deadline contract.
- Reuses `Endpoint`, `Connection`, `ByteDuplex`, `MonotonicClock`, and
  `Effect.useReleaseNonParking`; it adds no compiler-known networking name, runtime socket shim,
  resolver policy, TLS server, reactor, task spawning, or ambient clock.
