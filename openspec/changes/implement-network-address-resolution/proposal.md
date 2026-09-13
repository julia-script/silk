## Why

Silk can preserve an HTTPS service identity but cannot yet turn an origin host into an owned,
connectable endpoint. The networking stack needs strict address values and a replaceable resolver
whose native cancellation limits are explicit before socket and HTTP work can safely build on it.

## What Changes

- Add owned IPv4, IPv6, endpoint, port, domain-host, and classified host values with strict textual
  parsing, canonical formatting, and bounded validation.
- Add a replaceable Resolver service whose requests include family selection, finite result
  capacity, and an optional absolute monotonic deadline.
- Define numeric resolution as pure lookup bypass with family filtering and exact deadline
  preflight, while preserving the original host identity for TLS and HTTP policy.
- Add the selected synchronous Darwin/GNU native resolver: domain requests with deadlines fail
  before observable work, and requests without deadlines make exactly one blocking `getaddrinfo`
  call with owned bounded results and exactly-once chain cleanup.
- Add deterministic provider and native-boundary acceptance evidence without live DNS.
- Document the finite-result guarantees and the deliberate lack of a native in-flight timeout,
  cancellation boundary, scheduler-parking guarantee, or libc memory/time bound.

## Capabilities

### New Capabilities

- `network-address-resolution`: Owned network address/domain/endpoint values, Resolver service
  semantics, deterministic deadline ownership, and selected synchronous native resolution.

### Modified Capabilities

None.

## Impact

- New standard-library actors under `packages/compiler/stdlib/silk/` for network addresses,
  resolution policy, and selected native resolution.
- New focused compiler acceptance fixtures for pure, service, ownership, target-selection, and
  native ABI behavior.
- Standard-library manifest/catalog registration and public reference documentation must publish
  the new actors; dependent socket and HTTP tickets consume the service in later changes.
- No compiler-known actor, intrinsic, DNS cache, search-list expansion, live-DNS test, or
  compatibility path is introduced.
