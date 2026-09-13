## Why

Silk can authenticate TLS only inside a callback that borrows and then closes its transport. That
shape cannot be retained by JUL-23 or JUL-197: keeping the borrow is invalid, while separating TLS
state from the transport can pair authenticated state with the wrong or already-closed provider.

## What Changes

- **BREAKING** Replace the callback-only TLS connection as the authoritative representation with an
  affine `OwnedConnection<P>` that owns the authenticated TLS state and the exact concrete
  `ByteDuplex` provider that carried its handshake.
- Add protected `authenticateOwned` acquisition, cancellation-safe borrowed operations over the
  owner, and a scoped convenience built from the same owner and TLS pump rather than a parallel
  driver. Every suspending direct-owned operation protects ambiguous transfer locally rather than
  relying on the caller to install an outer lease.
- Limit private authentication provenance to facts established by this TLS session: the original
  reference identity, fixed and negotiated ALPN evidence, authentication evidence and validation
  instant, and unity with the provider that carried the handshake.
- **BREAKING** Make trust preparation explicit: `authenticateOwned` and the scoped convenience
  consume an independently prepared `TrustSnapshot`; remove `ConnectionOptions.trust`, the
  `TrustSource` import and service requirement, and `TrustSourceError` from their errors, callers,
  examples, and reference documentation.
- Make graceful TLS write shutdown distinct from terminal nonparking close. Terminal close
  invalidates ownership first, attempts the underlying close at most once, and is suitable for
  structured cancellation and abandonment.
- **BREAKING** Expose protected owned native outbound acquisition in `silk.native_socket`; retain
  scoped convenience only as composition over the owned connection instead of making callback
  ownership the sole public path.
- Preserve exact typed errors and generic callback requirement channels. Install the provider guard
  only after `authenticateOwned` receives both independently prepared owned inputs, and use the
  delivered nonparking resource bracket for later success, typed failure, structured cancellation,
  and interruption. Fatal traps remain outside recoverable cleanup.

## Capabilities

### New Capabilities

- `owned-transport-lifetimes`: Affine native and authenticated TLS transport ownership, protected
  publication, borrowing, trust-input transfer, provenance, and terminal cleanup.

### Modified Capabilities

- `scoped-byte-duplex-tls-connection`: Replace the borrowed callback-only TLS connection contract
  with one owned authenticated connection and a scoped convenience implemented over that owner;
  make trust an explicit prepared input and add external absolute-deadline clamping and terminal
  close semantics.

## Impact

- Public standard-library APIs and documentation in `silk.tls_connection` and
  `silk.native_socket` change incompatibly; all repository callers, fixtures, and generated
  registration/reference material must migrate together under the green-field policy.
- `silk.tls_client`, `silk.byte_duplex`, `silk.trust_snapshot`, `silk.effect`, and the existing
  native connection actor remain the underlying mechanisms; no compiler-known TLS, socket, HTTP,
  pool, or security-policy actor is introduced.
- JUL-23 depends on this owner to define the HTTP connection owner, independently owned buffers,
  outer origin/route/security-context identity, exchange-view lifetime, and complete reusable/close
  proof. None of those APIs or requirements are placeholders in this change.
- JUL-197 remains blocked on JUL-23 and owns pool reservations, active return, trust-refresh
  eviction, idle ordering, expiration, capacity, and pool close. This change specifies none of
  those policies.
- Existing in-flight native-socket planning must be reconciled so its earlier rejection of owned
  outbound publication is removed rather than preserved as a second callback-only path.
