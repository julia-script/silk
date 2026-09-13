## 1. Reconcile Native Ownership

- [ ] 1.1 Revise the in-flight native-socket planning artifacts to remove the callback-only
      ownership decision, and verify they describe one owned resolved-TCP/pathname-Unix acquisition
      implementation plus scoped composition.
- [ ] 1.2 Expose owned resolved-TCP and pathname-Unix acquisition over the existing affine native
      `Connection`, implement scoped helpers through those constructors, and verify structured
      analysis rejects copying and raw descriptor escape while preserving exact error and service
      rows.
- [ ] 1.3 Extend the existing native acceptance source with distinct owner-retention,
      pre-publication failure/cancellation, move-into-scope, and invalidate-before-close cases, and
      verify the shared native corpus observes one descriptor close with no later syscall activity.

## 2. Make Trust Preparation Explicit

- [ ] 2.1 Remove `ConnectionOptions.trust`, the now-unused `TrustLoadLimits` import, the
      `TrustSource` import and service requirement, and the `TrustSourceError` import/failure
      alternative from `tls_connection` public and helper rows, and verify one structured analysis
      snapshot exposes only explicit `TrustSnapshot` inputs and the reduced precise rows.
- [ ] 2.2 Migrate every TLS connection caller and fixture to load, decode, combine, or copy one
      independent snapshot before transferring its provider, and verify preparation failures retain
      their exact trust/allocation errors without entering TLS provider cleanup.
- [ ] 2.3 Update TLS connection documentation and executable examples to distinguish caller-owned
      snapshot preparation from guarded authentication, and verify the examples contain no ambient
      `TrustSource` provisioning or obsolete trust option.

## 3. Implement the Owned TLS Connection

- [ ] 3.1 Refactor the existing TLS connection pump to operate on private state plus an explicit
      provider reborrow, introduce affine `OwnedConnection<P>` with private TLS-only provenance and
      a `Closed` phase, and verify one analysis snapshot rejects copying, provider/provenance
      substitution, escaped views, raw construction, and independent ambient `ByteDuplex` access.
- [ ] 3.2 Implement guarded `authenticateOwned` so its guard begins after receiving `P` and one
      prepared independent snapshot, publishes only after complete authentication, and verify typed
      failure and scheduled cancellation close the retained provider once while ordinary ownership
      releases unpublished TLS state.
- [ ] 3.3 Extend `ConnectionOptions` with the optional external absolute deadline, clamp it to the
      finite handshake-duration deadline without renewal, and verify already-expired, earlier
      external, later external, fragmented-I/O, and post-Finished publication-boundary cases receive
      the exact expected deadline and emit no late transport bytes.
- [ ] 3.4 Move read, write, flush, TLS authentication metadata, and graceful write-shutdown
      operations onto the owner without changing TLS progress/error semantics; protect every
      suspending direct-owned operation with an armed/completed nonparking cancellation guard; and
      verify a moved owner continues the same session while cancellation of a pending direct-owned
      write marks that owner terminal, closes once, never reoffers the ambiguous buffer, and rejects
      later I/O before provider dispatch.
- [ ] 3.5 Implement idempotent nonparking terminal close that invalidates before one provider close
      and performs no TLS grace, and verify it remains distinct from `shutdownWrite`, preserves a
      direct typed close failure, and cannot replace an earlier scoped success, failure, or
      cancellation.
- [ ] 3.6 Replace borrowed-provider `withClient` with a consuming scoped convenience over
      `authenticateOwned` and `Effect.useReleaseNonParking`, delete the obsolete constructor and
      duplicate state shape, and verify arbitrary callback error and requirement channels remain
      precise under `Without<R, ByteDuplex>`.

## 4. Migrate the Shipped Surface and Evidence

- [ ] 4.1 Migrate every native/TLS integration caller to the owned APIs, delete callback-only
      compatibility paths, and verify the standard-library manifest and generated embedding expose
      only the new public surface.
- [ ] 4.2 Update public reference documentation for owned authentication, explicit trust snapshot
      preparation, limited TLS provenance, external deadlines, scoped cleanup, graceful shutdown,
      terminal close, and the JUL-23/JUL-197 downstream boundary, and verify snippets resolve and
      execute in their admitted native or portable profile.
- [ ] 4.3 Consolidate runtime evidence into the existing TLS acceptance source, shared native corpus,
      and one representative LLVM-to-Wasm memory-provider witness, and verify every ownership,
      trust-boundary, deadline, progress, cleanup, and portability claim has a distinct oracle
      without a new per-feature backend compilation pass.
- [ ] 4.4 Update this change's implementation-task state together with ticket-local generated
      artifacts after the complete source migration, and verify strict OpenSpec validation and the
      ticket-scoped diff contain no HTTP exchange, buffer, pool, or obsolete dual API.
