## 1. Public values and exact contracts

- [ ] 1.1 Add `silk.http_fetch` request, method-preserving `FetchRequest.get`, redirect/content/pool/
      proxy modes, admitted finite limits, defaults, and checked option validation. Extend the one
      existing positive HTTP analysis source with phase-valid pure witnesses for Manual/Raw/Fresh/
      Direct independence, context-dependent pool default, empty-only zero budgets, positive working
      buffer, overflow, and pre-contact invalid-option outcomes.
- [ ] 1.2 Add immutable `ResponseMetadata`, owned `FetchResult` and `CollectedResponse`, representation
      provenance, redirect/encoded/delivered counters, and checked URI/header/trailer payload-plus-index
      accounting. Prove in the same positive source that ordered metadata remains owned after live-head
      release, sink metadata is header-only, decoded facts do not rewrite wire Content-Length, and the
      exact metadata cap admits or rejects before publication.
- [ ] 1.3 Add bounded phase/progress `FetchError` values and the higher-ranked fetch client and sink
      contracts with precise generic cause/requirement rows. Keep response, current URI, chunk, exchange,
      and producer loans nonescaping, and add their exact public-row/reachability declarations to the
      existing analysis fixture rather than a new source or worker.

## 2. Shared final-response delivery

- [ ] 2.1 Implement one private final-response engine that copies final URI/head/representation metadata
      before reads, drives Raw or Decode through the existing client/content actors, copies trailers only
      after verified completion, finishes the response, and publishes one owned result. Extend the shared
      runtime scenario with distinct ordered-header/trailer, raw-vs-decoded metadata, forbidden-body,
      checksum/framing, and completed-404 signals without duplicating parser or codec cases.
- [ ] 2.2 Implement sequential effectful sink delivery with one borrowed chunk, awaited backpressure,
      post-success delivered counting, and typed failure context containing only prior successful bytes
      plus current offered length. Reuse the shared scenario to prove tiny-chunk sequencing, generic sink
      error/requirement preservation, provisional prefix visibility, no chunk escape, and conservative
      eviction after sink/cancellation or late validation failure.
- [ ] 2.3 Implement the bounded collector on the same destination path with required `maxBodyBytes`,
      checked growth before reserve/copy, no untrusted Content-Length reservation, and atomic
      `CollectedResponse` publication. Add the empty/zero and nine-bytes-with-cap-eight boundaries to the
      shared scenario and prove failure publishes no partial body or result.
- [ ] 2.4 Implement explicit bounded discard on the same destination path with required
      `maxDiscardBytes`, delivered-representation accounting, decoder/framing/trailer validation, and no
      redirect-drain or cleanup-read semantics. Add distinct empty/zero, cap, and checksum/completion
      outcomes to the shared scenario and prove cap/failure evicts rather than returning head-only success.

## 3. Source-specific request operations

- [ ] 3.1 Add `toSinkEmpty/Bytes`, `collectEmpty/Bytes`, and `discardEmpty/Bytes` as thin adapters from
      semantic requests to the matching delivered redirect operations and the one destination engine.
      Preserve explicit methods, repeatable-byte borrowing, request-owned Host/target/framing/auth/
      Accept-Encoding generation, and exact source-free rows; cover GET-with-bytes and Manual-one-attempt
      behavior in the existing analysis/runtime sources.
- [ ] 3.2 Add `toSinkOneShot/Replay`, `collectOneShot/Replay`, and `discardOneShot/Replay` with affine
      OneShot and higher-ranked fresh-per-attempt ReplayFactory authority and only their relevant generic
      rows. Reuse the redirect/source fixtures to prove attempt-zero acquisition, replay on retained-body
      redirects, body-drop transitions, no restoration after OneShot use, precise producer/factory failure,
      and no hidden whole-body buffering or placeholder witness.

## 4. Route, pool, deadline, and lifecycle composition

- [ ] 4.1 Implement the concrete native fetch context beside the existing client owner, sealing provider,
      HTTP, TLS/ALPN, trust, proxy credential, route, and optional pool identity. For every current redirect
      origin, recompute InheritContext routes or force Direct and prepare Direct/Tunnel origin-form versus
      Forward absolute-form through existing owners; extend current proxy structural/native evidence for
      proxy-to-bypass-to-proxy bytes and credential non-leakage without another socket/TLS matrix.
- [ ] 4.2 Compose UseContextPool/Fresh acquisition through the delivered pool and fresh client seams,
      including compatible default fallback, conservative route/security keys, original-origin TLS, and
      client-owned reuse eligibility. In the shared integration prove one compatible reused connection,
      explicit Fresh bypass, incompatible/unsupported pre-contact failure, and no per-request trust or TLS
      verification override.
- [ ] 4.3 Thread one unchanged optional absolute deadline through validation, route, checkout, acquisition,
      handshake, source, redirects, body/sink/trailers, and finish; implement final 101/successful-CONNECT
      rejection, 4xx/5xx success, and precise phase/cause mapping. Arm exact-once nonparking release for
      response, source, decoder, destination, reservation, lease, and connection ownership; the shared
      scenario SHALL distinguish timeout, producer/sink/component/finish failure, cancellation, no hidden
      cleanup read, suppressed close failure, and exact release/eviction counts.

## 5. Bounded integrated evidence

- [ ] 5.1 Complete fetch declarations, exact error/requirement rows, pure configuration/metadata
      `Evaluation`, native-context structural reachability, and positive MIR in one existing HTTP
      Analysis snapshot. Consolidate only genuinely distinct response/URI/chunk/source affine escape or
      duplication diagnostics in the existing frontend-negative boundary, with exact codes/spans and no
      additional realization, snapshot, source program, or worker.
- [ ] 5.2 Add one compact table-driven `http-fetch` source to the shared native acceptance corpus combining
      compatible pool reuse, relative redirect, explicit proxy routing, and compressed final content plus
      only the distinct destination, mode, cap, progress, 404, switching, failure, cancellation, and release
      signals from tasks 2–4. Reuse that exact source for exactly one named LLVM-to-Wasm leg and reuse the
      existing HTTP/HTTPS/TLS/provider vectors; add no per-case compilation, fresh-process, stress, timing,
      memory, browser, or duplicate parser/codec/proxy/socket/TLS matrix.

## 6. Registration and prescriptive documentation

- [ ] 6.1 Register `silk.http_fetch`, publish complete public doc comments, and update only its targeted
      tracked standard-library/catalog surfaces from canonical source. Verify the manifest and catalog
      expose the final twelve operations and public values without running or adding whole-stdlib
      documentation generation or a full local pipeline.
- [ ] 6.2 Add the canonical one-shot HTTP fetch reference and explicit native HTTP/HTTPS examples covering
      source-specific operations, defaults, provisional sinks, collection/discard bounds, metadata timing,
      Raw/Decode provenance, deadlines, errors, release, pool/proxy/redirect composition, exclusions, and
      deliberate differences from the pinned Zig implementation. Reuse the shared acceptance source for
      executable examples where possible and keep provider, trust, deadline, destination cap, and all four
      policy modes explicit without a separately compiled documentation fence.
