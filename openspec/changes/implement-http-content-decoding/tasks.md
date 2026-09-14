## 1. Public Model and Planning

- [x] 1.1 Add the ordinary-source `silk.http_content` actor with documented affine
      `ResponseContext`, `Mode`, coding, finite content/codec `Limits`, `CodingPlan`,
      progress, error, representation, completion, reuse-disposition, and negotiation types; verify one
      shared analysis snapshot resolves the public API without a new intrinsic, compiler-known actor,
      provider, or target-specific path.
- [x] 1.2 Implement the sole `ResponseContext.make(head, method, trailerPolicy)` constructor around
      `http_body.selectResponse`, atomically retaining the original inputs, affine selection, framing,
      and anomaly behind `ResponseContext`; make `CodingPlan` consume only that context and
      verify ownership analysis rejects public field construction, separate or mismatched selections,
      and context/plan reuse. Document that each reader invocation treats its caller-supplied body as
      encoded, while byte provenance and decode-once enforcement belong to JUL-23's affine
      response/body envelope.
- [ ] 1.3 Implement checked limit validation and owned-reservation calculation for fixed composition
      state, active stage buffers, inflate stages, and Zstandard stages while leaving body-decoder
      storage under `http_body.Limits`; verify structural cases distinguish every invalid range,
      arithmetic overflow, and `maxOwned` excess before allocation.
- [ ] 1.4 Implement Decode parsing across repeated `Content-Encoding` fields in wire order, including
      exact field/element coordinates, case-insensitive supported names, repeated and identity entries,
      reverse application order, and the one-to-four depth bound; verify one realized analysis source
      distinguishes valid stacks from empty elements, parameters, invalid tokens, unsupported names,
      and excess depth.
- [ ] 1.5 Implement Raw planning and response-context policy for semantic no-body responses, ordinary
      fixed zero, 206 nonidentity rejection, and tunnel rejection while preserving the context's exact
      `Selection.anomaly`; verify structural and runtime cases prove Raw never interprets coding
      metadata, all pre-start failures occur before codec allocation or body reads, and prohibited or
      HTTP/1.0 no-body anomalies survive into a nonreusable plan.

## 2. Bounded Pure Composition

- [ ] 2.1 Implement the fixed four-slot codec representation and Effectful all-before-read
      acquisition for zlib-only deflate, concatenated gzip/x-gzip, and Zstandard with partial-acquisition
      release; verify allocator/provider counters distinguish successful acquisition, each construction
      failure ordinal, and exactly-once release with zero body activity.
- [ ] 2.2 Implement the private pure core combining `http_body.Decoder`, one fixed-capacity edge per
      active codec, reverse codec stages, and direct Raw/identity paths; verify fixed, chunked, and
      close-delimited runtime cases return the same representation bytes without admitting chunk
      syntax, trailers, or a buffered next message to a codec.
- [ ] 2.3 Implement cursor scheduling with the consume/write/phase-or-return progress invariant and
      edge-by-edge final propagation; verify input/output chunk matrices exercise zero-producing steps,
      retained input, full destinations, final suffixes, and no outer-EOF broadcast or zero-work loop.
- [ ] 2.4 Enforce encoded bytes at deframed-body output, aggregate intermediate bytes at every
      nonfinal stage, decoded bytes at the final output, and checked cumulative arithmetic before the
      excess byte; verify independent boundary cases fail with distinct reasons and exact prior totals
      while unrelated limits remain available.
- [ ] 2.5 Translate body, buffer/byte-I/O, inflate, Zstandard, trailing-zlib, arithmetic, and content
      limit failures once into stage-aware `ContentError`, then poison the reader; verify each cause and
      call/cumulative coordinate survives translation and every later operation returns zero-progress
      `InvalidState` without Raw fallback or draining.
- [ ] 2.6 Complete strict codec integration: HTTP deflate uses `Format.Zlib` only, gzip accepts its
      configured concatenated members, and Zstandard retains frame/skippable/checksum/dictionary policy;
      verify compact runtime cases distinguish raw-deflate rejection, zlib trailing data across split
      body chunks, gzip concatenation, and representative stage-indexed codec failures without
      duplicating the codecs' conformance suites.

## 3. Scoped Reader, Metadata, and Completion

- [x] 3.1 Implement `withReader` as a higher-ranked Effect bracket over an exclusive borrowed
      `BufferedDuplex`, consuming the plan and preserving callback success/error/requirement channels;
      verify ownership analysis rejects escaping readers, plan reuse, concurrent Raw/Decode readers,
      and direct buffered access during the callback.
- [ ] 3.2 Implement Effectful `ContentReader.readSome` as the thin peek/core/consume/fill adapter with
      one unchanged absolute deadline and an inert empty-destination path; verify provider counters show
      no I/O for empty output or internal-only progress and exact buffered consumption for successful
      and failing reads.
- [ ] 3.3 Implement non-I/O reader abandonment and partial-construction cleanup while leaving close
      ownership to the outer buffered transport scope; verify success, typed failure, and scheduled
      structured cancellation/interruption abandon incomplete state exactly once and cannot yield reuse
      evidence, while the API and tests make no cleanup claim for fatal traps that bypass finalizers and
      Drop.
- [ ] 3.4 Implement borrowed `RepresentationView`, untouched original header access, applied-plan and
      decoded-state metadata, exact framing anomaly, and decoded length only when known; verify gzip,
      Raw, identity-only, and semantic no-body cases preserve original `Content-Encoding`/
      `Content-Length`, without claiming that the view or context proves body-byte provenance.
- [ ] 3.5 Implement opaque content completion and trailer access only after the body boundary, every
      codec terminal validation, and final output drain, carrying exact anomaly and explicit
      `ReuseDisposition`; verify checksums and decoded bytes alone do not prove completion, delimited
      `Anomaly.None` success becomes reusable, and close-delimited/anomalous/failure/structured-
      cancellation/abandonment paths remain nonreusable.

## 4. Request Negotiation and Public Delivery

- [x] 4.1 Implement pure `Accept-Encoding` generation as
      `Result<Option<http.Header>, http.ValueError>` using caller header limits and `Header.make` for
      automatic and borrowed override values; verify all enabled subsets use stable
      `gzip, deflate, zstd` order, never advertise `x-gzip`, Raw automatic mode omits the field, valid
      overrides preserve exact bytes, invalid overrides retain precise `ValueError` coordinates, and
      negotiation never alters response validation.
- [x] 4.2 Register `silk.http_content` in the standard-library manifest and regenerate the committed
      source table; verify the generated entry matches the ordinary source byte-for-byte and the module
      imports from a fresh compiler analysis.
- [ ] 4.3 Add one ticket-local structural acceptance file and one consolidated ordinary-source
      runtime fixture with distinct failure codes, then register that fixture in the existing shared
      native and portable LLVM-to-Wasm corpora; verify the combined evidence covers planning and
      atomic context/selection ownership statically and the required framing, stack, limits, structured-exit
      lifecycle, no-body anomalies, metadata, completion/reuse disposition, and validated negotiation
      behavior at runtime without a per-feature backend pass.
- [x] 4.4 Add the public HTTP content-decoding reference with compiler-verified examples and exact
      affine response-context construction, Raw/Decode planning, reverse order, limit accounting,
      typed progress/errors, scoped ownership and fatal-trap exclusion, provisional output, anomaly and
      completion/reuse disposition, the caller-supplied encoded-body assumption, the JUL-23 provenance
      boundary, and validated Accept-Encoding guidance; verify every named symbol and example resolves
      against the registered public module.
