## Why

Silk can frame HTTP bodies and can incrementally decode DEFLATE-family and Zstandard streams, but
it has no bounded adapter that composes those layers without exposing chunk syntax, trailers, or a
following message to a content decoder. The missing layer also leaves stacked-coding order,
intermediate expansion, response no-body rules, and decoded-completion semantics to each future
client or server integration.

## What Changes

- Add `silk.http_content` with one affine `ResponseContext` constructor that atomically selects
  response framing from a validated head, request method, and trailer policy; preserve its framing
  provenance and anomaly, and allow a `CodingPlan` to consume that context exactly once.
- Add explicit `Raw` and `Decode` planning over that context and a bounded coding depth. Each reader
  invocation treats its caller-supplied buffered body as the encoded representation; this low-level
  module does not prove byte provenance or prevent replanning a copied response head over different
  bytes. JUL-23 owns the affine response/body envelope that will enforce transparent decoding once.
- Parse `identity`, `gzip`, `x-gzip`, `deflate`, and `zstd` in wire order, retain repeated codings,
  and apply nonidentity decoders in reverse order; reject malformed and unsupported Decode plans
  before body I/O or codec acquisition while leaving Raw metadata uninterpreted.
- Add one callback-scoped `ContentReader` that exclusively composes strict HTTP body deframing,
  buffered byte input, and a pure fixed-depth codec state machine with precise typed progress and
  errors; clean up on structured Effect exits while explicitly excluding fatal traps from the
  finalization guarantee.
- Enforce independent encoded, intermediate, decoded, and owned-memory limits, codec-specific
  limits, a one-to-four planning-depth limit, and fixed intermediate capacities from 1 through
  65,536 bytes.
- Preserve strict zlib-only HTTP `deflate`, concatenated gzip and Zstandard semantics, checksum and
  dictionary policy, exact outer-boundary finality, and provisional output until complete
  validation.
- Preserve original response headers in an explicit representation view, expose whether decoding
  occurred and a decoded length only when known, carry the selected framing anomaly through plan,
  completion, and reuse disposition, and publish opaque completion only after both the content
  stack and framed body finish.
- Add deterministic `Accept-Encoding` control for enabled `gzip`, `deflate`, and `zstd` support,
  explicit identity/omission behavior, Raw-mode omission, and caller override values validated into
  a standard `silk.http.Header` with precise `ValueError` failures.
- Register and document the ordinary-source module and add consolidated structural plus shared
  native/LLVM-to-Wasm acceptance evidence without duplicating the codec conformance suites.

## Capabilities

### New Capabilities

- `http-content-decoding`: Bounded HTTP response content-coding planning, scoped streaming,
  metadata, completion, limits, error provenance, and Accept-Encoding generation.

### Modified Capabilities

None. The adapter consumes the existing buffered byte I/O, HTTP body-framing, streaming inflate,
and Zstandard contracts without changing their requirements.

## Impact

- Adds one standard-library actor at `packages/compiler/stdlib/silk/http_content.silk` and its
  manifest/generated registration.
- Builds on `silk.http`, `silk.http_head`, `silk.http_body`, `silk.buffered_duplex`, `silk.inflate`,
  and `silk.zstd`; it adds no compiler-known actor, host codec, native library, or general Stream
  dependency.
- Adds a public reference page and executable examples under
  `apps/docs/content/reference/`.
- Extends compiler acceptance support and the existing shared native and portable-Wasm corpora
  with one consolidated source that reuses small established codec fixtures.
