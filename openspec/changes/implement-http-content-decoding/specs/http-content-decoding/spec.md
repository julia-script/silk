## Purpose

Defines bounded, scoped streaming of framed HTTP response representations in either their raw
encoded form or through a validated stack of supported content decoders, with exact progress,
metadata, completion, and reuse semantics.

## ADDED Requirements

### Requirement: Affine response context and framing selection

The standard library SHALL expose exactly one public constructor for an affine
`ResponseContext`. The constructor SHALL receive a validated `ResponseHead`, request `Method`, and
trailer policy, SHALL invoke response body selection itself, and SHALL atomically retain the
original head, method, policy, framing, and `Selection.anomaly`. Callers SHALL NOT supply a
separately selected framing value. Forming a `CodingPlan` SHALL consume that context so neither the
context nor its affine selection can be reused.

#### Scenario: Selection failure creates no context

- **WHEN** response body selection rejects the supplied head, method, or trailer policy
- **THEN** the context constructor returns the exact typed body-selection error and creates no
  partially usable context

#### Scenario: Provenance cannot be mismatched

- **WHEN** code prepares a response for content planning
- **THEN** it cannot substitute a framing, anomaly, method, head, or trailer policy selected in a
  different operation because the affine context carries them atomically

#### Scenario: Low-level context does not claim byte provenance

- **WHEN** a caller copies a response head and constructs a fresh context
- **THEN** this module does not claim that subsequently supplied body bytes belong to that head or
  prevent the caller from supplying unrelated or already-decoded bytes; JUL-23's affine
  response/body envelope owns that provenance and decode-once guarantee

### Requirement: Raw and decoded response plans

The standard library SHALL expose `Mode.Raw` and `Mode.Decode` and SHALL produce a validated,
single-use `CodingPlan` by consuming a `ResponseContext` and finite limits. The
plan SHALL preserve the context's framing and anomaly provenance. A Raw plan SHALL preserve the
encoded representation and original headers without parsing `Content-Encoding`, including when
that metadata is malformed or names an unsupported coding. A Decode plan SHALL parse and validate
the complete coding list before any body read or codec acquisition.

#### Scenario: Raw bypasses invalid content-coding metadata

- **WHEN** a response has malformed or unsupported `Content-Encoding` metadata and Raw mode is
  selected
- **THEN** planning succeeds without interpreting that metadata and streaming returns the framed
  encoded representation bytes

#### Scenario: Decode rejects before starting the body

- **WHEN** Decode mode encounters malformed content-coding syntax or an unsupported coding
- **THEN** planning fails with a distinct typed error carrying the coding position and the
  unsupported token when one exists, before a body byte is read or a codec is acquired

### Requirement: Content-coding parsing and application order

Decode planning SHALL recognize `identity`, `gzip`, `x-gzip`, `deflate`, and `zstd`
case-insensitively. It SHALL combine repeated `Content-Encoding` fields in wire order, preserve
repeated codings, count every coding including `identity` toward an explicit maximum depth from one
through four, and apply nonidentity decoders in reverse wire order. Absence of the field and a list
containing only `identity` SHALL require no codec.

#### Scenario: Repeated fields form one reverse decoding stack

- **WHEN** the wire contains `Content-Encoding: gzip, identity` followed by
  `Content-Encoding: zstd`
- **THEN** the validated plan records all three codings in that wire order and decodes with zstd
  followed by gzip

#### Scenario: Identity consumes depth but not a codec

- **WHEN** a coding list reaches the configured depth using one or more `identity` entries
- **THEN** each identity entry counts against the depth while no decoder is acquired for it

#### Scenario: Coding depth is bounded

- **WHEN** the configured maximum depth is outside one through four or the parsed list exceeds it
- **THEN** planning fails before body I/O or codec acquisition

### Requirement: Response contexts that do not carry a decodable representation

Planning SHALL derive response semantics from the request method, response status, and selected
body framing. Responses to `HEAD` and statuses 1xx, 204, and 304 SHALL be treated as having no body
before content-coding validation or codec acquisition. Decode mode SHALL reject a 206 response with
any nonidentity coding before body I/O, while Raw mode remains available. A protocol tunnel SHALL
not be accepted as an HTTP representation reader. Every no-body plan and completion SHALL retain
the framing anomaly, and its reuse disposition SHALL be nonreusable when that anomaly is not
`Anomaly.None`.

#### Scenario: No-body response bypasses malformed metadata

- **WHEN** a HEAD, 1xx, 204, or 304 response carries malformed `Content-Encoding`
- **THEN** both modes complete with zero representation bytes without parsing that metadata or
  acquiring a codec

#### Scenario: Prohibited no-body framing remains nonreusable

- **WHEN** a 1xx or 204 response carries framing metadata that body selection classifies as
  `Anomaly.ProhibitedFraming`
- **THEN** content planning still yields an empty no-body representation, preserves that anomaly in
  the plan and completion, and reports a nonreusable disposition

#### Scenario: HTTP/1.0 transfer anomaly remains nonreusable

- **WHEN** an HTTP/1.0 semantic no-body response carries `Transfer-Encoding` and body selection
  classifies it as `Anomaly.Http10TransferEncoding`
- **THEN** the empty plan and completion preserve that exact anomaly and report a nonreusable
  disposition

#### Scenario: Permitted no-body metadata does not invent an anomaly

- **WHEN** a HEAD or 304 response has metadata permitted by body selection and its anomaly is
  `Anomaly.None`
- **THEN** content planning preserves `Anomaly.None` while still returning no representation bytes

#### Scenario: Fixed zero is a real encoded body

- **WHEN** an ordinary response uses fixed-length framing of zero and declares `gzip`
- **THEN** Decode mode supplies final empty input to the gzip decoder and reports the resulting
  truncated-stream error rather than treating the response as a semantic no-body response

#### Scenario: Encoded partial representation is rejected for decoding

- **WHEN** a 206 response declares a nonidentity content coding and Decode mode is requested
- **THEN** planning fails before body I/O and the caller can create a fresh Raw plan instead

#### Scenario: Tunnel cannot become a content reader

- **WHEN** body selection describes a successful protocol tunnel
- **THEN** content planning rejects the context rather than interpreting tunnel bytes as a response
  representation

### Requirement: Scoped exclusive content reading

The standard library SHALL expose a callback-scoped `ContentReader` that borrows exactly one framed
body and its buffered byte transport for the callback lifetime. Each invocation SHALL treat that
caller-supplied body as the encoded representation described by the plan without claiming that the
body's bytes originated from the retained response head. The scope SHALL preserve the
callback's generic success, typed error, and Effect requirements. A plan and its selected framed
body SHALL be single-use, and the type system SHALL prevent a Raw reader, Decode reader, or direct
buffered reader from coexisting over the same body. Cleanup SHALL run for all structured Effect
exits, including success, typed failure, and structured cancellation/interruption. Fatal traps that
bypass Effect finalizers and Drop are explicitly outside this guarantee. Empty output buffers SHALL
return zero progress without transport I/O or decoder work.

#### Scenario: Reader owns one body for its scope

- **WHEN** a plan is opened with `withReader`
- **THEN** only the callback can access the resulting reader and no competing body or buffered read
  can be formed until that callback exits

#### Scenario: Callback channels are preserved

- **WHEN** the scoped callback succeeds, fails with an application error, needs additional Effect
  services, or exits through structured cancellation or interruption
- **THEN** the bracket preserves those channels while reliably abandoning an incomplete reader and
  releasing its owned decoding state

#### Scenario: Fatal trap is outside cleanup guarantees

- **WHEN** execution terminates through a fatal trap that bypasses Effect finalizers and Drop
- **THEN** the content-reader contract makes no claim that abandonment or resource finalization ran

#### Scenario: Empty destination is inert

- **WHEN** `readSome` receives an empty destination before or after content completion
- **THEN** it returns zero per-call progress without reading, polling, consuming buffered input, or
  advancing a decoder

### Requirement: Framing precedes content decoding

Content streaming SHALL feed decoders only bytes yielded by the selected HTTP body decoder after
chunk syntax and trailers have been removed. It SHALL preserve buffered bytes belonging to the next
HTTP message. The same transport adapter and pure content composition semantics SHALL be used for
fixed-length, chunked, and close-delimited bodies.

#### Scenario: Chunk metadata is never decoded as content

- **WHEN** a chunked response contains coding data followed by trailers and a buffered next response
- **THEN** only chunk payload bytes enter the coding stack, trailers remain available through the
  body completion, and the next response bytes remain unconsumed

#### Scenario: Framing failures retain their provenance

- **WHEN** body framing or buffered byte I/O fails while a content reader is active
- **THEN** the content operation fails with the exact body or buffer/byte-I/O cause and does not
  replace it with a codec error

### Requirement: Exact staged streaming and finality

The decoder composition SHALL be streaming and bounded, SHALL retain unread input and final output
suffixes across calls, and SHALL send final input to each stage only after its immediate upstream
stage has completed and all bytes already produced for that stage have been offered. Completion of
one stage SHALL NOT imply completion of another stage or of the framed body.

#### Scenario: Output filling preserves a final suffix

- **WHEN** the last decoder completes after producing more bytes than fit in the caller destination
- **THEN** the first call reports only the written prefix and later calls return the retained suffix
  before content completion becomes observable

#### Scenario: Outer end is not broadcast through the stack

- **WHEN** the framed body reaches its boundary while an earlier decoding stage still has buffered
  output for a later stage
- **THEN** finality advances one stage at a time only after each intervening buffer is drained

#### Scenario: Chunking does not change the result

- **WHEN** the same encoded representation is supplied under different transport, framing, and
  destination chunk boundaries
- **THEN** Decode mode returns the same bytes, progress totals, terminal completion, and validation
  result

### Requirement: Independent bounded accounting

Every plan SHALL use finite `maxEncoded`, `maxIntermediate`, `maxDecoded`, and `maxOwned` limits plus
codec-specific limits and an intermediate-buffer capacity from one through 65,536 bytes. Encoded
accounting SHALL count representation bytes after HTTP deframing. Intermediate accounting SHALL
sum bytes produced by every nonfinal decoder stage. Decoded accounting SHALL count bytes produced
by the final representation stage. Owned accounting SHALL cover all codec history, windows,
workspaces, fixed composition metadata, and intermediate storage owned by the reader, while
excluding caller-owned buffers and allocator metadata. Limit and arithmetic checks SHALL reject
before reserving, consuming, or producing bytes beyond the relevant bound.

#### Scenario: Each byte class has an independent limit

- **WHEN** one of encoded input, summed intermediate output, or decoded output would exceed its
  configured limit while the other totals remain within theirs
- **THEN** the reader reports the corresponding distinct limit error with exact prior progress and
  does not consume or produce the excess byte

#### Scenario: Zero byte budgets are real limits

- **WHEN** an encoded, intermediate, or decoded byte allowance is zero
- **THEN** a genuinely empty representation can complete, while the first byte chargeable to that
  allowance fails before commitment rather than being treated as unlimited

#### Scenario: Owned limit is checked before acquisition

- **WHEN** the codec reservations, composition state, and intermediate buffers required by a plan
  would exceed `maxOwned` or overflow arithmetic
- **THEN** opening the reader fails before allocating those resources or reading the body

#### Scenario: Partial construction releases acquired state

- **WHEN** acquisition of a later codec or buffer fails after earlier stages were acquired
- **THEN** every earlier owned codec and buffer is released exactly once and no body byte is read

### Requirement: Strict codec profiles

HTTP `deflate` SHALL mean a zlib-wrapped stream only; the reader SHALL NOT sniff, retry, or fall back
to raw DEFLATE. Gzip SHALL accept concatenated members under its configured member/header/checksum
limits. Zstandard SHALL accept regular and skippable frames, validate checksums, apply its configured
window, workspace, frame, and skippable limits, and preserve the decoder's typed
`UnsupportedDictionary` failure for nonzero dictionary IDs. Codec errors SHALL identify the failing
reverse-decoding stage and original wire coding position. A completed zlib stream with additional
encoded bytes SHALL fail as `TrailingEncodedData` rather than silently ignoring them or starting
another zlib stream.

#### Scenario: Raw deflate fallback is forbidden

- **WHEN** Decode mode sees `Content-Encoding: deflate` but the representation is a raw DEFLATE
  stream
- **THEN** it fails with the zlib decoder's typed error without retrying the same bytes in raw mode

#### Scenario: Concatenated gzip is decoded

- **WHEN** a gzip-coded response contains multiple valid concatenated members
- **THEN** Decode mode returns their concatenated payloads and completes only after every member and
  the framed body complete

#### Scenario: Trailing zlib data is rejected

- **WHEN** a zlib stream reaches its end before the encoded representation boundary
- **THEN** Decode mode reports `TrailingEncodedData` at that coding stage even when the extra bytes
  arrive in a later body chunk

#### Scenario: Codec policy failure identifies its stage

- **WHEN** a stacked gzip, deflate, or Zstandard decoder fails checksum, dictionary, header, window,
  workspace, frame, or member policy
- **THEN** the content error preserves the codec's typed reason and identifies both the application
  stage and original coding position

### Requirement: Precise progress and sticky terminal state

Each content read SHALL report bytes written in that call and cumulative encoded, intermediate, and
decoded totals without conflating accepted input with downstream output. Failures SHALL retain the
same progress detail. After any content, body, byte-I/O, ownership, or limit failure, the plan and
reader SHALL be poisoned: later reads, completion queries, or attempts to reopen it SHALL fail with
`InvalidState`. No automatic Raw fallback or hidden drain SHALL occur after streaming starts.

#### Scenario: Zero-producing decoder progress remains visible

- **WHEN** a codec consumes encoded bytes without yet producing decoded output
- **THEN** the successful read reports zero bytes written and the advanced encoded or intermediate
  totals without fabricating decoded progress

#### Scenario: Failure after partial output is exact

- **WHEN** validation fails after bytes were returned provisionally in the same or earlier calls
- **THEN** the error reports the exact per-call and cumulative progress and subsequent operations
  fail with `InvalidState`

#### Scenario: Started Decode cannot fall back to Raw

- **WHEN** a Decode reader encounters an unsupported state, malformed stream, or limit failure after
  it starts
- **THEN** it neither reinterprets buffered bytes as Raw nor drains the remainder of the body

### Requirement: Representation metadata is explicit and non-destructive

The reader SHALL expose a `RepresentationView` containing the original response head, the applied
coding plan, its exact framing anomaly, whether content decoding actually occurred, and a decoded
length only when it is known. Original `Content-Encoding` and `Content-Length` fields SHALL remain
unchanged; effective representation metadata SHALL be exposed separately rather than by deleting
or rewriting headers. This view and the response context SHALL NOT claim byte provenance or prevent
a caller from copying the original head and constructing a new context. JUL-23 SHALL own the affine
response/body envelope that prevents transparent decoding from being applied twice.

#### Scenario: Decoded metadata preserves wire headers

- **WHEN** a gzip response is decoded successfully
- **THEN** its view still exposes the original gzip `Content-Encoding` and encoded
  `Content-Length`, marks decoding as applied, and exposes the decoded length after completion

#### Scenario: Identity view is not marked decoded

- **WHEN** Decode mode receives an absent or identity-only coding list
- **THEN** its view records the applied no-op plan, reports that decoding did not occur, and exposes
  the representation length only after it is known

### Requirement: Completion gates connection reuse

Content bytes SHALL be provisional until all selected codecs validate their terminal state and the
framed body reaches its exact completion boundary. An opaque completion proof SHALL become
available only after both conditions hold and SHALL carry the exact framing anomaly plus an explicit
reuse disposition. Reuse SHALL require that proof, the underlying body completion, delimited
framing, and `Anomaly.None`; abandonment, structured cancellation/interruption, any failure,
close-delimited framing, and any non-None anomaly SHALL never be reported reusable. Trailers SHALL
become visible only through successful framed completion.

#### Scenario: Decoded bytes do not prove completion

- **WHEN** a decoder emits payload bytes before its checksum or final body boundary is validated
- **THEN** the reader returns those bytes as provisional but exposes no completion or reusable
  connection proof

#### Scenario: Successful bounded completion is reusable

- **WHEN** every decoder finishes validly, a fixed-length or chunked body reaches its exact boundary,
  and the preserved anomaly is `Anomaly.None`
- **THEN** the reader exposes content and body completion, the final metadata view, and any trailers
  with an explicitly reusable disposition

#### Scenario: Anomalous completion is not reusable

- **WHEN** the body and all content stages complete but the preserved selection anomaly is not
  `Anomaly.None`
- **THEN** completion exposes that exact anomaly with a nonreusable disposition

#### Scenario: Close-delimited completion is not reusable

- **WHEN** all content decoders validate only after the transport EOF that ends a close-delimited
  body
- **THEN** content completion may be observed but the connection is explicitly nonreusable

#### Scenario: Early scope exit abandons reuse

- **WHEN** the scoped callback exits before completion through success, typed failure, or structured
  cancellation/interruption
- **THEN** the incomplete reader is abandoned and no completion or reuse proof can later be formed

### Requirement: Deterministic Accept-Encoding generation

The standard library SHALL generate automatic `Accept-Encoding` only for Decode mode, using enabled
codings in deterministic `gzip`, `deflate`, `zstd` order and never advertising `x-gzip`. If none is
enabled, the caller SHALL explicitly select either `identity` or omission. Raw mode SHALL omit an
automatic field. Generation SHALL receive caller-supplied HTTP header limits and return
`Result<Option<Header>, ValueError>`, constructing every emitted field through
`Header.make("Accept-Encoding", value, limits)`. A caller-provided override SHALL be an exact
borrowed field-value byte slice, SHALL be validated by the same constructor before emission, and
SHALL NOT weaken validation of the received response.

#### Scenario: Enabled codecs have stable order

- **WHEN** automatic Decode negotiation enables zstd and gzip but not deflate
- **THEN** generation returns a validated `Accept-Encoding` header whose value is `gzip, zstd`

#### Scenario: Empty automatic support is explicit

- **WHEN** automatic Decode negotiation enables no coding
- **THEN** the configured empty policy returns a validated field with value `identity` or returns no
  field

#### Scenario: Raw has no automatic advertisement

- **WHEN** Raw mode uses automatic negotiation settings
- **THEN** no `Accept-Encoding` field is generated

#### Scenario: Override does not authorize unsupported decoding

- **WHEN** a valid override is generated and the peer responds with a coding unsupported by the
  Decode planner
- **THEN** the validated field preserves the override bytes exactly but response planning still
  rejects the unsupported coding

#### Scenario: Invalid override is not emitted

- **WHEN** an override contains a forbidden field-value byte, leading or trailing OWS, or exceeds the
  supplied HTTP value limit
- **THEN** generation returns the precise `ValueError` component, reason, and offset from
  `Header.make` and returns no header

### Requirement: Ordinary-source portable API and reference contract

The content-decoding API and policy SHALL be implemented in ordinary Silk source over public body,
buffered-I/O, inflate, and Zstandard actors. It SHALL introduce no compiler-known standard-library
actor, target-specific semantic path, host codec dependency, or general-purpose Stream abstraction.
The public reference SHALL document planning, ownership, limits, errors, metadata, completion,
negotiation, and the provisional nature of streamed output.

#### Scenario: Native and WebAssembly share the same source contract

- **WHEN** a supported response is compiled and executed for the shared native and portable
  LLVM-to-WebAssembly acceptance targets
- **THEN** the same ordinary-source API exhibits equivalent planning, streaming, progress, error,
  and completion behavior without target-specific content-decoding policy

#### Scenario: Public documentation exposes safety boundaries

- **WHEN** a user reads the content-decoding reference
- **THEN** it explains decoder order, exact limits and accounting, scoped exclusivity, typed failures,
  metadata preservation, provisional output, and the conditions required for connection reuse
