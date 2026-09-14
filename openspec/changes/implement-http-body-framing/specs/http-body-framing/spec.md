## Purpose

Defines a strict, bounded, transport-independent HTTP/1 message-framing contract that preserves exact message boundaries, trailer policy, and trustworthy completion evidence.

## ADDED Requirements

### Requirement: Receiving framing selection follows HTTP precedence

The standard library SHALL expose pure request and response framing selection over validated heads. It SHALL represent `Empty`, `Fixed(length: u64)`, `Chunked`, `CloseDelimited`, and `Tunnel` framing, keep receiving selection separate from outgoing validation, and apply special-response, tunnel, transfer-coding, content-length, and default rules in that order.

#### Scenario: Semantically empty response overrides framing metadata

- **WHEN** a response corresponds to HEAD or has status 1xx, 204, or 304
- **THEN** receiving selection returns Empty without consuming a body
- **AND** legal hypothetical representation metadata on HEAD and 304 remains available
- **AND** HTTP/1.0 Transfer-Encoding on an Empty response or prohibited framing fields on 1xx or 204 produce an explicit non-reusable anomaly

#### Scenario: Successful CONNECT selects tunnel handoff

- **WHEN** a response to CONNECT has a 2xx status
- **THEN** receiving selection returns Tunnel and ignores Content-Length and Transfer-Encoding for delimitation

#### Scenario: Transfer coding is strict and unambiguous

- **WHEN** a non-special message contains both Transfer-Encoding and Content-Length
- **THEN** selection fails with conflicting framing
- **AND WHEN** Transfer-Encoding is anything other than one parameterless, final, non-duplicated `chunked` coding, or appears on HTTP/1.0
- **THEN** selection fails with invalid or unsupported transfer coding as applicable

#### Scenario: Content length is one canonical decimal field

- **WHEN** Content-Length is exactly one nonempty unsigned decimal digit sequence that fits u64
- **THEN** selection returns Fixed with that length
- **AND WHEN** Content-Length is repeated, comma-separated, signed, empty, non-decimal, or overflows u64
- **THEN** selection fails with invalid length or size overflow as applicable

#### Scenario: Unframed defaults depend on message direction

- **WHEN** an otherwise unframed request is selected
- **THEN** selection returns Empty
- **AND WHEN** an otherwise unframed ordinary response is selected
- **THEN** selection returns CloseDelimited

#### Scenario: Status 205 is not a framing shortcut

- **WHEN** a 205 response carries a valid zero-length delimiter
- **THEN** framing selection accepts that delimiter rather than automatically overriding it to Empty

### Requirement: Outgoing framing fields are validated independently

The standard library SHALL validate outgoing request and response framing separately from receiving selection. It SHALL reject prohibited framing fields on 1xx, 204, and successful CONNECT responses while permitting legal HEAD and 304 representation metadata.

#### Scenario: Prohibited outgoing fields fail before encoding

- **WHEN** an outgoing 204 or successful CONNECT response declares Content-Length or Transfer-Encoding
- **THEN** outgoing validation fails with invalid framing

#### Scenario: HEAD and 304 may describe a hypothetical representation

- **WHEN** an outgoing HEAD or 304 response carries an otherwise valid Content-Length
- **THEN** outgoing validation preserves the metadata while selecting an empty transmitted body

### Requirement: Decoder acquisition owns all bounded state

Decoder construction SHALL acquire all configured byte backing and metadata capacity before decoding. Successful `step(input, output, finalInput)` and discard calls SHALL allocate nothing, retain no borrow of caller input or output, and report exact per-call consumed and written counts plus cumulative committed progress.

#### Scenario: Construction enforces owned capacity

- **WHEN** configured backing bytes and metadata indices exceed maxOwnedBytes or cannot be allocated
- **THEN** decoder construction fails before accepting message bytes with the precise owned-limit or OutOfMemory failure

#### Scenario: Empty buffers do not manufacture progress

- **WHEN** an active decoder receives empty input and empty output without a newly observable EOF transition
- **THEN** consumed and written are zero and cumulative counters remain unchanged

### Requirement: Incremental decoding preserves the exact message boundary

Decoder steps SHALL return NeedInput, NeedOutput, Complete, or Tunnel. Empty and Tunnel SHALL consume and write zero bytes; Fixed SHALL stop at exactly its declared length; Chunked SHALL stop after the trailer section's terminating CRLF; and CloseDelimited SHALL complete only after framed input EOF without authorizing reuse.

#### Scenario: Chunked suffix remains untouched

- **WHEN** the decoder receives `4\r\nWiki\r\n0\r\n\r\nNEXT`
- **THEN** it writes `Wiki`, consumes exactly 14 wire bytes, reports delimited completion, and leaves `NEXT` unconsumed

#### Scenario: Fixed decoder stops at its declared boundary

- **WHEN** Fixed(4) receives `WikiNEXT`
- **THEN** it writes and consumes exactly four bytes, reports delimited completion, and leaves `NEXT` unconsumed

#### Scenario: Tiny output pauses payload consumption

- **WHEN** output capacity is smaller than available fixed or chunk payload
- **THEN** the decoder writes only what fits, does not consume unwritten payload, and reports NeedOutput

#### Scenario: Close-delimited EOF is non-reusable completion

- **WHEN** a CloseDelimited decoder consumes all final input
- **THEN** it reports close-delimited completion evidence distinct from reusable delimited completion

### Requirement: EOF is sticky and truncation is exact

`finalInput` SHALL mean EOF follows the final byte offered in that call. The decoder SHALL require callers to reoffer an unconsumed final suffix with `finalInput = true`, make EOF sticky after consuming all final bytes, drain pending output before deciding completion or truncation, and report Truncated for premature EOF in fixed data, chunk syntax, chunk data, or trailers.

#### Scenario: Offered final suffix is not prematurely truncated

- **WHEN** a final-input call cannot consume all offered bytes because output is full
- **THEN** it reports NeedOutput rather than Truncated
- **AND** accepts the reoffered suffix with finalInput still true

#### Scenario: Fixed body underrun is truncated

- **WHEN** Fixed(4) reaches sticky EOF after consuming only three payload bytes
- **THEN** decoding fails with Truncated at the committed wire offset

#### Scenario: Pending output drains before terminal EOF decision

- **WHEN** sticky EOF is known and buffered output remains
- **THEN** the decoder first reports NeedOutput and decides completion or truncation only after that output drains

### Requirement: Chunk syntax is strict and fully bounded

Chunked decoding SHALL require nonempty hexadecimal sizes with checked u64 arithmetic and strict CRLF. It SHALL accept bounded token or quoted-string extensions including valid quoted escapes while ignoring their semantics, and SHALL reject signs, `0x` prefixes, bare LF, leading whitespace, malformed quoting, and trailing invalid syntax.

#### Scenario: Valid extensions contribute only framing bytes

- **WHEN** chunks contain token and quoted-string extensions with valid escapes
- **THEN** the decoder accepts them, counts their bytes against extension and wire limits, and emits only payload bytes

#### Scenario: Malformed chunk syntax fails at the committed offset

- **WHEN** a size line is empty, signed, prefixed with `0x`, contains leading whitespace, uses bare LF, or has invalid extension syntax
- **THEN** decoding fails with ChunkSyntax and exact wire offset and committed progress

### Requirement: Every framing resource has an independent finite limit

Limits SHALL include finite maxWireBytes, maxPayloadBytes, maxChunkBytes, maxChunks, maxChunkLineBytes, maxExtensionBytes, maxTrailerBytes, maxTrailerFields, and maxOwnedBytes values. Zero SHALL be enforced as a real bound. Wire bytes SHALL include framing, payload bytes SHALL exclude it, chunk count SHALL include the terminating zero chunk, chunk-line and trailer byte limits SHALL include CRLF delimiters, extension bytes SHALL accumulate across chunks, and owned bytes SHALL include backing storage and metadata indices but exclude caller buffers and allocator bookkeeping. All counters SHALL be checked before overflow.

#### Scenario: Terminal chunk counts toward chunk limit

- **WHEN** one data chunk and its terminating zero chunk are decoded with maxChunks equal to one
- **THEN** decoding fails at the terminating chunk with the chunk-count limit

#### Scenario: Wire and payload limits fail independently

- **WHEN** payload fits maxPayloadBytes but chunk framing exceeds maxWireBytes
- **THEN** decoding fails with the wire-byte limit without misreporting a payload limit

#### Scenario: Zero is not unlimited

- **WHEN** any governed resource is used while its configured maximum is zero
- **THEN** the corresponding precise limit failure is returned before excess is accepted

### Requirement: Trailers remain separate and policy-gated

Completed chunked trailers SHALL remain separate from initial headers and SHALL not alter framing or authentication retrospectively. They SHALL be exposed only after successful completion as a decoder-borrowed view, with an explicit fallible copy to independent ownership. The same explicit trailer policy SHALL validate advisory Trailer declarations and received or emitted trailer fields.

The nested `trailerValues` limits SHALL be revalidated at every retention boundary: received
trailers, completed-trailer copies, and encoder finish snapshots. This includes field count,
individual name and value bytes, aggregate field bytes, and owned bytes. Trailer iteration SHALL
either yield every retained validated field or trap on an internal invariant violation; it SHALL
never translate an invalid retained record into ordinary end-of-iteration.

#### Scenario: Default policy is narrowly safe

- **WHEN** the default trailer policy is used
- **THEN** only Content-Digest and Repr-Digest are admitted
- **AND** an allowed field may arrive without an advisory Trailer declaration

#### Scenario: Forbidden names cannot be configured in

- **WHEN** a declaration, received trailer, emitted trailer, or application allowlist contains Content-Length, Transfer-Encoding, Host, Connection, TE, Trailer, Upgrade, Authorization, Proxy-Authorization, WWW-Authenticate, Proxy-Authenticate, Cookie, Set-Cookie, Content-Encoding, Content-Type, Content-Range, Content-Location, Range, Cache-Control, Max-Forwards, Expect, or a Connection-nominated field
- **THEN** validation fails with TrailerPolicy even if application configuration attempts to allow it

#### Scenario: Borrowed trailers block mutation

- **WHEN** a completed trailer view still borrows decoder-owned storage
- **THEN** destruction or another operation requiring exclusive ownership is rejected by the language ownership rules

#### Scenario: Only chunked completion publishes trailers

- **WHEN** an Empty, Fixed, or CloseDelimited decoder completes successfully
- **THEN** its trailer accessor returns None
- **AND WHEN** a Chunked decoder completes successfully with zero or more trailers
- **THEN** its trailer accessor returns the completed separate trailer section

#### Scenario: Nested limits cannot be bypassed by a looser collection

- **WHEN** received, copied, or encoder-snapshotted trailers exceed any nested trailerValues limit
- **THEN** the operation fails with the exact limit kind, allowed value, attempted value, field index, and committed progress before allocation or finish emission

### Requirement: Decoder failure and single-message lifecycle have explicit semantics

A framing, syntax, limit, discard, or truncation failure SHALL poison the current decoder message. Later steps SHALL return InvalidState without replacing the original failure with completion. A decoder SHALL own exactly one message's affine Selection and trailer policy; processing a distinct message SHALL require constructing a fresh decoder so retained Connection exclusions and policy state cannot cross message boundaries.

#### Scenario: Failure is terminal

- **WHEN** a decoder fails and step is called again
- **THEN** the later call fails with InvalidState and exposes no completion evidence

#### Scenario: A distinct message requires fresh policy state

- **WHEN** a transport proceeds to another HTTP message
- **THEN** it constructs a new decoder from that message's Selection and trailer policy rather than resetting the prior owner

### Requirement: Incremental encoding enforces framing completion

Encoder construction SHALL reserve bounded working and trailer storage. Encoding SHALL retain no caller borrow and allocate nothing after construction. Empty SHALL reject nonempty payload, Fixed SHALL reject excess before accepting it and fail finishing underrun, Tunnel SHALL not be a body encoder, Chunked SHALL emit one terminating chunk and trailer block, and CloseDelimited SHALL finish by requiring transport closure.

#### Scenario: Fixed overrun preserves the boundary

- **WHEN** a Fixed encoder is offered payload beyond its declared length
- **THEN** it rejects before accepting the excess and reports FixedLengthOverrun with committed progress

#### Scenario: Fixed underrun fails at finish

- **WHEN** finishing begins before the declared fixed payload length is accepted
- **THEN** it fails with FixedLengthUnderrun and emits no successful completion evidence

#### Scenario: Chunked terminator is exactly once

- **WHEN** chunked finish completes and continuation is called again
- **THEN** the wire contains exactly one terminating zero chunk and the later call fails with InvalidState

#### Scenario: Close-delimited finish requires close

- **WHEN** a CloseDelimited encoder finishes
- **THEN** completion evidence says transport closure is required and never claims reusable delimitation

### Requirement: Finish initiation snapshots trailers before output

Finishing SHALL have distinct initiation and continuation operations. Initiation SHALL validate and copy trailers into already reserved storage before any finish byte is emitted. Continuation SHALL drain that immutable snapshot, accept no replacement trailers, allocate nothing, and report partial written progress on failure or output exhaustion.

#### Scenario: Invalid trailers emit no finish prefix

- **WHEN** finish initiation receives a syntactically invalid or policy-forbidden trailer
- **THEN** it fails before emitting a terminating chunk or any trailer bytes

#### Scenario: Continuation uses the original snapshot

- **WHEN** chunked finish pauses for output after successful initiation
- **THEN** later continuation emits only the captured trailers and exposes no operation to replace them

### Requirement: Completion evidence is opaque and persistence-safe

Completion evidence SHALL not be caller-constructible and SHALL distinguish delimited completion, close-delimited completion, tunnel handoff, and incomplete or failed state. It SHALL not claim outgoing flush, peer health, or HTTP persistence, and emitted payload SHALL remain provisional until framing and trailers finish successfully.

#### Scenario: Tunnel handoff is exclusive

- **WHEN** Tunnel decoding is selected
- **THEN** the decoder consumes zero bytes and yields opaque tunnel-handoff evidence without delimited-body evidence

#### Scenario: Incomplete and failed states have no positive evidence

- **WHEN** a decoder or encoder is active, abandoned, or failed
- **THEN** no delimited, close-delimited, or tunnel completion evidence is available

### Requirement: Discard and abandonment are explicit and bounded

Abandonment SHALL be terminal and perform no I/O. A pure discard operation SHALL use an explicit finite wire-byte budget including framing overhead and SHALL fail with DiscardLimit when exhausted. The framing API SHALL require its transport owner to close on abandonment, cancellation, framing failure, or discard failure and SHALL contain no hidden drain loop.

#### Scenario: Discard budget counts framing overhead

- **WHEN** chunked payload fits the remaining discard budget but its chunk lines or delimiters do not
- **THEN** discard fails with DiscardLimit and cannot produce reusable completion evidence

#### Scenario: Abandonment performs no transport work

- **WHEN** an active decoder or encoder is abandoned
- **THEN** it becomes terminal without reading, writing, or draining caller transport

### Requirement: Failures are precise and allocation timing is explicit

The API SHALL expose typed failures for invalid or conflicting framing, unsupported transfer coding, invalid length, size overflow, chunk syntax, trailer syntax or policy, each finite limit, truncation, fixed-length underrun or overrun, discard exhaustion, allocation failure, and invalid state. Failures SHALL carry wire offset and committed progress. OutOfMemory SHALL arise only during acquisition or explicit copying, never from allocation-free step or continuation operations.

#### Scenario: Failure includes already committed progress

- **WHEN** a step writes payload before encountering a later syntax or limit failure
- **THEN** the error reports those consumed and written counts and their cumulative committed totals

#### Scenario: Trailer copy owns its allocation failure

- **WHEN** an explicit completed-trailer copy cannot allocate
- **THEN** the copy returns OutOfMemory while the successfully completed decoder and borrowed view remain valid
