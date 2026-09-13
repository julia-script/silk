## Context

See `proposal.md` for motivation and `specs/http-body-framing/spec.md` for the observable contract. The dependency head supplies validated start lines, ordered headers, exact head boundaries, and borrowed field iteration. No existing module owns body-length precedence, chunk syntax, trailer storage, or completion evidence. The implementation must remain ordinary Silk source, portable through the shared native and LLVM-to-Wasm paths, and independent of buffered transports and content decoding.

The compiler currently supports allocator-backed `Bytes`, affine owners with borrowed views, checked scalar arithmetic, and fixed-capacity caller slices. Those facilities are sufficient; no compiler privilege or host intrinsic is justified.

## Goals / Non-Goals

**Goals:**

- Keep every byte-boundary decision in one pure module shared by clients and servers.
- Make acquisition the only allocation point for streaming state, with explicit copy operations as the only later allocation boundary.
- Preserve exact per-call and cumulative progress across success and typed failure.
- Make reuse-related outcomes impossible to forge or infer from an ambiguous boolean.
- Keep the portable runtime corpus compact while exercising split input, tiny output, EOF, limits, and suffix preservation.

**Non-Goals:**

- Reading from or writing to a transport, flushing output, draining on cancellation, or closing sockets.
- Content-coding decompression, response semantics such as enforcing the 205 no-content rule, or authentication interpretation.
- Supporting transfer-coding stacks, legacy whitespace recovery, lenient line endings, or unbounded trailer storage.
- Treating Trailer declarations as a promise that listed fields will arrive.

## Decisions

### 1. One `silk.http_body` owner with selection, decoder, encoder, and evidence actors

The module will expose a small family of related public values:

- `Framing`: `Empty`, `Fixed { length: u64 }`, `Chunked`, `CloseDelimited`, or `Tunnel`.
- `Selection`: an affine framing/anomaly value retaining two fresh head iterators until construction copies Connection-nominated exclusions.
- `Limits`: u64 wire/payload/extension budgets plus usize chunk, line, trailer, field, and owned-capacity budgets where actual allocation or indexing requires usize.
- `TrailerPolicy`: a borrowed explicit allowlist with a default constructor for Content-Digest and Repr-Digest.
- affine `Decoder` and `Encoder` owners, their progress states, and borrowed `Trailers`/completion views.
- one closed `BodyError` family containing component, reason, wire offset, per-call progress, and cumulative committed totals.

Selection functions receive the validated borrowed `silk.http_head` request or response head. Response selection additionally receives the corresponding request method. Trailer declarations are validated during selection using the same policy later given to the decoder. The affine selection retains independent iterators so `Decoder.make` or `Encoder.make` can size and copy Connection-nominated exclusions without rescanning or extending the parsed-head borrow beyond construction. Outgoing validation is a separate entry point so received anomaly handling cannot accidentally relax sender requirements.

Alternative considered: attach framing methods to parsed heads. Rejected because it couples head parsing to transfer policy, prevents clean reuse for programmatically constructed heads, and makes request-method-dependent response rules awkward.

### 2. Scan ordered headers without normalization or combination

Selection walks the head's insertion-order iterator once. It counts exact case-insensitive matches for Content-Length, Transfer-Encoding, Trailer, and Connection. Content-Length is accepted only when its sole field value is a nonempty ASCII decimal sequence with checked u64 accumulation. Transfer-Encoding is accepted only when its sole value is exactly one parameterless `chunked` token after optional outer OWS; commas, parameters, duplicate fields, and other tokens fail rather than being normalized.

Connection values are parsed as comma-separated tokens solely to build the non-overridable trailer exclusion set. Trailer declarations use the same token parser and `TrailerPolicy` validation as completed trailers. The module never mutates or merges initial headers.

Alternative considered: use a convenience combined-value lookup. Rejected because combining destroys the distinction between repeated Content-Length fields and comma lists, both of which this profile must reject.

### 3. Decoder is a tagged phase machine with no payload staging

`Decoder.make` preallocates:

- a chunk-line buffer bounded by `maxChunkLineBytes`;
- trailer bytes bounded by `maxTrailerBytes`;
- fixed-width trailer records bounded by `maxTrailerFields`; and
- copied trailer-policy/Connection-nominated name bytes and indices needed after the initial head borrow ends.

Owned-byte accounting includes every reserved payload capacity and metadata record width, checked before allocation. The decoder then advances through explicit phases: fixed/close payload, chunk-size line, chunk payload, chunk payload CRLF, trailer lines, complete, tunnel, failed, or abandoned. Chunk payload is copied directly from caller input to caller output, so it is consumed only when it is written and needs no private payload buffer. Framing syntax is retained only in the bounded line/trailer backing required to validate it or expose the final view.

`finalInput` becomes sticky only when the call consumes the entire offered slice. If output fills first, the call returns NeedOutput and the caller reoffers the suffix with the flag. After sticky EOF, a zero-input call can finish close-delimited input, surface truncation, or drain any pending owned bytes; it cannot revert EOF.

Alternative considered: copy all input into a decoder ring buffer. Rejected because it obscures exact ownership of unconsumed suffixes, raises maxOwnedBytes unnecessarily, and duplicates buffered transport responsibility.

### 4. Chunk-line parsing validates grammar after complete strict CRLF

The decoder accumulates one line through CRLF, rejecting bare LF immediately and CR not followed by LF. Once complete, a private parser validates:

1. at least one hexadecimal digit and checked u64 accumulation;
2. zero or more `;` extensions;
3. token extension names, optional `=`, and token or quoted-string values;
4. valid quoted-pair escapes and visible/allowed bytes; and
5. no whitespace or trailing bytes outside that grammar.

The terminating zero chunk counts toward `maxChunks`. Per-chunk and cumulative extension budgets are checked before committing the line. Chunk-size, payload, and cumulative wire/payload arithmetic is checked before increment. Data-chunk CRLF is a separate two-byte phase, which makes split boundaries and exact wire offsets straightforward.

Alternative considered: validate syntax byte-by-byte while accumulating. Rejected because quoted-string escape context and limit error precedence become harder to audit, while bounded complete-line validation remains allocation-free and cheap.

### 5. Trailer backing is packed and published only at successful completion

Trailer lines reuse the strict field grammar: nonempty token name, immediate colon, OWS-trimmed value, strict CRLF, no obs-fold, and the shared field-value byte rules. Each accepted field is copied into packed trailer backing with fixed-width offset records. The terminating empty line is counted in trailer and wire budgets but produces no field record.

`Trailers<'decoder>` borrows the packed bytes and records only after chunked completion. Iteration yields existing validated `Header` values without allocation. `Trailers.copy` uses the allocator to create an independent `OwnedTrailers` owner with the same packed representation and value limits. Reset/destruction requires exclusive decoder ownership, so the language prevents it while a trailer or completion view is live.

Alternative considered: reuse the initial `Headers` collection by appending. Rejected because it would erase security provenance and let late trailers alter earlier framing or authentication decisions.

### 6. Encoder stages at most one bounded chunk and snapshots finish metadata

`Encoder.make(selection, limits, policy)` consumes the affine selection and preallocates one payload staging buffer up to `maxChunkBytes`, a finish snapshot, and copied policy/Connection-exclusion storage. A normal chunked `step` first drains pending syntax/data, then accepts at most one bounded input prefix into owned staging and emits it incrementally. Fixed and close-delimited encoding can copy directly into output; fixed checks remaining length before accepting input. Empty rejects any nonempty input and Tunnel construction fails as unsupported for body encoding.

Finishing is split into `beginFinish(trailers)` and `continueFinish(output)`. Initiation validates framing state, exact fixed length, trailer grammar, the policy/exclusions retained at construction, byte/field limits, and copies the full trailer snapshot before selecting any finish-emission phase. Continuation has no trailer argument and only drains that immutable snapshot. Chunked completion emits `0\r\n`, serialized trailer lines, and one final CRLF. Empty and fixed complete without suffix bytes; close-delimited completion records transport-close-required evidence. Nonempty trailers on non-chunked framing fail rather than being silently ignored.

Alternative considered: accept trailers on every finish call. Rejected because caller mutation between partial writes would make the wire image nondeterministic and could emit a valid prefix before discovering an invalid replacement.

### 7. Progress is committed at byte granularity and shared by success and failure

Each call starts local consumed/written counters and updates cumulative u64 wire/payload counters only after the corresponding byte is committed. Error construction copies both local counters and cumulative totals plus the current zero-based wire offset. Limit checks occur before the byte or unit that would exceed a budget. This means partial streaming output remains visible without pretending operations are atomic.

Counter conversion from slice `usize` lengths to u64 is checked, and every addition uses a checked helper. SizeOverflow is distinct from a configured limit failure.

Alternative considered: report only a wire offset. Rejected because a caller with distinct input and output cursors cannot recover safely from partial output using one coordinate.

### 8. Completion evidence is borrowed and opaque

The module will expose a public `CompletionKind` but make the evidence struct's discriminant field private. Evidence is obtained only as a view borrowing a completed decoder or encoder, with kinds for `Delimited`, `CloseDelimited`, and `Tunnel`. Active, failed, and abandoned owners return no evidence. The evidence deliberately says nothing about flush, peer health, or whole-connection persistence; the transport owner combines it with its own state.

Alternative considered: return `reusable: bool` in step progress. Rejected because a copied boolean can outlive reset, collapse tunnel and close-delimited outcomes, and be mistaken for proof of transport health.

### 9. Discard reuses decoder syntax but has a separate finite budget

`discard(input, finalInput, maxDiscardWireBytes)` drives the same phase machine while skipping payload rather than requiring output. It decrements its explicit budget for every consumed wire byte, including size lines, data delimiters, and trailers. Exhaustion is a terminal DiscardLimit failure. `abandon` changes the owner to a terminal state without touching caller buffers or any transport.

Alternative considered: repeatedly call `step` with a scratch output buffer. Rejected because it hides the drain loop and budget from the API and may accidentally authorize unlimited work.

### 10. Verification uses one analysis program and one portable runtime corpus

Focused TypeScript tests will build one structured analysis snapshot for source-contract claims: public importability, signatures, borrow rejection while trailer/evidence views live, and absence of hidden requirements. StaticEvaluation will cover small pure selection and checked-parse claims when supported. One exported Silk acceptance source will cover runtime framing, split/tiny-buffer behavior, limits, trailers, encoder finish, and terminal states. The coordinator will register that source once in the shared native corpus and run its intended LLVM-to-Wasm leg rather than adding a per-feature native compilation test.

## Risks / Trade-offs

- [Large state surface can hide precedence bugs] → Keep selection, decoder, encoder, and trailer-policy state transitions separate; use closed enums and table-oriented acceptance cases.
- [u64 wire counters meet usize slice indices differently on Wasm] → Check every conversion and keep allocation/index capacities in usize while protocol lengths remain u64.
- [Preallocating max chunk and trailer capacity can be expensive] → Make all capacities explicit, enforce maxOwnedBytes before allocation, and document exact accounting.
- [Strict transfer-coding rejection reduces interoperability with legacy peers] → This is intentional security policy; unsupported stacks fail explicitly rather than being guessed or partially decoded.
- [Connection-nominated exclusions can consume policy storage] → Bound and account copied names during construction/selection, failing before decoding if the policy cannot be represented.
- [General streaming output is not atomic] → Preserve local and cumulative committed progress in every typed failure and document payload as provisional until successful finish.

## Migration Plan

This is a green-field capability. Add the ordinary Silk module, its generated registration, focused tests, portable acceptance registration, and reference page together. Downstream HTTP client/server work adopts this API directly; there is no compatibility layer or legacy path to migrate. Rollback removes the new module and ticket-local artifacts before downstream consumers land.
