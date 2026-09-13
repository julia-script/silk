## Context

See `proposal.md` for motivation and `specs/http-content-decoding/spec.md` for the observable
contract. The relevant settled layers are deliberately narrow:

- `silk.http_body` selects framing and exposes an affine, allocation-free-after-construction pure
  decoder. It intentionally knows nothing about transport or content codings.
- `silk.buffered_duplex` is the exclusive callback-scoped owner of buffered transport access. Its
  input side preserves unread suffixes, accepts an absolute deadline, and retains the original
  `ByteIoError` inside `BufferError`.
- `silk.inflate` owns bounded raw/zlib/gzip state and `silk.zstd` owns bounded Zstandard state. Both
  are pure incremental machines after Effectful allocation and retain no caller buffers.
- A `Content-Encoding` stack has a hard maximum of four entries, so a fixed-size composition can
  remain ordinary Silk source. Repository policy forbids a compiler-known library actor or a host
  codec shortcut.

The content layer must preserve the framed decoder's exact boundary: trailers and bytes for a next
message may already be present in the buffered input when the final content decoder is still
draining or validating.

## Goals / Non-Goals

**Goals:**

- Keep framing, transport, and individual codec machines independently testable while composing
  them behind one scoped reader.
- Make acquisition, progress, terminalization, and connection-reuse evidence explicit enough that
  a later HTTP client can use them without inventing policy.
- Bound both expansion and owned storage before or at the byte boundary where each cost occurs.
- Use one target-neutral state machine for native and WebAssembly execution.

**Non-Goals:**

- Changing the public semantics of body framing, buffered I/O, inflate, or Zstandard.
- Request-body content encoding, transparent dictionary lookup, quality-value negotiation, or a
  general-purpose Stream abstraction.
- Draining failed or abandoned bodies, deciding pool membership, or closing the underlying
  transport from the nested content scope.
- Adding Brotli or accepting raw DEFLATE under HTTP `deflate`.
- Promising cleanup after a fatal trap that bypasses Effect finalizers and Drop.
- Proving that caller-supplied body bytes belong to the retained response head, or preventing a
  low-level caller from copying a head and replanning already-decoded bytes. JUL-23 owns that
  affine response/body envelope and its decode-once guarantee.

## Decisions

### 1. One `silk.http_content` actor owns public policy and composition

The module will expose the related affine `ResponseContext`, `Mode`, `Coding`, `Limits`,
`CodingPlan`, `ContentReader`, `ContentProgress`, `ContentError`, `RepresentationView`, completion,
reuse-disposition, and Accept-Encoding values and operations. Its private implementation will
contain the fixed composition state. No semantic, HIR, MIR, evaluator, native-provider, or backend
recognition is needed.

This keeps one actor per concept and makes the content layer ordinary source. Splitting planning,
negotiation, and reading into generic helper modules would create kind-based grab bags; teaching
the compiler or a native provider about `http_content` would violate minimal compiler privilege.

### 2. One affine context atomically owns response selection inputs

`ResponseContext<'head>` has private fields and one public constructor:

`ResponseContext.make(head, requestMethod, trailerPolicy) -> Result<ResponseContext, BodyError>`.

`make` invokes `http_body.selectResponse` itself and, only on complete success, stores the original
head, method, trailer policy, affine `Selection`, selected `Framing`, and exact
`Selection.anomaly()`. Callers can neither inject a separately computed selection nor replace one
component after construction.

`CodingPlan.make` consumes only `ResponseContext` plus mode, body limits, and content limits; it
does not accept a separate head, method, selection, or trailer policy. It moves the complete
single-use context into the plan, including the anomaly, and stores at most four parsed coding
entries in wire order plus a reverse application schedule. `withReader` later consumes the plan.
This ownership prevents reuse or mismatch of one context and its affine selection; it does not
establish the provenance of bytes supplied later to `withReader`.

For each invocation, `withReader` treats the caller-supplied `BufferedDuplex` body as the encoded
representation described by the plan. A caller can copy the `ResponseHead`, construct another
context, and supply unrelated or already-decoded bytes. The low-level content actor cannot prove
otherwise because it does not own the response/body envelope. JUL-23 will own that affine envelope,
tie the selected head and body bytes together, and prevent transparent decoding from being applied
twice at the HTTP-client layer.

Semantic no-body classification is taken from the context's selected `Framing.Empty`. A plan
retains `Anomaly.None`, `Anomaly.ProhibitedFraming`, or `Anomaly.Http10TransferEncoding` exactly;
non-None anomalies make its reuse disposition permanently nonreusable even though the empty body
can complete. `Framing.Tunnel` is rejected. No-body returns before `Content-Encoding` parsing.
Otherwise Decode parses every repeated header field and comma element, rejecting empty elements,
parameters, invalid tokens, unsupported names, and excess depth. A 206 plan with a parsed
nonidentity coding is then rejected. Raw records no parsed coding list at all.

Alternatives considered:

- Accepting a caller-built `Selection` would permit head/method/policy provenance mismatches.
- Reselecting later inside `CodingPlan.make` would make context construction non-atomic and duplicate
  JUL-195 policy.
- An encoded/decoded type-state on the context would claim a byte-provenance guarantee that this
  low-level API cannot enforce because the caller supplies the body separately.
- Borrowing rather than consuming the context or selection would permit two decoders over one
  response.
- Parsing Raw metadata for display would make malformed metadata an accidental Raw-mode failure.

### 3. `withReader` nests inside the existing buffered transport scope

`withReader` consumes a plan, exclusively borrows one caller-supplied `BufferedDuplex` as the
encoded representation for that invocation, and invokes a higher-ranked once callback with a
`ContentReader` whose lifetime cannot escape. The callback preserves generic success, typed error,
and requirements. Reader acquisition uses an Effect bracket: codecs, body
decoder, and buffers are acquired before use; the non-failing release marks an incomplete reader
abandoned and lets owned state drop on success, typed failure, and structured
cancellation/interruption. Partial acquisition is nested so every earlier acquisition is released
exactly once. Fatal traps that bypass Effect finalizers and Drop are explicitly outside this
cleanup contract.

The content release performs no I/O, drain, flush, or close. The outer `BufferedDuplex` owner remains
responsible for the transport lifetime. A later HTTP client may retain the connection only if it
obtains the content completion proof; otherwise its outer scope closes or discards the connection.

An alternative in which `ContentReader` owns a raw `ByteDuplex` would duplicate buffering and close
ownership. A freely returned reader would allow its transport and response-head borrows to escape.

### 4. A private pure core composes body framing and decoder stages

After acquisition, `ContentReader.readSome(output, deadline)` is the only Effectful adapter. It
peeks the buffered input, advances a private pure core, consumes exactly the core's committed wire
prefix, and calls `fill` only when the core needs input. It passes the same caller-supplied absolute
deadline to every fill. Codec work is synchronous and performs no clock or provider access.

The core owns the `http_body.Decoder`, up to four codec variants, fixed progress/terminal metadata,
and fixed-capacity staging buffers. For `N` nonidentity stages it allocates `N` buffers: one receives
deframed encoded bytes and each remaining buffer joins adjacent stages. The final stage writes
directly to the caller. Raw and identity-only Decode plans bypass codec staging and let the body
decoder write directly to the caller.

Each edge retains read/write cursors and an upstream-final flag. The scheduler first drains pending
downstream output, then advances the nearest stage that can make progress, and only asks for more
transport input when no internal transition is possible. Every loop iteration must consume input,
write output, change a final/terminal phase, or return `NeedInput`/`NeedOutput`, preventing zero-work
spins.

Keeping the body decoder outside the core was considered, but it would require a second imperative
state machine in the transport adapter and make framing/content chunking invariance harder to
establish.

### 5. Finality advances edge by edge

Transport end is supplied only to the body decoder. The framed body becomes the encoded edge's
final producer only after its exact boundary is complete. Stage `i` receives final input only when
stage `i - 1` is finished and the edge between them contains no unoffered bytes. If a stage reports
`NeedOutput`, its unread input and final flag remain stable while downstream space is made.

Content completion is recorded only after the body decoder exposes completion, every codec has
validated `Finished`, and the final pending output suffix has been returned. This prevents outer EOF
from being broadcast through the stack and prevents a full destination from hiding terminal bytes
or checksum validation.

For the single-stream zlib profile, a finished decoder with unconsumed input, a nonempty encoded
edge, or later body payload is a content-layer `TrailingEncodedData` failure. The gzip and Zstandard
machines retain their existing concatenated-member/frame semantics.

### 6. Limits are enforced where bytes become committed

`Limits` contains finite `maxEncoded`, `maxIntermediate`, `maxDecoded`, `maxOwned`, a default coding
depth of four constrained to one through four, an intermediate capacity constrained to one through
65,536, and one complete limit value for each codec family. Defaults are finite and are copied into
the plan.

- The body decoder's payload output is capped by remaining `maxEncoded` before bytes enter the
  encoded staging edge or caller output.
- Output capacity passed to every nonfinal codec is capped by the remaining aggregate
  `maxIntermediate` allowance.
- Output capacity passed to the final representation stage, or to the body decoder on a no-codec
  path, is capped by remaining `maxDecoded`.
- All additions use checked arithmetic. A zero remaining allowance is distinguished from an empty
  caller destination so the next attempted byte raises the matching limit instead of reporting
  `NeedOutput` forever.

Content `maxOwned` is preflighted before allocation. It charges every staging-buffer reservation,
fixed composition record, `inflate.MEMORY_BOUND` reservation for each inflate-family stage, and the
configured `windowBytes + workspaceBytes` reservation for each Zstandard stage. The body decoder's
own trailer/framing allocations continue to be bounded by `http_body.Limits.maxOwnedBytes` and are
not double-counted. Caller destinations and allocator bookkeeping are excluded.

Charging configured codec reservations rather than inspecting allocations gives deterministic,
portable admission and does not require widening codec internals. It can conservatively reject a
plan that might use fewer bytes at runtime, which is preferable to under-accounting.

### 7. Progress and errors are translated once at the outer boundary

`ContentProgress` reports call-local `written`, cumulative encoded/intermediate/decoded totals, and
`Data` or `End`. A successful zero-producing codec transition can therefore advance totals without
inventing output. `ContentError` includes the same coordinates and a tagged reason for invalid
configuration/state, syntax or unsupported coding (with element index/token), partial response,
body framing, buffered/byte I/O, codec failure (with reverse stage and wire index), trailing encoded
data, arithmetic overflow, and each content limit.

The core translates the private codec variants into `ContentError` exactly once. `BufferError` and
its nested `ByteIoError`, `BodyError`, `DecodeError`, and `ZstdError` remain available as typed causes
rather than being flattened to text. A failure atomically poisons the reader after recording its
coordinates; all later operations return `InvalidState` with zero new progress. Output already
returned remains explicitly provisional.

### 8. Metadata and completion remain borrowed evidence

`RepresentationView` borrows its reader and exposes the untouched original `ResponseHead`, applied
plan, exact framing anomaly, whether decoding was applied, and optional decoded length. Raw reports
that decoding was not applied. Decode reports it only when at least one nonidentity codec was
applied; no-body and identity-only plans remain an effective no-op. Length is `None` until
completion except semantic no-body, whose length is immediately zero.

The view does not synthesize a replacement head or delete `Content-Encoding`/`Content-Length`.
It exposes no convenience operation that reconstructs a response context from the view. This is an
API-shape decision, not a byte-provenance guarantee: a caller can still copy the original head and
create a new low-level context. JUL-23's affine response/body envelope is responsible for preventing
already-decoded bytes from being planned again by the streaming client.

An opaque `ContentCompletion` borrows the completed reader, includes the underlying body completion
kind, decoded total, exact anomaly, and a `ReuseDisposition`. The disposition is
`EligibleAfterCompletion` on a live delimited plan with `Anomaly.None`, `Reusable` only on its
successful completion, and `NonReusable` for close-delimited framing or any non-None anomaly.
Failures and abandonment produce no completion at all. Trailers are borrowed from the completed
body decoder.

### 9. Accept-Encoding is a pure, explicit request policy

`AcceptEncoding<'value>` distinguishes automatic, explicit override value bytes, and omission.
Automatic settings use an `EnabledCodings` value and an explicit empty policy (`Identity` or
`Omit`). Decode selects enabled names in the static order `gzip`, `deflate`, `zstd`; Raw automatic
mode selects no value. `x-gzip` is accepted only as response metadata and is never advertised.

The pure generator has the shape
`acceptEncoding(mode, policy, httpHeaderLimits) -> Result<Option<Header>, ValueError>`. Whenever a
field is selected, including an override, it calls
`Header.make("Accept-Encoding", selectedValueBytes, httpHeaderLimits)`. Automatic bytes are static;
override bytes are borrowed and preserved exactly. Thus invalid bytes, leading/trailing OWS, and
value/name limit failures retain the standard `ValueError` reason, component, and offset, while
omission returns `Option.None`. Negotiation output does not alter response-plan validation.

A pure value generator is sufficient because header insertion belongs to the future HTTP client.
Automatically mutating a request head here would couple this actor to request serialization and
make caller overrides ambiguous. Returning a bare string or byte slice was rejected because it
would bypass `silk.http` validation. Accepting an arbitrary prebuilt `Header` was rejected because
the caller could supply the wrong field name; taking only override value bytes lets this actor own
the fixed name while `Header.make` owns all HTTP value validation.

### 10. Evidence is consolidated at the cheapest adequate tiers

One analysis snapshot will cover the sole context constructor, atomic selection ownership, parsing
shape, affine context/plan/reader ownership, higher-ranked scope, Effect rows, and opaque
completion/representation constraints. One compact ordinary-source acceptance program
will be shared by native and portable LLVM-to-Wasm corpus entries for runtime composition behavior.
It will use existing small codec fixtures rather than rerunning codec conformance matrices already
owned by inflate and Zstandard tests.

The shared runtime source will use distinct return codes for raw bypass, reverse stacks, every
framing mode, staged finality, no-body/206 and no-body anomaly policy, strict zlib/trailing bytes,
independent limits, progress/error provenance, metadata/completion/reuse, structured-exit
acquisition release, and validated Accept-Encoding output/`ValueError`. Documentation examples will
be compiler-realized once, not each compiled as an additional backend program.

## Risks / Trade-offs

- [Conservative owned-memory admission can reject a configuration whose codec would use less than
  its allowance] → Document that `maxOwned` charges declared reservations and keep the calculation
  deterministic and checked.
- [A four-stage fixed scheduler has many cursor/finality combinations] → Centralize transitions in
  one pure core, require a progress invariant, and exercise chunk/destination boundary matrices in
  the shared acceptance source.
- [Decoded bytes can be acted on before a late checksum failure] → Mark all output provisional and
  make the opaque completion proof the only positive validation/reuse evidence.
- [Nested content abandonment does not itself close a compromised connection] → Make release
  synchronously abandon the body and withhold completion; the outer connection owner must close or
  discard any session without proof.
- [Fatal traps can bypass the language's Effect finalizers and Drop] → State this boundary in the
  API and reference; guarantee cleanup only for structured Effect exits.
- [The private body-to-content buffer adds one copy per coded response] → Use one caller-configured
  fixed buffer per active stage, bypass it entirely for Raw/identity, and preserve bounded memory
  and exact framing over speculative zero-copy complexity.
- [Header token offsets span repeated borrowed fields rather than one contiguous string] → Report
  wire coding index plus source field index/offset so diagnostics remain exact without allocating a
  normalized header value.

## Migration Plan

This is a new actor in a green-field repository, so there is no compatibility migration. Add and
register `silk.http_content`, its public reference, and its consolidated acceptance evidence in one
change. Existing direct uses of body framing, inflate, and Zstandard remain valid and unchanged.
Rollback removes only the new actor, registration, documentation, and ticket-local acceptance
material; no stored data or wire format is introduced.
