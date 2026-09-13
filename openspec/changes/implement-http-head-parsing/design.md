## Context

JUL-193 delivered byte-preserving HTTP values, ordered headers, request targets, Host validation,
and owned head copies. No head parser exists. The parser must accept fragmented caller slices while
retaining exact progress and producing views whose lifetime is tied to one reusable owned parser.
See `proposal.md` and `specs/http-head-parsing/spec.md` for the behavioral contract.

## Goals / Non-Goals

**Goals:**

- Keep request and response parsing on one shared byte-storage/state-machine foundation while
  preserving distinct public affine parser types and result heads.
- Allocate byte storage and header-offset records once at construction; keep feed, view, reset, and
  serialization allocation-free.
- Make every success and failure boundary exact enough for buffered transports to preserve suffixes
  without guessing.

**Non-Goals:**

- Body-length selection, chunk decoding, trailers, connection persistence, content decoding, I/O,
  sockets, TLS, and a general Stream abstraction.
- Historical permissive parsing such as bare LF, leading blank lines, obs-fold normalization, ICY,
  or HTTP/0.9 fallback.

## Decisions

### Store bytes and offsets, never self-referential HTTP views

Each parser owns a fixed byte allocation and fixed-capacity field-record allocation. Records contain
start and length values only. `head` resolves shared HTTP values and header views against a borrow of
that storage after completion. This matches the delivered `OwnedHeaders` representation and makes
reset/release conflicts visible to ownership analysis. Storing self-referential heads was rejected
because moving the parser could invalidate internal references.

### Share an internal scanner but expose request/response actors

`RequestParser` and `ResponseParser` own equivalent storage and state, while private helpers scan
CRLF lines, copy accepted bytes, record field ranges, and translate failures. Separate public types
avoid a runtime tag on every head access and keep request-only Host/target validation out of response
paths. Duplicating complete scanners was rejected because exact progress and line-ending behavior
would drift.

### Parse incrementally by committed cursor

The parser keeps a committed byte count, current line start, CR-pending state, field count, and
active/completed/failed state. Each input byte is examined once; it is copied only when accepted.
The terminating LF is the last consumed byte. Validation that can identify an offending byte occurs
before committing it. Parsing the whole accumulated buffer after every feed was rejected because it
rescans attacker-controlled prefixes and obscures per-call progress.

### Validate line components only when their CRLF completes

The scanner enforces line endings incrementally, then start-line and field helpers validate complete
ranges in owned storage and record offsets. On the final empty line it constructs shared values and
applies request Host validation before changing state to complete. This keeps partial views private
and ensures syntax completion is not confused with framing completion.

### Use two-pass serialization

Size helpers first validate every shared value and compute checked bytes. `writeRequestInto` and
`writeResponseInto` compare capacity before any mutation, then write canonical separators and field
lines. A scratch copy is unnecessary because all remaining failures have been excluded before the
write pass. Streaming serializers were rejected because JUL-194 owns only mutation-atomic memory
serialization and later transport actors can write the resulting bounded slices.

### Keep errors local and translate shared value failures

`ParseError` contains a closed parser reason, absolute offset, optional field index, and optional
limit counts. Shared `ValueError` failures are translated once with component-aware offsets while
retaining semantic distinctions. Allocator failure remains the existing separate effect error during
construction or explicit owned copy. Transport errors never enter this pure actor.

## Risks / Trade-offs

- [Upfront capacity can reserve more than a particular head uses] → finite caller-provided limits and
  `maxOwnedBytes` make the cost explicit and avoid mid-feed allocation failure.
- [Shared-value construction may expose missing borrowing primitives] → store only offsets and build
  views at `head`; extend shared HTTP values only when a genuine ownership gap is proven, without a
  compatibility path.
- [Split-point matrices can inflate the compiler suite] → compile one reusable acceptance program and
  run a small discriminating in-program table rather than compiling once per split.
- [Error-offset translation can drift between request and response paths] → centralize absolute range
  translation in the private scanner and assert representative first-failure boundaries.

## Migration Plan

This is an additive green-field actor. Register the module and generated surfaces, then consumers
JUL-195, JUL-23, and JUL-201 adopt it directly; no old parser or compatibility adapter exists.
