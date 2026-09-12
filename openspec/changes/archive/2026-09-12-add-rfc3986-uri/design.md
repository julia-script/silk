## Context

PR #401 review requests allocation-free borrowed parsing and caller-controlled output storage.
The original implementation copied every parsed input and used multiple intermediate buffers for
resolution. Existing storage has reserve/clear/truncate internally, but lacks exact reservation
and ownership-taking text conversion. Runtime borrowed subslicing is not currently available.

## Goals / Non-Goals

**Goals:** Preserve strict RFC syntax and lossless ranges while making copies explicit, reusing
output storage, and supporting construction and selective serialization from components.

**Non-Goals:** URI-specific compiler recognition, networking policy, normalization, or compatibility
with the superseded owned-only API. Retain all policy exclusions in the specification.

## Decisions

- `UriReference` and `Uri` are immutable lifetime-bound borrowed values holding input text and
  validated scalar ranges. `parse` is synchronous and returns a typed Result without an allocator.
  `OwnedUriReference` and `OwnedUri` hold String plus ranges, never references to their own fields.
  Explicit copy operations allocate; ownership-taking String constructors validate and adopt storage.
  Owned values expose borrowed views using their stored ranges rather than reparsing.
- Add one general checked shared-slice view intrinsic, preserving input lifetime and element type.
  Bounds follow indexing's trap contract, including a valid zero-length range at the end. Ordinary
  slice/text wrappers and the URI validator own all higher-level policy. RawBuffer views cannot
  serve borrowed input without inventing ownership, so that alternative is rejected.
- Expose exact reservation through existing Vector/Bytes storage, Bytes clear/truncate, and
  allocation-free String/Bytes ownership conversion. Owned percent and component outputs calculate exact required size; resolution reserves a safe
  upper bound before in-place dot removal. All owned outputs
  reserve once, and initialize only actual output. No URI-specific storage escape hatch is added.
- Resolution uses a single reusable Bytes destination: reserve enough space, emit the selected
  prefix/path, compact literal dot segments in place, then append selected query/fragment. Reparse
  the final text without allocation. Owned convenience output transfers the completed Bytes into
  String and then into the owned URI; it never copies the finished serialization.
- Percent coding exposes reusable Bytes output, writer encoding, and strict in-place decoding.
  Validation and size calculation precede output mutation. Owned conveniences reserve exact size
  once and share emission logic. An unchanged decoding fast path borrows its original bytes.
- Component construction distinguishes raw bytes from already encoded text. Structured authority
  retains unsplit userinfo and lexical ports. Builders serialize into reusable storage, validate
  component boundaries, and reparse the completed reference. Callers modify a components value
  derived from a parsed reference and rebuild it. Selective serialization supports omitting
  authentication and emitting path/query without forcing allocation.
- Encoding contexts include whole path and unreserved-only, in addition to the existing six.
  A first-relative-path policy protects colon in the first segment. Already encoded components
  retain escape spelling and must still satisfy their component grammar.
- Recomposition remains literal RFC 3986 sections 5.2/5.3. Dot removal may expose authority syntax
  or produce a typed syntax failure from valid inputs; retain the existing examples and tests.
- Extend the existing consolidated native corpus, and use focused semantic assertions for lifetime
  safety. Verify allocation contracts through a counting/failing allocator, not timing assertions.
  Do not create a per-feature backend matrix or per-vector compilation.

## Risks / Trade-offs

- Borrowed results can escape input storage → explicit lifetimes and negative ownership assertions.
- Output can alias input → exclusive borrowing prevents overlap; in-place decode uses one buffer.
- Component delimiters can change meaning → validate each supplied encoded component and the
  recomposed result; preserve explicit empty/absent distinctions.
- Writer failure can leave partial output → document streaming behavior; malformed decoding and
  size/validation failures occur before reusable output is changed.
- Reusable destinations may grow → explicit allocator requirement and exact reservation; warm
  capacity does not allocate, and owned output uses only its single backing allocation.
