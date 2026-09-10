## Purpose

Define portable DEFLATE decoding with bounded storage, caller-owned output, and explicit stream termination for raw, zlib, and gzip inputs.

## ADDED Requirements

### Requirement: Decoding is resumable ordinary Silk

`silk.inflate` SHALL expose an owned decoder, explicit `Raw`, `Zlib`, and `Gzip` formats, caller-configured limits, and a pure step over borrowed input and mutable caller-owned output. Construction SHALL acquire bounded storage through the ordinary allocator. Steps SHALL retain no input or output borrow and SHALL perform no allocation. Success SHALL report exact consumed and written byte counts and `NeedInput`, `NeedOutput`, or `Finished`. Failure SHALL report a typed reason and exact progress for that call. Failed state SHALL reject reuse; finished state SHALL return `Finished` with zero progress.

#### Scenario: Suspend inside a field

- **WHEN** a non-final input chunk ends inside a header, block, code, extra bits, or trailer
- **THEN** the decoder preserves partial state and returns `NeedInput` without requiring replay of consumed bytes

#### Scenario: Suspend during an overlapping match

- **WHEN** output fills before a backward match is complete
- **THEN** the next call resumes the match with the same bytes and exact per-call progress

### Requirement: Final input and terminal states are explicit

Each step SHALL accept a final-input boolean. Once true, the caller SHALL resubmit only the unconsumed suffix with the flag true; the decoder SHALL reject an inconsistent flag or remaining length. Missing required bytes after final input SHALL produce `TruncatedInput`. Raw and zlib SHALL finish at their first validated stream boundary and leave trailing bytes unconsumed. Gzip SHALL decode all concatenated members, SHALL return `NeedInput` at a non-final member boundary, and SHALL finish only at a validated member boundary with no remaining final input. Gzip trailing non-member data SHALL fail. Every loop SHALL consume input, emit output, advance bounded internal work, or return a suspension, terminal state, or failure. An empty output slice SHALL produce `NeedOutput` when emission is required, including with final input.

#### Scenario: Gzip member ends before the input stream

- **WHEN** a complete gzip member is supplied without the final-input signal
- **THEN** the decoder waits for another member or explicit end-of-input and does not return `Finished`

#### Scenario: Final chunk requires several output buffers

- **WHEN** a final input chunk causes `NeedOutput`
- **THEN** the caller can resubmit its unconsumed suffix as final input until completion without truncation or duplicate consumption

### Requirement: One core decodes every DEFLATE block kind

The decoder SHALL implement RFC 1951 stored, fixed-Huffman, and dynamic-Huffman blocks with at most 32768 bytes of history. It SHALL support overlapping backward copies and matches crossing block boundaries. It SHALL reject reserved block and symbol values, invalid stored lengths, oversubscribed or invalid incomplete code trees, missing end-of-block codes, invalid repeat sequences, and distances beyond initialized history or the admitted window. Legal empty distance alphabets and one-symbol incomplete alphabets SHALL be accepted where DEFLATE permits them.

#### Scenario: Match crosses the circular history boundary

- **WHEN** a legal match references initialized bytes across history wraparound
- **THEN** decoding emits the exact referenced sequence with overlap semantics

#### Scenario: Malformed Huffman table

- **WHEN** a dynamic header defines an oversubscribed tree or a repeat beyond the declared alphabet
- **THEN** decoding fails with a typed coding error without indexing outside internal storage

### Requirement: Container validation precedes success

Zlib SHALL validate RFC 1950 CMF/FLG, compression method, window declaration, and FCHECK; FDICT SHALL produce a specific unsupported-dictionary failure. Zlib SHALL compare big-endian Adler-32 over emitted uncompressed bytes. Gzip SHALL validate RFC 1952 magic, method, reserved flags, optional extra data, zero-terminated name and comment, and FHCRC when present. Optional metadata SHALL be skipped without retaining it. Gzip SHALL compare little-endian CRC-32 and ISIZE for each member and reset member checksum/history while preserving cumulative limits. Errors SHALL distinguish malformed headers/coding, unsupported method/dictionary, truncation, checksum mismatch, size mismatch, invalid use, and the violated resource limit.

#### Scenario: All gzip optional fields arrive one byte at a time

- **WHEN** a valid gzip header includes FEXTRA, FNAME, FCOMMENT, and FHCRC across separate chunks
- **THEN** the decoder validates its header checksum, discards metadata, and decodes the member normally

#### Scenario: Corrupted trailer

- **WHEN** any wrapper checksum or gzip ISIZE disagrees with the decoded bytes
- **THEN** decoding fails before `Finished`, even if unverified output was emitted on earlier calls

### Requirement: Limits apply cumulatively

Caller limits SHALL bound total consumed input bytes, emitted output bytes, members started, header bytes including optional fields, and decoder-owned state/table memory. All counters SHALL be overflow-safe and checked before the corresponding consumption, emission, member start, or allocation. Limits SHALL never reset across calls or gzip members. Raw and zlib SHALL each count as one member. Fixed state/table storage SHALL have a documented upper bound enforced at construction; increasing the configured bound SHALL not grow decoder storage. Limit failures SHALL identify the exceeded limit. Caller input/output storage and allocator metadata SHALL be excluded from the decoder memory bound and documented as such.

#### Scenario: Output limit across members

- **WHEN** each gzip member fits the output limit but their combined output exceeds it
- **THEN** the first byte beyond the cumulative limit is not emitted and decoding returns an output-limit failure

#### Scenario: Header and memory limits

- **WHEN** an optional field exceeds remaining header allowance or the configured memory allowance cannot hold decoder storage
- **THEN** decoding fails before consuming the excess header byte or allocating decoder storage respectively

### Requirement: Conformance and documentation are reproducible

The manifest and generated stdlib surfaces SHALL expose the module. Prescriptive and source documentation SHALL describe ownership, provisional output before checksum validation, resource accounting, final-input obligations, trailing input policy, and terminal/error reuse. Repository tests SHALL use published vectors and differential fixtures verified with a pinned independent implementation. One consolidated shared native corpus program SHALL cover stored/fixed/dynamic blocks, every input-byte boundary of representative syntax and trailers, small output buffers, concatenated members, malformed input, and cumulative limit boundaries without repeated compiler pipelines per case.

#### Scenario: Reproduce fixture provenance

- **WHEN** a maintainer reads the fixture provenance and executes the documented independent verification
- **THEN** the fixed implementation revision and expected decoded bytes are unambiguous
