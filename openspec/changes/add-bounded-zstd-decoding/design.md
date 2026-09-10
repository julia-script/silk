## Context

See proposal.md for motivation. No decoder exists. RawBuffer/Allocator/Layout provide exact owned storage; Bytes/Vector geometric growth would complicate a strict memory budget. The graph does not index Silk source, so source inspection supplements graph queries.

## Goals / Non-Goals

**Goals:** implement the complete non-dictionary frame subset with one-block latency, bounded memory, and explicit progress; preserve ordinary ownership and portable integer semantics.

**Non-Goals:** add compiler intrinsics, codec FFI, reusable generic stream machinery, or speculative encoder/dictionary APIs.

## Decisions

- Public `Zstd` owns exact allocations made once during construction for configured history and fixed block/literal workspaces. The constructor checks window/workspace budgets before allocation. No allocation occurs during steps. This trades eager bounded allocation for simple allocation-free resumability and no transient growth peaks.
- Parse magic, frame header, block header, block body, output drain, checksum, and skip payload as explicit phases. Buffer one compressed block (at most 128 KiB); reverse entropy bitstreams require that complete block. Keep decoded block output separately, then drain it into caller output and the circular history window.
- Internal block actor owns five exact allocations with packed Huffman/FSE entries and repeat offsets; it decodes complete blocks into bounded scratch output using immutable circular history plus the already decoded block prefix. Raw/RLE blocks use the same output-drain path. Reset entropy/history/checksum per regular frame.
- Separate scalar metadata from an owned workspace. Each step temporarily moves the workspace into independent local buffer owners, borrows them for decoding, and restores the workspace for every returned outcome. This avoids overlapping owner loans and keeps steps allocation-free.
- Track final input as an absolute byte offset fixed by the first final call. Require subsequent input to be the remaining final suffix. Separate this from completed-frame counts so concatenation and trailing truncation remain observable.
- Return a typed completed result containing progress on both success and failure. Failed state retains its reason and position; repeated calls return zero progress. This avoids hidden progress when a checksum fails after bytes have escaped.
- Budget input/output/skippable bytes and frame count cumulatively; budget window and workspace as peak live storage, not cumulative lifetime allocations. Use subtraction-before-addition guards and validate before indexing/conversion. Include fixed tables and transient bounded entropy scratch in documented workspace accounting.
- Follow RFC 8878 and RFC 9659. Use independent libzstd fixtures and published vectors, retaining fixture provenance. The pinned Zig source is a structural reference only; checksum validation is mandatory here.
- Put target-neutral runtime cases in one shared native acceptance program. Use existing analysis tests for module/API/ownership claims only when they add distinct signal. Keep large randomized differential sweeps opt-in.

## Risks / Trade-offs

- Reverse-bitstream termination and FSE state transitions are subtle → independent differential data, compressed table/weight fixtures, corruption tests, and separate code review.
- Output precedes checksum validation → document provisional output and sticky failure.
- A large default history allocation costs memory even for small frames → explicit configuration and bounded exact storage; no hidden growth.
- Compiler restrictions can surface in substantial ordinary-source algorithms → validate small module/API slices before integrating the full decoder.

## Migration Plan

Add the new module and internal support, regenerate embedding/docs, validate acceptance and package contents, and deliver a draft PR. There is no superseded decoder or compatibility path.
