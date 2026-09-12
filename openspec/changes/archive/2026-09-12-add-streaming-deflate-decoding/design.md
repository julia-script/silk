## Context

The reviewed work base is `f5ada543a1738e4f0c69b311bdeb9a72e32e9dd3`. There is no compression actor. Existing SHA actors demonstrate mutable fixed-array state; `Vector.reserve`, `get`, and `set` provide bounded owned history without retained loans. Fixed arrays require explicit literals, making a 32768-element inline history initializer unsuitable. See proposal.md for motivation.

## Goals / Non-Goals

The decoder must survive suspension inside every syntactic field without buffering whole input or output. No encoder, dictionary provider, generic stream framework, exported checksum utility, or compiler recognition is needed.

## Decisions

- Use one `silk.inflate` actor with `Format` (`Raw`, `Zlib`, `Gzip`), `Status` (`NeedInput`, `NeedOutput`, `Finished`), `Limits`, `Progress`, `ErrorKind`, `DecodeError`, and owned `Decoder`. Expose `Decoder.make(format, limits)` with allocation and typed initialization failures, and `Decoder.step(self, input, output, finalInput)` returning `Result<Progress, DecodeError>`. Error progress is explicit because steps can emit bytes before a later error.
- `Limits` uses `u64` cumulative input/output/header/member counters and a byte memory allowance. Require all fields explicitly; a zero limit is meaningful. A public fixed memory upper bound covers decoder storage and bounded scratch, excluding caller buffers and allocator metadata. Check it before acquiring history; a larger allowance does not alter storage size.
- Allocate one 32768-byte `Vector<u8>` history using reserve and initialization during construction. Allocate a second fixed-capacity `Vector<u32>` workspace: 1232 initialized words in 2048-word capacity hold three canonical Huffman tables and code lengths. The payload total is 40,960 bytes. Steps and independent-stream reset allocate nothing. No input buffering, recursion, or retained borrowed views are needed. Keeping tables behind this owner avoids returning 1298 scalar LLVM aggregate lanes through Effect; profiling the initial inline-table representation found IPSCCP dominated compilation for several minutes even for the small example. Ordinary drop releases both owned buffers.
- Use an explicit finite state machine for wrapper fields, block headers, stored payloads, dynamic table construction, Huffman symbols, length/distance extra bits, match copying, trailers, and gzip member boundaries. A partial little-endian bit reservoir consumes only bytes required by a field; preserve partially decoded Huffman symbols across calls. No speculative whole-byte prefetch past a raw/zlib stream end.
- Canonical Huffman decoding uses code-length counts and ordered symbols. Validate oversubscription and permissible incomplete alphabets during table construction. A match copies forward one byte at a time through the circular history, which naturally supports overlap and output suspension.
- Once final input is declared, latch its unconsumed length. Later calls must keep the final flag and present exactly that remaining suffix length. Content identity remains a caller obligation. Raw/zlib finish at their container boundary, leaving unrelated suffix bytes. Gzip requires final exhaustion after a complete member; non-member suffix bytes are malformed input.
- Validate zlib FCHECK/method/window and reject FDICT explicitly. Validate gzip optional fields incrementally with cumulative header accounting; compute header CRC as fields are consumed. Adler-32, CRC-32 and modulo-2^32 member size advance only when an uncompressed byte is emitted. Trailer mismatch poisons the decoder; output before complete validation is provisional.
- Test runtime behavior through one combined source program in the existing shared native acceptance corpus. Committed short fixture vectors carry external provenance; fixture generation/independent verification is opt-in, not a default-suite dependency. Byte-at-a-time feeding splits every byte boundary with linear execution cost, supplemented by representative whole/uneven chunks and tiny output buffers. No per-fixture compiler invocation.

## Independent-stream storage reuse (JUL-174)

Expose `Decoder.reset(self: &mut Self, format: Format, limits: Limits) -> Result<(), DecodeError>`.
Validate memory allowance first, then the nonzero member allowance, before any mutation. Return
zero-progress `MemoryLimit` or `MemberLimit` directly; do not call the step failure helper because
it poisons the decoder. Rejected reset leaves the original stream resumable.

Successful reset replaces the format and limits and restores every scalar field to its constructor
value, including the full raw window, first-member count, checksum seeds, counters, final-input
tracking, bit reservoir, and Huffman traversal. Preserve both vectors and their initialized lengths.
An empty logical history prevents references to previous bytes. Fixed and dynamic table construction
overwrite all entries that can subsequently be read, so reset need not clear either buffer.
Gzip member transitions retain their existing within-stream cumulative accounting.

Reset is valid in every decoder phase. Ordinary terminal step behavior persists until reset.
Previously emitted bytes remain caller-owned and provisional after failure or abandonment. Reset
neither reclaims caller output nor securely erases retained bytes. Caller-supplied raw storage,
concurrent sharing, and performance guarantees remain outside this change.

Extend the existing native acceptance program with atomic rejection followed by continuation,
format/window replacement, stale-history rejection, checksum/counter/final-input renewal, and
Huffman abandonment witnesses. Use ordinary functions without allocator requirements for reset
and subsequent steps. Preserve existing conformance and concatenated-member coverage.

## Risks / Trade-offs

- Malformed dynamic trees and resumed symbols can hide boundary bugs → independently sourced fixtures, malformed cases, and a separate implementation review.
- An owned history requires initialization and two allocations → reserve each once; allocator failure remains typed and cleanup remains ordinary ownership.
- CRC bit loops favor small source and bounded storage over peak throughput → keep performance claims out of correctness tests; optimize only with measurements.
- Stream output precedes its checksum → document provisional output and require consumers to discard it after failure.
- The compiler test suite is expensive → share one corpus compilation and obtain mandatory independent test-economics approval with base/head timings.

## Migration Plan

Add the source module and manifest entry, regenerate standard-library and documentation surfaces, run the repository gates, and deliver a draft PR. There is no legacy API to migrate.
