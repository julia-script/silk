# bootstrap-zstd-decoding Specification

## Purpose

Define portable zstd decoding with bounded storage and explicit progress so consumers can process untrusted chunked content without buffering the complete input or output.

## Requirements

### Requirement: Decode the non-dictionary zstd format

The decoder SHALL implement RFC 8878 raw, RLE, and compressed blocks, including raw/RLE/Huffman/treeless literals, one and four Huffman streams, direct and FSE-compressed weights, and predefined/RLE/compressed/repeated sequence tables. It SHALL retain entropy tables and repeat offsets between blocks and reset them between regular frames. Nonzero dictionary IDs SHALL fail as unsupported dictionaries; zero IDs SHALL be accepted.

#### Scenario: Decode independently encoded compressed data

- **WHEN** a supported frame produced by an independent zstd implementation is supplied in arbitrary chunks
- **THEN** the emitted bytes equal its uncompressed source regardless of block mode or chunk boundaries

#### Scenario: Reject unavailable dictionary

- **WHEN** a frame declares a nonzero dictionary ID
- **THEN** a typed unsupported-dictionary failure occurs before decoding its blocks

### Requirement: Streaming progress has explicit terminal semantics

A decoder SHALL consume borrowed input into caller-owned output without retaining either borrow. Every successful step SHALL return exact consumed and written counts with `NeedInput`, `NeedOutput`, or `Finished`. `NeedInput` SHALL mean the supplied input is exhausted and more input is needed. `NeedOutput` SHALL mean output space is exhausted and pending output requires a larger nonempty destination. Zero-progress suspension SHALL only occur when the caller has not supplied the needed resource. Input marked final SHALL establish an immutable end offset; subsequent calls SHALL supply only the unconsumed final suffix until it is exhausted. `Finished` SHALL occur only at that end offset after complete frame/trailer validation, not merely at a frame boundary. Empty final input with no frames SHALL be truncated input. Finished and failed decoders SHALL be terminal.

#### Scenario: Drain the final chunk through small output buffers

- **WHEN** final input contains more decoded bytes than the destination can hold
- **THEN** the decoder returns `NeedOutput` with exact progress, accepts the unconsumed final suffix on subsequent calls, and finishes only after all bytes and trailers are processed

#### Scenario: Suspend at an ordinary frame boundary

- **WHEN** a complete frame ends at the end of a nonfinal input slice
- **THEN** the decoder requests more input rather than reporting completion of the concatenated stream

### Requirement: Validate complete concatenated streams

The decoder SHALL accept concatenated regular and skippable frames, concatenate regular-frame output, and skip metadata without retaining its payload. It SHALL validate declared frame content sizes and the low 32 bits of seed-zero XXH64 when checksums are present. Reserved frame/block encodings, invalid entropy tables, invalid offsets, corrupt checksums, and incomplete final input SHALL fail through precise typed reasons. A failure SHALL report exact call progress and cumulative input position; previously emitted output SHALL remain provisional until final success. Failures SHALL be sticky and repeated calls SHALL emit no further output.

#### Scenario: Detect a checksum error after output

- **WHEN** a decoded frame has a corrupted checksum trailer
- **THEN** the decoder reports a checksum failure even if earlier calls emitted all its payload

#### Scenario: Skip metadata between frames

- **WHEN** regular frames surround a skippable frame split across input chunks
- **THEN** the output concatenates only the regular payloads and final-input validation includes the complete skippable payload

### Requirement: Budgets survive chunk and frame boundaries

Configuration SHALL bound cumulative consumed input, decoded output, frame count including skippable frames, and skipped payload bytes. Counters SHALL not reset at frame boundaries. Configuration SHALL separately bound the window and live decoder workspace, including block/literal staging and entropy tables. The default maximum window SHALL be 8 MiB. Larger configured windows SHALL be supported within representable allocation limits. Budget checks SHALL precede allocation, output production, and unsafe arithmetic. Storage SHALL be bounded independently of total input size and released through ordinary ownership. Allocation failures SHALL remain typed.

#### Scenario: Reject a large advertised window

- **WHEN** a frame advertises a window above the configured maximum
- **THEN** the decoder reports the window limit without allocating storage for that window

#### Scenario: Concatenation cannot reset output limits

- **WHEN** individually small frames collectively exceed the output or frame budget
- **THEN** the decoder reports the corresponding limit without resetting the cumulative counter

### Requirement: Publish ordinary-source ownership and validation evidence

The decoder SHALL be canonical ordinary Silk source registered in the manifest, generated embeddings, and generated documentation. Prescriptive documentation SHALL describe borrowed inputs, owned state, caller-owned outputs, budget defaults, provisional output, and final/failed terminal states. Validation SHALL include published and independently generated fixtures, compressed entropy modes, boundary suspension/progress, corruption, checksums, truncation, and resource limits at the cheapest adequate execution tier.

#### Scenario: Resolve and inspect the decoder

- **WHEN** a program imports the zstd actor without vendoring source
- **THEN** ordinary resolution reaches documented canonical Silk declarations without compiler-known codec behavior or native codec linkage
