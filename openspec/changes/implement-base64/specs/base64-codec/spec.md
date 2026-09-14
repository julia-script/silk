## Purpose

Defines Silk's reusable, bounded standard-padded Base64 codec for protocol components that require one canonical byte representation without allocation or platform services.

## ADDED Requirements

### Requirement: Public standard-padded codec surface

The standard library SHALL export `silk.base64` with a stateless `Base64` actor, a `Base64Error` record, and a `Base64Reason` enumeration containing `InvalidLength`, `InvalidByte`, `InvalidPadding`, `NonCanonicalPadBits`, `SizeOverflow`, and `OutputTooSmall`. `Base64Error` SHALL expose `reason`, `offset: Option<usize>`, `required: Option<usize>`, and `available: Option<usize>`.

#### Scenario: Consumer imports the codec

- **WHEN** ordinary Silk source imports `Base64`, `Base64Error`, and `Base64Reason` from `silk.base64`
- **THEN** the declarations resolve through the generated standard-library catalog without compiler-known handling

#### Scenario: Codec remains a pure byte operation

- **WHEN** a target analyzes or executes the module natively or through LLVM-to-Wasm
- **THEN** the codec requires no allocator, Effect provider, operating-system import, network service, or host fallback

### Requirement: Overflow-safe encoded sizing

`Base64.encodedLength(inputLength: usize)` SHALL return the standard-padded Base64 length as a `Result<usize, Base64Error>`. It SHALL calculate `ceil(inputLength / 3) * 4` without performing a potentially overflowing addition or multiplication, return zero for zero, and return `SizeOverflow` with all optional fields absent when the result is not representable on the selected pointer width.

#### Scenario: Encoded length succeeds at its boundary

- **WHEN** `inputLength` is the largest value whose standard-padded result fits the selected `usize`
- **THEN** `encodedLength` returns that exact multiple-of-four result

#### Scenario: Encoded length rejects its first overflow

- **WHEN** `inputLength` is one greater than the largest representable success on a 32-bit or 64-bit target
- **THEN** `encodedLength` returns `SizeOverflow` rather than trapping or narrowing through a host integer

### Requirement: Strict exact decoded sizing

`Base64.decodedLength(input: &[u8])` SHALL completely validate one standard-padded Base64 value and return its exact decoded byte length. Empty input SHALL be valid. Nonempty input SHALL have a length divisible by four, use only the standard alphabet, and use one or two `=` bytes only in the exact final-quantum positions required by the decoded tail. The validator SHALL reject whitespace, NUL, URL-safe alphabet bytes, interior or excess padding, missing required padding, and nonzero unused pad bits.

#### Scenario: Canonical values have exact decoded lengths

- **WHEN** the input is empty, unpadded only because its decoded length is divisible by three, or canonically ends in one or two required pad bytes
- **THEN** `decodedLength` returns the exact number of decoded bytes

#### Scenario: Noncanonical input is rejected

- **WHEN** the input contains an invalid alphabet byte, malformed padding, or nonzero unused pad bits
- **THEN** `decodedLength` returns the corresponding `Base64Error` rather than a partial or approximate length

### Requirement: Deterministic validation and error precedence

Base64 validation SHALL first reject a nonzero input length not divisible by four, then scan bytes from left to right for alphabet and padding validity and canonical pad bits, and only after a valid encoding consider output capacity. `InvalidLength` SHALL report `offset = Some(input.length)`. `InvalidByte` SHALL report the offending byte index. `InvalidPadding` SHALL report the first misplaced or excess `=` index, or `input.length` when a required pad position is missing. `NonCanonicalPadBits` SHALL report the final significant sextet index. Size failures SHALL omit `offset`. `OutputTooSmall` SHALL populate `required` and `available`; other reasons SHALL omit both capacity fields.

#### Scenario: Invalid length wins over byte inspection

- **WHEN** an input has a nonzero length not divisible by four and also contains a nonalphabet byte
- **THEN** validation returns `InvalidLength` at the input length

#### Scenario: Earliest malformed byte or padding wins

- **WHEN** a length-valid input contains multiple alphabet or padding faults
- **THEN** validation reports the first fault encountered by the defined left-to-right scan

#### Scenario: Encoding failure wins over insufficient output

- **WHEN** `decodeInto` receives both an invalid encoding and an output slice smaller than the encoding would otherwise require
- **THEN** it returns the encoding error and leaves the complete destination unchanged

### Requirement: Atomic allocation-free encoding into caller storage

`Base64.encodeInto(output: &mut [u8], input: &[u8])` SHALL compute the required length and validate output capacity before its first write. On success it SHALL emit the RFC 4648 standard alphabet with required `=` padding, return the exact initialized prefix length, and leave every byte after that prefix unchanged. On `SizeOverflow` or `OutputTooSmall`, it SHALL leave the complete destination unchanged. The operation SHALL allocate nothing, perform a linear scan, and rely on ordinary Silk ownership to reject overlapping mutable output and shared input borrows.

#### Scenario: Standard vectors encode canonically

- **WHEN** the input is an RFC 4648 section 10 vector or binary data exercising `+`, `/`, and each tail length
- **THEN** `encodeInto` returns the independently expected standard-padded bytes without whitespace or a NUL terminator

#### Scenario: Output is too small

- **WHEN** the validated encoded size exceeds `output.length`
- **THEN** `encodeInto` returns `OutputTooSmall` with exact required and available lengths and leaves every output byte unchanged

#### Scenario: Suffix remains caller-owned

- **WHEN** the output has more capacity than the successful encoding requires
- **THEN** only the returned prefix changes and the suffix remains byte-for-byte unchanged

### Requirement: Atomic strict decoding into caller storage

`Base64.decodeInto(output: &mut [u8], input: &[u8])` SHALL complete strict validation and exact sizing, then validate output capacity, and only then decode. On success it SHALL return the exact initialized prefix length and leave every byte after it unchanged. On malformed input or insufficient capacity it SHALL leave the complete destination unchanged. The operation SHALL allocate nothing and use at most two linear passes.

#### Scenario: Standard and binary vectors decode exactly

- **WHEN** the input is an RFC 4648 section 10 vector or an independently expected binary `+` or `/` case
- **THEN** `decodeInto` returns the expected bytes and exact written length without relying on a round trip as its oracle

#### Scenario: Strict malformed cases are atomic

- **WHEN** the input contains whitespace, URL-safe bytes, missing, excess, or interior padding, nonzero pad bits, or an invalid suffix after a valid prefix
- **THEN** `decodeInto` reports the defined error and leaves every destination byte unchanged

#### Scenario: Decoded output is too small

- **WHEN** a valid encoding requires more bytes than `output.length`
- **THEN** `decodeInto` returns `OutputTooSmall` with exact required and available lengths and leaves every output byte unchanged

### Requirement: Public reference defines boundaries and examples

The public reference SHALL document the four Base64 operations, structured failures, strict validation and mutation order, caller-storage behavior, canonical examples, target portability, and the absence of URL-safe, unpadded, MIME, PEM-whitespace, streaming, allocating, and platform variants.

#### Scenario: Reader distinguishes this codec from PEM handling

- **WHEN** a reader consults the Base64 reference
- **THEN** the documented contract directs framed or whitespace-tolerant certificate input to its owning certificate API rather than implying that `silk.base64` accepts it
