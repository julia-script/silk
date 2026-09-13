## Purpose

Defines bounded incremental parsing and atomic in-memory serialization for strict HTTP/1.0 and
HTTP/1.1 request and response heads while preserving exact caller-owned suffix bytes.

## ADDED Requirements

### Requirement: Parser construction has finite owned storage

The standard library SHALL expose affine request and response head parsers constructed with explicit
finite head-byte, start-line-byte, field-line-byte, field-count, shared-value, and owned-byte limits.
Construction SHALL validate checked capacity arithmetic and acquire all parser byte and field-index
storage before publishing the parser. Feeding, reading a completed head, reset, and serialization
MUST NOT allocate or grow capacity.

#### Scenario: Owned budget is insufficient

- **WHEN** configured byte and field-index capacities exceed `maxOwnedBytes` or checked size arithmetic overflows
- **THEN** construction fails before publishing a parser or retaining partial storage

#### Scenario: Zero is a real bound

- **WHEN** any configured limit is zero and nonempty input requires that capacity
- **THEN** the operation reports that exact limit instead of treating zero as unlimited

### Requirement: Incremental progress preserves every suffix byte

`feed` SHALL report the exact count consumed from the current input and either `NeedInput` or
`Complete`. A successful nonempty `NeedInput` SHALL consume the full offered slice. `Complete` SHALL
stop immediately after the terminating `CRLF CRLF`, even when body bytes or another message follow
in the same slice. Previously accepted prefixes MUST NOT be reported again or rescanned.

#### Scenario: Terminator is split across calls

- **WHEN** the first input ends after the first byte of the final CRLF and the next input begins with LF followed by body bytes
- **THEN** the first call reports `NeedInput`, the second call consumes exactly one byte and reports `Complete`, and every body byte remains unconsumed

#### Scenario: Empty incremental input

- **WHEN** an empty nonfinal slice is fed to an active parser
- **THEN** it reports zero consumed and `NeedInput`

#### Scenario: Final incomplete input

- **WHEN** `final` is true before a complete head exists, including an empty final call
- **THEN** the parser reports `Truncated` with exact progress and publishes no head

### Requirement: Completed and failed parser states are explicit

Syntax and limit failures SHALL poison a parser while retaining the original typed failure for
diagnosis. Feeding a complete or failed parser SHALL report `InvalidState` and consume zero.
`head` SHALL succeed only for a completed parser, and its head and ordered fields SHALL borrow parser
storage. `reset` SHALL reuse allocated storage and MUST be rejected statically while a published head
view is live.

#### Scenario: Failure after a valid prefix

- **WHEN** a later call contains an invalid byte after previously accepted bytes
- **THEN** the failure identifies the absolute head offset and only the valid prefix consumed from that call, excludes the offending byte, and no partial head is published

#### Scenario: Reset and reuse

- **WHEN** a completed or failed parser has no live borrowed view and is reset
- **THEN** it accepts a new head without allocating replacement storage

### Requirement: Request heads use the strict selected grammar

Request parsing SHALL accept exactly `method SP request-target SP HTTP/1.0|HTTP/1.1 CRLF`, followed
by zero or more field lines and one empty line, all with CRLF endings. It SHALL reject leading blank
lines, bare LF, obs-fold, whitespace before a colon, invalid controls, and separator substitutions.
It SHALL preserve field case, order, duplicates, interior bytes, and empty values while trimming
only surrounding SP and HTAB. It SHALL delegate method, target, header, and Host semantics to the
shared HTTP value actors.

#### Scenario: HTTP/1.1 Host validation

- **WHEN** an HTTP/1.1 request has no Host, more than one Host, or an invalid Host
- **THEN** parsing fails with the corresponding typed reason, field index, and byte offset

#### Scenario: Absolute target authority wins

- **WHEN** an absolute-form target and one syntactically valid Host field name different authorities
- **THEN** parsing succeeds and the target authority remains the effective authority

#### Scenario: Historical whitespace is rejected

- **WHEN** a request contains a leading blank line, obs-fold, bare LF, whitespace before a colon, or anything other than one SP at a start-line separator
- **THEN** parsing fails at the first disallowed byte without consuming it

### Requirement: Response heads use the strict selected grammar

Response parsing SHALL accept exactly `HTTP/1.0|HTTP/1.1 SP 3DIGIT SP reason CRLF`, followed by field
lines and one empty line. Status MUST be 100 through 599. The second SP SHALL be required even for
an empty reason. The reason SHALL preserve permitted SP, HTAB, VCHAR, and obs-text octets and reject
other controls and DEL. HTTP/0.9, ICY, bare-LF, and obs-fold fallbacks SHALL NOT be accepted.

#### Scenario: Empty reason phrase

- **WHEN** the status line is `HTTP/1.1 204 ` followed by CRLF
- **THEN** parsing succeeds with an empty borrowed reason phrase

#### Scenario: Missing reason separator

- **WHEN** the status line ends immediately after the three status digits
- **THEN** parsing fails as an invalid response start line

### Requirement: Failures expose precise bounded context

Parse errors SHALL distinguish start-line, version, method, target, status, reason, field name,
field value, line-ending, obs-fold, Host, limit, overflow, truncation, and state failures. Errors
SHALL carry the absolute head offset, current field index when applicable, and exact configured and
attempted counts for limit failures. Transport errors SHALL remain outside this parser contract.

#### Scenario: Field count boundary

- **WHEN** one more field than `maxFields` is received
- **THEN** parsing fails before recording the excess field and reports the fields limit and attempted count

#### Scenario: Head byte boundary

- **WHEN** accepting the next delimiter or octet would exceed `maxHeadBytes`
- **THEN** parsing fails before storing that byte and reports its absolute offset

### Requirement: Complete-head serialization is checked and atomic

Serialized-size helpers and request/response `writeInto` operations SHALL validate the complete head
and checked size before writing. They SHALL emit canonical single-SP separators, CRLF field lines in
original order, and exactly one terminating empty line, with no body bytes. An empty response reason
SHALL still emit the required preceding SP. Invalid input, overflow, or insufficient output SHALL
leave the entire caller output unchanged.

#### Scenario: Insufficient output

- **WHEN** the destination is one byte shorter than the checked serialized size
- **THEN** serialization reports required and available sizes and leaves every destination byte unchanged

#### Scenario: Exact round trip

- **WHEN** a validated head with repeated fields, an empty field value, and an empty response reason is serialized and parsed again
- **THEN** its semantic values and exact ordered field bytes are preserved

### Requirement: Ownership and target behavior are portable

Completed head views SHALL remain valid only for the parser borrow. Independent copies SHALL use the
existing owned HTTP head contract and allocator failure channel. Pure parser and serializer behavior
SHALL be available for static evaluation where admitted and for the supported native and intended
LLVM-to-Wasm runtime profiles without socket, TLS, buffered-I/O, or Stream requirements.

#### Scenario: Borrow cannot escape reset

- **WHEN** code retains a completed head view and attempts to reset or release its parser
- **THEN** ownership analysis rejects the program

#### Scenario: Owned copy outlives parser

- **WHEN** a completed head is copied through the shared owned-head API and the parser is then released
- **THEN** the owned copy retains the same validated values and ordered field bytes
