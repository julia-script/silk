## Purpose

Define fixed-capacity byte buffering with retained lookahead, exact progress, scoped transport
ownership, and bounded transfer for incremental protocol implementations.

## ADDED Requirements

### Requirement: Buffers have explicit bounded storage and ownership

The standard library SHALL expose ordinary-source `BufferedInput`, `BufferedOutput`, and
`BufferedDuplex` actors. Allocated capacities MUST be between 1 and 1,048,576 bytes inclusive,
MUST be validated before allocation, MUST allocate once through `Allocator`, and MUST never grow.
Duplex convenience construction SHALL default each direction to 8,192 bytes. A zero or excessive
capacity SHALL fail with `InvalidCapacity`. Duplex construction SHALL validate both capacities
before allocating either direction or acquiring the transport lease. A memory-input view MAY
borrow immutable caller storage. Paired duplex construction SHALL validate all four capacities
before allocating either session or acquiring either transport lease.

#### Scenario: Reject invalid capacity before allocation

- **WHEN** construction is requested with zero or more than 1,048,576 bytes
- **THEN** it fails with `InvalidCapacity` without allocating storage or publishing a session

#### Scenario: Retain fixed capacity

- **WHEN** repeated reads, writes, compaction, and flushes exceed the lifetime byte count of a buffer
- **THEN** the actor reuses its original allocation and never increases its capacity

### Requirement: Buffered input retains lookahead and sticky end

`fill(minimum, deadline)` SHALL compact unread bytes and continue reading until at least `minimum`
bytes are available, the source ends, or an operation fails. Zero minimum SHALL perform no I/O. A
minimum above capacity SHALL fail with `BufferTooSmall` without mutation. The result SHALL be
`Available(count)` or `End(availableCount)`, and end SHALL remain sticky while retained bytes remain
readable. `peek` SHALL borrow exactly the initialized unread range and `consume(n)` SHALL advance
only when `n <= unread`; overrun SHALL fail without mutation. The ownership system SHALL reject a
peek escaping its owner or a conflicting mutation while the peek is borrowed.

#### Scenario: Preserve a body suffix after head lookahead

- **WHEN** one underlying read produces `head\r\nbody`, a caller fills and consumes only `head\r\n`
- **THEN** the next peek returns the identical `body` bytes before another source read occurs

#### Scenario: Expose an incomplete final prefix

- **WHEN** end is observed before `minimum` is reached but unread bytes remain
- **THEN** fill returns `End(unread)` and those bytes remain readable until consumed

#### Scenario: Reject consume overrun

- **WHEN** consume exceeds the unread count
- **THEN** it fails with `InvalidConsumption` and preserves the unread bytes and cursor

### Requirement: Reads and discards report precise progress

For nonempty output, `readSome` SHALL copy a positive retained or newly read prefix or report end;
empty output SHALL perform no I/O. `readExact` SHALL fill the destination or report the precise
copied prefix with `UnexpectedEnd` or the source failure. `discardAtMost` SHALL consume no more than
its finite limit and SHALL report the count plus whether end was actually observed; reaching the
limit SHALL NOT probe for end. `discardExact` SHALL report early end with the discarded count. The
API SHALL provide no unbounded read-to-end or delimiter-reading convenience.

#### Scenario: Stop discard at the limit

- **WHEN** `discardAtMost` reaches its finite limit while the source might contain more bytes
- **THEN** it returns that count without performing another read solely to discover end

#### Scenario: Report exact-read truncation

- **WHEN** end occurs after only a destination prefix was copied
- **THEN** `readExact` reports `UnexpectedEnd` with that exact prefix count

### Requirement: Buffered output never repeats accepted bytes

For nonempty input, duplex `writeSome` SHALL accept one positive prefix into owned pending storage,
flushing as necessary to make room; empty input SHALL return zero without I/O. `writeAll` and
`writeVecAll` SHALL process finite inputs in order, validate aggregate length before output, and
report the exact caller-input prefix accepted on failure separately from the exact pending prefix
drained during the failing transport operation. `BufferedDuplex` SHALL forward all three operations
with the same deadline and provider contract. `flush` SHALL advance its pending cursor by each exact
underlying successful write and then call the provider flush. `finish` SHALL explicitly flush.
Scope exit and drop SHALL never implicitly flush or park.

#### Scenario: Preserve pending suffix across short writes

- **WHEN** flushing pending output requires several short provider writes
- **THEN** every accepted byte is offered in order exactly once and only the unaccepted suffix is retried

#### Scenario: Reject vector overflow before output

- **WHEN** a vector's aggregate length cannot be represented in `usize`
- **THEN** `writeVecAll` fails with `LengthOverflow` without accepting or emitting any input

#### Scenario: Abandon without flushing

- **WHEN** a buffered output scope exits with pending bytes and the caller did not call finish
- **THEN** teardown discards the pending bytes without invoking provider write or flush

### Requirement: Failures make buffered sessions terminal

Any underlying read, write, or flush failure SHALL make the complete buffered duplex session
terminal and SHALL prevent input and output retry on that or a replacement connection. Directional
buffer cursors SHALL remain inspectable for diagnostics. Exact caller-input acceptance and pending
transport-drain progress reported before failure SHALL remain separate from an uncertain current
external transfer. A Writer-only adapter SHALL
preserve Writer's all-or-error boundary: failed `Writer.writeAll` SHALL record unknown external
progress, SHALL NOT fabricate a zero prefix, and SHALL make the output terminal. StandardInput and
Writer adapters SHALL NOT independently close their borrowed providers.

#### Scenario: Retain known progress before provider failure

- **WHEN** preceding short writes reported accepted prefixes and a later write fails
- **THEN** the error preserves the caller retry cursor separately from pending-drain progress and the session rejects every later operation

#### Scenario: Terminalize both duplex directions

- **WHEN** an underlying read fails or an underlying write or flush fails
- **THEN** the opposite direction rejects its next operation without consulting the provider

#### Scenario: Preserve unknown Writer progress

- **WHEN** Writer rejects an all-or-error call
- **THEN** the adapter reports unknown external transfer rather than an exact prefix and becomes terminal

### Requirement: Transfer consumes only accepted source bytes

`transferAtMost(source, destination, limit, deadline)` SHALL use retained source bytes and an
exact-prefix destination, consuming source bytes only after the destination reports accepting
them. It SHALL preserve every unaccepted source byte after partial destination failure. Exact
transfer SHALL report early end with the transferred count. Zero limit SHALL perform no I/O. The
ownership system SHALL reject aliased endpoints or an aliased underlying exclusive lease. The API
SHALL NOT accept Writer as an exact-prefix destination or claim zero-copy, sendfile, seek, pread, or
streaming filesystem support. Canonical `silk.buffered_duplex` SHALL provide one explicitly bounded
paired scope that publishes both sessions to one higher-ranked callback and terminally closes both
leases after every structured Effect exit (success, typed failure, or structured
cancellation/interruption) without implicitly flushing either output. Fatal traps SHALL follow the
language rule that bypasses finalizers and `Drop` and are outside this release guarantee.

#### Scenario: Preserve source suffix after destination failure

- **WHEN** the destination accepts only a source prefix and then fails
- **THEN** only that reported prefix is consumed and the complete unaccepted suffix remains in source peek

#### Scenario: Keep zero transfer local

- **WHEN** the transfer limit is zero
- **THEN** the operation returns zero without reading, writing, flushing, or probing end

### Requirement: Deadlines match source capabilities

Every ByteDuplex-backed operation SHALL accept an optional absolute `MonotonicClock` deadline,
forward the same value unchanged to provider operations, and check it between repeated operations.
StandardInput and Writer adapters SHALL omit deadline parameters and SHALL NOT inspect provider
types dynamically or advertise cancellation or deadlines those services do not supply.

#### Scenario: Reuse one absolute deadline

- **WHEN** one buffered operation requires multiple underlying duplex reads or writes
- **THEN** every provider call observes the same absolute deadline rather than a renewed duration

#### Scenario: Keep adapter contracts honest

- **WHEN** input or output is backed only by StandardInput or Writer
- **THEN** its public operations expose no deadline argument and preserve that service's existing semantics
