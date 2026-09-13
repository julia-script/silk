## Context

`ByteDuplex` already provides validated positive-prefix reads and writes, absolute deadlines, sticky
transport end at the provider, and nonparking terminal close. `StandardInput` instead reports
partial input without deadlines, while `Writer` accepts a whole value or fails without an exact
external count. The new layer must keep those contracts distinct and must fit Silk's affine borrows,
higher-ranked scoped callbacks, and source-defined standard-library policy.

## Goals / Non-Goals

**Goals:**

- Centralize cursor, compaction, retained lookahead, exact-prefix output, and terminal-state rules.
- Make the ownership model statically visible through ordinary structs, borrows, service rows, and
  the existing nonparking resource bracket.
- Keep every loop bounded by fixed capacity, a finite input, or an explicit transfer limit.

**Non-Goals:**

- No delimiter parser, HTTP policy, file-handle abstraction, unbounded drain, native `writev`,
  zero-copy/sendfile path, concurrency wrapper, or general Stream integration.
- No implicit drop-time flush or graceful shutdown promise.

## Decisions

### Split actors by state owner

`BufferedInput` owns one allocation plus `[start, end)` unread cursors and sticky terminal/end state.
`BufferedOutput` owns one allocation plus pending cursors, terminal state, and external-progress
certainty. `BufferedDuplex` combines both states with one exclusive concrete provider borrow and a
shared session-terminal gate for a higher-ranked callback. `BufferedTransfer` owns only transfer
result/error data and algorithms.

Keeping state owners separate makes input-only and output-only adapters possible without inventing
unsupported directions. A monolithic protocol utility module was rejected because it would obscure
which mutation invalidates a peek and which actor owns pending bytes.

### Use one fully initialized fixed allocation per direction

Single-direction construction validates capacity first, allocates one raw byte buffer through
`Allocator`, initializes it once, and retains the same storage until drop. Duplex construction
validates both direction capacities before allocating either buffer or acquiring the lease. Input
compacts unread bytes to offset zero only when it needs contiguous tail space. Output advances a
pending start cursor after each exact provider acknowledgment and compacts only when future input
needs the reclaimed prefix.

A growable `Vector`/`Bytes` design was rejected because capacity would become an incidental rather
than enforced contract. Per-operation scratch was rejected because transfer return paths must retain
unaccepted source bytes.

### Reify provider failures before mutating terminal state

Repeated external operations run through `Effect.result`; success updates cursors, while any typed
provider failure first marks the direction state and complete duplex session terminal and then
returns a `BufferError` variant containing the exact caller-input prefix separately from pending
transport-drain progress plus the original typed error. Writer failures use a separate unknown
external-transfer variant because Writer intentionally exposes no partial prefix.

This is more explicit than returning the underlying error row directly, but it is necessary for
aggregate `writeAll`, exact reads, and transfer to preserve the caller retry cursor without
double-counting already buffered bytes or fabricating certainty.

### Scoped duplex closes but never flushes during release

`withBuffered` places the transport borrow and both fixed states in an owned lease passed to
`Effect.useReleaseNonParking`. The use callback receives a temporary `BufferedDuplex`; release calls
only terminal `ByteDuplex.close` and recovers close failure so it cannot replace the protected
outcome. The callback requirement row excludes `ByteDuplex`.

`withBufferedPairCapacity` applies the same bracket once to a pair of exclusive transport leases.
It validates all four capacities before constructing the pair lease or allocating either session,
then gives one higher-ranked callback two independently scoped sessions. Its nonparking release
attempts terminal close on both providers, suppressing only typed close failures so neither can
replace the protected outcome. This makes the two-endpoint transfer API constructible without
nesting a recursively specialized single-session acquisition.

For both scopes, the protected outcomes are the structured Effect exits: success, typed failure,
and structured cancellation/interruption. Fatal traps intentionally bypass finalizers and `Drop`
under the language's resource-lifetime contract and are not a release guarantee.

Implicit flush was rejected because a blocked flush would violate the nonparking finalizer contract
and could turn abandonment into an externally visible duplicate/partial transfer.

### Bounded transfer couples source consumption to destination acknowledgment

Transfer peeks retained source storage, asks the destination to accept at most the remaining limit,
then consumes exactly the returned count. No source cursor moves before that acknowledgment. Exact
transfer layers an early-end check over the same loop. Writer is excluded because it cannot provide
the count required to couple the two cursors.

## Risks / Trade-offs

- **[Large surface can hide cursor regressions]** → Keep cursor transitions in actor-local private
  helpers and exercise distinct compaction, retained-end, short-write, and partial-failure cases.
- **[Cancellation can make an in-flight external transfer unknowable]** → The scoped lease is closed
  by the existing structured-cancellation bracket and is never reused; only previously returned
  progress is called exact.
- **[Manifest and generated artifacts are shared integration points]** → Land actor source first,
  then regenerate once after all concurrently developed standard-library tickets have registered
  their modules.

## Migration Plan

Add the actors and focused acceptance evidence, register their module namespaces and aliases in the
standard-library manifest, regenerate committed catalogs/embeddings/reference output once, and then
adopt the actors from HTTP tickets. Rollback removes the new actors and registrations; no existing
contract or stored data is migrated.
