# Define the minimum runtime and standard library

Type: grilling
Status: resolved
Blocked by: 01, 02, 03, 04

## Question

Which runtime primitives and standard-library actors are strictly required to implement and run the
single-threaded bootstrap compiler—including allocation, scopes, strings, collections, source bytes,
files, diagnostics, and process boundaries—and which capabilities must remain outside the milestone?

## Answer

The bootstrap runtime and standard library are admitted by workload rather than by aspiration. A
capability belongs when the self-hosting compiler or its conformance and acceptance tests exercise
it, but every admitted API remains a coherent general actor rather than a compiler-specific helper.
The compiler must load an exact source-module closure, retain lossless source bytes, build and query
its syntax/HIR/MIR graphs, emit path-backed LLVM artifacts, run pinned Clang processes, render
diagnostics, report phase timing and allocator memory, and close a native entry without Node.js or
TypeScript. The surface below is sufficient for that program.

Dynamic allocation uses a copyable nominal `Layout` containing a byte size and a validated power-of-
two alignment. Layout construction and repeated-element multiplication return ordinary validation
data distinguishing invalid alignment from representational overflow. Zero-sized layouts and
representable over-aligned layouts are valid. An allocator receives only a valid layout plus a named
destination scope, and its sole fallible primitive creates an owned allocation. Bootstrap has no
primitive resize, zero-fill, user-callable `free`, or unchecked size-and-alignment pair. Collection
growth allocates a new block, moves or copies initialized elements, and drops the old block.

Allocation exhaustion is the typed `OutOfMemory` failure rather than a trap. It propagates through
ordinary failure rows independently of the allocator requirement; automatic release after failure
remains infallible. Each allocation carries a private unforgeable reclaim ticket containing enough
information to invoke the originating allocator's release behavior. The allocator provider must
outlive the allocation's destination scope, so cleanup never consults whichever allocator is
currently provided. The public container type does not expose or own an allocator, and a static
optimization may erase a reclaim ticket when its origin is known.

As revised in issue 03, every service requirement is keyed by a nominal capability and nominal role
pair. The notation `Allocator@Durable` and `Allocator@Scratch` here is semantic notation only; issue
08 owns its spelling. A function may require both roles simultaneously and select one for each
allocation. It may also be generic over a role. An omitted role denotes `DefaultRole`, never the sole
implementation that happens to be visible. This mechanism applies uniformly to every service, not
only allocators. It adds distinct statically selected slots rather than runtime names, keys, or
registries.

Bootstrap ships two allocator implementations. `SystemAllocator` is backed by the platform shim and
may physically reclaim storage when an owner ends. `ArenaAllocator` obtains chunks from an outer
allocator and retains physical storage until a caller-provided named scope closes; it never creates
or encapsulates its own scope. A deterministic quota/failing allocator is a test fixture rather than
public standard-library surface. Pools, shared ownership, garbage collection, and specialized
allocation policies are deferred.

Every allocator exposes an infallible copyable `AllocationMetrics` snapshot containing current live
logical bytes, current reserved physical bytes, peak reserved bytes, cumulative requested bytes,
and allocation count. Logical liveness falls when an owner ends even if an arena retains the
physical bytes. Compiler phases take snapshots directly; the external acceptance harness measures
process peak RSS.

Named scopes privately maintain last-acquired, first-released cleanup records. Registration is
compiler/runtime machinery, not a safe user API. An automatic finalizer is infallible,
non-allocating, independent of ambient services, and skipped after its resource has already been
consumed and cleaned. Cleanup that matters to the caller is an explicit fallible consuming
operation followed by the infallible automatic fallback. Bootstrap therefore needs no owned
capturing closure facility merely to implement finalization.

The core owned indirection and sequence surface consists of the already settled `Box<T>`, intrinsic
`Array<T, N>`, lexical `Slice<T>`, and one dynamic `Vector<T>`. `Vector` owns growable contiguous
storage with length and capacity, exposes shared or exclusive slices, and supports the ordinary
exclusive-borrowing append, insert, remove, pop, reserve, and truncate operations. Stacks use a
vector directly; compiler-specific queues pair one with a head index. Bootstrap has no linked list,
deque, rope, immutable-sequence family, or small-vector variant.

Arbitrary data and human text are separate nominal actors. `Bytes` is an owned growable sequence of
arbitrary octets and may expose an exclusive mutable byte slice. `String` is an owned growable
sequence of valid UTF-8; safe mutation appends valid text or Unicode scalars and truncates only at
UTF-8 boundaries, and it never exposes mutable raw bytes. `StringSlice` is a lexical text borrow.
Bytes-to-text conversion validates explicitly and returns `InvalidUtf8`; viewing text as bytes is
infallible. `SyntaxFile` owns the exact source `Bytes`, including malformed UTF-8, so the parser can
produce byte-precise diagnostics. Bootstrap supports Unicode scalar decoding and UTF-8 encoding,
but not normalization, grapheme segmentation, locale-sensitive behavior, or Unicode collation. A
separate string-builder actor is unnecessary because a mutable owned `String` already retains and
reuses capacity.

`StaticString` and `StaticBytes` are copyable immutable pointer-and-length values backed by compiler-
emitted read-only program data. Static strings are validated UTF-8. Neither value allocates, owns
cleanup, nor requires a named scope; each may produce a lexical view or be explicitly copied into
its growable owned counterpart. Only literals and compile-time constants create them. There are no
general static references, mutable globals, lazy initialization, or user-controlled linker
sections.

`Path` is a nominal owned platform-native filesystem path and `PathSlice` is its lexical view. On
the required Unix hosts, a path preserves arbitrary non-NUL bytes rather than claiming to be UTF-8.
Path construction, joining, parent, extension, and component operations remain path operations.
Rendering uses deterministic escaping and never silently substitutes invalid text. `OsString` is
the corresponding nominal platform-boundary value for child arguments and environment entries;
valid strings and paths convert without loss. Source-module identity remains a separate normalized,
case-sensitive logical value relative to a source root rather than an OS path.

The associative collections are `HashMap<K, V>` and a nominal `HashSet<T>` built over the same
machinery. Keys use one type-owned `HashKey` witness containing equivalence and a 64-bit hash under
the law that equivalent values hash equally. Standard conformances cover booleans, integer scalars,
strings, static strings, bytes, OS strings, and paths; compiler ID modules declare their own.
Bootstrap supplies no automatic structural conformance for floats, pointers, arrays, aggregates,
vectors, or unions.

Map and set construction takes an explicit copyable `HashSeed`; the compiler and conformance tests
use the fixed `HashSeed.deterministic`. There is no global randomized seed or entropy capability.
Iteration order has no semantic meaning. Compiler products use canonical identities, deterministic
worklists, or keys extracted into a vector and sorted with complete tie-breakers. The standard sort
is deterministic, in-place, allocation-free, and unstable, and accepts a pure infallible three-way
comparator. Bootstrap has no ordered/tree map, multimap, multiset, stable sort, or concurrent
collection.

Bootstrap introduces no general iterator protocol. Arrays, vectors, bytes, and strings traverse
through lexical slices or actor-specific scalar operations. Hash maps and sets expose non-escaping
visitor functions whose callbacks preserve their failure and requirement rows. Shared traversal
cannot mutate the collection; exclusive traversal may mutate values but not keys or table shape.
There are no lazy chains, generators, heap-allocated iterators, or iterator-invalidation model.

Scalar actor modules provide the deterministic text conversions required by source parsing,
command-line processing, diagnostics, textual IR, and phase encoders: checked ASCII integer parsing
with explicit radix; locale-independent correctly rounded `F32` and `F64` parsing; decimal and
hexadecimal integer append operations; and shortest-round-trip plus exact hexadecimal float
formatting. Invalid text is ordinary result data so compiler phases can accumulate source
diagnostics. Only destination growth may fail with `OutOfMemory`. A generic formatter,
interpolation framework, `printf`, locale, and arbitrary-precision arithmetic are excluded.

The bootstrap constant evaluator is a closed expression subset. It admits scalar, static-string,
and static-byte literals; references to other constants; finite array, nominal struct, and union
construction and selection; checked scalar arithmetic and bitwise operations; comparisons; boolean
operations; and finite `if` or exhaustive `match`. Overflow, division by zero, invalid shifts,
bounds violations, and constant dependency cycles are compile-time diagnostics. Constants cannot
call functions, loop, recurse, allocate, borrow runtime storage, create raw pointers, fail through a
typed row, require services, perform I/O, inspect types, or own cleanup. String concatenation and
general compile-time execution are deferred.

Host access is split into four nominal services rather than one platform capability:

- `FileSystem` performs explicit path-based whole-file reads and writes, path inspection and
  resolution, directory creation, unique scoped temporary-directory creation, rename, and removal.
  It exposes no handles, seeking, streaming, mapping, buffering, locking, directory-discovery API,
  implicit current directory, or environment lookup. `PathResolution` reports exact absolute host
  spelling, final entry kind, and whether any traversed component was a symlink. The source loader,
  not the service, applies the policy that a requested module path is normalized beneath its source
  root, is a regular file, crosses no symlink, and matches physical casing byte-for-byte.
- `TemporaryDirectory` is an owned resource in a caller-provided scope. Deterministically named
  bitcode and object children live beneath it; automatic cleanup recursively removes it. Saving an
  intermediate explicitly copies or renames it to a durable path before scope closure. There is no
  standalone temporary-path or temporary-file API.
- `ChildProcess` synchronously executes one explicit executable path with ordered `OsString`
  arguments, an optional explicit working directory, an exact environment map, and closed standard
  input. It returns owned stdout/stderr bytes plus a status distinguishing normal exit from signal
  termination. Nonzero exit is result data; failure to start, wait, or capture is `ProcessError`.
  There is no shell string, implicit environment inheritance, background child, pipe API,
  interactive input, signal control, or streaming output.
- `StandardStreams` offers blocking all-or-failure byte writes to stdout and stderr. `StreamError`
  includes broken pipe as a typed failure. Formatting happens above this boundary; stdin, terminal
  detection, colors, cursor control, flushing, logging, and locking are deferred.
- `MonotonicClock.now` returns an opaque copyable `Instant` infallibly; subtracting ordered instants
  produces a nominal nanosecond `Duration`. Implementations are replaceable for deterministic
  tests. Calendar time, time zones, sleeping, deadlines, timers, and scheduling are absent.

`FileError` retains the operation, owned path, portable reason, and any native code as diagnostic
detail. Its closed bootstrap reasons are not found, already exists, permission denied, invalid
path, wrong type, not empty, no space, too large, unsupported, and otherwise unclassified platform
failure. Raw `errno` values do not become control-flow tags, and allocation failure remains
separate. Process and stream failures follow the same rule: stable semantic recovery branches own
control flow while native codes remain presentation detail.

The native adapter receives one owned `HostInput` containing ordered `OsString` arguments, an exact
environment map, and the resolved startup path. No later code reads global arguments, environment,
or current-directory state. A child request selects the exact environment it forwards from this
data. Compiler configuration such as source roots, output paths, target, and toolchain paths remains
explicit owned request data.

Structured diagnostics remain compiler-domain data as settled in issue 06. A pure
`DiagnosticRenderer` consumes deterministically sorted diagnostics plus source files and produces
colorless UTF-8 bytes containing escaped paths, one-based line and column, severity, stable code,
message, labeled related spans, notes, and applicable edits. Invalid source and path bytes are
escaped. The driver writes the result through `StandardStreams`. Machine formats and styled output
may later be alternate pure renderers over the same diagnostic model; they are not services.

The platform boundary is a deliberately small compiler-versioned C shim compiled by the pinned
Clang toolchain. Its private ABI uses fixed-width scalars, raw pointers with explicit lengths,
transient integer handles, caller-owned output buffers, and numeric status codes. It covers aligned
allocate/release; open/read/write/close and path inspection/resolution; directory, unique temporary-
directory, rename, and removal primitives; synchronous spawn/wait with stdout and stderr redirected
to caller-supplied temporary paths; standard-stream writes; monotonic time; startup-directory
capture; and the fixed native entry handoff. Silk owns buffer growth, whole-file loops, values,
failure translation, and resource lifetimes.

The shim never retains a Silk pointer, allocates and returns a higher-level object, calls an
arbitrary Silk callback, unwinds across the boundary, or exposes C structs as language ABI. Darwin
Arm64, Linux x86-64, and Linux Arm64 each compile a matching target implementation into a bundled
runtime object. Private symbols are compiler-versioned; compiler, standard library, and shim come
from one toolchain bundle. There is no stable runtime ABI, dynamic shim discovery, compatibility
negotiation, user-facing FFI, LLVM API, or independently replaceable system runtime.

Finally, the native entry adapter creates the root scope and approved provider values, constructs
`HostInput`, specializes the compiler-driver flow with the allocator roles and four host services,
and runs it. It closes three ordinary outcome classes: successful artifact production, source
rejection with diagnostics, and operational failure. It deterministically renders the latter two,
writes them to stderr, cleans up providers, and returns a class-specific platform status. If
allocation or rendering fails, a fixed allocation-free emergency path writes through the shim; a
broken stderr pipe returns the operational-failure status. Exact numeric statuses, toolchain files,
and acceptance assertions belong to issue 09. Traps remain abnormal termination without a cleanup
guarantee.

The actor inventory is therefore intentionally finite: layouts and allocation metrics; system and
arena allocator implementations; `Box`, arrays, slices, vectors, bytes, strings and their static
forms, OS strings, paths, hash maps and sets, hash keys and seeds; scalar conversion operations;
host input, instants and durations; the four host capabilities and their owned results/errors;
temporary directories; and pure diagnostic rendering. Concurrency, atomics, async scheduling,
networking, serialization, observability, testing frameworks, general FFI, directory discovery,
open or streaming files, random entropy, wall-clock time, shared ownership, stored borrows, public
finalizers, general iterators and formatting, richer allocators, and specialized collections remain
outside the bootstrap milestone.

All service-role and operation spelling above is illustrative. Issue 08 owns concrete syntax; issue
09 owns staged construction, packaging, exact exits, and acceptance fixtures.
