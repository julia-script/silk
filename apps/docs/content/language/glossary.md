# Silk Effect Language

The language-design context for Silk Effect, from its initial bootstrap subset through later
self-hosted implementations.

## Language

**Silk Effect**:
A low-level systems programming language prioritizing explicit control, memory safety, predictable
performance, and suitability for systems software. Effect informs parts of its semantic model;
interoperability with Effect is an important later convenience, not the language's purpose.
_Avoid_: native Effect, systems Effect

**Bootstrap language**:
The smallest credible subset of Silk Effect whose reference program is the compiler itself; a
feature belongs in this subset when the compiler or its conformance tests require it. It is a
coherent subset of the intended language rather than a disposable dialect, although early
implementations may impose restrictions and alpha discoveries may still justify breaking changes.
_Avoid_: v0 language, minimal language

**Affine value**:
A non-`Copy` value with one current owner; it may be transferred or discarded, but never duplicated
implicitly. Its cleanup responsibility follows ownership.
_Avoid_: linear value, manually managed value

**Move**:
An explicit transfer that consumes the source binding and gives its value and cleanup responsibility
to a new owner. An explicit move consumes even a `Copy` binding.
_Avoid_: assignment copy, ownership hint

**Lexical borrow**:
Temporary shared or exclusive access to an owned value without transferring its cleanup
responsibility. The backing owner remains constrained until the borrow's last permitted use.
_Avoid_: stored reference, shared ownership

**Cleanup obligation**:
The responsibility carried by an affine owner to perform its deterministic, infallible cleanup
exactly once unless ownership is transferred.
_Avoid_: manual free, ambient finalizer

**Effect function**:
A function whose call constructs one lazy `Effect` value; its declared result is the success value
produced when that Effect executes. Calling it does not execute or flatten the Effect.
_Avoid_: async function, implicitly executed effect

**Actor module**:
A module centered on one minimal data or service concept whose core capabilities preserve its
invariants. Its richer public API consists of qualified, data-first sibling functions that are
individually importable and pipeable without retroactively changing the concept's method set.
_Avoid_: extension implementation, open method set, class-per-entity

**Source module**:
An inert declaration namespace defined by exactly one source file, whose canonical, case-sensitive
identity is its extensionless path relative to a compiler-provided source root. Importing one never
runs code; modules cannot be declared independently of their locations, assembled from partial
files, or hold implicit runtime initialization.
_Avoid_: partial module, declared module name

**Self-hosting compiler**:
A compiler written in Silk Effect that can compile its own source into a working native compiler
without Node.js or TypeScript at runtime. The first self-hosting compiler may emit LLVM IR and use
the LLVM toolchain for code generation and linking.
_Avoid_: frontend-only self-host, LLVM-independent compiler

**Stage-0 compiler**:
The trusted Effect/TypeScript seed compiler that produces the first native compiler under a pinned
Node.js runtime.
_Avoid_: stage-1 TypeScript compiler, self-hosted compiler

**Stage-1 compiler**:
The first native Silk compiler, produced by stage 0 and used to build stage 2 from the same source
snapshot.
_Avoid_: seed compiler, accepted compiler

**Stage-2 compiler**:
The native Silk compiler produced by stage 1 and promoted only after it reproduces itself and passes
the complete bootstrap acceptance procedure.
_Avoid_: verification compiler, automatically accepted compiler

**Fixed-point rebuild**:
The verification-only compiler output produced when stage 2 rebuilds the same source snapshot with
the same recipe; it is evidence for stage 2 rather than a separately distributed stage.
_Avoid_: stage-3 release, second compiler product

**Source snapshot**:
The content-addressed, canonical manifest and exact bytes of all compiler, standard-library,
runtime, and shim source inputs used by a bootstrap build.
_Avoid_: Git revision, checkout

**Build recipe**:
The canonical identity of every declared source, target, toolchain, environment, and path input that
must remain equal across comparable bootstrap builds.
_Avoid_: build log, compiler options

**Build record**:
The provenance and observed results of one recipe execution, including its producer, commands,
artifact hashes, diagnostics, statuses, and measurements.
_Avoid_: build recipe, acceptance result

**Acceptance corpus**:
The content-addressed fixture manifest that traces every normative bootstrap decision to required
positive, negative, failure, recovery, runtime, and native-inspection evidence.
_Avoid_: coverage target, smoke suite

**Silk toolchain bundle**:
The promoted per-target stage-2 compiler together with its version-matched bootstrap standard
library, runtime shim, target specification, and content manifest.
_Avoid_: native dependency bundle, compiler executable

**Native dependency bundle**:
The per-target content-addressed Clang/LLVM, linker, SDK or sysroot, platform-startup, compiler-rt,
and system inputs required by the bootstrap compiler's native backend and linker services.
_Avoid_: Silk toolchain bundle, ambient host toolchain

**Acceptance evidence bundle**:
The retained recipes, build records, stage artifacts, comparisons, inspections, measurements, and
gate report that justify promoting one stage-2 Silk toolchain bundle.
_Avoid_: release bundle, build cache

**Analysis snapshot**:
An immutable, source-identified view of one compilation state that preserves every syntax and
semantic fact the implemented frontend phases can still determine, together with diagnostics and
explicit unknown or erroneous states for incomplete code. Batch executable generation may reject
an erroneous snapshot, but source mistakes do not invalidate unrelated facts. The bootstrap
snapshot need not initially expose every future tooling query; its identities, provenance, and
phase boundaries must allow the supported analysis surface to grow without creating a second
language implementation.
_Avoid_: valid-program model, compiler-success result

**Syntax file**:
The lossless frontend artifact for one source module, owning its original bytes, tokens including
comments and whitespace, and a source-faithful tree with explicit missing and error nodes. Stable
source identities and byte spans connect its tokens and nodes to diagnostics and later semantic
facts; semantic phases do not store tooling trivia in HIR.
_Avoid_: trivia-free AST, valid-source tree

**Bytes**:
A nominal owned growable sequence of arbitrary octets. A borrowed `Slice<U8>` views its storage
without asserting that the contents are text; an exclusive borrow may expose mutable bytes, and
syntax files retain source as `Bytes` even when UTF-8 is malformed.
_Avoid_: byte string, unvalidated string

**Path**:
An owned normalized UTF-8 location rooted in the namespace of an explicitly selected filesystem
provider. It is always absolute within that provider, never consults ambient process state, rejects
NUL and lexical root escape, and is not interchangeable with a native OS path.
_Avoid_: process path, ambient path

**FileSystem service**:
The portable source-defined seven-operation whole-file contract for complete reads and writes,
minimal metadata and listings, one-directory creation, one-file removal, and empty-directory
removal. Applications explicitly provide a mutable implementation; the contract contains no OS
handle, current directory, platform ABI, or built-in provider.
_Avoid_: platform filesystem, global filesystem

**String**:
A nominal owned growable sequence of valid UTF-8 text, distinct from `Bytes` even when both use the
same physical storage shape. Exclusive mutation preserves UTF-8 and never exposes writable raw
bytes; bootstrap uses no separate string-builder type. It supports UTF-8 encoding and Unicode
scalar decoding but not normalization, grapheme segmentation, locale-sensitive operations, or
Unicode collation.
_Avoid_: branded byte array, text bytes

**String slice**:
A lexical borrowed view of a `String` whose endpoints are valid UTF-8 boundaries. Converting bytes
to a string or string slice requires explicit validation; viewing string storage as bytes is
infallible.
_Avoid_: unchecked text slice, code-unit string

**Static string**:
A copyable immutable value pointing into compiler-emitted read-only UTF-8 data. It requires no
allocation, cleanup, or named scope; it may produce a lexical string slice or be explicitly copied
into an owned growable string.
_Avoid_: global string, immortal borrow

**Static bytes**:
A copyable immutable value pointing into compiler-emitted read-only arbitrary bytes. It requires no
allocation, cleanup, or named scope; it may produce a lexical byte slice or be explicitly copied
into owned growable bytes.
_Avoid_: global byte buffer, immortal borrow

**Compile-time constant**:
An order-independent value produced by the closed bootstrap constant-expression subset: literals,
constant references, finite aggregate construction and selection, checked scalar operations, and
finite conditional matching. It cannot call functions, loop, recurse, allocate, borrow runtime
storage, create raw pointers, fail, require services, perform I/O, or own cleanup.
_Avoid_: compile-time program, const function

**Numeric text conversion**:
The locale-independent bootstrap scalar operations for checked ASCII integer parsing with explicit
radix, correctly rounded floating parsing, integer append formatting, and deterministic shortest-
round-trip or exact hexadecimal floating formatting. Invalid source text is ordinary result data;
only destination growth may fail with out-of-memory.
_Avoid_: printf, generic formatter

**OS string**:
A nominal platform-boundary value for command arguments and environment entries. Bootstrap Unix
hosts preserve arbitrary non-NUL bytes; valid `String` and `Path` values convert without loss, while
diagnostic rendering uses deterministic escaping rather than claiming the value is UTF-8.
_Avoid_: process string, lossy argument

**Host input**:
The owned startup data constructed once by the native entry adapter: ordered OS-string arguments,
an explicit OS-string environment map, and the resolved startup path. The compiler driver receives
it as ordinary data; later code has no global argument, environment, or current-directory getters.
_Avoid_: process environment, runtime context

**Native entry adapter**:
The typed root that constructs the bootstrap host providers and input, invokes the compiler driver,
renders source rejection or operational failure, performs root cleanup, and maps success, source
rejection, or operational failure to the platform exit convention. It retains no unresolved
failure or requirement row and has an allocation-free emergency reporting path.
_Avoid_: fallible main, runtime launcher

**Path**:
A nominal owned portable filesystem path, distinct from both `String`, source-module identity, and
any host-native path. Its normalized component model and explicit root semantics have the same
meaning for native providers, deterministic in-memory providers, and browser virtual file systems.
File-system operations never consult a hidden current directory. Converting to a host-native path
is a provider responsibility and may fail when the portable path cannot be represented honestly.
_Avoid_: path string, OS path, implicit working-directory path

**Path slice**:
A lexical borrowed view of a `Path` used by normalized path-component operations without treating
the value as an arbitrary String.
_Avoid_: path substring

**Platform path**:
A nominal host-specific path value exposed only by a lower-level platform file-system service.
Native Unix providers may preserve arbitrary non-NUL bytes and other providers may use a different
representation. Rendering uses deterministic escaping rather than silent replacement. Portable
programs use `Path`; `PlatformPath` is an explicit loss of cross-host portability.
_Avoid_: ordinary path, portable path

**Path resolution**:
Structured provider facts for an explicit portable path: its normalized resolved path, final entry
kind, and whether resolution crossed a provider-defined indirection such as a symbolic link. The
portable file-system service reports only facts with stable cross-provider meaning. A lower-level
platform service may expose exact host spelling and richer native metadata; callers such as the
source loader impose their own canonicality and admissibility policy.
_Avoid_: canonical source path, implicit realpath policy

**Vector**:
The bootstrap generic owned growable contiguous sequence. Shared or exclusive lexical borrows
produce `Slice<T>` values; stacks use a vector directly, and compiler-specific queues pair one with
a head index. Bootstrap has no general linked list, deque, rope, immutable sequence, or small-vector
variant.
_Avoid_: dynamic array, list

**Hash map**:
The sole general bootstrap associative collection: an owned mutable mapping whose keys satisfy an
explicit type-owned hash-and-equality contract. Its iteration order has no semantic meaning;
canonical compiler output uses canonical identities, deterministic worklists, or explicitly sorted
keys rather than table order.
_Avoid_: ordered hash map, dictionary

**Hash key**:
The single bootstrap generic-key interface, pairing equivalence with a 64-bit hash under the law
that equivalent values produce equal hashes. A hash map receives one type-owned `HashKey` witness
rather than independently selected hashing and equality semantics; equality-only algorithms accept
an ordinary callback during bootstrap. Standard conformances cover booleans, integer scalars,
strings, static strings, bytes, OS strings, and paths; floats, pointers, aggregates, and collections
receive no automatic structural conformance.
_Avoid_: independent hash witness, generic equality protocol

**Hash seed**:
A nominal copyable value supplied explicitly when constructing a hash map or set. Bootstrap
provides a fixed deterministic seed for the compiler and conformance tests; there is no hidden
global seed or entropy capability, and canonical products never depend on table iteration order.
_Avoid_: randomized global hash, implicit seed

**Hash set**:
A nominal bootstrap set actor implemented over hash-map machinery. It expresses membership,
reachability, and duplicate detection without exposing dummy map values; it shares `HashKey` and
unordered iteration semantics with `HashMap`.
_Avoid_: unit-valued map, ordered set

**Canonical sort**:
The bootstrap deterministic, in-place, allocation-free unstable sort over a vector and a pure
infallible three-way comparator. Callers producing canonical output supply complete tie-breakers
rather than depending on the relative order of values that compare equal.
_Avoid_: stable-by-accident sort, hash-order canonicalization

**Bootstrap traversal**:
Collection traversal through lexical slices or actor-specific non-escaping visitor functions whose
callbacks preserve failure and requirement rows. Hash visitors never promise canonical order;
bootstrap has no general iterator protocol, lazy chain, generator, or heap-allocated traversal
object.
_Avoid_: iterator pipeline, enumerable collection

**High-level intermediate representation (HIR)**:
The compiler's generic-aware semantic representation after name, type, and function-contract
elaboration. It uses canonical declaration and type identities, normalized contracts, core semantic
operations, and source provenance; separate stable-ID-keyed tables retain partial semantic facts
for tooling and incomplete programs.
_Avoid_: annotated syntax tree, LLVM-like IR

**Mid-level intermediate representation (MIR)**:
The compiler's monomorphic, backend-neutral control-flow representation after ownership and scope
checking. It makes moves, borrows, cleanup, typed-failure branches, service slots, witness calls,
traps, and runtime operations explicit while retaining logical Silk types and source provenance;
physical target layout and backend instructions remain outside it.
_Avoid_: LLVM wrapper, source-module object model, WebAssembly stack IR

**LLVM lowering**:
The compiler stage that converts backend-neutral MIR plus an explicit target and codegen request
into LLVM bitcode. It owns target-specific lowering and object or WebAssembly emission; native
linking remains a separate compiler-driver responsibility.
_Avoid_: backend registry, codegen plugin lookup

**Native linker service**:
A nominal compiler capability that combines compatible relocatable object artifacts, the selected
target runtime, and approved system libraries into a native executable at a durable destination.
Bootstrap provides a pinned-Clang implementation; the compiler driver depends on the capability
rather than constructing platform linker command lines itself.
_Avoid_: shell link command, backend-owned linking, external build harness

**Platform shim**:
The private compiler-versioned C boundary beneath bootstrap host-service implementations. Its ABI
uses fixed-width scalars, raw pointers with explicit lengths, transient integer handles, caller-
owned output buffers, and numeric status codes; it never retains Silk pointers, returns C-owned
objects, calls arbitrary Silk callbacks, or unwinds across the boundary. It remains private:
`extern "C"` foreign functions are the public foreign-function boundary, and the shim is not
reachable through them. Its surface is limited to aligned allocation, host-path and whole-file primitives, unique temporary
directories, redirected synchronous child execution, standard-stream writes, monotonic time, and
startup handoff; higher-level portable values and typed semantics remain in Silk. It is one native
implementation boundary, not the definition of `FileSystem` or `Logger`. Each required host
compiles a matching implementation of the same semantic ABI into a toolchain-bundled runtime
object, with private compiler-versioned symbols and no independent compatibility promise.
_Avoid_: C runtime library, platform SDK

**File-system service**:
The portable capability for explicit path-based file and directory operations. Public I/O reads and
writes complete `Bytes` values and exposes only semantics that a native provider, an in-memory test
provider, and a browser virtual file system can implement honestly. It has no native handles,
implicit current directory, process environment, terminal behavior, mapping, locking, or
platform-specific metadata. Programs require it explicitly and remain unchanged when a different
provider is selected.
_Avoid_: ambient filesystem, native filesystem, process filesystem

**Platform file-system service**:
The optional lower-level host capability for programs that deliberately need native paths, handles,
seeking, mapping, locking, platform metadata, or other behavior without a portable contract. A
portable `FileSystem` provider may be implemented over it, but ordinary programs and standard-
library APIs do not require it. Depending on this service is an explicit portability decision.
_Avoid_: default filesystem, FileSystem implementation detail exposed as the common API

**File error**:
The owned typed failure for a file-system operation, retaining its operation, explicit path, a
portable semantic reason, and any native code as diagnostic detail. Bootstrap reasons distinguish
not found, already exists, permission denied, invalid path, wrong type, not empty, no space, too
large, unsupported, and otherwise unclassified platform failure; allocation exhaustion remains a
separate failure.
_Avoid_: errno failure, I/O exception

**Temporary directory**:
A unique directory resource created through the file-system service in a caller-provided named
scope. It owns temporary child artifacts and removes them recursively through private infallible
cleanup; retaining an artifact requires an explicit fallible copy or rename to a durable path.
_Avoid_: temporary path, global temp file

**Child-process service**:
The narrow bootstrap host capability for synchronously executing a program from structured input
and returning structured output. Requests provide an executable path, ordered arguments as exact
platform bytes, optional explicit working directory, exact environment, and closed standard input;
results own captured output and distinguish exit codes from signal termination. Nonzero exit is
result data, while failure to start, wait, or capture is typed process failure. It never interprets
a shell command string.
_Avoid_: shell service, process manager

**Standard-streams service**:
The narrow bootstrap host capability for writing bytes to standard output and standard error. It
offers only blocking all-or-failure byte writes, with broken pipes represented by typed stream
failure. Formatting and diagnostic presentation happen above the boundary; the service does not
imply terminal control, color detection, flushing, logging, or interactive input.
_Avoid_: console service, terminal service

**Standard-input service**:
The narrow bootstrap host capability for reading bytes from standard input. It is separate from the
standard-streams service, which writes only: a write is all-or-failure while a read is inherently
partial. One blocking read fills a prefix of the caller's buffer and reports the exact committed
count; the end of input is outcome data rather than a typed failure, and only a host error is typed
read failure. The service does not imply terminal control, raw mode, line editing, prompting, or
non-blocking reads.
_Avoid_: console service, stdin stream, reader service

**Host-input service**:
The narrow bootstrap host capability for reading what the process was started with: its ordered
command-line arguments, the value of a named environment variable, and its working directory. It
reads only — it never sets a variable or changes the directory. Every value is raw bytes, exactly as
the process received them, because neither an argument nor an environment value is required to be
valid UTF-8; a checked textual view layers on top and stays fallible. An index past the last
argument and an unset name are absence rather than typed failure. Argument parsing, flag grammar,
and configuration layering happen above the boundary.
_Avoid_: argv service, environment service, process service

**Log invocation**:
One complete semantic observability message submitted to a Logger with a separate closed severity.
It is one provider call rather than a sequence of stream fragments. The bootstrap message is a
borrowed immutable UTF-8 view consumed during the call; providers that retain it copy the bytes.
Later annotations, span context, and OpenTelemetry data may enrich this boundary without treating
rendered bytes or one provider's physical writes as the canonical event.
_Avoid_: stdout bytes, log line fragment, stream chunk

**Logger service**:
The portable explicit service consumed by `Effect.log` and its level-specific aliases. A Logger
receives a closed `LogLevel` severity and one
complete borrowed message in call order and decides whether to render it to standard output, retain
it in memory, forward it to browser or OpenTelemetry facilities, fan it out, or discard it according
to explicit provider policy. Logging is not `Writer.writeAll`, does not expose
byte-at-a-time appends, and remains an Effect requirement until provided. The first stdout provider
forwards complete messages through `Writer`; a bounded in-memory provider proves host
independence and deterministic failure.
_Avoid_: stdout logger intrinsic, console service, ambient global logger

**Monotonic-clock service**:
The narrow bootstrap host capability for measuring elapsed compiler-phase time without exposing
calendar time, time zones, sleeping, or scheduling. Reading produces an opaque copyable `Instant`
infallibly; subtracting ordered instants produces a nominal nanosecond `Duration`.
_Avoid_: wall clock, timer service

**Phase encoder**:
An optional observer that converts one canonical compiler-phase artifact, such as a syntax file,
HIR, or MIR, into a requested textual or binary representation without changing that artifact or
participating in the next phase. A phase has one semantic processor but may have multiple encoders;
writing or transporting the encoded bytes is a separate boundary.
_Avoid_: phase emitter, alternate phase processor, serialized pipeline handoff

**Diagnostic renderer**:
A pure compiler actor that converts sorted structured diagnostics plus their source files into a
deterministic presentation. Bootstrap emits colorless UTF-8 bytes with escaped invalid source and
path bytes; writing those bytes is a separate standard-streams operation, and future machine or
styled formats remain alternate pure renderers over the same diagnostic data.
_Avoid_: diagnostic service, printing diagnostic

**Safe code**:
Code outside an explicit unsafe boundary. Safe code cannot cause undefined behavior, use-after-free,
double-free, invalid aliasing, or data races; its resources are released deterministically without
requiring a tracing garbage collector.
_Avoid_: managed code, garbage-collected code

**Owned value**:
A non-copyable value governed by affine single ownership: transferring it moves ownership and the
previous binding becomes unusable. Its resources are reclaimed automatically when its owner ends
or through an explicit consuming `drop`; ordinary code neither implicitly copies it nor manually
frees it.
_Avoid_: managed object, implicitly shared value

**Whole-value move**:
An ownership transfer that consumes an entire initialized value. Aggregates may be consumed through
complete destructuring, but extracting from a retained aggregate must replace the field so safe
code never contains a partially moved value.
_Avoid_: partial move, moved-out field

**Mutable owner**:
An owned binding explicitly declared to permit mutation. Mutation requires an exclusive scoped
borrow of a mutable owner; read-only borrowing and whole-value moves do not.
_Avoid_: implicitly mutable binding, shared mutation

**Copy value**:
A value whose declaration is compiler-verified to contain only copyable fields and no cleanup
behavior, so assignment duplicates it instead of moving it. Other duplication is an explicit
actor-module operation whose function contract exposes requirements such as allocation.
_Avoid_: implicitly cloned value, copyable owner

**Automatic cleanup**:
Compiler-invoked, consuming destruction when an owner ends or its maximum scope closes. Automatic
cleanup is typed-infallible and runs in deterministic last-acquired, first-released order; cleanup
whose failure matters is an explicit consuming operation.
_Avoid_: fallible destructor, manual free

**Scope finalizer**:
A private compiler/runtime cleanup record attached to a named scope and invoked in last-acquired,
first-released order if its resource has not already been consumed. A scope finalizer cannot fail,
allocate, or require an ambient service; bootstrap safe code cannot register arbitrary finalizer
callbacks.
_Avoid_: user finalizer, cleanup callback

**Scoped borrow**:
A temporary, non-owning view of an owned value that cannot outlive its explicit lexical scope. A
bootstrap-language borrow may be passed and captured only when it cannot be returned, stored in an
owned value, or otherwise made to escape that scope; overlapping borrows are either all read-only
or one exclusive mutable borrow.
_Avoid_: reference, lifetime parameter

**Named scope**:
A first-class, lexically named maximum resource lifetime that is independent of allocation policy;
ownership may end a resource earlier. Named scopes form an outlives hierarchy: the nearest scope is
the default destination, targeting an ancestor is explicit, and values tied to a descendant cannot
escape into an ancestor.
_Avoid_: allocator lifetime, implicit scope

**Scoped value**:
A simple or composite value with one compiler-inferred maximum named scope. A composite is limited
to the shortest-lived scope among its components; ordinary structs and unions neither own scopes
nor expose independent per-field scope parameters.
_Avoid_: scope-owning struct, per-field lifetime

**Scope requirement**:
A function-contract requirement for a particular named scope used by scoped operations in the
function body. Functions do not create scopes implicitly: an unsatisfied scope requirement
propagates whether or not the function returns a scoped value, while a locally created scope
discharges that requirement.
_Avoid_: function scope, implicit lifetime

**Unsafe boundary**:
A small, explicit region that may perform operations whose safety invariants the compiler cannot
prove, such as low-level foreign-function or memory access. Unsafe behavior does not implicitly
spread into ordinary code.
_Avoid_: escape hatch, unchecked mode

**Foreign function**:
A bodiless `unsafe extern "C" fn` declaration whose implementation is native code linked into the
artifact. It carries a Silk name, a logical native symbol (`as "..."` or the Silk name), and the
C ABI; it admits only by-value scalars, is called only inside an unsafe boundary, lowers to one
direct foreign call, and is admitted only when the selected execution surface supplies a matching
binding.
_Avoid_: FFI binding, extern block, intrinsic

**C function pointer**:
A Copy, noncapturing native address typed as `extern "C" fn(P...) -> R`. Only an exact,
synchronous, nongeneric `export "C" fn` item contextually converts to it; ordinary and capturing
Silk callables remain distinct values.
_Avoid_: callable, closure, imported function

**Foreign static**:
An immutable Silk binding backed by a C data symbol. `unsafe extern "C" static` imports and loads
native data; `export "C" static` defines an externally visible scalar initialized by a literal.
Pointer pointee mutability does not make the binding assignable.
_Avoid_: global variable, static-phase binding, foreign constant

**Native export**:
An `export "C" fn` declaration with a body that native code may call through a compiler-generated
thunk under a logical native symbol (`as "..."` or the Silk name). It admits the same by-value
scalars as a foreign function, carries no Silk-only contract, cannot suspend, is an additional
discovery root on native targets, and keeps its implementation under a private compiler symbol.
_Avoid_: FFI export, public symbol, entry point

**Raw pointer**:
A `*const T` or `*mut T` value holding one machine address with no ownership, loan, or validity
guarantee. It is Copy and may be null; forming one from a reference or slice is safe and ends no
loan, while offsetting and dereferencing through `silk/pointer` require an unsafe boundary. It is
admitted by the C ABI for any pointee, so it is the value a foreign signature means by a pointer.
_Avoid_: reference, unsafe reference, handle, `RawBuffer`

**Allocation requirement**:
A typed capability in a function signature indicating that the function may perform dynamic
allocation through a selected nominal service role. Allocation requirements propagate through
callers and are satisfied by an allocator provided for that role; a function needing several
policies simultaneously uses distinct roles such as durable and scratch. Ownership, lifetimes, and
scopes determine reclamation without ordinary code calling `free`.
_Avoid_: allocator parameter, manual allocation

**Allocation metrics**:
An infallible copyable snapshot required from every bootstrap allocator, reporting live logical
bytes, reserved physical bytes, peak reserved bytes, cumulative requested bytes, and allocation
count. Compiler phases observe snapshots directly; process peak RSS remains an external harness
measurement.
_Avoid_: allocator log, process memory usage

**Layout**:
A copyable validated allocation request containing a byte size and power-of-two alignment. Layout
construction and repeated-element multiplication report invalid alignment or representational
overflow as ordinary result data; zero-sized and supported over-aligned layouts are valid, and only
storage exhaustion reaches the allocator failure channel.
_Avoid_: size-and-align pair, unchecked layout

**Dynamic allocation**:
An owned allocation obtained by a fallible request for a byte size and alignment in a named
destination scope. Its private release participates in automatic infallible cleanup; the allocator
may reclaim storage when the owner ends or retain it until the scope closes. Bootstrap has no
primitive resize, zero-fill, or user-callable release operation.
_Avoid_: raw allocation, manual reallocation

**Reclaim ticket**:
Private, unforgeable metadata carried by a dynamic allocation that invokes the originating
allocator's infallible release behavior. The allocator provider must outlive the allocation's named
destination scope, so cleanup never depends on whichever allocator is currently provided.
_Avoid_: captured allocator, ambient deallocation

**System allocator**:
The bootstrap root allocator backed by the platform allocation boundary. It may physically reclaim
an allocation as soon as that allocation's owner ends.
_Avoid_: global allocator

**Arena allocator**:
A bootstrap allocator that obtains storage from an outer allocator and retains that physical
storage until a caller-provided named scope closes. It does not create or encapsulate its own scope.
_Avoid_: arena-owned scope, implicit region

**Out-of-memory failure**:
The typed failure produced when an allocator cannot satisfy a dynamic allocation request. It
propagates through ordinary function failure rows and may be handled at the native entry boundary;
allocation exhaustion is not a trap, while automatic cleanup after it remains infallible.
_Avoid_: allocation trap, OOM panic

**Function contract**:
The type-level description of what a function returns, which typed failures it may produce, and
which service capabilities it requires; this complete contract is part of a function value's type.
Calling a function executes it directly: success yields its return value, while unhandled failures
and unsatisfied requirements propagate through callers. A function value retains its requirements
and resolves them at invocation; it never captures currently provided services implicitly. A pure
function has neither. Externally visible and recursive functions declare the complete contract;
private non-recursive functions may infer it.
_Avoid_: effect wrapper, hidden effects

**Type row**:
An unordered, duplicate-free set of nominal types. Failure rows use this form directly; requirement
rows refine it by associating each nominal service capability and role pair with a shared or
exclusive access mode. A row has no source-order identity or runtime lookup semantics.
_Avoid_: type list, runtime dependency map

**Requirement row**:
A compile-time row keyed by pairs of nominal service capability and nominal service role, with one
shared or exclusive access mode per pair. Combining entries retains the strongest mode; the row does
not itself create a runtime union or require runtime tag checks.
_Avoid_: dependency union, service map

**Contract-row parameter**:
A compile-time generic parameter ranging only over failure rows or requirement rows so a
higher-order function can preserve a callback's complete contract. It is inferred at calls,
monomorphized, and confined to function-contract positions rather than general type-level code.
_Avoid_: erased effect, runtime row, general row polymorphism

**Failure row**:
A type row naming the error types a function may fail with. When a failure occurs, its value has one
active member and can be discriminated by that member's nominal type.
_Avoid_: error list, error codes

**Typed failure**:
An abortive, non-resumable exit carrying a value whose nominal type belongs to the current function's
failure row. It transfers control to the nearest matching handler or propagates through the caller,
while automatic cleanup runs for exited ownership scopes. An explicit `fail` consumes and transfers
ownership of its failure value. Generated code uses explicit discriminated success-or-failure
returns and branches rather than native exception unwinding. Each exit carries one value; collecting
multiple diagnostics is explicit ordinary data modeling rather than failure-channel behavior. Its
payload obeys the same named-scope escape checks as a returned success value.
_Avoid_: exception, resumable condition

**Trap**:
An unrecoverable bootstrap-runtime termination caused by conditions such as bounds violations,
ordinary integer overflow, impossible compiler states, or violated unsafe contracts. Traps are not
typed failures, cannot be handled, and do not promise stack unwinding or automatic cleanup.
_Avoid_: catchable panic, implicit failure

**Host adapter**:
A typed platform boundary that constructs and provides approved root services, handles every
remaining typed failure from the user entry function, and exposes a final native machine entry with
empty failure and requirement rows.
_Avoid_: ambient runtime, unchecked main requirements

**Failure handler**:
A lexical expression or block that handles selected nominal members of a failure row and produces a
replacement result or control-flow exit. Handled members are removed from the surrounding failure
row; unmatched members continue to propagate. A matching branch receives ownership of its failure
value and uses the ordinary consuming or borrowing match rules. Only an unguarded exact-member or
universal branch proves coverage; guarded branches do not remove a member from the residual row. Its
success type is the normalized union of the protected expression and reachable recovery branches.
_Avoid_: catch-all exception handler, resumption point

**Type union**:
A normalized, unordered set of detached concrete value types such as `Full | Empty` or
`i32 | string`; order, nesting, and duplicate members do not affect its identity. A value has one
active member and an implicit discriminant, allowing exhaustive matching without user-defined
tag-field names.
_Avoid_: tagged record, variant map

**Scalar enum**:
A closed nominal type whose qualified, fieldless members have fixed-width integer discriminants.
It is Copy, supports exhaustive matching by member name, and does not implicitly convert to or from
its backing integer. Alternatives that carry data use a type union of nominal structs instead.
_Avoid_: data-carrying enum, open enum, integer alias

**Finite type-set constraint**:
A compile-time generic bound restricting a type parameter to a closed set of nominal types while
preserving the concrete type at each monomorphized call. It is not a runtime union or permission to
branch on type identity.
_Avoid_: overload set, conditional type, runtime type switch

**Interface conformance**:
A declaration owned by a nominal type that maps interface operations to existing actor-module
functions. It neither adds instance methods nor permits another module to attach retroactive
behavior. A conforming operation may have smaller failure and requirement rows or weaker access
needs than the interface operation, but never stronger ones.
_Avoid_: extension implementation, orphan conformance, method injection

**Intrinsic**:
One irreducible compiler operation exposed only through the sealed `Intrinsic` namespace. An
intrinsic is the smallest target-neutral mechanism needed to build a feature in ordinary Silk
source: concrete scalar operations, representation queries, ownership state transitions, Effect
substrate, language-only place operations, or audited platform crossings. Public policy,
validation, generic selection, services, and safe reusable APIs are never intrinsic merely for
convenience.
_Avoid_: standard-library builtin, privileged actor, compiler-known service

**Raw pointer**:
A typed, non-null, non-owning machine address whose existence alone grants no lifetime or access
right. Holding one is inert in safe code; interpreting or manipulating its address requires an
unsafe boundary.
_Avoid_: reference, nullable pointer, owned pointer

**Pattern condition**:
An `if` condition that borrows a value, tests a refutable pattern, and binds the successful nominal
member without treating the value as truthy.
_Avoid_: truthiness test, implicit case check

**Service**:
A nominal source-defined contract named in a function's requirement row. Implementations declare
conformance explicitly; provisioning supplies a runtime value through statically known arguments or
environment slots rather than runtime tag lookup. A lexical environment has at most one current
implementation of each service-role pair. Service-and-role-qualified operation calls use that
implementation implicitly; allocation is an ordinary service. Unlike an ordinary interface, a
service creates Effect requirements and can be replaced lexically at runtime.
_Avoid_: capability declaration, ordinary interface, global service, injected object

**Service role**:
A nominal compile-time marker distinguishing one statically known use of a service capability from
another, such as primary and replica databases or durable and scratch allocators. Omitting the role
selects the built-in `DefaultRole`; roles are never strings, runtime keys, or a substitute for an
explicit router or pool when the instance set is dynamic.
_Avoid_: service name, dependency key

**Service implementation**:
A runtime value satisfying a service capability's interface. Implementations are replaceable even
though the required contract and its provision path are checked at compile time.
_Avoid_: dependency tag, service singleton

**Service witness**:
A compiler-shaped table mapping one service capability's operations to functions for a conforming
implementation type. Capability calls use statically known table offsets; the witness carries no
runtime service tag and requires no registry lookup.
_Avoid_: service tag, reflection table, global vtable registry

**Service slot**:
A hidden, non-owning pair of an implementation pointer and its service witness, passed as an
individual function argument for each required capability-role pair in canonical row order. The
role selects a slot at compile time and has no runtime lookup representation. A slot is not a
heterogeneous environment object and cannot escape its provision lifetime.
_Avoid_: service container, ambient context object

**Service provision**:
A lexical binding of one service implementation to a capability-role pair, discharging that pair
from the enclosed computation's requirement row. Providing an already satisfied pair requires an
explicit override; leaving the region restores the outer implementation. Provision may move and
own an implementation for the region or borrow an existing one explicitly, following ordinary
ownership, lifetime, aliasing, and automatic-cleanup rules. Multiple providers initialize in source
order, may depend only on earlier or outer providers, and clean up in reverse order.
_Avoid_: global registration, implicit override

**Effect reference model**:
The TypeScript Effect library as a semantic reference for how programs compose through typed
failures, requirements, services, scopes, concurrency, interruption, and observability. Silk Effect
does not target Effect API parity, source compatibility, identical runtime behavior, or Effect
integration as a primary goal.
_Avoid_: Effect port, native Effect runtime
