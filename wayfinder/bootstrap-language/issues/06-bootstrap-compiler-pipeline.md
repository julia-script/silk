# Design the bootstrap compiler pipeline and intermediate representations

Type: grilling
Status: resolved
Blocked by: 01, 02, 03, 04, 05

## Question

What staged compiler architecture takes Silk Effect source through parsing, name and type analysis,
function-contract checking, ownership checking, lowering, LLVM emission, and native linking while
remaining simple enough to port from Effect TypeScript to Silk Effect and avoiding gratuitous
barriers to a later direct WebAssembly backend?

## Answer

The bootstrap compiler is a deterministic, single-threaded batch pipeline over the source-module
closure reachable from one compilation request. It does not introduce a general incremental query
engine. Each phase publishes an immutable result for the next phase, while declaration dependency
resolution may use localized memoization and deterministic worklists to support cyclic imports and
recursive declarations. Canonical module, declaration, type, and instance identities determine
work and diagnostic ordering; filesystem traversal and insertion order do not affect program
meaning or output.

The frontend begins with a lossless `SyntaxFile` for every loaded source module. It owns the
original source bytes, a token stream retaining whitespace and comments, and a source-faithful
surface tree with explicit missing and error nodes. Tree nodes and tokens carry stable source IDs
and byte spans. The surface tree preserves the grammatical form the programmer wrote; semantic
desugaring waits until names, types, and contracts can be checked. Documentation comments have a
distinct token kind but need not be semantically attached during bootstrap.

Source mistakes are ordinary diagnostic data rather than fail-fast phase errors. Parsing,
declaration collection, resolution, type and contract elaboration, and ownership analysis return
the facts they can determine together with diagnostics. Missing, ambiguous, unresolved, and
erroneous states remain explicit and retain the diagnostic that originated them; an unknown fact
must never masquerade as a valid empty contract, resolved declaration, or concrete type. A damaged
body may be unavailable for MIR lowering while unrelated declarations and bodies remain fully
queryable. Filesystem, process, toolchain, and equivalent operational failures remain typed
compiler failures, while violated compiler invariants remain traps.

All tooling-relevant products form an immutable analysis snapshot. Syntax, declaration, HIR, and
semantic entity IDs key separate fact tables rather than successively rewritten annotated trees.
The initial tables include the declaration index, name-resolution facts, type facts, function-
contract facts, ownership and scope facts, and diagnostics. A phase may use exclusive mutable state
while building its table, but publishes a read-only result. A supported analysis facade exposes
queries over sources, syntax, declarations, references, types, contracts, ownership facts, and
diagnostics without exposing raw HIR storage. Bootstrap need not implement every future editor
query, but its identities, recovery states, provenance, and phase boundaries must allow the facade
to grow without reimplementing Silk semantics in a separate tool.

The compiler has two owned intermediate representations between syntax and code generation. HIR is the
resolved, typed, generic-aware semantic representation. It uses canonical declaration and type IDs,
normalized function contracts and rows, core semantic operations, and source provenance while
retaining type and contract-row parameters. MIR is a monomorphic, backend-neutral structured
control DAG. Ordered operations, conditionals, loops, cleanup, and terminal outcomes retain their
canonical region identities; repetition is the meaning of an explicit loop region rather than a
graph back-edge. MIR makes moves, borrows, drops, cleanup paths, success/failure outcomes, service
slots, witness calls, matches, traps, and runtime-helper calls explicit without containing LLVM or
WebAssembly types, instructions, labels, branch depths, intrinsics, attributes, or metadata nodes.
MIR is backend-neutral but target-aware: the complete compiler-selected target and concrete
data-layout plan are part of the MIR program, including physical field offsets when aggregate types
require them.

Frontend checking proceeds in this order:

1. Starting at the root module, load and parse modules while following their syntactic imports
   until the complete reachable closure is known.
2. Collect every top-level declaration header and assign canonical identities before resolving any
   body. Resolve imports, declared types, interfaces, conformances, public signatures, and explicit
   function contracts, reporting irreducible dependency cycles as specified by issue 04.
3. Elaborate function bodies through their declaration dependency graph. Local and referenced-name
   resolution, expression and pattern typing, function-contract inference or validation, handler
   row subtraction, and HIR construction are one integrated phase because a function's contract is
   part of its type. Private non-recursive dependencies may be memoized on demand; recursive
   strongly connected components use their required explicit contracts.
4. Check ownership, lexical borrowing, mutation, complete initialization, named-scope outlives and
   escape rules, and live owners at structured exits once on typed generic HIR. This produces
   ownership facts and a target-neutral cleanup plan; it does not insert target-specific drops.
5. Analyze every declaration in the reachable source closure, including unused private and generic
   declarations, but discover concrete runtime instances only from the typed host adapter and user
   entry. Reachability additionally follows function values, service-witness entries, drop glue,
   and runtime helpers. An instance key is the canonical declaration ID plus normalized concrete
   type and contract-row arguments. The deterministic worklist records an instance before following
   it so ordinary recursion terminates; the existing restriction that recursive generics preserve
   their parameters prevents polymorphic instance expansion.
6. Select the canonical target profile and compute one backend-neutral layout plan for every
   concrete runtime type discovered by the instance worklist. This phase runs before MIR lowering
   so layout facts and diagnostics are available to the compiler and its analysis facade early,
   while generic types that have no concrete runtime instance need no speculative layout.
7. Lower reachable instances to MIR while preserving structured control as an acyclic region graph
   and inserting concrete drops and cleanup regions from the generic ownership proof. Loop repeat
   and exit outcomes name lexical loop ports rather than successor edges. Typed failures become
   explicit success/failure outcomes, requirements become canonical hidden service slots, and
   source and semantic provenance remain attached to lowered operations.

MIR uses logical Silk types and operations together with the compiler's canonical target and layout
table. The layout plan supplies target triple, pointer width, endianness, concrete sizes,
alignments, field offsets, union discriminant and payload placement, scalar representations, and
private ABI decisions without using LLVM or another backend's types. A backend must realize this
plan exactly and cannot independently choose physical layouts at emission time. MIR does not adopt
LLVM control flow merely because LLVM is the bootstrap backend, nor WebAssembly stack and
structured control flow in anticipation of a future backend. LLVM deterministically flattens the
common DAG into backend-private blocks and back-edges; WebAssembly maps the same regions directly to
its nested control constructs. Neither backend may reconstruct compiler-known source structure from
a flattened graph.

Code generation is selected through a nominal `Backend` service. Its bootstrap operation consumes
the whole target-aware monomorphized MIR program plus a codegen request and produces one relocatable
object artifact. It does not receive a second independently selectable layout input. Source modules
are semantic namespaces rather than codegen units: one compilation request produces one MIR
program, one LLVM module, and one program object. A later backend may partition MIR internally
without changing source semantics or the service contract.

The bootstrap `LlvmBackend` lowers MIR into the existing Silk LLVM builder, emits deterministic
LLVM bitcode directly, writes it to a scoped build artifact, and invokes a pinned external Clang
process with `-c` to optimize and emit the target object. It does not load `libLLVM`, use the LLVM C
API, or require a compiler-private native FFI. Textual LLVM IR remains an implementation-specific
inspection artifact over the same builder model. A future direct WebAssembly implementation may
provide the same `Backend` capability; the compiler driver neither inspects backend identity nor
receives backend-private IR.

Bitcode and object intermediates are owned, path-backed artifacts tied to a named build scope.
Leaving that scope removes them after success or failure. Retaining an intermediate is an explicit
promotion or copy to a durable destination, as with a `save temps` request; ordinary compilation
does not read large Clang outputs into memory merely to write them again.

Native linking is a separate nominal `NativeLinker` service. Its bootstrap `ClangLinker`
implementation validates target-compatible inputs, combines the program object with the selected
runtime objects and approved system libraries, and invokes the pinned Clang driver with structured
arguments rather than a shell command string. It retains process output, status, and command
provenance on failure and writes the executable to the requested durable destination. The compiler
itself orchestrates backend and linker calls; a Node.js or TypeScript harness may test the compiler
but may not perform a stage required for stage-2 self-hosting.

Bootstrap supplies a deliberately small C runtime shim compiled by the pinned Clang toolchain. It
exposes a private, compiler-versioned scalar ABI for the unsafe platform boundary needed by issue
07. Silk code owns higher-level allocation, filesystem, process, failure, ownership, and service
behavior, and the typed host adapter constructs approved root services, handles every remaining
typed failure, and reaches a closed native entry. The private shim is not user-facing FFI and may
shrink as more runtime behavior becomes practical to implement in Silk.

Syntax, HIR, and MIR remain in memory between phases. They may each expose an ordinary,
deterministic textual encoder for debugging, inspection, and golden tests. Encoders observe a
completed artifact and never participate in the following phase. They are not services, no binary
phase format is required during bootstrap, and no persistent compatibility promise attaches to
their output. A generic encoder abstraction should be introduced only after multiple real formats
or consumers justify it.

Diagnostics are structured ordinary data. Each diagnostic has a stable code, severity, concise
message, one primary source span, optional labeled related spans and notes, optional unambiguous
machine-applicable edits, its originating phase and semantic entity, and an optional causal
diagnostic ID. Error sentinels preserve that provenance so dependent cascades can be suppressed or
attached to the primary error. Phases never print diagnostics themselves. The compiler driver sorts
them deterministically by canonical module identity, primary span, code, and a stable tie-breaker;
human and future machine renderers consume the same data.

Every HIR and MIR operation retains enough provenance to recover its source location and semantic
origin. Compiler-generated cleanup, failure-forwarding, and witness-dispatch operations inherit the
nearest causative source span and are marked generated. Debug builds emit native LLVM debug compile
units, files, subprograms, lexical scopes, and instruction locations, preserving readable Silk
function names alongside unique linkage names. Source line and column positions are derived from
the original bytes only at emission. Bootstrap acceptance requires source breakpoints, stepping,
and useful stack traces. Full optimized local-variable and source-type inspection is deferred, but
binding and type provenance must remain available to add it without replacing MIR. Native LLVM/
DWARF metadata is the source-map mechanism; bootstrap does not create a separate JavaScript-style
source-map artifact.

MIR performs no general optimization. It may remove lowering-created unreachable regions, fold
conditions whose values are already constant, share mechanically identical cleanup regions, and
verify its invariants while preserving provenance. LLVM owns optimization through three bootstrap
profiles: debug uses `-O0` with debug metadata, release uses `-O2` and strips debug metadata by
default, and release-with-debug uses `-O2` with line information. Bootstrap excludes a Silk SSA
optimizer, custom inlining, `-O3`, and a configurable LLVM pass pipeline. Correctness, cleanup, and
resource behavior never depend on LLVM optimizing successfully.

Performance and predictability are milestone gates rather than aspirations. Identical compiler,
source snapshot, target, profile, and pinned toolchain inputs must produce byte-identical syntax,
HIR, and MIR textual encodings and LLVM bitcode. Every phase reports elapsed time, input and output
counts, diagnostic counts, and allocator-backed memory totals; an external benchmark harness also
records peak RSS. On the same reference machine and source, the release stage-2 frontend must
compile the compiler no slower than the stage-0 TypeScript frontend's median and use no more peak
memory, with external Clang and linking measured separately. Doubling independent modules,
declarations, or function bodies may not increase a frontend phase beyond 2.5 times after fixed
startup cost without an explicit approved explanation. Release scalar, aggregate, direct-call,
checked-arithmetic, failure, cleanup, and witness-dispatch microbenchmarks must remain within twice
the time of equivalent Clang `-O2` C programs that perform the same checks and indirect calls.
Issue 09 owns the fixed corpus, reference machines, run protocol, and stored measurements.

The Rust tooling comparison that informed the incomplete-program and public-analysis boundaries is
recorded in [the research note](../research/rust-tooling-resilience.md).
All source spelling remains issue 08's responsibility; the exact runtime capabilities and shim ABI
belong to issue 07, and final staged-build commands and acceptance fixtures belong to issue 09.

## Amendment — 2026-08-05

The compiler is backend-agnostic, not target-agnostic. This amendment moves canonical target and
all concrete layout decisions from backend emission into an explicit compiler phase after concrete
instance discovery and before MIR lowering. It supersedes the earlier backend-owned aggregate and
union layout wording.

## Amendment — 2026-08-06

All compiler-published control relationships remain DAG-shaped. MIR preserves structured operation,
conditional, loop, cleanup, and continuation regions; `repeat` and `exit` are lexical outcomes of an
enclosing loop, not traversable graph edges. Cyclic CFG blocks and back-edges are backend-private
derived artifacts for targets such as LLVM. Structured targets such as WebAssembly consume the same
DAG directly and must not recover loops or conditionals from flattened control flow. This amendment
supersedes the earlier basic-block lowering and WebAssembly CFG-structuring wording.
