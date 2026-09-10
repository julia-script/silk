# Retained compiler privilege audit

The review unit is the sealed operation identity, not a standard-library actor name.
`intrinsic-inventory.json` enumerates all 700 operations with their complete signature,
phase, admission category, consumer, lowering identity and target set. The inventory
fixture in IntrinsicCatalog.test.ts checks that enumeration against the implementation.
The following review covers every inventory row by its category and names the distinct
runtime boundaries that the category counts alone cannot justify.

| Category       | Operations | Reason for compiler ownership and policy boundary                                                                                                                                                                                                                                                                                            |
| -------------- | ---------: | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Scalar         |        618 | Primitive scalar arithmetic, comparison and checked/unchecked conversion over compiler-owned machine representations. The table expands by scalar type; it introduces no provider selection, allocation, I/O or process policy.                                                                                                              |
| Representation |          6 | String byte representation/exact equality, target layout and scalar enum representation. UTF-8 validation and safe constructors remain source operations. Layout facts come from the selected target; equality may require the independently admitted memory helper.                                                                         |
| Ownership      |         39 | Execution/package/Wake and Shared control-block ownership; raw buffers, pointer qualifiers/access and initialized slots. These operations manipulate compiler-checked ownership or concrete layout. Source acquires allocations, selects storage limits, constructs public wrappers and chooses callbacks.                                   |
| Effect         |          8 | Suspension, three lexical provider binding modes, selected failure recovery, lexical/terminal diagnostic observation and transparent finalization. These preserve requirement rows, continuation control and failure ownership. Scheduling, reporting text, context-pool capacity, terminal status and retry policy remain source.           |
| Language       |         27 | Static target/profile facts, static text/sequence construction, reflection and field projection, plus place replacement. Static-only operations produce no runtime target availability. Profile data does not invoke an application or install a runtime.                                                                                    |
| Platform       |          2 | System allocation acquisition and admitted native assembly. The former is the existing allocation/reclaim-ticket boundary, not execution-frame accounting. The latter is explicitly unsafe, target-selected and requires register, memory, stack and no-unwind proofs. Raw Linux syscall wrappers are ordinary source consumers of assembly. |

## Changes requiring individual admission

`Intrinsic.pointerReinterpret` changes pointee interpretation while preserving address
space, nullability, mutability and extent. It creates neither allocation ownership nor
initialized storage. Its unsafe alignment/initialization contract is needed by source
storage records and raw mappings; no general integer-to-pointer primitive was added.

`Intrinsic.observeDiagnostics` binds an owned lexical state and a NonParking callback.
`Intrinsic.observeUnhandled` observes the active selected terminal context after source
payload cleanup. Six-word diagnostic handles and suspension transport are compiler facts;
the bounded persistent pool, iterative rendering, output limits and status policy are in
NativeDiagnostics, NativeReport and NativeStart. Their precise ownership contracts and
32/64-bit conformance are in report-observer-contract.md and report-context-contract.md.

`Intrinsic.finalizeEffect` is necessary to retain the original diagnostic owner while an
arbitrary infallible finalizer runs with its own requirement row. A Result/rethrow loses
that owner; selected recovery changes its cause; Drop cannot run that arbitrary Effect.
The seam adds no scheduling or cancellation policy. It uses ordinary exact Effect calls,
conditional control and outcome propagation, with shared concrete capture cleanup for
normal exits and suspension destruction. Source Effect.ensuring supplies the public API.

`Intrinsic.catchFailure` accepts a handler that returns a once Effect because the handler
and its result execute once. The change preserves typed selected-recovery semantics and
allows captured owners; it does not add an error representation or alternate catch path.

## Compiler machinery outside the callable inventory

`Intrinsic.application` is a source import-selection seam, not a callable builtin.
ArtifactComposition reads installed composition data and explicit roots generically.
Foreign exports, retention, source interfaces and ordinary call discovery determine
invocation. NativeStart/WasmStart/RawStart names are absent from non-generated semantic
and backend code. The same applies to the process, storage, input and report providers.

The execution-storage capability knows four C operation signatures and their lifecycle,
not any provider declaration spelling. Bootstrap checking follows executed MIR calls,
callable identities, cleanup hooks, owned package/shared cleanup, diagnostic callbacks
and matching source C exports. Implicit malloc/free/memcmp dependencies participate in
that walk. Direct, destructor-mediated and implicit allocator recursion are rejected;
harmless deferred construction is accepted. External allocations cannot call back into
a storage-demanding Silk computation under this bootstrap contract.

Native helper supply remains independently checked. HelperSource builds source helpers
as entry-none/runtime-none/libc-none objects. NativeProgram's support admission rejects
allocation, frames, foreign calls/data/indirect calls and assembly. HelperCapability
checks exact exports, rejects self references and undeclared object imports, resolves
provider cycles, and rejects unsupported helper families. LLVM-created memory calls thus
do not silently import execution storage. Pinned native libc malloc/free are the explicit
source-provider dependency; storage signature fixtures and fault/reentry receivers test
their required C shape and ownership behavior. The reentry receiver initiates calls only
from source callbacks, never from the allocator itself.

The Wasm memory bootstrap remains a separate linear-memory allocator. Its production
non-reclamation behavior is not a retained execution-frame policy. Independent Wasm
receivers count and release allocations to prove semantic storage ownership, separately
from import-free Driver lifecycle and source-entry lanes.

The C foreign-unwind guard and LLVM symbol/helper bookkeeping remain compiler boundary
mechanisms. They contain no generated main, argv/environment snapshot, process capture,
execution-storage counter, report context or output loop. Fatal traps retain their existing
non-unwinding behavior. No new foreign-thread, retaining callback or atomic capability is
implied by this migration.

## Absence and scope

source-absence-audit.json records source digests and the exact retired names scanned.
artifact-absence-audit.json records the corresponding inspected native objects and Wasm
exports. The source module spellings in the inventory's consumer column are documentation
references; they do not select semantic behavior. Generated Stdlib source is excluded
from spelling scans because it embeds the ordinary source catalog.

The audit preserves the explicit remaining capabilities in coverage-ledger.md. In
particular, static PIE, retained foreign contexts, generalized aggregate ABI/TLS/atomics,
Wasm command/reactor composition and networking are not delivered by these five tickets.
Repository checks remain a separate handoff gate in tasks.md.
