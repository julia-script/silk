# SPEC-02 execution-storage boundary inventory

This inventories the implemented source-selected boundary. It supersedes the
admission inventory of generated C push/pop policy. The source component, lifecycle and bootstrap evidence are present. Full repository
gates in tasks.md remain the handoff condition.

## Selection and source policy

`RuntimeComponent` records a capability, operation-to-source bindings and diagnostic
provenance. `ArtifactComposition` retains this catalog independently of application
invocation and entry selection. A catalog entry alone does not retain a provider.
`ExecutionStorageComponent.demanded` detects private suspension regions or retained
Execution packages requiring an initial continuation segment. Selection then requires
exactly one complete four-operation binding, and resolution checks its monomorphic
source C exports. Provider module, type and declaration names carry no privilege.

| Operation | Source C signature                     | Ownership                                                                                             |
| --------- | -------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| create    | `() -> ?*mut u8`                       | Acquires one independent state, or returns null without ownership                                     |
| acquire   | `(?*mut u8, usize, usize) -> ?*mut u8` | Borrows the state and acquires a fixed-address frame of the requested size/alignment, or returns null |
| release   | `(?*mut u8, ?*mut u8) -> ()`           | Consumes one frame from its originating state after semantic cleanup                                  |
| destroy   | `(?*mut u8) -> ()`                     | Consumes the state after its semantically live frames have been released                              |

The default `silk/execution_storage` provider owns a linked reservation list and
per-instance used/limit fields. Its public source constructor accepts an explicit
capacity. Each reservation charges its header, alignment allowance and payload;
the separate fixed state allocation is outside that frame budget. The default C
adapter explicitly selects `usize.MAX`. Allocation, checked alignment/arithmetic,
reservation unlinking and backing malloc/free calls are ordinary source policy.
No environment knob, implicit TLS counter or generated frame allocator remains.

## Compiler-private lifetime and representation

| Boundary                                    | Current representation and behavior                                                                                                                                                          |
| ------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| NativeExecutionStorage                      | Direct selected source handles; lazy null-initialized state; bootstrap calls disable observation                                                                                             |
| NativeFunction                              | Acquires a new invocation frame only when there is no resume frame; reuses retained storage on resume                                                                                        |
| NativeReturn / NativeSuspension.returnStep  | Completed steps release the invocation frame after semantic cleanup; transferring steps retain it                                                                                            |
| NativeSuspension driver                     | Owns transient state and destroys it after the final result has exited every retained frame                                                                                                  |
| NativeExecutionOperation                    | Keeps state across parked activations, releases cancelled frames, destroys state, then releases package ownership                                                                            |
| ContinuationTransfer                        | Eighteen pointer-width words: child, head, append position, execution owner, storage owner, observer, six borrowed cause fields and six owned completed-result fields                        |
| ExecutionPackage.InitialContinuationSegment | Uses the same eighteen-word header; construction initializes its words to zero without creating storage state                                                                                |
| CoroutineFrame                              | Parent/state header, plus observer and two six-word causes when observation is retained; planned payload, stable ten-word observation descriptors and separate six-word outcome slots follow |
| NativeExecutionOperation initial body calls | Start an independent diagnostic invocation; they cannot retain a driving observer whose scope may end before parked execution                                                                |
| LLVM-to-Wasm                                | Uses the same selected four-operation source contract and lifetime; its underlying linear-memory allocator remains a separately admitted dependency                                          |

An unstarted Execution does not create private-storage state. Retained Wake
ownership can keep an inert package alive after cancellation, but not live frames
or state. Null private-state/frame acquisition remains fatal without extending the
public Effect failure or requirement row. Fatal traps do not promise cleanup.

Typed-failure metadata follows report-observer-contract.md: the cause reference is borrowed
under its observer lifetime, while the completed-result slot owns its transferred reference.
Storage release follows semantic cleanup of those fields on completion and cancellation.

## Bootstrap closure and evidence

The bootstrap walk starts at each selected source export and follows executed
ordinary calls, Effect runners, selected callables and matching source C exports.
It follows implicit malloc/free/memcmp source exports, shared payload cleanup and
owned Execution/Wake package cleanup. It follows observation state/callback cleanup without
executing a lazy body solely because that body was constructed. Callable selection
uses the concrete environment or stored realization arguments used by native
emission. A reachable private suspension or retained Execution drive is rejected
as `DependencyCycle` before object emission.

`Instances.test.ts` checks arbitrary provider names, missing bindings, signature
mismatch, changed provider cache identity, dormant catalogs, harmless deferred
construction, direct/cleanup-mediated recursion and observation-state cleanup.
The last case exposed an omitted bootstrap edge: its state destructor drove an
Execution and was accepted before DiagnosticScope cleanup traversal was added.
The matching harmless observation remains accepted.

Native admission evidence is recorded in storage-supplies.json and
storage-reentry-conformance.json; LLVM-to-Wasm evidence is in
storage-wasm-conformance.json. The current matrices pass 36 native storage lanes and 40 Wasm lanes; their
independent receivers distinguish semantic reclamation from the production Wasm allocator. The pinned native tools are LLVM 22.1.8, Darwin SDK 15.5 with
minimum 11.0, and the recorded GNU glibc 2.36 supplies.

## Closure evidence

privilege-audit.md records the external allocator contract and independent helper
bootstrap admission. Source C overrides of implicit malloc/free are included in
DependencyCycle regression tests. Helpers cannot acquire private storage or import
undeclared dependencies. Native/Wasm independent receivers verify reentry, frame
and package cancellation, and the separate unstarted/retained-Wake lifetimes.

The finalized-destroy fixture checks success/failure payloads, nonempty diagnostic
handles, observer destruction and cancellation both before the protected outcome
and while the finalizer is parked. It exposed erased Effect cleanup in suspension
slots: ConcreteCleanup now supplies the same captured ownership plan to ordinary
lowering and suspension planning. No finalizer is driven during cancellation.
The independent receivers prove all captured allocations are released, beyond the
source callback counters. A structured assertion checks the retained unrun
finalizer's reclaiming EffectCleanup without another runtime pipeline.

Cache emission identity v9 includes the finalization and cleanup changes. Source
and artifact absence audits, prescriptive references and conformance files record
the selected current paths. Required full repository gates remain in tasks.md 6.4.
