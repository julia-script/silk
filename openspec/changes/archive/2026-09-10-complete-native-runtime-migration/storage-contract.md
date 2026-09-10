# SPEC-02 selected execution-storage contract

This is the implemented selected-component contract. The inventories and adjacent
conformance files record admission and execution separately; repository gates are
tracked in tasks.md.

## Capability and ownership

The `execution-storage` capability binds four operations to ordinary source
foreign exports. The compiler knows the capability's machine contract, never the
provider's module, type or declaration names. Artifact configuration selects the
bindings. Reachable private-frame operations demand this capability; a catalog
entry alone does not retain its modules. Missing or incompatible operations fail
before object emission. Provider code must not itself demand execution storage.

Bootstrap validation follows executed source calls and private Effect runners from
all four selected exports. It includes concrete callable environment/realization arguments,
observation state/callback cleanup, selected callable branches, cleanup hooks and
calls through a matching source C export. It rejects a reachable suspension frame or
Execution drive whose retained result-package dispatch needs an initial continuation
segment. The latter is a dependency even when the outer provider function itself
returns directly. Constructing and dropping an unexecuted Effect is not a storage
acquisition. Branch analysis follows retained executable behavior, without relying
on an optimizer to delete a potentially executable dependency. External native
supplies still require their independent bootstrap/helper closure evidence.

| Operation | Target C ABI                                                                    | Contract                                                                                                         |
| --------- | ------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------- |
| create    | `() -> nullable mutable byte pointer`                                           | Acquires an independent state instance with explicit source-selected configuration; null means no state acquired |
| acquire   | `(state pointer, usize size, usize alignment) -> nullable mutable byte pointer` | Returns fixed-address, sufficiently aligned frame storage or null; failure retains no partial reservation        |
| release   | `(state pointer, frame pointer) -> void`                                        | Consumes one frame from this state, after semantic cleanup                                                       |
| destroy   | `(state pointer) -> void`                                                       | Consumes the state after every frame is released                                                                 |

The byte-pointer ABI erases private provider representation at this boundary;
source owns its state and allocation headers. No source API may turn an arbitrary
pointer into a valid state or frame safely. The compiler guarantees matching
originating state, valid size/alignment, and exactly-once release/destroy on
structured exits. The source provider owns capacity checks, accounting,
reservation metadata, allocation/release and invariant policy.

State construction belongs to first activation of private storage, after any
recoverable owned-Execution package construction. Null create/acquire follows the
existing private-stack exhaustion trap. No allocator service or typed error is
added to ordinary suspension. A provider's independently exposed source
constructor may report allocation refusal recoverably; it must release partial
state before returning that failure. The ABI adapter translates only this
explicit construction refusal to null. Fatal traps do not promise destruction.

## Transfer and package lifetime

Each transient suspension driver owns a state for that invocation. The state
pointer occupies an additional private transfer-header word and is passed with
the existing transfer pointer into machine steps, child thunks and resume thunks.
Nested transfer shares that invocation state. Before returning the final ordinary
or typed-failure result, the driver destroys state after all exited frames have
been released. A reentrant exported call creates a separate invocation state.

An owned Execution keeps its state in its InitialContinuationSegment across
activations. Construction initializes the pointer to null. First activation
creates state; parking preserves it; subsequent activation restores it into the
transfer header. Completion and cancellation release retained frames and then
destroy state before releasing the owning package. An unstarted package never
acquires state. A retained Wake may extend the inert package lifetime, but must
not extend live frame or storage-state ownership after cancellation.

Different invocations and Execution packages do not share accounting implicitly.
Concurrent calls with distinct states are supported when the selected provider's
underlying allocator supports them. Concurrent mutation of one Execution remains
subject to its existing ownership rules. No global TLS feature or hidden ambient
state is added. A provider that chooses shared accounting must expose and verify
that separate source policy; the default component uses independent states.

## Native and LLVM-to-Wasm migration

The hosted source component explicitly declares its allocator dependency and
stores capacity/accounting in each state. Configuration is source-owned; the old
environment-variable knob and thread-local counter are deleted. Header size,
alignment padding and overflow checks belong to that source implementation.

LLVM-to-Wasm uses the same four-operation contract and transfer/package lifetime,
with a target-appropriate source provider. Its existing linear-memory allocator
is a separate underlying boundary, not a reason to retain the old frame push/pop
ABI. Required closure evidence must distinguish that allocator from the migrated
frame policy and identify its actual imports and non-reclamation behavior.

Delete CoroutineRuntime's generated C policy, push/pop reservations and shared
call plumbing only with their complete replacement. Update the transfer header,
ExecutionPackage layout, release contexts, frame reuse/cancellation checks,
explicit exhaustion configuration and cache identity together. No compatibility
ABI remains after the migration.

## Admission obligations

Independent C signature fixtures must cover all four operations against the exact
target widths, along with the source provider's raw-pointer/layout initialization
contract and restricted bootstrap closure. Prescriptive suspension/termination
references must remain coherent with that contract. Completed evidence and
remaining closure obligations are tracked in progress.md. The existing
borrowed-outcome and dependent-cleanup fixtures remain semantic authorities.

## Minimal typed-storage primitive

The source provider must interpret allocator-owned bytes as its state and
reservation records. The existing qualifier conversion preserves pointee identity
and cannot express that operation. Add unsafe `Intrinsic.pointerReinterpret` for
this boundary: it preserves address space, mutability, nullability and extent,
changes the pointee, and requires an explicit caller alignment/initialization
proof. Existing pointer-address observation and positive byte indexing suffice
for aligned payload placement; no integer-to-pointer reconstruction is needed.
This primitive creates no owner, loan or initialized state and replaces no
language ownership machinery. Semantic, MIR and independent native object
fixtures must prove the boundary before provider admission.

## LLVM-to-Wasm direct C admission

The source provider's declared allocator cannot use the previous blanket rejection
of source C calls on Wasm. Admit the minimum direct C ABI required by the shared
component: data pointers in address space zero, 32-bit signed/unsigned integers,
and void results. Variadics, narrow or 64-bit scalars, floats, callbacks, borrowed
foreign arguments, indirect calls and foreign data remain outside this Wasm
subset. No allocator name is recognized specially. The static finalizer resolves
all symbols and supplies no arbitrary host imports. Existing fatal foreign-unwind
guards remain in generated LLVM; this change does not add a Wasm exception ABI.

The shared native/LLVM-to-Wasm frame ABI now selects source create/acquire/release/
destroy operations. Transient suspension, retained Execution frames, cancellation
and refusal probes exercise that ABI. The Wasm conformance runner checks import-free
linking, provider layout/fault cases and the shared lifecycle programs in both modes.
The JUL-130 source main export/invocation and exact zero-argument Wasm C signature
are verified separately by the source-entry lanes in storage-wasm-conformance.json.

## Reentrant native callback boundaries

The existing multiple-package fixture now uses a C receiver that records every
allocation and release. It first observes the successful outer lifecycle, then
repeats it with a nested invocation at each source callback boundary. Each nested call
must release exactly its own allocations and preserve the bytes of all live outer
allocations. The outer call must still complete both packages and release all its
allocations. This exercises independent invocation state during readiness notification,
registration, suspension and completion, without assuming a constant callback count.
It complements the provider-ABI refusal sweep and the latched-destruction fixture;
the finalized-destroy probe adds borrowed diagnostic ownership and captured cleanup,
and privilege-audit.md records the separate bootstrap supply review.

The allocator interposer only records allocations and releases. It never initiates
reentry: an allocator that itself requires execution storage would violate the
bootstrap contract. Reentry occurs through ordinary source callbacks.
