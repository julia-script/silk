## Context

See `proposal.md` for motivation. The storage and backend machinery already represents structural
unions as one canonical sum calling shape: a tag lane followed by the maximum payload lanes, with
unused lanes zero-filled when values are constructed. `Slot.copy` and shared `RawBuffer.read`
already load every lane in that shape, but MIR's recursive Copy predicate stops before structural
unions and reports the otherwise well-typed operation as lost provenance.

The existing structural-union contract admits only nominal members and already says an all-Copy,
cleanup-free union is Copy. Nominal Drop declarations are forbidden for recursively Copy records,
so the layout-driven recursion can remain consistent with cleanup eligibility.

## Goals / Non-Goals

**Goals:**

- Make one canonical recursive Copy decision cover nominal aggregates, fixed arrays, and structural
  unions at raw-storage verification boundaries.
- Preserve the full canonical sum value across Slot and shared raw-buffer copies on all engines.
- Prove the behavior with both focused storage tests and the stack VM's ordered event stream.

**Non-Goals:**

- Implicitly duplicate move-only union values or introduce a source-level `Copy` declaration.
- Change union normalization, numeric discriminants, physical layout, or external ABI promises.
- Add member-dispatch branches, reference-counting, initialization bitmaps, or Vector-specific
  compiler behavior.

## Decisions

### Extend the existing recursive verifier predicate to canonical unions

The MIR verifier will classify a structural union as Copy only when every canonical member is
recursively Copy. It will retain the existing cycle guard for nominal aggregates and reject any
unavailable, intrinsic-owner, or non-aggregate member. Both `SlotCopy` and `RawBufferRead` will use
this same decision.

A new parallel union-copy operation was rejected because the current operations already carry the
canonical element and result types and the layout plan already owns the sum shape. Specializing the
operation would duplicate storage semantics and make generic `Vector.get` union-aware.

### Copy the complete canonical calling shape without runtime dispatch

Evaluator values preserve their existing immutable active-member object. Native and Wasm continue
to load the union tag and every planned payload lane using canonical lane offsets. Construction and
Slot writes already materialize the complete zero-filled sum calling shape, so the read can remain a
straight lane copy with no branch on the active member. This keeps cost proportional to the union's
fixed maximum representation and identical to copying any other aggregate of that shape.

Member-directed copy was rejected because all admitted members are cleanup-free Copy values; a
runtime branch would add cost without changing ownership or observability.

### Keep rejection at MIR verification

Generic Silk code can mention `Vector<T>` and raw-storage intrinsics before a concrete `T` exists.
The concrete layout and canonical union members are available at monomorphic MIR verification, so
that remains the single rejection point for a union containing any move-only member. Evaluation and
backends consume only verified operations.

### Restore one ordered VM event vector

The VM will append injected `Step` and `VmDiagnostic` values to one
`Vector<Step | VmDiagnostic>`, then fingerprint the vector after execution via shared `Vector.get`
and exhaustive matching. This simultaneously proves append-time conversion, raw union storage,
shared read-back, match dispatch, ordering, allocation rollback, and three-engine parity using
ordinary Silk.

Keeping the temporary two-vector shape was rejected because it would test only synthetic storage
fixtures and leave the real program that exposed the defect permanently shaped around it.

## Risks / Trade-offs

- **Payload lanes of differently shaped members could drift from storage offsets.** → Add focused
  unions with distinct member shapes and compare tag/payload values across evaluator, native, and
  Wasm; retain the layout plan as the sole lane-offset authority.
- **A union with one owning member could be admitted accidentally.** → Add verifier-negative cases
  for both Slot and shared raw-buffer copies and assert engines never receive the malformed MIR.
- **The unified VM changes allocation ordinals.** → Recompute its baseline ordinal count and sweep
  every new ordinal, requiring balanced release and deterministic reruns across all engines.
- **Copying the maximum payload does more work than copying the active member.** → Accept the fixed,
  transparent representation cost for now; benchmark before adding a branch whose cost and code size
  may be worse.

## Migration Plan

Land focused recursive-Copy and union-lane parity tests first. Remove the temporary non-union
verifier boundary, update the standard-library acceptance coverage, then restore the unified VM
source and its differential/allocation tests. Strict OpenSpec and full repository gates precede
sync, archive, and merge. Rollback is the branch revert because no persisted format or public ABI is
released.
