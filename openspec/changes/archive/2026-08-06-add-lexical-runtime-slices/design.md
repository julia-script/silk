## Context

See `proposal.md` for motivation and the delta specs for observable requirements. The compiler's
current fixed arrays are complete inline values: MIR and both executable backends can flatten every
element into a statically known lane vector, and evaluator calls pass immutable value snapshots.
That representation cannot implement one slice-taking function for multiple lengths or propagate a
callee's exclusive mutation back to its caller.

The target-aware MIR is already a structured control DAG. Loops repeat through lexical outcomes,
and only backend-private lowering may introduce cyclic control flow. The layout plan is already the
compiler-owned authority for aggregate representation and calling shapes, but its calling lanes are
currently scalar Silk values. This change must add addressability without making either raw
addresses or backend storage policy part of safe Silk.

Ownership already rejects partial moves and schedules deterministic cleanup. Slice loans extend
that analysis from match-arm access to function calls, but the deliberately narrow source surface
avoids stored borrows, arbitrary lifetime inference, and path-overlap solving.

## Goals / Non-Goals

**Goals:**

- Establish one reusable logical slice type and one compiler-owned physical layout per target.
- Make address-taken fixed arrays authoritative storage across calls in every execution engine.
- Express call-scoped loans and their endings as verifiable facts without runtime borrow checks.
- Preserve the structured MIR DAG and existing complete-replacement cleanup ordering.
- Leave a direct path from slices to scoped allocation and a Silk-written `Vector<T>`.

**Non-Goals:**

- General reference types, standalone slice locals, stored or returned borrows, non-lexical
  lifetimes, path-disjoint loans, or lazy-flow capture.
- Range values, subslices, iterators, unchecked indexing, or moving elements out through a slice.
- Heap allocation, allocator requirements, raw pointers, typed slots, drop hooks, or bulk memory
  primitives.
- Avoiding a later breaking source and ABI migration from bootstrap `I32` lengths to `USize`.

## Decisions

### 1. Use explicit borrowed sequence syntax, not a nominal-looking container

`&[T]` and `&mut [T]` are canonical semantic slice types with element type and access mode. A fixed
array never decays implicitly. `&array` and `&mut array` appear only as ordinary-call arguments and
borrow an entire direct array binding; the latter requires a mutable binding. The parser retains
borrow expressions elsewhere for recovery, but semantic analysis rejects unsupported positions.

Slice parameters may be forwarded through call-scoped reborrows. A shared parent permits only a
shared reborrow. An exclusive parent permits shared or exclusive reborrowing and is suspended until
the nested ordinary call returns. This is the only initial reborrow tree.

Standalone slice bindings were rejected because they immediately require rules for early loan
termination, moved exclusive handles, nested lexical scopes, and captured values. A nominal
`Slice<T>` value was rejected because it makes non-ownership and access mode implicit. Implicit
array decay was rejected because a caller must visibly choose borrowing rather than copying or
moving.

### 2. Represent slices as a closed semantic type with recursive non-escape checking

The semantic type vocabulary gains a slice constructor containing canonical element type and
`Shared` or `Exclusive` access. Type encoding and substitution recurse through the element type.
Copyability is semantic: a shared slice permission may be duplicated only within its call region;
an exclusive slice permission is affine. Neither type owns cleanup.

One recursive `containsBorrow` property rejects slices inside return types, fixed arrays, nominal
fields, unions, applied owned types, lazy environments, and captures. Direct ordinary-function
parameters and immediate call arguments are the only accepted positions. This single recursive
property avoids a list of storage-specific loopholes.

Source-visible `length` is `I32` for this slice. Fixed-array formation already caps lengths at the
largest non-negative `I32`; slice formation preserves that invariant. Internal addresses remain
target-width and must never be narrowed merely because length is temporarily `I32`.

Adding `USize` first was considered but rejected: it would add arithmetic, literal, conversion,
layout, and backend semantics not required to prove borrowed runtime cardinality. The project is
pre-stable, so the later migration may replace the source and calling shape cleanly.

### 3. Attach compiler-only loans to stable owner roots

Every accepted borrow gets a deterministic `BorrowId`, source root, access mode, start region, end
region, and provenance. Initial roots are complete direct array bindings; array fields and indexed
subplaces remain unsupported, so conflict checking is root equality rather than a range or overlap
solver.

Argument evaluation remains left-to-right, but every borrow started for one argument remains live
through all later argument evaluation and the complete callee invocation. Multiple shared loans of
one root are compatible. Any exclusive loan conflicts with all other loans and direct owner access.
Reborrowing creates a child loan and suspends its parent permission for the child call.

Every structured return, typed failure, early return, `break`, `continue`, and fallthrough closes
the loans of exited regions before owner cleanup. Traps keep the existing no-normal-cleanup
semantics. This ordering is derived once in ownership planning and verified again in MIR rather
than trusted to evaluator or backend behavior.

Runtime borrow tokens were rejected because all accepted lifetimes and aliases are statically
visible. Allowing owner access and copying data at calls was rejected because exclusive mutation
would either be lost or require unsound copy-back behavior around aliases and cleanup.

### 4. Extend the existing place model inside the MIR DAG

HIR retains slice formation/reborrow, loan identity, source root, access, element type, and source
span. Slice indexing produces a borrowed place, not an element value. A shared place materializes
only Copy leaves; exclusive places additionally support complete replacement. Field projection
preserves the borrowed access mode.

Lowering creates a logical slice local and ordered begin/end loan facts. Existing place operations
are generalized to accept a verified slice root whose runtime bound, element type, and address all
derive from that same local. The selector no longer embeds an unrelated static length for a slice.
The verifier rejects mismatched roots, missing ends, conflicting owner operations, and cleanup
before loan end.

Slice operations remain ordinary ordered nodes in existing regions. A loan formed in a loop body
ends before the lexical `Repeat` or `Exit` outcome, so no borrow or storage edge creates a MIR cycle.
A separate slice control graph was rejected. A fully separate read/write operation family was also
rejected because field/index projection, check-before-replacement ordering, and cleanup already
belong to the place abstraction; the root and bounds source are what differ.

### 5. Make layout lanes heterogeneous and keep addresses internal

The target layout vocabulary gains an internal address scalar and a slice representation. For a
concrete element type it records address width and alignment, the `I32` length offset, padding,
total size and alignment, and element stride. Shared and exclusive access reuse the same physical
entry because permission is compile-time information.

Calling shapes gain typed lanes rather than assuming every lane is an `I32`/`Bool` Silk scalar. A
slice shape contains one internal `Address<T>` lane followed by one `I32` lane. On 64-bit native
targets the first lane is a real pointer-width value; on the current Wasm target it is a 32-bit
linear-memory offset. Backends consume this plan and cannot invent a different slice ABI.

Encoding native addresses as `I32` was rejected because it is incorrect on 64-bit targets. Treating
the internal address as a safe `RawPointer<T>` was rejected because pointer arithmetic, provenance,
unsafe access, and allocation belong to later changes.

### 6. Give each engine authoritative addressable storage

The evaluator gains a deterministic frame store. Address-taken array bindings live in mutable cells
identified by frame and cell ordinals; a slice value carries that stable cell identity, base
element, and length. It never relies on JavaScript object identity and never copies the backing
array at a call. Access and `BorrowId` remain outside the runtime payload.

Native lowering materializes address-taken arrays in entry-block stack storage using planned size,
alignment, stride, and field offsets. Construction stores into that allocation; slice calls pass
pointer and length; and reads after a potentially mutating exclusive call reload authoritative
storage rather than stale SSA lanes. Non-address-taken arrays keep the existing scalarized lowering.

Direct Wasm lowering adds private linear-memory stack frames only when reachable address-taken
values require them. A private mutable stack pointer starts after static data, each invocation
aligns and reserves its compile-time frame size, and every normal structured exit restores the
previous pointer. Nested and recursive calls therefore receive distinct storage. Capacity checks
grow memory by deterministic page counts or trap deterministically when growth fails. Traps do not
pretend the aborted instance restored normal state. The memory and stack state are compiler runtime
machinery, not public allocation APIs.

A fixed per-function Wasm offset was rejected because recursion aliases live invocations. Wasm GC
arrays were rejected because they would create a target-specific reference representation that
does not match the planned linear-memory boundary needed by later raw memory primitives. Copy-in/
copy-out was rejected because it breaks alias coherence and cleanup identity.

Zero-length and zero-sized-element arrays still receive a stable aligned provenance position when
borrowed. Logical indices and lengths remain distinct even if valid zero-sized elements share a
physical address; bounds and ownership never infer identity from pointer inequality.

### 7. Prove cardinality erasure and write-through separately

The existing multi-module coverage fold changes from `[I32; 6]` to `&[I32]` and loops over
`values.length`. The entry calls the same declaration with reviewed three- and six-element arrays,
asserts one discovered instance and symbol, retains the short result `40`, and returns the existing
complete result `42`. This directly closes the limitation recorded by the prior acceptance change.

A focused exclusive fixture passes a mutable array of move-only aggregates to a helper, performs a
runtime-indexed complete replacement, returns, and immediately reads the owner in the caller. This
catches evaluator copies, stale native SSA values, Wasm frame aliasing, and incorrect displaced
cleanup more directly than complicating the coverage fold.

Focused fixtures cover negative indices, index equal to length, zero length, zero-sized elements,
check-before-replacement ordering, conflicting call arguments, recursive storage/return, immutable
exclusive formation, unsupported local binding, and moves from borrowed non-Copy elements. A nested
or recursive Wasm fixture proves per-invocation frames. `/labs` embeds the canonical fixture bytes
and adds no standalone inspector.

## Risks / Trade-offs

- [Risk] Addressability silently splits one logical value between SSA lanes and memory. → Once an
  array is address-taken, its engine-specific backing cell is authoritative; mutation acceptance
  reads the owner immediately after a helper call.
- [Risk] Heterogeneous lanes destabilize existing aggregate and union ABIs. → Add typed-lane
  verification first and retain unchanged shapes for all existing non-slice types.
- [Risk] Wasm shadow-stack arithmetic aliases frames or overlaps static data. → Derive one
  deterministic aligned frame plan, test nested/recursive calls, and validate overflow, growth, and
  restoration on every normal exit.
- [Risk] Loan endpoints drift from structured cleanup endpoints. → Derive both from the same lexical
  regions, retain explicit end facts, and make MIR verification reject cleanup under a live loan.
- [Risk] Exclusive slice replacement double-cleans or leaks the displaced value. → Reuse the
  existing check-before-RHS, complete replacement, cleanup-before-commit protocol and assert ordered
  cleanup traces.
- [Trade-off] Whole-root call borrows cannot express retained views or disjoint parallel mutation.
  → Keep the initial checker finite; add local slices and path overlap only when a concrete compiler
  pass requires them.
- [Trade-off] `I32` length limits source values and will require a breaking migration. → Keep the
  invariant explicit, keep addresses target-width, and schedule `USize` before public stability.

## Migration Plan

Implement the change as one vertical capability: syntax and types; semantic and ownership facts;
HIR/MIR and verification; target layout and typed lanes; evaluator storage; native storage; Wasm
frames; then acceptance and inspector projections. Keep existing fixed-array programs on their
unchanged value path throughout.

The change adds no compatibility bridge because the project is pre-stable. If the vertical slice
cannot meet three-engine or determinism acceptance, remove the slice syntax/types and the
address-taken storage paths together; existing fixed-array lowering, artifacts, and presets remain
the rollback boundary.
