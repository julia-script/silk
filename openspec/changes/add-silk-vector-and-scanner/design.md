## Context

See proposal.md — Why. The substrate from `add-self-contained-owned-allocation` is complete and
gated; probes against the current compiler establish the exact frontier:

- Generic structs, generic fields, `RawBuffer.from<T>` under a type parameter, and *concrete*
  conformance instantiations (`impl Drop for Vector<I32>`) already work with zero diagnostics.
- The parametric form `impl<T> Drop for Vector<T>` fails in the parser (no type-parameter slot on
  `ImplDeclaration`) and in the declaration index (`SEM0083` demands concrete nominal types).
- No standard-library mechanism exists; all `.silk` files are test fixtures. Module closure
  currently resolves only modules present in the compilation request.
- Scanner input is already expressible: `&[U8]` runtime slices shipped in
  `add-lexical-runtime-slices`.

Constraints carried forward: no compiler-known collections, no allocator privilege, no iterator
abstraction, deterministic artifacts, three-engine differential parity, evaluator as semantic
oracle.

## Goals / Non-Goals

**Goals:**

- The smallest parametric-conformance feature that lets one `impl<T>` declaration serve every
  reachable instantiation of one generic nominal target.
- The smallest stdlib mechanism that makes `silk.vector` (name illustrative) importable without
  vendoring, deterministic, and privilege-free.
- `Vector<T>` and the scanner as pure Silk source, compiled by the existing pipeline unchanged
  below the declaration index except for parameter substitution in monomorphization.

**Non-Goals:**

- Where-clauses, capability bounds on impl parameters, overlapping-impl resolution, coherence
  rules beyond exact-duplicate rejection. One generic target type, parameters used positionally.
- Package management, versioning, a user-extensible library path, or a prelude of implicit names.
  Importing stdlib modules stays explicit.
- Bulk byte-memory primitives (memcpy-shaped ops). Growth moves elements one slot at a time; if
  the scanner workload proves that unacceptable, that evidence feeds a *future* change.
- Iterators, slices over vectors, or any API surface beyond create/append/get/length/capacity/
  move/drop.

## Decisions

### D1: Parametric conformances substitute at monomorphization, not a new dispatch layer

The declaration index records the conformance with bound parameters; Instances already keys every
generic reachability question with normalized concrete arguments, so parameter substitution rides
the existing worklist. Alternative — instantiation-time impl synthesis (textual expansion) — was
rejected: it would duplicate diagnostics per instantiation and break canonical artifact identity.
The Copy-cannot-Drop check moves from header time to instantiation time for parametric impls only;
concrete impls keep header-time rejection (probe-verified today's behavior).

### D2: Stdlib ships as source compiled into the closure, not as a prebuilt artifact

The compiler embeds the stdlib `.silk` sources and module closure adds them to resolution when
imported. Compiling from source keeps the no-privilege claim checkable (same pipeline, same
diagnostics) and keeps artifact determinism a plain byte question. Alternative — precompiled
HIR/MIR artifacts — rejected for the bootstrap: it would create a second trust root and a version
skew surface before there is any compile-time problem to solve. Library module identity gets a
reserved namespace prefix so user modules cannot collide silently (spec requires the collision
diagnostic).

### D3: Vector layout is an ordinary generic struct

`Vector<T>` = `{ storage: RawBuffer<T> | None-like state, length: Usize, capacity: Usize }` in
whatever shape ordinary Silk types allow — the design constraint is only that the empty state
allocates nothing and the struct is affine via its `Allocation`-bearing field. Growth policy:
capacity 0 → 4 → ×2 (geometric, spec-visible only as "amortized geometric"). Checked access reuses
the fixed-array checked-index contract.

### D4: The scanner is the acceptance program, not a compiler component

It tokenizes a small fixed grammar (enough to force growth and produce `Vector<Token>`) and lives
with the other acceptance fixtures. It deliberately does not replace the TypeScript lexer;
self-hosting substitution is a Later roadmap item.

### D5: Failure-ordinal sweeps extend the existing harness

The dispatch-change harness already injects `OutOfMemory` by allocation ordinal. The scanner gate
reuses it verbatim over a larger ordinal range; no new injection mechanism.

## Risks / Trade-offs

- [Parametric substitution surfaces latent assumptions that conformance targets are concrete
  (witness lookup, `callTargets`, Lower's witness dispatch)] → the probe-passing concrete-impl path
  stays untouched; parametric impls normalize to the same concrete witness form *before* Lower, so
  downstream phases never see an unsubstituted parameter. Any place that still does is a bug found
  by the differential gates.
- [Stdlib determinism obligations grow every future artifact check] → scope stdlib to the vector
  module now; the determinism gate cost is one module.
- [Element-at-a-time growth may be slow enough to tempt a primitive mid-change] → explicitly out of
  scope; the boundary in the proposal makes adding one a new proposal, not a task.
- [Deferring Copy-check to instantiation could let a never-instantiated absurd impl pass silently]
  → accepted: uninstantiated generic code is unchecked in exactly the same way uninstantiated
  generic functions already are.

## Implementation findings (running log)

Probing during apply surfaced substrate gaps the archived specs promised but never implemented,
plus expressiveness gaps Vector needs. State as of the last checkpoint:

- **Fixed and shipped**: user `Drop` hooks never executed (bodies were never elaborated); nested
  cleanup plans released nothing below the top level in any engine; automatic cleanup inside
  deferred effect bodies emitted no Drops at all. All three now work on all three engines
  (`DropHookExecution.test.ts`).
- **Added and shipped**: `Place.replace` atomic place swap (`PlaceReplace.test.ts`) and unsafe
  `Slot.copy` for Copy elements (`SlotCopy.test.ts`) — the machinery an `&mut self` Vector needs
  because unions cannot be projected through references.
- **Open gap blocking Vector growth**: match patterns bind member *fields* only; there is no
  whole-member binding (`Layout value => move value`). A bare `Layout` therefore cannot be
  extracted from `Layout.repeat`'s `Layout | LayoutOverflow` result, and `Layout` cannot be
  reconstructed from its fields — so runtime-count allocation is inexpressible in Silk today.
  The planned fix is whole-member match bindings (pattern grammar through elaboration, match
  analysis, and all three engines); it also removes the rewrap dance `get`/`append` would need
  for `Vector`'s `Empty | Full<T>` storage union.
- Match arms are single expressions (no statement blocks), so Vector helpers thread affine
  values through call/return pairs (`Taken<T>`-style structs) rather than arm bodies.

### Checkpoint 2 findings (Vector landed)

`silk/vector` now passes three-engine growth acceptance (`VectorAcceptance.test.ts`): make, six
appends across two geometric growths with element migration, checked reads, hook-driven release,
no vector-shaped MIR. Landing it surfaced and fixed more latent machinery:

- Effect-call argument borrows (`bump(&mut counter)` then `run`) never emitted `EndLoan`; they
  now end at the run that consumes the effect, resolved through the ownership facts' end spans.
- Generic bodies leaked open type parameters into slot/raw-buffer/layout operations and stale
  union-member mappings; every consumer now substitutes at instantiation.
- `Layout.of<T>`/`RawBuffer.from<T>` layout lookups used the open parameter and silently
  stubbed whole functions (`trapFunction`); ordinary-fn `unsafe { return <runtime op> }` region
  publishing remains fragile — silk/vector binds before returning as a workaround.
- LLVM: `destinationOf` was missing every post-slice operation (slot family, layout ops,
  allocate, callables), producing cross-block SSA leaks; duplicate instruction names
  (`mut*_load`, `return_value`, reload/addr sequences) are now suffixed uniquely; address roots
  persist through mutable-root storage.

### Checkpoint 3 findings (scanner and closure gates landed)

The remaining substrate gaps were fixed rather than worked around:

- `Effect.catch` now accepts provided and stored Effect values, ends their captured loans at the
  consuming run, and preserves the vector across failed growth so source code can observe its
  original contents and capacity.
- Forwarded exclusive capability providers now write witness mutations back through nested
  requirements. The scanner reuses the allocation harness to fail each allocation ordinal, with
  equal acquire/release counts and a successful run once quota covers all three allocations.
- Effect environments and backend runner parameters represent shared and exclusive slice/reference
  captures by their semantic layouts. The evaluator, native backend, and direct Wasm backend all
  execute the same runtime-sized scanner borrow and observe token kinds `1,2,3,1,2,3,1,2,3,1`.
- Fresh-process scanner determinism covers the imported stdlib closure, HIR, ownership, instances,
  layout, MIR, evaluation trace, symbols, and both backend artifacts. Five `/labs` presets expose
  vector growth, failed growth preservation, destruction order, early drop, and the exact scanner
  acceptance source.

The implementation settled the remaining cosmetic choices: the reserved module is `silk.vector`,
and the scanner's small identifier/integer/punctuation grammar produces ten tokens, forcing two
reallocations without adding a bulk-memory primitive.
