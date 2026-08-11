## Context

See `proposal.md` for motivation. The stage-0 compiler currently publishes 22 source-less actors
and 627 expanded callable operations from one intrinsic catalog. Concrete scalar actors account for
608 entries; the remainder mixes layout policy, `Allocator`, `SystemAllocator`, `StandardStreams`,
raw storage, Effect machinery, and place replacement. Only seven raw-buffer and slot operations are
marked unsafe. Thirteen additional `silk/core` nominal identities and two compiler-shipped
conformances are registered directly.

Silk already has qualified calls, ordinary actor functions, source-defined Effect combinators,
generic row-preserving provision, conformance mappings, specialization, service slots in MIR, a
canonical standard-library manifest, and evaluator/native/direct-Wasm agreement. It does not have
source syntax for declaring a service contract, and conformance validation still recognizes a
closed set of compiler-sealed capability names. The active Logging and FileSystem proposals both
need a general service mechanism and must not add more privileged names.

## Goals / Non-Goals

**Goals:**

- Make every callable compiler primitive explicit under one sealed `Intrinsic` namespace.
- Make the intrinsic admission rule structural and testable rather than a naming convention.
- Let ordinary Silk source declare, implement, require, and provide services.
- Rebuild existing public abstractions in canonical source without compatibility aliases.
- Let generic numeric source APIs specialize to concrete scalar primitives with no runtime cost.
- Preserve safe-code guarantees, deterministic tooling facts, and three-engine behavior.

**Non-Goals:**

- Logger, FileSystem, String, collections beyond existing Vector, concurrency, or default providers.
- A public FFI, dynamic service registry, reflection, runtime type tags, associated types, or
  retroactive third-party conformances.
- Making every intrinsic unsafe, exposing backend instruction names, or promising the intrinsic
  catalog as a stable external ABI during alpha.
- Moving language syntax such as `move`, `run`, `fail`, matching, borrowing, or operators into a
  callable namespace.

## Decisions

### 1. Intrinsic is the only compiler-recognized callable namespace

The compiler creates one sealed namespace binding named `Intrinsic`. Every operation in the
authoritative intrinsic catalog is a member of that namespace and has a globally unique descriptive
name such as `i32Add`, `f64ToBits`, `layoutOf`, `effectResult`, `bindRequirement`,
`rawBufferRead`, or `standardStreamWrite`. Qualified lookup supplies the intrinsic identity;
ordinary actor or service spelling never does.

Primitive scalar types remain language types. Syntax remains syntax. Compiler-controlled opaque
representations may remain compiler-known types where ownership, layout, or cleanup requires it,
but their operations still cross the explicit namespace and public wrappers remain source-defined.
`Drop` and `Report` are compiler-sealed language interfaces, not callable intrinsics or runtime
services, and leave the callable catalog.

Nested scalar namespaces such as `Intrinsic.i32.add` were rejected because current qualified calls
have one actor/member boundary and the extra hierarchy adds syntax and tooling work without stronger
semantics. Retaining `i32.add` as the intrinsic spelling was rejected because a normal-looking
actor operation would still receive hidden compiler privilege.

### 2. Admission follows the minimal intrinsic rule

An intrinsic is admitted only when at least one of these is true:

- it observes a compiler-selected representation fact that source cannot derive, such as
  `layoutOf<T>`;
- it performs an irreducible scalar or memory operation;
- it establishes or consumes an ownership or cleanup fact unavailable to ordinary source;
- it is the primitive boundary of Effect execution or requirement binding;
- it crosses a private target/platform boundary; or
- it is required directly to implement language semantics.

The candidate must also have a canonical source consumer unless language semantics call it
directly. Pure validation, aggregation, formatting, generic selection, service policy, provider
construction, and convenience composition fail admission. Verification compares the catalog with
HIR operation identities, MIR primitives, evaluator branches, backend branches, and host imports.

Treating the existing catalog as grandfathered was rejected: it would preserve precisely the
abstraction leakage this change exists to remove. Exposing raw backend instructions was rejected:
intrinsics remain target-neutral semantic primitives implemented by each backend.

### 3. Service is a source declaration, not a compiler capability entry

The source form uses the established complete function contract:

```silk
pub service Allocator {
  effect fn allocate(layout: Layout)
    -> Allocation ! OutOfMemory ? &mut Allocator
}
```

A service has no fields, initialization, hidden default, or operation bodies. Its access is explicit
in its operation requirement. A provider keeps actor-oriented implementation functions and maps
them through the existing conformance form:

```silk
impl Allocator for SystemAllocator {
  allocate: SystemAllocator.allocate
}
```

The mapped actor function receives the provider value explicitly. It may use weaker access or
smaller failure and requirement rows than the service contract, but never stronger ones. The
compiler creates the same statically shaped witness and service slot for every service declaration.
`Effect.provide`, `provideMut`, and `provideWith` operate only on types, rows, and conformance facts.

Reusing `interface` for services was rejected because an ordinary interface selects static type
behavior and creates no runtime requirement or provision lifetime. Method bodies inside `service`
were rejected because they would violate actor modules and mix contracts with implementations.
A dynamic tag or registry was rejected because service identity, role, witness, and slot are all
known statically.

### 4. Concrete scalar intrinsics support source-defined generic numeric interfaces

Each concrete primitive has a concrete intrinsic contract. The standard library owns the canonical
numeric interfaces, actor functions, and conformances for language primitive scalar types:

```silk
pub interface Integer {
  fn add(left: Self, right: Self) -> Self
}

impl Integer for i32 {
  add: Intrinsic.i32Add
}
```

The canonical core standard-library module is the declaration owner for conformances of primitive
language types; other modules cannot add retroactive scalar conformances. A generic wrapper such as
`add<T: Integer>` selects the ordinary interface witness during specialization. The resulting MIR
contains the concrete primitive and no type tag, switch, service slot, or numeric registry.

A single generic `Intrinsic.add<T>` was rejected because it moves the generic mapping back into the
compiler. Keeping every public per-type actor operation compiler-defined was rejected because it
prevents the standard library from shaping a coherent generic numeric API.

### 5. Existing abstractions migrate according to one explicit matrix

- Scalar actor operations become concrete `Intrinsic` members; numeric interfaces and actor
  wrappers move to source.
- `Layout.of<T>` wraps `Intrinsic.layoutOf<T>`; layout construction and repeat validation move to
  source.
- `Allocator` becomes a source service; `SystemAllocator` becomes a source provider over primitive
  acquire/adopt/release operations.
- `StandardStreams` becomes a source service; its native provider uses
  `Intrinsic.standardStreamWrite`, while an in-memory provider uses no platform primitive.
- `RawBuffer` and `Slot` become source-facing actors over the smallest ownership and unchecked
  storage intrinsics. Bounds and reusable safe policy remain in source.
- `Effect.result` and `Effect.bindRequirement` become source wrappers over
  `Intrinsic.effectResult` and `Intrinsic.bindRequirement`; the other Effect combinators remain
  ordinary source.
- `Intrinsic.replace` remains a direct language primitive. A source wrapper would turn the caller's
  place into an ordinary borrowed parameter and cannot preserve the syntactic place identity needed
  for move-out-and-reinitialize analysis; the catalog records its consumer as language semantics,
  not a privileged `Place` actor.
- Failure and validation data such as `OutOfMemory`, `InvalidAlignment`, `LayoutOverflow`, and
  `StreamWriteFailure` move to source when their representation does not need compiler ownership.

The exact surviving primitive count is an output of applying the admission test, not a target
quota. Compatibility aliases for old spellings are forbidden during alpha.

### 6. Unsafe marks an obligation, not a namespace

Every intrinsic contract is reviewed independently. Concrete scalar operations, checked
conversions, layout queries, Effect machinery, and complete host writes remain safe when their
contracts preserve memory safety; a typed failure or trap does not make an operation unsafe.
Acquiring a raw platform block may be safe if it returns an opaque unadopted value. Adopting a block
with a release operation, unchecked initialized-slot access, raw pointer interpretation, and ABI
calls whose invariants cannot be checked require `unsafe`.

Marking the whole namespace unsafe was rejected because it conflates compiler implementation with
caller proof obligations and would spread unsafe blocks around ordinary arithmetic. Leaving current
safety flags unchanged was rejected because system allocation and host boundaries have not yet been
audited under this rule.

### 7. Source identity remains visible through lowering and tooling

Before specialization, HIR retains ordinary source calls, interface conformances, service
operations, and explicit intrinsic calls as distinct facts. Instance discovery selects concrete
interface and service witnesses. MIR contains general witness dispatch where a service remains
dynamic within a specialized program and concrete primitive operations only at the intrinsic
boundary. It never contains a standard-library-name operation merely because a wrapper was
inlined.

The analysis facade exposes source declarations and intrinsic identities separately. Completion
offers primitives only after `Intrinsic.`; normal completion favors source APIs. Hover shows
catalog signatures for intrinsics and authored docs for wrappers. Navigation follows every wrapper
to source and stops without inventing a location only on a direct intrinsic.

Generating synthetic source files for intrinsics was rejected because it would misrepresent the
compiler boundary. Hiding intrinsic identities after inlining was rejected because verification and
tooling need to explain the selected primitive.

### 8. Portable services wait for the boundary

`establish-minimal-intrinsic-boundary` archives before implementation begins on
`add-portable-logging` or `add-portable-file-system`. Those proposals may retain their desired
behavior, but their designs and tasks must depend on source-declared services and the final
intrinsic catalog. This ordering prevents Logger or FileSystem from becoming temporary
compiler-known actors that later require a second migration.

## Risks / Trade-offs

- **[The refactor touches nearly every compiler phase]** → Migrate one catalog family at a time,
  keep differential fixtures at each step, and prohibit mixed old/new spellings per family.
- **[Generic numeric wrappers regress code quality]** → Inspect specialized HIR/MIR and native/Wasm
  artifacts for direct concrete primitives and add no runtime numeric dispatch.
- **[Service syntax becomes a method system accidentally]** → Permit contracts only in `service`;
  keep implementations as mapped actor functions and reject bodies or stored fields.
- **[The standard library receives hidden privilege]** → Limit its special ownership to canonical
  conformances for closed language primitive types; copied wrapper implementations remain ordinary.
- **[Unsafe expands without evidence]** → Require an invariant statement and negative safe-code
  test for every unsafe intrinsic; keep traps and typed failures safe.
- **[Target-specific host behavior leaks upward]** → Specify target-neutral intrinsic contracts and
  keep concrete ABI symbols private and compiler-versioned.
- **[Tooling loses navigation while wrappers move]** → Make source declaration identity a required
  acceptance result before removing each old intrinsic spelling.

## Migration Plan

1. Add service syntax, declaration facts, general conformance checking, and witness/provision tests
   without migrating existing services.
2. Introduce the sealed `Intrinsic` namespace, catalog verification, and presentation/tooling
   support while preserving behavior under new spellings in focused fixtures.
3. Move scalar primitives, add canonical numeric interfaces and conformances, then migrate
   operators, examples, and pressure programs.
4. Move Effect and place primitives behind source wrappers and remove their old privileged actor
   entries.
5. Move layout, allocation, RawBuffer, and Slot policy to source over the audited storage
   primitives; verify ownership and cleanup before removing old entries.
6. Move StandardStreams and its providers to source over the complete-write primitive; verify
   evaluator, native, and direct-Wasm parity.
7. Remove every old intrinsic actor spelling and compiler-sealed service branch, run the catalog
   audit, update all docs and labs, and run full release-candidate validation.
8. Unblock the Logging and FileSystem changes and revise their source API designs against the
   archived service and intrinsic contracts.

Rollback is by family before old spellings are removed. After the final breaking removal, rollback
reverts the complete change; no aliases or dual catalog remain.
