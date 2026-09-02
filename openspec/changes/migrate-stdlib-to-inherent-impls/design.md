## Context

See proposal.md. After `add-inherent-impl-members` the declared member set wins and the legacy
projection is consulted only when a spelling has no declared member. This change makes the
`Missing` branch final.

Inventory at the time of writing: about 40 standard-library modules expose operations through a
basename-matching nominal or a zero-data owner, roughly 230 `pub fn` plus 30 `pub effect fn` in
`effect.silk`; 93 files contain selective imports of former root operations, dominated by
`vector` (193 member imports) and `option` (97, mostly `none`/`some`); the reference has 273
`Owner.operation(` calls, which survive migration unchanged; five documentation pages declare a
filename-matching type in a fence and no non-stdlib `.silk` file does. Thirty conformance
mappings `name: Owner.fn` in the provider modules target private root functions through the
projection. About 230 `[`name`]` documentation links point at migrating functions. `Effect` is an
ordinary `pub struct Effect {}` bound by `import silk.effect { Effect }`; the intrinsic-qualifier
branches fire only for a namespace import and are unaffected. Modules with no owner nominal
(`bool`, `char`, `reflect`, `static_sequence`, `static_text`, `target`) are reached through
namespace aliases today and do not migrate; nor do the primitive modules.

## Goals / Non-Goals

**Goals:**

- One mechanism for type-qualified lookup at the end of the change.
- Source moves are mechanical: indent into an impl, drop owner binders from the function, replace
  the explicit owner type in parameter zero with `Self` where it is the whole type.

**Non-Goals:**

- Renaming operations, changing signatures, or reorganizing modules beyond the move.
- Deciding per-function whether something "should" be a method: everything that is qualified
  through the owner today becomes a member; free helpers stay free.
- Receiver-syntax adoption in stdlib bodies or docs; that is `add-method-call-syntax`'s
  documentation task, and stdlib bodies keep explicit calls.

## Decisions

### Migration rule is mechanical, decided by current qualification

An operation becomes an inherent member of owner `O` if and only if callers reach it as `O.name`
today, whether public or through a same-module conformance mapping (`readFile:
OsFileSystem.readFile` targets a private root function; it becomes a private member of
`OsFileSystem`). Private helpers and public functions not reached through an owner stay root
declarations. Owner binders move to the impl head; a function's own remaining binders stay on the
function; parameter zero typed exactly `O<...owner binders>` becomes `Self`. A `self` parameter of
another type stays spelled as it is and the member is an associated function: every `Effect`
operation (`self: once Effect<A ! E ? R>`), `Order.isLess(ordering: &Ordering)`, and
`Order.compare<T: Order>(left: T, right: T)` migrate into their owner's impl unchanged in
signature and in public spelling. Bodies are unchanged except that unqualified sibling calls
(`some<U>(...)` inside `map`) become `Owner.some<U>(...)`, because an inherent member is not in the
module's root scope; `format.silk` alone has about 75 such sites and gets its own task.

Several modules declare more than one nominal. A function joins the owner that callers select
as its qualifier today, even when its parameter zero is another nominal of the module; that keeps
every public spelling stable and the migration mechanical. Namespace-alias call sites that reach
such a function (`import silk.system_clock as SystemClock` then `SystemClock.make(...)`) switch to
the selected owner import, because a namespace exposes only root declarations afterwards.

| module                          | owner receiving the functions                                                        | note                                                                                                                                                                                             |
| ------------------------------- | ------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `system_clock`                  | `service SystemClock`                                                                | `make`/`seconds`/`nanoseconds` take `Instant`; no `Instant.` call site exists                                                                                                                    |
| `monotonic_clock`               | `service MonotonicClock`                                                             |                                                                                                                                                                                                  |
| `logger`                        | `service Logger`                                                                     | `length`/`levelAt`/... take `&InMemoryLogger` and stay `Logger.*`                                                                                                                                |
| `hash`                          | `struct Hash`                                                                        | `seed`/`mix`/`combine` operate on `HashSeed` and stay `Hash.*`                                                                                                                                   |
| `insecure_seed`                 | `service InsecureSeed`                                                               | `first`/`second` take `&Seed` and stay `InsecureSeed.*`                                                                                                                                          |
| `writer`, `format`, `allocator` | the basename nominal                                                                 |                                                                                                                                                                                                  |
| `order`                         | `interface Order`                                                                    | `isLess`/`isEqual`/`isGreater` take `&Ordering`; `compare`/`less`/`equal` take `T: Order`; all stay `Order.*` as a deliberate spelling-stability exception to the STYLE-002 rewrite, noted there |
| `filesystem`                    | `struct Path` for parameter-zero `Path` functions, `service FileSystem` for the rest | the only module whose call spellings change; callers that imported `make`/`root` selectively are rewritten to `Path.make`/`Path.root`                                                            |

Alternative: a scripted rewrite. Worth doing for the indentation and header move, but each module
is reviewed by hand because the sibling-call rewrite and `Self` substitution need the type.

### Delete the projection, do not deprecate it

`scopedModule` and every caller are removed. The `Missing` branch of `lookupAssociated` reports
the unknown associated member diagnostic. The service and interface fallbacks that previously
searched the declaring module are gone with it; contract operations and inherent members are the
only associated items of a contract. A bare contract-operation call with no bound in scope
(`HashKey.hash(&value)` outside a generic body) resolves nothing today, because no root function
of that name exists, and keeps reporting the unknown associated member; the applied
`HashKey<...>.hash(...)` form remains the concrete-provider spelling. This matches the greenfield
policy: no compatibility shim, no deprecation diagnostic.

### Type paths through a selected type import are rewritten to direct imports

`import silk.format { Format }` followed by `Format.ParseError` reaches a root struct through the
projection today, in about twenty documentation pages and several stdlib doc fences compiled by
the doctest. After the cut a selected type exposes only its associated items, so every such type
path becomes a direct selection (`import silk.format { Format, ParseError }` then `ParseError`).

### The source move is one cut

`option` is a leaf of the import graph but a root of the reverse graph: the moment its operations
become members, every primitive module and most collection modules that write
`import silk.option { Option, none, some }` stop compiling. The migration therefore lands as one
atomic change to stdlib sources and their importers, with the gate verified once at the end; the
per-module ordering below is a reading order for review, not a sequence of green states.

### Zero-data owners stay; alias-namespace modules do not migrate

Modules whose qualifier is a zero-data struct (`RawBuffer`, `Unicode`, `Format`, `Metrics`,
`Hash`, `Numeric`, `Effect`, ...) keep that struct as the owner and gain an `impl`. The struct's
role changes from "scope actor" to "owner" with no source-level difference other than the impl
block. `static fn` members join an impl like any function. `const` declarations stay root
declarations. Modules with no owner nominal are reached through namespace aliases and are
untouched.

### Selective and namespace imports of former root functions are rewritten to qualified calls

`import silk.option { Option, none, some }` becomes `import silk.option { Option }` and each
`none<T>()` becomes `Option.none<T>()`. Where a site aliased a function to disambiguate
(`make as vectorMake`), the alias disappears because the owner qualifier disambiguates. A
namespace import used to reach members (`import silk.option as Option` then `Option.some`)
becomes the selected owner import, because a namespace exposes only root declarations. No
re-export of members through the module is added.

### Documentation states the model, not the mechanism

`NAME-005` is replaced by a rule that a nominal qualifier exposes associated items only.
`STYLE-002` becomes: operations intrinsic to one nominal type are declared in its inherent impl
with the receiver first so direct, section, and pipeline forms share one contract; operations over
several peer types remain free functions. `STYLE-003` becomes: examples import the owner type and
qualify operations through it; import a namespace when the module itself is the subject. The
receiver form is added to these pages by `add-method-call-syntax`.

### Collisions and specializations found by the cut

- A public function that only delegated to a same-named service operation
  (`pub effect fn now() { return run MonotonicClock.now() }`) is deleted: the operation is the
  member, and `MonotonicClock.now()` already resolved to it.
- A private field that shares a member's name is renamed (`Vector.count`, `Vector.limit`,
  `HashMap.count`, `HashSet.count`); the member keeps the public spelling.
- `Vector.appendBytes(self: &mut Vector<u8>, ...)` mentions no owner binder and specializes the
  receiver, so it stays a root function reached by selective import.
- Members that refine an owner binder's bounds keep the refinement in their own list
  (`fn get<K: HashKey + Copy, V: Copy>`), which `add-inherent-impl-members` admits as a refinement
  of the head binder rather than a duplicate.
- The Unicode-tables generator emits the impl form, since its check compares the shipped module
  byte for byte.

### Compiler rules the cut required

The migrated modules exercised the associated-member model in ways the first change did not, and
each became a rule with its spec delta in this change:

- **Mentioned owner binders.** A member's generic sequence carries only the owner binders it
  mentions (all of them when it names `Self`). `Fiber.cancel(canceller)` mentions neither `A` nor
  `E` and is callable without an instantiation; `Vector.appendBytes` mentions none and specializes
  the receiver, so it stays a root function instead.
- **Refined owner binders.** A member may redeclare an owner binder with stronger bounds
  (`HashMap.get<K: HashKey + Copy, V: Copy>`); the refinement keeps the head's binder identity, so
  it is neither a duplicate nor a second binder.
- **Member-minted binders.** Each member's binders are minted under the member's identity, so
  member-to-member calls inside one impl infer `?R` and friends per call instead of sharing one
  binder across the block.
- **Local owner wins.** A head whose spelling matches a declaration of its module names that
  declaration even when a builtin storage type shares the spelling (`Slot`, `RawBuffer`); the
  resolver's fact is kept for the head's argument facts so tooling still indexes them.
- **Rejected heads still close `Self`.** A head rejected at collection resolves its owner so its
  members report nothing about `Self`; the head's diagnostic is the only one.
- **`Self` closes only what mentions it.** Closing `Self` on a member rewrites only the facts that
  name `Self`; a row or type that never mentions it is kept as is, so representation binders such
  as `?R` survive the rewrite instead of being flattened.
- **A bare variant spelling defers to the members.** `Option.some(...)` no longer reports a
  misspelled variant; a bare union qualifier that names no variant defers to the associated lookup,
  which finds the member.
- **Shadowed builtin diagnostics are dropped.** When the resolver answered with the builtin that
  shares the local owner's spelling, its diagnostics describe the wrong type and are discarded;
  when it answered with a nominal, they stand.
- **Constructor fold through a parameter drop.** Deleting the service-operation wrappers makes a
  direct `MonotonicClock.now()` in a provided instance call the provider's witness operation
  directly. That operation's outer function constructs the effect and then drops its borrowed
  `self` in a cleanup region, a shape the MIR constructor fold rejected as `ComplexConstructor`;
  the wrapper had hidden it behind a parameterless function. The fold now accepts a construction
  that forwards to one cleanup region dropping only effect-free parameters and reports that shape
  as the `TrivialCleanup` guard, so the run stays a direct static run and `LocalScheduler.execute`
  keeps no coroutine frame of its own. Without the
  fold the frame's payload holds an execution handle, and the LLVM backend's inline frame-cleanup
  expansion recurses without bound (a latent backend bug filed separately).
- **Section runs identify their runner.** With the clock dispatched directly, `execute`'s provided
  instance is materialized on its own (its dispatch specializes), and every run inside it is
  classified. A run piped into an erased `Effect.provideMut` section is one recorded call of that
  section's declaration, so the provisional classifier now identifies it like a direct call instead
  of reporting the whole instance unknown; the deleted wrappers had kept `execute` out of the
  provided worklist entirely. A service operation provided through such a section is still not
  seen by the provider scan (pre-existing, filed separately).

## Risks / Trade-offs

- [A stdlib body calls a sibling unqualified and breaks after the move] → the compiler reports
  every such site as an unknown name; the rewrite is `Self.` or `Owner.`; the full gate covers all
  shipped modules.
- [Docs snippets that declared a basename-matching type silently relied on the projection] → the
  docs snippet tests compile every fence; each failure is a missing `impl` block in the example
  (five pages: effects, stdlib/effect, tutorial, functions-callables-and-control-flow, style-guide).
- [Documentation links to migrated functions silently degrade] → `add-inherent-impl-members`
  makes `docgen` resolve member links; this change verifies the regenerated stdlib reference keeps
  its link count.
- [Style-guide anchors are cited from other pages] → `documentation-style-guide.md` and
  `generics-interfaces-and-specialization.md` link STYLE-002/003 by heading anchor and are updated
  with the headings.
- [Deleting `scopedModule` changes diagnostics codes at existing sites] → SEM0010 remains the
  unknown-associated-member code; the diagnostic index is regenerated and the tests updated.
- [Migration PR is large and cannot be staged green] → review order is leaf-first (`option`,
  `result`, `order`, then collections, then `effect`, then providers); the gate runs once on the
  complete cut, and per-module compile checks use the compiler's analysis on each module's own
  doctest examples.
- [A wrong owner choice still compiles] → the owner table above is the acceptance criterion; a
  reviewer checks each multi-nominal module against it.
