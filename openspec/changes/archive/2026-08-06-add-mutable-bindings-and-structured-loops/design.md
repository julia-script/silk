## Context

See `proposal.md` for motivation. The compiler currently publishes HIR statement trees but lowers
them to a MIR basic-block CFG. Conditional diamonds already lose their explicit region identity, and
the present MIR contract says a structured backend recovers source constructs from that graph. The
direct WebAssembly backend therefore pays a reconstruction cost that the LLVM backend does not.

Loops make that asymmetry structural: a cyclic CFG is convenient for LLVM, but discarding the
compiler-known loop before the backend boundary makes WebAssembly recover nesting, exits, and branch
depths. The accepted compiler architecture remains backend-neutral and target-aware; the correction
is to preserve more semantic control information, not to put WebAssembly constructs into MIR.

The current language has immutable `let`, statement conditionals and returns, whole-value ownership,
field/index places, fixed arrays, deterministic cleanup plans, and target-planned aggregate lanes.
It has no mutable binding, write operation, repeated control, borrow value, or partial initialization.

## Goals / Non-Goals

**Goals:**

- Establish one repository-wide invariant for published compiler relationships: syntax is a tree
  (therefore a DAG), semantic and HIR references are acyclic canonical-ID graphs, and MIR control is
  a structured DAG. Cycles may appear only in backend-private lowered control.
- Represent repetition without a graph back-edge by keeping a first-class loop region with condition,
  body, repeat, exit, and following continuation semantics.
- Make mutation transactional at the language level: a place is checked, a complete replacement is
  produced, old cleanup runs, and one commit changes the root.
- Reuse the existing place-selector, ownership, cleanup, layout, and calling-shape authorities.
- Give LLVM and WebAssembly enough common structure to perform simple target-native lowering.

**Non-Goals:**

- General `goto`, irreducible control flow, labels, loop values, `for`, iterators, ranges, or a loop
  expression.
- Partial initialization, move-out followed by later repair, compound assignment, increment syntax,
  destructuring assignment, or implicit mutation.
- Introducing general borrow values; this slice enforces the exclusive-access rule directly on the
  mutable root and leaves lexical `&mut` values to their own change.
- A backend-independent linear instruction stream or a second backend-specific control IR in the
  compiler.
- Preserving the current MIR constructor API. The project is unreleased and the cleaner model wins.

## Decisions

### 1. Amend the compiler architecture from CFG interchange to DAG interchange

Wayfinder issue 06 and the main MIR specification will be amended before implementation changes to
state that compiler-published control representations are acyclic. The phrase “monomorphic,
backend-neutral control-flow graph” remains true only if “graph” is understood as the structured
control DAG, not an arbitrary basic-block CFG. Its current instructions to turn control into basic
blocks and ask WebAssembly to recover structure are superseded.

This is intentionally stronger than merely tagging loops in a CFG. If MIR retained both a canonical
loop and authoritative cyclic edges, consumers could disagree about which structure wins. There will
be one authority: the structured DAG. Backend CFG blocks and WebAssembly label depths are derived
artifacts.

Alternative considered: keep the CFG and attach optional loop metadata. Rejected because metadata
can become stale or incomplete and still leaves structured consumers with reconstruction and
validation work.

### 2. Use structured regions with terminal outcomes

Each MIR function owns a canonical entry region and an ordered table of immutable region nodes. The
closed region vocabulary is:

- an operation region containing a source-ordered list of operations and one terminal outcome;
- a conditional region containing the condition value, taken child, otherwise child, and following
  continuation;
- a loop region containing a condition child, body child, and following continuation; and
- a cleanup region containing ordered releases and one terminal outcome.

Terminal outcomes include fallthrough to a forward continuation, function return, trap, loop
`Repeat(loopId)`, and loop `Exit(loopId)`. `Repeat` and `Exit` are lexical outcomes, not traversable
graph edges: they name the enclosing loop's semantic ports. That distinction keeps the region graph
acyclic while expressing repetition exactly. Conditional joins and shared cleanup continuations may
have multiple incoming references, which is why the representation is a DAG rather than only a
tree.

Regions receive deterministic identities during source-ordered lowering. The verifier follows only
child and forward-continuation references for cycle detection and topological encoding. It separately
checks that repeat/exit outcomes name a lexical ancestor loop and that no region escapes its owning
function.

Alternative considered: nested recursive region values without identities. Rejected because stable
IDs are required for facade links, shared cleanup, diagnostics, deterministic encoding, and backend
provenance.

### 3. Keep HIR structured and acyclic before MIR

HIR gains canonical region identities for function bodies, conditional arms, and loop bodies.
`While` owns condition and body regions; `Break` and `Continue` carry the resolved enclosing loop ID
as terminal statement outcomes. HIR does not contain a successor edge back to the loop.

Existing semantic fact tables remain separate immutable tables keyed by syntax and semantic IDs.
They may cross-reference canonical entities but must not create ownership or control cycles. The
facade publishes ordered region/edge queries rather than exposing mutable node objects.

Alternative considered: keep cyclic HIR and normalize only during MIR lowering. Rejected because it
would make ownership fixed-point analysis and tooling consume a different, harder graph than the
backend boundary.

### 4. Assignment commits a complete replacement

The surface forms are `let mut name = expression` and statement-form `place = expression`. A place
uses the existing binding, field, and index selector vocabulary. Assignment is not an expression and
has no value.

Execution order is fixed:

1. resolve the root and evaluate each dynamic selector once from left to right;
2. perform every bounds and writability check;
3. evaluate the right-hand expression exactly once;
4. run the old non-Copy value's infallible replacement cleanup; and
5. install the new complete value and publish the updated root.

The old root remains initialized through steps 1–3, so a trap or blocked right-hand side cannot leave
partial state. Cleanup is infallible by the existing ownership contract, so commit cannot fail after
the old value is released. Overlapping consuming assignments such as moving from a place into itself
are rejected in this slice rather than acquiring partial-initialization semantics.

The evaluator models a successful write as a new immutable logical root value. Native and WebAssembly
may mutate backend-local storage, but observable order and cleanup must match the logical transaction.

Alternative considered: clean the destination before evaluating the right-hand side. Rejected
because a right-hand trap would leave safe code with an uninitialized place.

### 5. Ownership computes a loop-header fixed point

Ownership analysis operates over the HIR region DAG. A loop begins with the incoming binding state.
The body is analyzed using that state; every fallthrough and `continue` state is joined back into a
candidate header state until the finite state vector stabilizes. States are ordered canonical
binding-by-binding, and the worklist is deterministic.

The join is strict rather than permissive. A root must have compatible liveness and complete
initialization on every repeating path. Moving a non-Copy value on one path requires a complete
replacement before that path repeats. `break` states join at the following continuation; `return`
states remain function exits. Because the bootstrap slice has no borrow values or partial states,
the lattice is finite and convergence is bounded by the number of bindings and ownership states.

Cleanup planning attaches releases to the region exit being crossed:

- iteration locals release on fallthrough and `continue`;
- loop locals release on `break` and return;
- outer owners remain live across inner exits; and
- replacement cleanup is attached to `WritePlace`, not duplicated as a scope exit.

Alternative considered: conservatively mark any loop-touched non-Copy binding unavailable after the
loop. Rejected because it would prevent ordinary compiler worklists and scans while hiding real path
errors behind excessive rejection.

### 6. Lower structured loops without backend-shaped control

Lowering preserves source evaluation order while producing the region table. A `while` becomes one
loop region whose condition child ends in a boolean decision, whose body ends in repeat/exit/return
outcomes, and whose following continuation represents the statement after the loop. `break` lowers
to `Exit(loopId)` through ownership-selected cleanup; `continue` and body fallthrough lower to
`Repeat(loopId)` through iteration cleanup.

`WritePlace` reuses the compiler-owned selector paths, canonical lengths, logical types, layout
entries, and calling shapes established for reads. The operation records replacement cleanup and a
commit boundary so evaluator and backends cannot reorder the transaction.

Alternative considered: lower `while` directly to the existing `Branch`/`Jump` blocks and annotate
the header. Rejected because it preserves the very backend asymmetry this change is correcting.

### 7. Backends own target control conversion

The LLVM backend memoizes a backend block for each region entry and exit, emits conditional branches,
and creates its private loop back-edge for `Repeat`. Shared DAG continuations map to shared LLVM
blocks. The generated LLVM CFG may be cyclic; its shape is neither imported into MIR nor visible as
compiler semantic authority.

The WebAssembly backend recursively emits the loop region as an outer exit `block` containing an
inner `loop`. `Repeat` branches to the loop label, `Exit` branches to the outer block label, and
conditionals emit structured `if` regions. A lexical label stack derives branch depths directly from
region nesting. No relooper, dispatch loop, or general CFG structurizer is introduced.

The evaluator uses an explicit execution stack over regions and handles loop outcomes iteratively,
avoiding host recursion for repeated iterations. All three consumers use the same verified region
order and operation semantics.

Alternative considered: add a shared compiler pass producing a second structured form only for
WebAssembly. Rejected because the structured form is the more informative common representation;
LLVM flattening is simpler than general structure recovery.

### 8. Inspect and encode the DAG as an authoritative artifact

The deterministic MIR encoder prints region IDs in canonical topological order and nests or links
children by ID without backend labels. The facade exposes regions, forward edges, lexical loop
outcomes, writes, cleanup, and backend provenance as immutable arrays. `/labs` adds a control-DAG
view inside the unified registry and updates existing syntax, HIR, ownership, MIR, evaluation, and
backend rows. Visual edges always have textual equivalents and coordinated source selection.

Fresh-process gates compare the semantic facts, HIR, ownership fixed points, MIR encoding, traces,
LLVM IR/bitcode, and WAT/wasm bytes. The parity corpus must include enough mutation and early exits to
prove that the target conversions preserve the common DAG semantics.

## Risks / Trade-offs

- **[Risk] The region vocabulary accidentally becomes WebAssembly-shaped.** → Keep target labels,
  branch depths, stack typing, and nesting instructions out of MIR; require LLVM and evaluator to
  consume the same nodes without translation through WebAssembly concepts.
- **[Risk] The DAG cannot represent future irreducible control flow.** → Bootstrap deliberately has
  structured source control and no `goto`; add a new representation only when a real language feature
  requires it rather than discarding structure preemptively.
- **[Risk] Shared cleanup continuations make lexical ownership unclear.** → Give every region one
  lexical owner, allow multiple incoming forward edges only when verifier-proven states and cleanup
  obligations are identical, and encode the join explicitly.
- **[Risk] Loop fixed-point analysis diverges or depends on traversal order.** → Use a finite explicit
  ownership-state lattice, canonical binding order, deterministic worklist, and verifier limits tied
  to the number of states rather than an arbitrary iteration count.
- **[Risk] Transactional writes require temporary storage for large values.** → Treat the temporary
  as a compiler-owned logical local with normal layout and cleanup; optimize copies later only from
  measured evidence without changing commit order.
- **[Risk] Replacing MIR touches every evaluator/backend test at once.** → Land the new constructors,
  verifier, encoder, and hand-built fixtures first, then migrate lowering and each consumer behind the
  same checked boundary before deleting the old CFG vocabulary.

## Migration Plan

1. Amend Wayfinder issue 06 and the roadmap to establish the structured control DAG as the source of
   truth before code migration.
2. Introduce the region-DAG contracts, verifier, encoder, and hand-built tests, then replace the old
   cyclic block constructors without compatibility adapters.
3. Migrate existing straight-line and conditional lowering to regions and prove current corpora and
   deterministic goldens before adding loops.
4. Add mutable syntax, semantic facts, HIR writes, ownership replacement, and `WritePlace`.
5. Add loop syntax, HIR regions, fixed-point ownership, cleanup outcomes, and MIR lowering.
6. Migrate evaluator, LLVM, WebAssembly, facade, and `/labs` consumers to the DAG and delete every
   reconstruction path or old CFG assumption.
7. Run focused, repository-wide, packed-consumer, strict OpenSpec, parity, and fresh-process gates.

Rollback is a normal commit revert before release. There is no compatibility bridge: reverting must
restore the old MIR producers and all consumers together.
