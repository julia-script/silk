## Context

The work base is `8675336f1ee0110457682964ccd12801f0de7c97`, matching JUL-116's reviewed baseline. See proposal.md for motivation and the accepted Linear design for acceptance. References/slices currently carry access but no validity region. `DeclarationFacts.returnedBorrow` identifies exactly one direct parameter; generic/field positions reject views. Ownership represents initialization as sets of roots; projected moves are rejected and lowering erases the Move expression. Cleanup and suspension reconstruct complete-owner recipes. Module invalidation records whole-module observations rather than proving declaration-body reuse.

## Goals / Non-Goals

**Goals:** one semantic lifetime model, ordinary generic storage, independently owned projections with exact cleanup, inspectable declaration relationships, finite attributable analysis and erased runtime lifetimes. Each admitted source program must be safe before later stack layers exist.

**Non-Goals:** public dependency-source/write-history contracts, lifetime-based candidate selection, self-referential movable owners, pinning, lending item families, arbitrary nested quantifiers, general backend/representation sharing, or restoration of the removed runtime evaluator. JUL-117 owns exclusive stored fields and conservative dependent Drop; JUL-118 owns dependent Effect outcomes and suspended partial-owner support.

## Decisions

### Syntax and declaration elaboration

Use named lifetime binders and arguments in the existing angle-bracket lists: `<'data, A>`, `&'data A`, `&'data [A]`, and `&'call mut Holder<'data, A>`. `'static` is the distinguished program-valid region. Inline bounds use `<'long: 'short, A: 'short>` and existing conjunction punctuation for multiple bounds. `string<'text>` is the explicit string-view type. A quoted character remains a character literal; an apostrophe followed by an identifier without a closing character delimiter is a lifetime token. Invalid character syntax must retain a bounded diagnostic rather than swallowing subsequent declarations.

Use `for<'call> fn(&'call A) -> &'call A` for the one outer quantified expected contract. Function/operation lifetime parameters supply its equivalent invocation binder. A callable's independently retained environment uses `fn<'env>(...) -> ...`, including `for<'call> fn<'env>(&'call A) -> &'call A`. Generic lifetime parameters and ordinary parameters keep distinct argument namespaces while retaining declaration order; explicit expansion appends generated binders so existing semantic ordinals remain unchanged. Reject nested universally quantified callable signatures and inference without an expected contract. The binder can refer to already-bound surrounding parameters.

Use `Effect<'env; A ! E ? R>` for a public environment bound while retaining independent run access and exact representation facts. The first layer preserves bounds and rejects dependent success/failure channels; the third layer admits those outcomes. An omitted environment bound is elaborated in its declaration/local context and never assumed detached merely because channels are detached. An effect-function declaration can explicitly bind its retained environment with `effect<'env> fn name<...>(...) -> ...`. The environment resolves in the complete declaration binder scope. Explicit expansion names the previously inferred region without changing lifetime authority or selecting among incomparable ambient bounds.

Elaboration runs on declaration headers and referenced nominal summaries, never return bodies. Each omitted top-level borrowed input introduces an independent binder. An outer borrowed receiver, otherwise one top-level borrowed input, supplies omitted result lifetimes. Omitted nominal input arguments introduce independent binders; they do not become the wrapper's outer borrow. Each omitted field lifetime introduces its own declaration parameter. Local omissions are body-scoped inference variables. Ambiguous output relationships diagnose and offer explicit alternatives. Stable readable generated names avoid existing binders and are presentation only.

### Lifetime identity, constraints and variance

Introduce a `Lifetime` actor for static regions, declaration-relative bound parameters, body-relative inference variables and scoped rigid placeholders. Identity includes the stable declaration/binder position, never the parameter spelling, source offset or concrete loan owner. Type lifetimes and generic lifetime arguments remain distinct from ordinary value types. Preserve full semantic types through Type transformations, header resolution, HIR, callable/Effect contracts and module surfaces. Builtin strings carry a semantic lifetime, including static literals.

Generate outlives obligations from already resolved types and selected operations. Reference well-formedness implies payload outlives the access region. Shared references are covariant; exclusive references are covariant only in outer access and invariant in their target. Derive nominal variance using a finite lattice and actual recursive declaration components. Opaque or interior-mutable boundaries default invariant unless validated intrinsic representation rules justify otherwise. Type comparisons memoize canonical type pairs together with binder scope, assumptions and relevant static/target inputs. Higher-rank comparison opens the expected binder with fresh rigid placeholders and rejects escaping placeholders.

Local solving uses finite body program points with monotone propagation from uses, escapes and required cleanup. A loan separately records concrete referent place, access, ancestry, origin and retaining values. Multiple values can have one abstract lifetime without sharing a loan. Transfer moves dependent obligations; Copy retains another dependent; shared reborrow descendants keep exclusive parents restricted until all their dependents end. Diagnostic witnesses retain bounded source/retention/invalidation/later-use paths.

### Stored data, mutation and escape

Declared result lifetime occurrences relate calls to input lifetime occurrences, replacing the one-direct-source return helper. `Pair<'left, 'right>` projecting its left view retains only the left semantic region. Borrowing the wrapper's inline index retains the outer wrapper loan. Returning external data through a by-value holder is valid; returning its inline storage is invalid. Nominal/structural unions, fixed arrays, named/synthesized aggregates and generic payloads use the same rules. A constrained empty variant retains its type's bounds; only a fresh unconstrained empty value can infer freely.

Mutation checks the unchanged destination type, including invariant embedded lifetimes through `&mut`. Local constraints include future uses and cleanup, so another source is allowed when it satisfies that destination. An opaque setter may conservatively retain earlier possible loans; no public mutation summary is introduced. Capture and detachment checks consume complete types plus exact environments from this layer onward. Dependent user Drop and exclusive stored views remain diagnosed until the next layer proves them.

### Sparse move paths and initializedness

Introduce one body-owned `MovePath` forest with canonical root identities and source-accessible field, constant-index and refined-variant selectors. Necessary ancestors exist once; large fixed arrays keep only touched indices plus an inherited remainder state. A subtree's default initializedness and sparse exceptions describe definite initialized, definite missing or maybe initialized storage. Joins operate per path with reachability separately; they do not enumerate combinations of holes or variants. Shape summaries support restoration of the whole owner when every required component is definitely initialized, without expanding untouched arrays.

Moves validate initialization, visibility, overlap, dereference ancestry and every whole-value user Drop ancestor. Moving a complete Drop-bearing child from a plain outer owner is valid. Explicit moves consume Copy values too; ordinary Copy reads do not. Dynamic-index moves and holes through references remain rejected. Reads, borrows, captures, passing, return and ordinary receiver calls require the accessed subtree complete. Disjoint initialized siblings remain usable. Loop backedges use the finite join and reject repeated moves unless all returning paths restore the source.

Use the contextual spelling `match place value { ... }` for discriminant-only owned-place refinement. It reads the tag without moving or borrowing the whole payload, introduces the selected variant proof for that arm, and permits explicit field moves after guard success. Existing complete consuming and borrowed match modes retain their access meanings. Guards cannot commit a speculative move. Payload access after a join requires a new refinement; complete matches reject partial scrutinees.

The existing `drop value` statement performs place cleanup and can terminate a partial owner; it is not a generic by-value call. `drop packet` cleans its live remainder, whereas `consume(move packet)` requires completeness. A second cleanup is use after termination. `packet.field = incoming` initializes a missing subtree or replaces a live/maybe-live subtree. Projecting inside a wholly missing subtree is rejected until it receives a complete value of its unchanged type.

### MIR transfer and cleanup authority

Preserve consuming extraction through HIR and emit an owned consuming place read in MIR, extending the existing paired borrowed replacement rule without allowing borrowed holes. Publish the canonical initialization state at moves, assignments and exits. Cleanup elaboration combines shared type recipes with these body facts, including sparse array exclusions and variant-gated payloads. Conditional presence requires ordinary MIR flags only when the state is not statically known. No nominal type, runtime instance or cleanup-function family encodes a combination of holes.

Incoming evaluation precedes assignment cleanup/install; moves and writes completed during evaluation remain committed on failure. Destination cleanup, install and ownership-state update form a non-suspending commit. Propagation and explicit cleanup use each exit's exact recipe rather than reconstructing `binding.cleanup`. Native lowering and compile-time execution consume the same ownership authority and established cleanup order. Fatal traps retain their existing no-unwind contract. Suspension of partial owners diagnoses until JUL-118 transports their flags and remainder recipes in frames and proves cancellation.

### Query reuse and runtime erasure

Separate declaration semantic signatures from implementation fingerprints and record actual consumed summaries for each checked body. Header/lifetime/variance/cleanup/environment changes invalidate dependent semantic bodies. Private-body changes revalidate that body while preserving sibling/downstream checks unless they explicitly consume static body or representation facts. Alpha-renaming updates presentation but preserves semantic identity. Existing module invalidation remains useful aggregate reporting, backed by declaration query executions rather than treating a reusable label as evidence that no body ran.

Cache generic semantic bodies and abstract ownership obligations once per declared semantic/static context. Additional calls instantiate obligations; residual checks consume those facts plus selected representation/static inputs with separate accounting. Lifetimes erase from runtime arguments, layout keys, instance identity and backend symbols, while ownership verification metadata remains available. Lifetime solving has no API to discover implementations, providers, conversions or speculative static evaluations. Unsupported/exhausted inputs diagnose with the dimension and declaration; they never accept unchecked.

## Risks / Trade-offs

- Cross-layer admission holes → do not remove storage/result restrictions before dependent ownership, capture, cleanup and erasure proofs are wired together; negative tests cover abstraction boundaries.
- Double cleanup from legacy root reconstruction → replace every exit/lowering reconstruction with canonical remainder facts, and structurally verify transfers and conditional cleanup.
- Overly broad invalidation disguised by counters → inspect real query execution and include negative invalidation tests, actual recursive components and explicit static-body dependency fixtures.
- Nonlinear solver growth → expose constraints, type comparisons, region points, loans, sparse paths, joins, cleanup work and discovery initiators; benchmark dimensions independently with failing cases. Measured superlinear behavior requires an explained algorithm/domain decision before handoff.
- Test-suite growth → extend existing semantic/MIR files, share snapshots, use native corpus only for unique runtime oracles and keep growth measurements opt-in. Dedicated test-economics review measures the exact committed diff against an isolated base.

## Migration Plan

Implement and validate JUL-116 on the first `gh stack` branch; remove superseded whole-owner/one-source paths and reconcile reference, diagnostics and fixtures in that change. Stack JUL-117 and JUL-118 in dependency order with their own OpenSpec extensions and issue-scoped reviews. Use focused checks during development, required full checks at substantial milestones, and draft PRs only after committed test-economics approval. No compatibility shims or incomplete feature is presented as a completed ticket.
