# Roadmap — Silk Effect

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-07 · Review cadence: after each OpenSpec archive, or monthly when no
> change ships · Scope: whole project

## Vision

Silk Effect will be a low-level systems language that combines explicit memory and execution
control with typed failures, explicit service requirements, deterministic resource scopes, and
tooling-friendly semantics. The first destination is the smallest coherent language capable of
compiling its own compiler; broader language and ecosystem work follows evidence from that
self-hosting core.

**Current objective:** establish the smallest memory boundary that lets a real compiler pass
consume source-dependent input and produce source-dependent output. The accepted algorithmic
baseline already composes target-aware layouts, cross-module declarations, operators, nominal
structs and values, fixed-size arrays, mutation, structured loops, normalized structural unions,
exhaustive matching, and lexical runtime-sized borrowed input through the inspectable,
determinism-gated compiler architecture. Owned growable output is the next missing boundary.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Validate the language's defining effect execution model

- **Problem:** The first executable typed-flow baseline proves lazy calls, failure propagation,
  recovery, ownership, cleanup, and backend parity, but it also commits several Wayfinder-era
  surface decisions before they have survived enough real programs: `flow fn` versus a first-class
  lazy `effect {}` scope, `Flow` versus `Effect`, and unconditional `fail move` even for Copy or
  freshly constructed payloads. These choices define the language more than the allocation API that
  currently depends on them.
- **Outcome & done-when:** A broad, executable example corpus covers pure construction, eager setup
  around lazy work, generic success, one and several failures, exact and residual recovery, borrowed
  and moved captures, an effect discarded without running, repeated execution, loops, scoped
  allocation, and cleanup. A reviewed OpenSpec then chooses the eager-to-lazy boundary, source type
  and block syntax, representation constraints, terminology, and ordinary ownership rule for
  failure transfer without weakening the proven compiler/backend contract.
- **Status:** in discovery — the implemented `add-flow-functions-and-typed-failures` change is an
  evidence-producing baseline, not a syntax freeze. Capability roles, scopes, and allocation remain
  behind this review.
- **Appetite:** one focused design cycle and adversarial review before the next execution-substrate
  proposal; prefer changing the unreleased surface now over preserving a doubtful abstraction.
- **Links:** change: `add-flow-functions-and-typed-failures` ·
  [effect model decision](../wayfinder/bootstrap-language/issues/03-effect-system-and-typed-failures.md) ·
  [bootstrap syntax](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md)

### Widen the language, slice 1: bindings, arithmetic, branching

**Status: complete (2026-08-05).** All three changes shipped and archived; see the changelog.

- **Problem:** The realigned spine (diagnostics through native link, all 13 changes archived
  2026-08-05) runs end to end over a grammar too small to exercise it: ownership is trivially
  satisfiable, lowering never emits `Branch` or `Drop`, and the differential harness compares only
  straight-line integer programs.
- **Outcome & done-when:** Silk programs with `let` bindings and `move`, signed literals with
  arithmetic, and `Bool`/comparison/`if` compile through every phase — real liveness ranges and
  cleanup drops in the ownership and MIR labs, real CFG diamonds, interpreter and native agreeing
  across both branch arms — with every artifact encoder and golden extended, not replaced.
- **Status:** shaped — dependency order pinned by the map: bindings first (the pressure issue 01
  deferred to issue 08's syntax), then arithmetic, then branching. Includes threading the `Backend`
  service through `Driver.CompileRequest` and `Analysis.codegen` instead of the current hardwired
  `LlvmBackend` call, so the declared seam is actually exercised.
- **Appetite:** three or four focused changes, one per feature, same
  propose → implement → inspect → archive loop as the realignment; structs, unions, match,
  failures, generics, and cross-module calls stay outside this slice.
- **Links:** [bootstrap-language map](../wayfinder/bootstrap-language/map.md) ·
  [ownership decision](../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md) ·
  [syntax decision](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md) ·
  [realignment record](compiler-realignment.md)

### Make the language algorithmic: modules, operators, data, and loops

**Status: complete (2026-08-06).** All nine changes shipped and archived. Compiler-published
control remains a DAG until each backend converts it to its required target form.

- **Problem:** The compiler spine is complete, but the language can only express small scalar,
  call, binding, and branching examples. It lacks a settled operator surface, aggregate data,
  indexed inline storage, mutation, and loops, so it cannot yet express even compact algorithms
  over compiler-shaped values.
- **Outcome & done-when:** Silk programs resolve declarations across modules and define, construct,
  and project nominal structs; compute through a coherent operator model; store values in checked
  fixed-size arrays; and use mutable bindings with structured loops. The same foundation then grows
  normalized structural unions and exhaustive mode-aware matching. Every capability runs through
  layout, ownership, MIR, interpretation, native and WebAssembly emission, and the inspector labs.
- **Sequence:**
  1. `standardize-target-aware-layouts` — complete and archived
  2. `resolve-cross-module-declarations` — complete and archived
  3. `standardize-expression-and-operator-semantics` — complete and archived
  4. `declare-nominal-struct-types` — complete and archived
  5. `construct-and-project-struct-values` — complete and archived
  6. `add-fixed-size-arrays-and-indexing` — complete and archived
  7. `add-mutable-bindings-and-structured-loops` — complete and archived
  8. `normalize-structural-unions` — complete and archived
  9. `match-exhaustively` — complete and archived
- **Dependencies:** cross-module resolution precedes operator desugaring into canonical actor
  operations and all cross-module data use. Struct declarations precede construction. Arrays build
  on aggregate layout and the expression/operator foundation; the first useful loop slice adds
  `let mut`, assignment, `while`, `break`, and `continue` over checked array access. Construction
  and layout precede unions, and unions precede match.
- **Appetite:** one focused OpenSpec change at a time, followed by implementation, inspection,
  archive, and reassessment before proposing the next ticket.
- **Links:** [type and value decision](../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md) ·
  [module decision](../wayfinder/bootstrap-language/issues/04-modules-visibility-and-name-resolution.md) ·
  [pipeline decision](../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md) ·
  [syntax decision](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md)

### Borrow compiler input whose size is known only at runtime

**Status: complete (2026-08-06).** `add-lexical-runtime-slices` shipped the first memory boundary.

- **Outcome:** Explicit call-scoped `&[T]` and `&mut [T]` consume fixed arrays of different lengths
  through one monomorphized contract. Root-attached loans remain compiler facts; evaluator cells,
  aligned LLVM storage, and private Wasm shadow frames realize the same target-aware address-plus-
  length layout. Shared traversal and exclusive move-only replacement agree across all three
  engines and the unified `/labs` inspector.
- **Deliberate boundary:** No allocator, owned dynamic sequence, raw pointer source API, implicit
  array decay, escaping or stored slice, range, subslice, or iterator behavior was admitted.
- **Evidence:** The coverage fold returns `40` and `42` from three- and six-element arrays through
  one slice-taking instance and symbol; the exclusive fixture observes caller-visible replacement
  with exactly-once displaced cleanup.

## Next

### Establish the execution substrate required by explicit allocation

- **Problem:** The accepted allocation model is not an isolated memory primitive. `OutOfMemory`
  requires executable flow failures; allocator access requires capability-role service slots and
  origin-bound witness dispatch; named allocation lifetime requires `Scope.scoped`; and permanent
  layout arithmetic requires `Usize`. None of those source-visible foundations is executable yet.
- **Outcome & done-when:** Land the general language mechanisms without allocator-specific syntax,
  ambient callbacks, backend-owned ABI choices, or temporary `I32` allocation arithmetic. Each
  change runs through syntax, semantic facts, ownership, HIR/MIR, target layout, evaluator, native,
  Wasm, determinism, and unified inspection before the next begins.
- **Sequence:**
  1. ✅ `add-usize-scalar` — archived 2026-08-07
  2. ✅ `add-flow-functions-and-typed-failures` — archived 2026-08-07 as the executable baseline
  3. Review and settle the effect execution model and source surface
  4. `add-capability-requirements-roles-and-provision`
  5. `add-named-scope-wrappers-and-cleanup`
  6. `add-scoped-allocation-primitives`
- **Evidence for the split:** Adversarial minimality, ownership, and backend review found that doing
  allocation first would either hard-code the allocator into the compiler or silently implement all
  four foundations inside one oversized memory ticket. The same review replaced impossible static
  runtime-slot typestate with unsafe lexical `Slot<T>` places plus a restricted drop hook, allowing
  `Vector<T>` to remain entirely Silk-written without a bitmap or compiler-known collection.

### Allocate owned compiler output in a deterministic scope

- **Problem:** Slices now borrow source-dependent input, but a lexer still cannot own a token list
  whose length emerges at runtime. Safe values must stay fully initialized, cleanup must be
  deterministic on every structured outcome, and allocation must not become hidden compiler magic.
- **Outcome & done-when:** After the four general prerequisites, add the smallest explicit unsafe
  allocation boundary: validated target-aware `Layout` and `SlotLayout<T>`, an affine byte-owning
  allocation carrying its originating reclaim capability, unsafe lexical `Slot<T>` places, named
  destination scopes, typed `OutOfMemory`, and restricted infallible drop hooks. Evaluator, LLVM,
  Wasm, ownership, MIR, and `/labs` must agree on acquisition and exactly-once cleanup.
- **Boundary:** No safe raw allocation, ambient/default-static allocator, user-callable `free`,
  primitive resize, zero-fill promise, collection behavior, runtime initialization bitmap, general
  capturing finalizer, arena policy, or allocation-metrics surface. Runtime-indexed initializedness
  and aliasing remain explicit unsafe invariants. The allocator is an explicit requirement; a future
  Silk-written `Vector<T>` consumes these primitives and uses its restricted hook to drop its
  initialized prefix before byte release rather than being compiler-known.
- **Sequence after this change:** implement `Vector<T>` in Silk, prove a scanner with borrowed bytes
  and owned tokens, then admit only the bulk byte-memory primitives that workload demonstrates.
- **Links:** [ownership and scoped allocation](../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md) ·
  [types and values](../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md) ·
  [compiler pipeline](../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md) ·
  [bootstrap syntax](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md)

## Later

- **Supply the compiler's native platform** — add the minimum runtime, standard library, host
  services, and private C shim in response to real self-hosting compiler needs.
- **Make Silk capable of expressing its own compiler** — progressively replace the TypeScript seed
  implementation with Silk modules while preserving reference equivalence; revisit the exact port
  boundaries once the frontend, MIR, and runtime interfaces have survived real use.
- **Prove native self-hosting** — produce stage 1, stage 2, and a byte-identical fixed-point rebuild
  with complete conformance, failure, debug, resource, and performance evidence on all required
  native hosts.
- **Grow beyond the bootstrap subset** — concurrency, networking, schemas, observability, richer
  tooling, and broader standard-library families become candidates only after the self-hosting core
  exposes their real constraints.
- **Deepen WebAssembly integration** — extend the direct backend toward host integration and
  generated Effect interop after the native bootstrap path is accepted.

## Maintenance budget

Reserve approximately 20% of project capacity for keeping the foundation trustworthy.

- Keep `@silk-effect/llvm` aligned with its pinned upstream baseline, deterministic fixtures, Effect
  architecture rules, and packed release-candidate checks.
- Keep OpenSpec capabilities, archived changes, the Wayfinder decision index, and this roadmap
  synchronized with implementation discoveries instead of allowing multiple competing truths.

## Not doing

- General concurrency, atomics, async scheduling, networking, or a broad user-facing FFI during
  bootstrap — the compiler workload does not require them.
- WebAssembly-hosted self-hosting as a prerequisite — native fixed-point acceptance remains the
  bootstrap gate even though direct WebAssembly emission is maintained in differential parity.
- A package registry, dependency solver, production build system, or full language server — none is
  required to prove the bootstrap language.
- A general incremental query engine — immutable analysis snapshots and localized deterministic
  worklists are the accepted bootstrap architecture.
- Backward compatibility for unreleased compiler APIs — early implementation evidence should be
  allowed to improve boundaries rather than fossilize them.

## Open questions

- Is the primitive lazy boundary an `effect {}` expression with `effect fn` sugar, a distinct
  function kind, or another form that keeps eager construction and lazy imperative execution
  explicit?
- Should the defining computation type be named `Effect` rather than `Flow`, and should `fail`
  follow ordinary Copy-versus-move rules instead of requiring `move` for every payload?
- What executable name and public analysis facade should eventually accompany
  `@silk-effect/compiler`?
- Should Silk compiler modules replace their TypeScript counterparts continuously as capabilities
  land, or should the first port begin after the stage-0 subset is feature-complete?
- Which smallest real pass best distinguishes the required memory semantics: byte-to-token
  lexing, token-to-syntax grouping, or another source-dependent transform?
- Can one canonical runtime-sized representation serve both borrowed input and owned output, or do
  their ownership and allocation duties require distinct actors from the first slice?

## Changelog

- 2026-08-07: Shipped and archived the first executable typed-flow baseline, then promoted a full
  effect-model review ahead of capability roles, scopes, and allocation. The baseline proves lazy
  construction, typed failure outcomes, exact recovery, cleanup, compiler-owned target layout,
  structured DAG lowering, evaluator/native/Wasm parity, determinism, and unified Labs inspection;
  its `flow fn`, `Flow`, and `fail move` spellings remain deliberately open to replacement after a
  broad scenario corpus and adversarial review.
- 2026-08-06: The composed remaining-member acceptance fold established the first algorithmic
  baseline across module resolution, all compiler representations, logical evaluation, native LLVM,
  direct WebAssembly, fresh-process determinism, and the unified `/labs` workbench. The program
  succeeds by fixing cardinality in `Array<T, N>`; promoted runtime-sized compiler input and output
  to Next as the first demonstrated memory boundary, without freezing its syntax or feature
  sequence.
- 2026-08-06: Shipped and archived `match-exhaustively`, completing the nine-change algorithmic
  language slice. Precise and structural-union values now support bare, move, shared, and exclusive
  exhaustive matching with guarded and nested destructuring, canonical result joins, selected-path
  ownership and cleanup, structured DAG HIR/MIR, logical evaluation, native and direct WebAssembly
  parity, and coordinated inspection in `/labs`. No successor is promoted until a real
  compiler-shaped program exposes the next smallest gap.
- 2026-08-06: Shipped and archived `normalize-structural-unions`. Structural unions now have
  canonical normalized identities, compiler-owned target-aware sum layouts and calling shapes,
  explicit widening, ownership and cleanup semantics, evaluator/native/WebAssembly parity, and
  unified inspector coverage. Next: `match-exhaustively`.
- 2026-08-06: Shipped and archived `add-mutable-bindings-and-structured-loops`. Silk now has explicit
  `let mut`, transactional whole-value assignment through binding/field/index places, structured
  `while` with lexical `break` and `continue`, deterministic loop-header ownership fixed points,
  exact cleanup on every transfer, and evaluator/native/WebAssembly parity. Compiler-published HIR
  and MIR control are canonical DAGs; repetition is an explicit loop-region semantic, while cyclic
  LLVM control and WebAssembly nesting remain backend-private derivations. Next:
  `normalize-structural-unions`.
- 2026-08-06: Began `add-mutable-bindings-and-structured-loops` after shipping fixed-size arrays.
  Corrected the compiler-pipeline boundary before implementation: compiler-published control is a
  structured DAG with explicit loop semantics, LLVM owns derived cyclic back-edges, and WebAssembly
  consumes the preserved structure without CFG reconstruction.

- 2026-08-05: Shipped and archived `resolve-cross-module-declarations`. Dotted logical imports now
  support namespace, selective, aliased, and hybrid bindings over canonical slash module identities;
  declarations are private by default with explicit `pub`; closure-wide name resolution feeds
  ordinary canonical HIR calls, instance discovery, MIR, interpreter, native, and WebAssembly
  paths; and the facade-only name-resolution lab exposes bindings, conflicts, visibility failures,
  cycles, and diagnostic causes. Next: `standardize-expression-and-operator-semantics`.
- 2026-08-05: Shipped and archived `standardize-target-aware-layouts`, then expanded the active
  compiler-data sequence into an algorithmic-language milestone. Operators, fixed-size arrays,
  mutable bindings, and structured loops now land before unions and matching so the language can
  express small real algorithms rather than only declare data shapes.
- 2026-08-05: Promoted the compiler-data slice to Now and shaped six dependency-ordered changes.
  Settled that the compiler is backend-agnostic but target-aware: canonical target and concrete
  layout are computed before MIR lowering and embedded in MIR. Also fixed the bootstrap import,
  nominal construction, union widening, and mode-aware match decisions in Wayfinder.
- 2026-08-05: Shipped and archived `branch-on-boolean-conditions` — slice 1 complete. `Bool` as
  the second scalar (literals, declared types, comparisons, `Bool.not`), `if`/`else` statements
  with brace arms, condition and argument type checking (`SEM0011`/`SEM0012`), arm-scoped
  ownership with per-return and arm-end exits and conservative conditional moves, MIR branch
  diamonds with join blocks and arm drops, exact interpreter branching, and native `icmp`/`zext`
  emission. Six branching corpus programs hold interpreter/native parity arm by arm. The
  language now binds, computes, and decides through every phase of the spine.
- 2026-08-05: Shipped and archived `compute-integer-arithmetic` — slice 1's second change.
  Signed literals with full `I32` range, qualified callees, and the compiler-known `I32` actor
  (`add`/`subtract`/`multiply`/`divide`/`remainder`) as HIR builtin calls lowering to a trapping
  MIR `Binary` operation; the interpreter traps exactly on overflow, division by zero, and
  MIN/-1, and the backend expands to overflow intrinsics plus guarded division branching to trap
  blocks. Six new corpus programs hold interpreter/native parity including native trap behavior.
  Two recorded design deviations: built-ins live in an elaboration table (not the declaration
  index), and the checked expansion is visible at the LLVM level (MIR stays compact). Next:
  `Bool`, comparisons, `if`/`else`.
- 2026-08-05: Shipped and archived `bind-local-values` — slice 1's first change. `let` bindings
  and `move` now run through the whole spine: statement sequences in grammar and HIR, initializer
  inference, non-shadowing (`SEM0008`), the first real ownership analysis (liveness ranges, moves,
  `OWN0001` use-after-move with a new ownership diagnostic phase and `Violation` verdict),
  populated cleanup plans, lowered `Drop` operations, and interpreter/native trap parity across
  four new corpus programs. Backend injection landed alongside in `fa83a5f`. Next: arithmetic.
- 2026-08-05: Completed the compiler realignment — all 13 changes from
  [compiler-realignment](compiler-realignment.md) implemented and archived in one loop. The spine
  now runs source → module closure → declaration index → HIR → ownership → instances → MIR →
  LLVM bitcode → pinned Clang object → linked native executable, with a MIR interpreter as the
  differential oracle, deterministic encoders and goldens at every artifact, and nine facade-only
  inspector labs. The two-function "Now" milestone and the former "Later" items for MIR lowering
  and the first native program shipped inside it. Grammar freeze lifted; promoted language
  widening slice 1 (bindings, arithmetic, branching) to Now and data types (issue 02) to Next.
- 2026-08-04: Shipped and archived `analyze-first-bootstrap-function` in commit `373c4d8`; direct
  declaration, `I32`, integer, compatibility, and semantic diagnostic facts held without AST/HIR.
  Recast Now as a checkable two-function milestone split into four dependency-ordered changes, each
  with a required inspector checkpoint and a sync/reassessment boundary.
- 2026-08-04: Shipped and archived `parse-first-bootstrap-function` in commit `ba6feaf`; its
  lossless tree, bounded recovery, deterministic diagnostics, and hidden inspector met the recorded
  outcome. Promoted one-function declaration and `I32` fact analysis to Now, explicitly keeping HIR
  behind evidence from a second semantic form.
- 2026-08-04: Shipped exact source text and lossless lexing, then promoted a one-function concrete
  syntax tree and direct-link inspector to Now; semantic interpretation remains Next.
- 2026-08-04: Replaced the oversized end-to-end compiler-kernel initiative with source text and
  lexing. Moved parsing, semantic facts, HIR/MIR, native code generation, and runtime work behind
  evidence-producing capability boundaries.
- 2026-08-04: Created after completing the bootstrap-language Wayfinder map and archiving the LLVM
  builder and Tiny-language OpenSpec portfolios. The first bet is an end-to-end compiler kernel,
  followed by frontend semantics, ownership-aware lowering, and the native bootstrap platform.
