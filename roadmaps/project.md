# Roadmap — Silk Effect

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-09 · Review cadence: after each OpenSpec archive, or monthly when no
> change ships · Scope: whole project

## Vision

Silk Effect will be a low-level systems language that combines explicit memory and execution
control with typed failures, explicit service requirements, deterministic cleanup, and
tooling-friendly semantics. The immediate destination is a small, coherent language whose design
survives recognizable programs; broader compiler, ecosystem, and eventual self-hosting work follows
evidence from that language rather than defining it in advance.

**Current objective:** shape Silk through complete, recognizable pressure programs — measured by
ordinary Silk source exposing and categorizing language, standard-library, compiler, tooling, and
cost-model gaps while preserving evaluator, native, and WebAssembly agreement.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Add typed scalar constants from repeated program evidence

**Status: complete (2026-08-09).** Silk now accepts literal-only, explicitly typed scalar
constants with optional public visibility. Boolean, integer, `usize`, and floating declarations
resolve in local, selected-import, and qualified scopes; editor presentation and navigation use the
same canonical identity. Accepted references inline into existing typed immediate values, so there
is no global storage, runtime initialization, allocation, cleanup, or new backend representation.
The lexer and stack VM now name their repeated token codes, opcodes, limits, and diagnostics while
preserving evaluator, native LLVM, direct Wasm, allocation-failure, and fresh-process evidence.

Computed and aggregate constants, inferred types, addressable globals, and enum/exhaustiveness
semantics remain deliberately separate.
- **Links:** [real-programs initiative](real-programs.md)

### Exercise Silk with a bounded stack bytecode VM

**Status: complete (2026-08-09).** The ordinary Silk VM differentially matches its TypeScript oracle
over arithmetic, taken and untaken branches, malformed bytecode, stack bounds, invalid jumps, and
bounded loops. One owned ordered `Vector<Step | VmDiagnostic>` event stream supplies realistic
allocation pressure; every growth ordinal preserves typed failure and balanced cleanup across the
evaluator, native LLVM, and direct Wasm, and fresh processes reproduce the same artifacts.

The VM independently confirms the need for named typed values and shared Vector reads. It also
exposed nested dynamic reference-place lowering, structural-union `Slot.copy`, and a native
path-sensitivity defect for address-taken mutable roots. The native defect blocked the direct
two-vector result shape and was selected as the next focused repair; no parser port or continuous
self-hosting sequence follows from the exercise.
- **Links:** [real-programs initiative](real-programs.md)

### Repair contextual integer literals exposed by the lexer

**Status: complete (2026-08-09).** Focused characterization corrected the initial finding: direct,
explicit-generic, and piped calls already apply concrete integer parameter contexts. The live defect
was `bool` from an enclosing return position suppressing homogeneous operand refinement in
expressions such as `return byte == 13`. Operator analysis now lets a resolved scalar first operand
refine the remaining exact literals even when an enclosing result expectation exists. The lexer
uses `u8` classifiers without the `i32` workaround, and semantic/HIR/MIR facts, diagnostics,
evaluator, native LLVM, direct Wasm, and fresh-process artifacts agree.

Enum/exhaustiveness and cost findings remain unpromoted. Shared Vector reads now have independent
evidence from both pressure programs and follow typed constants as the next focused boundary.
- **Links:** [real-programs initiative](real-programs.md)

### Validate the language's defining effect execution model

**Status: complete (2026-08-07).** The review replaced the evidence-producing Flow surface with
`effect {}` as the primitive lazy imperative boundary, `effect fn` as whole-body sugar, `Effect` as
the public computation type, and ordinary Copy-versus-move failure transfer. Capture access now
derives shared-repeatable, exclusive-repeatable, or take-once execution; retry reconstructs locals
while preserving captures; provision distinguishes captured providers from per-run acquisition;
and typed failure remains separate from traps. First-class callable values and automatic
data-first sections subsequently proved Effect composition without pipeline-only callback syntax.

Named lifetime scopes, dynamic cleanup registries, provider-dependent result sets, and allocator
magic were rejected from the bootstrap model. Allocation owners are self-contained affine values,
restricted synchronous infallible `Drop` is the cleanup mechanism, and arena-backed escaping values
remain deferred until Silk has a general non-privileged validity model.

- **Links:** [effect model decision](../wayfinder/bootstrap-language/issues/03-function-contracts-services-and-failures.md) ·
  [ownership and allocation](../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md) ·
  [bootstrap syntax](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md)

### Widen the language, slice 1: bindings, arithmetic, branching

**Status: complete (2026-08-05).** All three changes shipped and archived; see the changelog.

- **Problem:** The realigned spine (diagnostics through native link, all 13 changes archived
  2026-08-05) runs end to end over a grammar too small to exercise it: ownership is trivially
  satisfiable, lowering never emits `Branch` or `Drop`, and the differential harness compares only
  straight-line integer programs.
- **Outcome & done-when:** Silk programs with `let` bindings and `move`, signed literals with
  arithmetic, and `bool`/comparison/`if` compile through every phase — real liveness ranges and
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

### Complete the self-contained owned-allocation substrate

**Status: complete (2026-08-08).** `unsafe` blocks, `impl` conformances, nominal `SystemAllocator`,
affine `Allocation`, generic `RawBuffer<T>`, lexical `Slot<T>`, and compiler-sealed `Drop` hooks run
through every phase and all three engines. A user-authored `impl Allocator for X` dispatches to its
own operation through an exclusive call-scoped provider loan, so a counted quota allocator — state
decremented through `&mut self` — exhausts identically on the evaluator, Wasm, and native
executables. Allocation failure stays typed and allocation-free, a rejected request acquires
nothing, and owners live at a failing run site release before the failure propagates.

- **Problem:** Slices can borrow source-dependent input, but a compiler pass still cannot own an
  output whose size emerges at runtime. The general prerequisites are now executable: target-sized
  `usize`, typed Effects, capability requirements and provision, target-aware `Layout`, affine
  ownership, deterministic cleanup, and first-class callables. What remains is the smallest unsafe
  allocation boundary that composes those mechanisms without privileged allocator kinds.
- **Outcome & done-when:** Add general `Allocator` capability dispatch, a standard-library
  `SystemAllocator`, and an affine self-contained `Allocation` whose private reclaim ticket is
  sufficient for exactly-once cleanup. Allocation failure is typed and allocation-free. Evaluator,
  LLVM, Wasm, ownership, MIR, determinism, and `/labs` agree on acquisition, transfer, and release.
  Then add restricted synchronous infallible `Drop` plus unsafe typed raw-buffer/slot operations
  sufficient to initialize and roll back a dynamic sequence safely.
- **Boundary:** The compiler recognizes capability, ownership, layout, and drop rules—not arenas,
  allocator policies, or collection types. No named lifetime `Scope`, dynamic finalizer registry,
  ambient/default allocator, user-callable `free`, primitive resize, zero-fill promise, or compiler-
  known `Vector`. An arena is only another Silk standard-library implementation of `Allocator`.
- **Sequence:**
  1. General allocator dispatch, `SystemAllocator`, affine `Allocation`, and typed `OutOfMemory`
  2. Restricted `Drop` and unsafe typed storage operations with deterministic rollback
  3. Differential allocation and cleanup evidence across evaluator, LLVM, Wasm, and `/labs`
- **Evidence:** A construction guard carrying runtime-counted move-only elements on the substrate
  alone — no `Vector` or collection intrinsic — returns `42` from the evaluator, an instantiated Wasm
  module, and a linked native executable, with one acquire and one release around ordered `Slot`
  writes and takes. The failure-ordinal sweep proves atomic rejection, release-once, unchanged
  `OutOfMemory`, and a successful subsequent run; MIR and Wasm bytes are identical across analyses.
- **Links:** [ownership and scoped allocation](../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md) ·
  [types and values](../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md) ·
  [compiler pipeline](../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md) ·
  [bootstrap syntax](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md)

### Implement growable compiler output in Silk

**Status: complete (2026-08-08).** `add-silk-vector-and-scanner` ships the first importable Silk
standard-library module and implements `Vector<T>` entirely in `silk.vector` over `Allocator`,
`Allocation`, `RawBuffer<T>`, `Slot<T>`, and parametric `Drop`. Empty vectors allocate nothing;
append grows 0 → 4 → ×2; failed replacement allocation preserves the original vector; initialized
elements drop before storage release; and move-out plus early drop retain exactly-once ownership.

- **Outcome:** The acceptance scanner borrows `&[U8]`, produces ten tokens in an owned
  `Vector<Token>`, and crosses three allocation ordinals. Evaluator, native LLVM, and direct Wasm
  all return `42` with identical token observations; quota sweeps fail each growth ordinal without
  leaks and succeed once the quota covers all allocations. Two fresh processes produce identical
  closure, HIR, ownership, instances, layout, MIR, traces, symbols, and backend artifacts.
- **Boundary held:** No compiler phase recognizes `Vector`, no vector-shaped MIR or backend
  primitive was added, and the scanner demonstrated no need for a bulk byte-memory or iterable
  abstraction. The generic collection and its cleanup policy remain ordinary Silk source.
- **Evidence:** `VectorAcceptance.test.ts`, `ScannerAcceptance.test.ts`, and
  `ScannerDeterminism.test.ts` cover growth, rollback, early and lexical destruction, failure
  ordinals, three-engine parity, and fresh-process determinism. Five coordinated `/labs` presets
  expose the same sources and facts, with the scanner preset byte-identical to its acceptance
  fixture.

## Next

Typed scalar constants and the native address-root repair are complete. Shape shared `Vector<T>`
reads for ordinary Copy elements while keeping the structural-union `Slot.copy` provenance repair
as an explicit prerequisite for union elements. No neighboring compiler port is preselected.

## Later

- **Make Silk capable of expressing its own compiler** — progressively replace the TypeScript seed
  implementation with Silk modules while preserving reference equivalence, once the language is
  small and well-defined enough that self-hosting tests it rather than prematurely steering it.
- **Supply the compiler's native platform** — add runtime, host services, and any private C shim
  demanded by an eventual real Silk compiler module, rather than speculating ahead of that need.
- **Prove native self-hosting** — produce stage 1, stage 2, and a byte-identical fixed-point rebuild
  with complete conformance, failure, debug, resource, and performance evidence on all required
  native hosts.
- **Preserve a pay-for-use path to Effect synchronization** — keep sequential Stream demand,
  single-thread concurrency, and later parallel execution compatible with low-level cost
  transparency without scheduling async work now; see the
  [direction note](../wayfinder/bootstrap-language/research/concurrency-and-parallelism-direction.md).
- **Grow beyond the bootstrap subset** — concurrency, networking, schemas, observability, richer
  tooling, and broader standard-library families become candidates when pressure programs expose
  their real constraints.
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

- What executable name and public analysis facade should eventually accompany
  `@silk-effect/compiler`?

## Changelog

- 2026-08-09: Completed `add-typed-constants`. Literal-only explicit scalar names participate in
  module visibility, import resolution, tooling, and direct immediate lowering with no runtime
  storage. The lexer and stack VM now name representative codes and limits without losing their
  allocator, evaluator/native/Wasm, or determinism gates. Shared Vector reads become the next
  evidence-backed boundary; enums remain separate.

- 2026-08-09: Selected a bounded stack bytecode VM as the second language-pressure program. It
  independently tests closed opcode vocabulary, Vector observation, allocation rollback, invalid
  execution, and cross-engine determinism without implying a production VM or self-hosting step.
- 2026-08-09: Completed `fix-contextual-integer-call-literals`. Characterization showed ordinary
  and piped calls were already correct; the repaired defect was enclosing `bool` result context
  suppressing `u8` operand-to-literal refinement. The lexer now stays in byte types throughout its
  classifiers with evaluator/native/Wasm and deterministic artifact parity.
- 2026-08-09: Closed the first real-program milestone after lowercase primitive families, physical
  stdlib sources, static text, standard streams, four baseline algorithms, allocation-pressure BFS,
  recursive quicksort, and executable FFT all reached cross-engine parity.
- 2026-08-09: Reframed the next compiler-shaped program as a language-pressure exercise. A Silk
  lexer comes first; parser porting and continuous self-hosting remain later decisions.
- 2026-08-09: Completed and archived `exercise-language-with-silk-lexer`. The full token surface,
  invalid recovery, allocation rollback, all three engines, and fresh-process determinism agree;
  the exercise also repaired an LLVM CFG inlining defect and selected contextual integer literals
  as the next focused repair.

- 2026-08-09: Recorded a non-binding concurrency and parallelism direction: synchronous programs
  pay no scheduler or fiber cost; suspension, fork, and parallelism add progressively explicit
  runtime costs; and future Stream/Sink adapters build above primitive effectful standard I/O.
- 2026-08-09: Promoted the real-programs initiative to Now and split its former umbrella proposal
  into six independently implementable changes. Native-platform expansion remains Next; String,
  Logger, default providers, and Stream/Sink stay explicit future seams rather than hidden scope.

- 2026-08-08: Completed `add-silk-vector-and-scanner` and promoted the minimum native compiler
  platform to Next. `silk.vector` is the first embedded, explicitly imported standard-library
  module; its generic `Vector<T>` grows, rolls back failed growth atomically, moves out values, and
  destroys initialized elements before storage release without compiler-known collection behavior.
  A Silk scanner now borrows runtime-sized bytes and returns ten owned tokens through three
  allocation ordinals with evaluator/native/Wasm parity, typed quota-failure sweeps, fresh-process
  artifact determinism, and coordinated `/labs` evidence. The shipped outcome answers the prior
  pass-selection question: byte-to-token scanning was sufficient to expose and close the remaining
  generic ownership, provider-forwarding, effect-capture, and backend representation gaps without
  requiring bulk memory primitives.
- 2026-08-08: Completed the self-contained owned-allocation substrate and promoted the Silk-written
  `Vector<T>` and scanner change to the current objective. `unsafe` blocks, `impl` conformances,
  nominal `SystemAllocator`, affine `Allocation`, generic `RawBuffer<T>`, lexical `Slot<T>`, and
  compiler-sealed `Drop` hooks run through every phase and all three engines, with no named scope,
  allocator-kind branch, ambient allocator, public `free`, implicit zeroing, or initialization
  bitmap. Provision dispatches to user-authored allocator witnesses through an exclusive
  call-scoped loan, and field projection through nominal references — the prerequisite for a
  witness reading its own provider state — landed with it, making a counted quota allocator
  expressible. Two pre-existing defects surfaced and were fixed on the way: ownership never
  descended into lazy `effect` bodies, so use-after-move inside them went unreported, and failed
  run propagation returned past every cleanup region, leaking owners live at the run site.
- 2026-08-07: Settled the defining execution model around `Effect`, `effect {}`, and `effect fn`;
  rejected named lifetime scopes and allocator privilege in favor of affine self-contained owners
  with deterministic `Drop`. Shipped first-class callable values and automatic data-first sections,
  making pipelines ordinary unary application and enabling reusable `Effect.map`, retry, and custom
  combinator callbacks. Synced and archived the completed callable, source-resolution, project-CLI,
  and LSP-navigation changes, leaving no completed records in the active change queue.
- 2026-08-07: Shipped and archived the first executable typed-flow baseline, then promoted a full
  effect-model review ahead of capability roles, scopes, and allocation. The baseline proves lazy
  construction, typed failure outcomes, exact recovery, cleanup, compiler-owned target layout,
  structured DAG lowering, evaluator/native/Wasm parity, determinism, and unified Labs inspection.
  Its `flow fn`, `Flow`, and `fail move` spellings were deliberately left open at archive time and
  settled later the same day after a broad scenario corpus and adversarial review.
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
- 2026-08-05: Shipped and archived `branch-on-boolean-conditions` — slice 1 complete. `bool` as
  the second scalar (literals, declared types, comparisons, `bool.not`), `if`/`else` statements
  with brace arms, condition and argument type checking (`SEM0011`/`SEM0012`), arm-scoped
  ownership with per-return and arm-end exits and conservative conditional moves, MIR branch
  diamonds with join blocks and arm drops, exact interpreter branching, and native `icmp`/`zext`
  emission. Six branching corpus programs hold interpreter/native parity arm by arm. The
  language now binds, computes, and decides through every phase of the spine.
- 2026-08-05: Shipped and archived `compute-integer-arithmetic` — slice 1's second change.
  Signed literals with full `i32` range, qualified callees, and the compiler-known `i32` actor
  (`add`/`subtract`/`multiply`/`divide`/`remainder`) as HIR builtin calls lowering to a trapping
  MIR `Binary` operation; the interpreter traps exactly on overflow, division by zero, and
  MIN/-1, and the backend expands to overflow intrinsics plus guarded division branching to trap
  blocks. Six new corpus programs hold interpreter/native parity including native trap behavior.
  Two recorded design deviations: built-ins live in an elaboration table (not the declaration
  index), and the checked expansion is visible at the LLVM level (MIR stays compact). Next:
  `bool`, comparisons, `if`/`else`.
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
  declaration, `i32`, integer, compatibility, and semantic diagnostic facts held without AST/HIR.
  Recast Now as a checkable two-function milestone split into four dependency-ordered changes, each
  with a required inspector checkpoint and a sync/reassessment boundary.
- 2026-08-04: Shipped and archived `parse-first-bootstrap-function` in commit `ba6feaf`; its
  lossless tree, bounded recovery, deterministic diagnostics, and hidden inspector met the recorded
  outcome. Promoted one-function declaration and `i32` fact analysis to Now, explicitly keeping HIR
  behind evidence from a second semantic form.
- 2026-08-04: Shipped exact source text and lossless lexing, then promoted a one-function concrete
  syntax tree and direct-link inspector to Now; semantic interpretation remains Next.
- 2026-08-04: Replaced the oversized end-to-end compiler-kernel initiative with source text and
  lexing. Moved parsing, semantic facts, HIR/MIR, native code generation, and runtime work behind
  evidence-producing capability boundaries.
- 2026-08-04: Created after completing the bootstrap-language Wayfinder map and archiving the LLVM
  builder and Tiny-language OpenSpec portfolios. The first bet is an end-to-end compiler kernel,
  followed by frontend semantics, ownership-aware lowering, and the native bootstrap platform.
