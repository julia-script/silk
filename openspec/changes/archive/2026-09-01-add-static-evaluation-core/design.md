## Context

See [proposal.md](proposal.md) for motivation. The current compiler indexes declarations and their
constant literals in `DeclarationCollection`, resolves names, elaborates function bodies into HIR,
runs ownership over those bodies, discovers concrete runtime instances, selects target layouts, and
then lowers to MIR. `Analysis.realize` is the facade boundary that assembles those realized facts.

Two current details shape this design:

- Target-dependent constants bypass ordinary resolution. `DeclarationCollection` recognizes the
  syntax `Target.<fact>`, `ExpressionAnalysis` records an unselected value plus a selector, and
  lowering chooses the target value. The `Target` spelling is not an ordinary source actor.
- `Instances` follows calls already present in HIR, and `IntrinsicAvailability` validates the calls
  retained by that executable closure. Static selection must therefore happen before a body's
  residual calls enter discovery, ownership, or availability planning.

The repository is green-field. The special target-constant route is an implementation to replace,
not a compatibility surface to preserve alongside the general mechanism. The compiler's minimal
privilege rule still applies: the public target API must be ordinary source over one irreducible
sealed intrinsic.

## Goals / Non-Goals

**Goals:**

- Add one explicit phase model in which source can compute static values and specialize runtime
  bodies without a token or AST macro system.
- Make a successful specialization publish ordinary residual HIR, so ownership, instance
  discovery, layout, MIR, evaluation, and both backends remain runtime-only consumers.
- Make static values value-semantic, identity-free, and outside runtime ownership without teaching
  the type system that their runtime types are `Copy`.
- Preserve deterministic incremental facts, tooling provenance, target selection, and diagnostics.
- Delete the syntax-only target selector and derive the public target API in source through the
  sealed intrinsic boundary.

**Non-Goals:**

- Conditional declarations or target-dependent module surfaces.
- Runtime reflection, heterogeneous static iteration, tuples, ephemeral records, or formatting.
- Static references, in-place mutation, manual allocation, Effect execution, host access, unsafe
  operations, or arbitrary runtime-function interpretation.
- A phase-polymorphic `fn` whose execution phase is inferred from its arguments.
- Runtime panic semantics; `compileError` is a compile-time control form only.

## Decisions

### 1. Syntax marks the phase boundary, not every literal

The initial source forms are:

```silk
static fn parse(value: string) -> Result { ... }

fn render(static template: string, value: &Value) -> String {
  let static parsed = parse(template)
  static if parsed.isEmpty {
    compileError("empty template")
  }
  // ordinary runtime operations
}
```

The exact modifier order follows existing declaration conventions: `static fn` or `pub static fn`,
`static` before a parameter name, and `let static` as a mode distinct from `let mut`. The initial
grammar does not combine `static` with `unsafe`, `effect`, implementation/service/interface
operations, mutable parameters, or mutable mixed-function static bindings. `static if` is initially
a statement form because Silk's existing block conditional is a statement; expression-position
static selection can be added with an expression-shaped grammar rather than overloading this node.
A static function's ordinary local
bindings, `if`, `while`, and returns execute statically because the whole function has one phase.
`let static` and `static if` exist for ordinary mixed functions. `compileError(message)` is an
inherently compile-time terminal expression, so it carries no redundant `static` prefix. It is a
dedicated syntax form rather than an ordinary resolved call: source cannot import, shadow, capture,
or pass it as a value, and its argument must be a statically evaluated `string`. The spelling names
the requested outcome rather than suggesting a runtime panic or an internal compiler crash; this
change introduces no general runtime panic.

Literals are phase-available expressions rather than a second literal type. A literal directly in a
static-demanding context is accepted. Once stored in an ordinary local it is runtime; retaining it
for later static use requires `let static`. This avoids optimizer-dependent constant propagation and
makes the phase visible without noisy `static "text"` or `static 32` syntax.

Alternatives rejected:

- Infer static parameters from function bodies. A refactor could silently change the call contract.
- Preserve static availability through every immutable `let`. The source would no longer reveal
  whether a local is a runtime value or specialization input.
- Require `static` at every literal call site. The declaration already states the requirement.

### 2. Declarations retain body templates; concrete applications produce residual HIR

Declaration indexing and signature resolution remain declaration-wide. A function containing
static constructs additionally retains a source-provenanced body template. It does not publish one
fully elaborated runtime body that contains both static arms.

Realization selects and validates the concrete compilation target before constructing the
executable worklist. The worklist then requests a concrete body using:

```text
declaration identity
+ concrete generic and row arguments
+ selected evidence
+ canonical static argument values
+ selected compilation target
```

A new `StaticEvaluation` actor elaborates and evaluates the static portions of that body and returns
either `ResidualBody` or `StaticFailure`. `ResidualBody` contains ordinary typed HIR plus the exact
runtime call edges and source mapping for only the selected code. `StaticFailure` contains no HIR.
The existing runtime pipeline then consumes the residual body:

```text
parsed declarations and signatures
              │
              ▼
        target selection
              │
              ▼
   concrete application worklist
              │
              ▼
       StaticEvaluation
       ├─ StaticFailure ──▶ diagnostic
       └─ ResidualBody
              │
              ▼
 residual ownership + cleanup
              │
              ▼
 runtime call discovery → target availability → layout → MIR → engines
```

For each demanded application, the coordinator evaluates one residual body and records its direct
residual call candidates privately. Candidate traversal records the specialization key before
following those calls, preserving the existing recursion-termination discipline, but does not yet
publish an executable closure. A cleanup-edge prepass over the closed candidate graph adds any
cleanup-hook applications without publishing ownership facts. Once that graph is closed, the
coordinator runs ownership and cleanup exactly once over every successful residual specialization
and admits the resulting direct and cleanup call closure to availability and lowering. Equal
applications reuse an immutable evaluation result. Static
functions are invoked only from this evaluator or a constant initializer and never receive a runtime
instance. Phase-independent signature errors remain declaration diagnostics; evaluation,
selected-arm, compile-error, and resource errors belong to the demanded specialization. An uncalled
static function is indexed and navigable but not executed.

Alternatives rejected:

- Put `StaticIf` into runtime HIR and prune during lowering. Both arms would already have been name
  resolved, typed, assigned ownership, and admitted to target reachability.
- Let each backend evaluate conditions. Engine parity and the no-backend-reachability guarantee
  would depend on duplicated policy.
- Eagerly evaluate every body during module loading. Static parameters are not known there, and an
  unused `compileError` would reject otherwise valid executable closure.

### 3. The evaluator has its own values, not its own observable memory

`StaticValue` is an immutable canonical value algebra for admitted scalars, enum members, static
text, and recursively pure aggregates. Each value has a stable encoding used in specialization keys
and diagnostics. It contains no host object identity, address, reference, runtime allocation handle,
Effect, service, callable, unsafe pointer, or cleanup hook.

Static bindings may be read repeatedly regardless of the runtime type's `Copy` conformance. This is
an evaluator permission, not type evidence: `T: Copy`, interface selection, HIR ownership, and
runtime cleanup continue using the sealed runtime property. When a cleanup-free static value is used
by residual runtime code, residualization emits a fresh ordinary literal or aggregate construction;
it never emits a pointer into evaluator storage.

Inside a static function, `let mut` changes one environment slot by complete value replacement.
There are no static places, projections writable through aliases, references, `&mut`, partial moves,
or destructor calls. A parser can therefore use an accumulator:

```silk
let mut state = initial()
while hasNext(state) {
  state = step(state)
}
return finish(state)
```

The implementation may share immutable nodes or use copy-on-write, but neither identity nor copy
count is observable. Variable-sized compiler storage is an evaluator implementation detail; this
change exposes no static allocator or source type whose contract depends on one.

Alternatives rejected:

- Run the runtime ownership checker over evaluator storage. It reintroduces borrow friction for a
  problem with no runtime alias or resource lifetime.
- Treat every runtime type as implementing `Copy` in static code. Generic conformance would change
  merely because execution moved phases.
- Add mutable static references or builders. They require a second aliasing and lifetime model.

### 4. Static evaluation is deliberately not arbitrary ordinary-function interpretation

A static function may call another static function and use the evaluator's admitted primitive
operations: scalar and enum construction, equality and control flow, checked primitive arithmetic,
static-text inspection required by admitted library code, pure aggregate construction, and complete
binding replacement. An ordinary runtime function call is always residual in a mixed function and
is a phase error inside a static function.

This rule makes the callee declaration, not dataflow inference, determine whether a call executes
now. It also keeps runtime Effects, services, I/O, time, randomness, environment access, unsafe
operations, and destructors out of compiler execution. Future phase-polymorphic reuse can be designed
from measured duplication rather than becoming an implicit property of every apparently pure
function.

### 5. Static control elaborates only the selected arm

The condition of `static if` is elaborated and evaluated first. The parser has already retained both
arms, so syntax errors remain visible. `StaticEvaluation` then elaborates only the selected arm under
its lexical scope and expected result type. A selected ordinary call becomes residual HIR. Nested
static forms are evaluated recursively. An unselected arm has no resolution, type, Effect,
requirement, ownership, call, or availability facts.

`compileError` returns `StaticFailure.CompileError` and acts as bottom for the selected expected
type. Any residual operations accumulated earlier in the specialization are discarded with the
failure. Runtime control flow does not alter static traversal: source that must be statically
excluded must be placed in a `static if` arm rather than after an ordinary runtime `return`.

Conditional declarations are excluded syntactically. This keeps module surfaces, import resolution,
navigation, and canonical declaration identities independent of the selected target.

### 6. Static arguments extend specialization identity but not runtime ABI

Static parameter values are encoded in declared parameter order after generic arguments and
evidence. Equal canonical encodings share a residual function; unequal values remain distinct even
if optimization later makes their machine code equal. The selected target belongs to the containing
realization key because one compilation already selects exactly one target.

Static arguments, static locals, and static function frames are retained in semantic and inspection
facts but omitted from calling shapes, layout planning, MIR locals, and backend parameters. Runtime
ownership runs after residualization and keys its facts and cleanup plans by the complete residual
specialization rather than only the source declaration.

### 7. One static-only intrinsic replaces the compiler-known Target spelling

The intrinsic catalog gains a phase classification. Runtime operations retain their normalized
Evaluator/LLVM/Wasm target sets. `Intrinsic.targetProfile() -> u8` is `StaticOnly`, receives no
runtime target set, and is implemented only by `StaticEvaluation` from its supplied canonical
`Target.Target`.

Profile codes follow `Target.all` canonical order and are frozen as:

```text
0  aarch64-apple-darwin
1  aarch64-unknown-linux-gnu
2  wasm32-unknown-unknown
3  x86_64-unknown-linux-gnu
```

An ordinary `silk.target` module maps that primitive value through zero-argument static functions
`Target.profile()` and `Target.arch()` to public nominal enums. It derives the primitive constants
`Target.pointerBits`, `Target.usizeMax`, `Target.isizeMax`, and `Target.isizeMin` through ordinary
static constant evaluation. Keeping nominal values behind static functions preserves the initial
primitive-only top-level constant contract. The standard-library `usize` and `isize` modules import
that actor and initialize their constants through ordinary static constant evaluation.

`TargetConstant`, syntax matching in `DeclarationCollection`, selector-shaped declaration facts,
unselected placeholder values, and selector replacement in lowering are deleted. The source actor
has no name privilege; a user declaration with the same spelling resolves ordinarily.

Alternatives rejected:

- Make `Target` a compiler-known actor. That violates the sealed intrinsic boundary.
- Return a target string. It permits misspelling-driven policy and makes target checks text based.
- Expose one intrinsic per target fact. A single profile discriminator is sufficient for source to
  derive the rest.

### 8. Top-level constants use the same evaluator with a narrower result contract

Constant declarations retain their required explicit primitive type and no-runtime-storage
semantics. Their initializer is now a static expression rather than a literal-or-selector union.
The evaluator must produce exactly the declared primitive value for the selected target. Aggregate
results, type inference, ordinary calls, Effects, and runtime storage remain invalid.

Constant evaluation uses the same cache, value encoding, `compileError` behavior, target
environment, and limits as function specialization. Constant dependency cycles are detected
through the static call stack and reported without a partial value. Target-neutral declaration and
module-surface facts retain the explicit declared type, initializer body template, and source
provenance without claiming a selected value. The concrete target realization evaluates that
template and publishes the selected canonical constant value for residual HIR and inspection; no
backend selects it again.

### 9. Diagnostics are semantic traces, not host debugger output

Static evaluation records logical frames containing the static declaration/call span, canonical
argument presentation, selected target, and selected static-arm span. A requested compile error,
phase violation, cycle, or resource exhaustion converts those frames into deterministic diagnostic
details and related spans. Static-text processing may attach a byte offset into the originating
literal.

Four independent budgets are checked: evaluator steps, logical call depth, retained canonical value
bytes, and generated residual HIR nodes. Their concrete initial thresholds are compiler-owned and
tested at boundaries; changing them is a compiler policy change, not observable successful-program
semantics. Limit exhaustion receives a dedicated reason and never masquerades as `compileError`.
No host stack, JavaScript cause, address, cache key, or backend detail appears in source diagnostics.

## Risks / Trade-offs

- **[Risk] Body elaboration becomes application-sensitive.** → Keep declaration signatures and
  phase-independent diagnostics declaration-wide, isolate application work in one
  `StaticEvaluation` actor, and publish residual bodies under canonical specialization keys.
- **[Risk] Static argument diversity causes code growth.** → Count residual nodes, deduplicate equal
  keys before evaluation, expose deterministic specialization inventories, and fail through the
  residual-growth budget before backend work.
- **[Risk] Unselected branches receive fewer editor semantics.** → Preserve full syntax and spans;
  tooling presents the selected target/specialization and labels inactive source rather than
  fabricating navigation or type facts.
- **[Risk] Static reuse is mistaken for runtime `Copy`.** → Keep `StaticValue` outside ownership and
  interface evidence, and test an affine runtime type reused statically while remaining non-Copy in
  residual analysis.
- **[Risk] The existing target-constant implementation overlaps this change.** → Migrate all stdlib
  consumers in one step and delete every selector fact, special diagnostic, lowering branch, test,
  and document rather than retaining dual paths.
- **[Risk] Compiler execution becomes a nondeterminism or resource vector.** → Admit no Effects or
  host observations, canonicalize every value, use deterministic worklists and limits, and compare
  committed semantic encodings across engines and the existing global fresh-process canary.

## Migration Plan

1. Add syntax and declaration-surface support while keeping all new forms unavailable to lowering.
2. Introduce canonical static values, specialization keys, evaluator outcomes, limits, and traces.
3. Route concrete body realization through static evaluation before residual ownership and call
   discovery; update inspection and encodings to key facts by residual specialization.
4. Add the static-only target-profile intrinsic and ordinary `silk.target` source API.
5. Migrate typed constants and pointer-width stdlib constants to ordinary static initialization,
   then delete `TargetConstant` and every selector-specific path in the same change.
6. Move intrinsic availability to the residual closure, update docs and the acceptance corpus, and
   run the complete repository validation sequence.

Because the repository is green-field, rollback is a source revert of the complete change. No
compatibility parser, legacy selector path, dual module surface, or migration shim is retained.
