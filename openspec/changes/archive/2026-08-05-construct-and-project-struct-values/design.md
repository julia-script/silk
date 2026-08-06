## Context

See [proposal.md](./proposal.md) for motivation and the capability deltas under `specs/` for the
normative behavior.

The declaration slice already establishes canonical nominal identities, canonical field identities,
field visibility, declaration order, dependency completion, and target-specific physical layouts.
Runtime compilation is still scalar: HIR expressions and evaluator values cover scalar operations,
MIR types are `I32` and `Bool`, MIR has no construct or project operation, and both backends only
accept scalar parameter and result representations. Ownership also currently treats every binding as
Copyable, so its final moved-state shortcut is insufficient once control-flow paths can own or move a
nominal value.

Wayfinder fixes the architectural boundary: HIR expresses target-independent language semantics;
the compiler selects target layout and calling facts before lowering; MIR is backend-neutral but may
be target-aware; and neither LLVM nor WebAssembly may invent Silk layout or ABI policy. The existing
facade is the tooling boundary, and all inspectors belong to the unified `/labs` workbench.

## Goals / Non-Goals

**Goals:**

- Preserve one canonical nominal value model from semantic facts through evaluation and backend
  realization.
- Make aggregate representation and internal calling conventions compiler-owned, deterministic,
  and shared by both backends.
- Keep ownership whole-value and affine while allowing non-consuming reads of Copy scalar fields.
- Preserve exact source provenance and explicit unavailable states at every recoverable stage.
- Keep the implementation inspectable through immutable facade queries and the unified `/labs`
  workbench.

**Non-Goals:**

- Defining a stable foreign-function ABI or exposing aggregate host entry points.
- General place mutation, borrowing, references, pointers, partial initialization, or partial moves.
- Struct patterns, destructuring, structural records, anonymous aggregates, or user-declared `Copy`.
- Choosing a universal in-memory passing convention when scalar lane passing is sufficient.
- Using LLVM target ABI choices or WebAssembly GC types as Silk's semantic or physical authority.

## Decisions

### 1. Parse literals as primaries and projection as a postfix chain

A struct literal is a primary expression consisting of a type path followed by braces and
comma-separated labeled initializers. The concrete bootstrap path follows the same one- or
two-segment name shape already accepted for nominal references. Empty literals accept `{}` and a
trailing comma is permitted wherever the expression-list grammar already permits one.

Projection is a repeated postfix `.` plus field name. Postfix projection binds more tightly than
prefix operators, infix operators, and pipelines, and associates left-to-right. A qualified function
reference is distinguished by the following call syntax; a projection is resolved only after the
subject expression has a type. The lossless tree retains labels, punctuation, initializer source
order, missing pieces, and recovery anchors.

This keeps construction recognizable before name resolution while making `token.span.start` one
ordinary postfix chain. Treating literals as calls was rejected because it would erase labels and
module-owned raw-construction rules. Treating each dotted expression as a qualified name was rejected
because qualification and value projection have different authorities and failure states.

### 2. Semantic facts retain source order and also publish canonical construction order

Resolution produces a literal fact containing the resolved nominal identity, the defining-module
authority result, every source initializer, and a total mapping from canonical declared fields to
their initializer facts. Unknown, duplicate, missing, inaccessible, and mistyped fields remain
queryable; a complete construction value exists only if the mapping is exact. Projection facts retain
the subject fact, canonical field identity, visibility result, resulting declared type, and source
provenance for each step.

Stable diagnostics are assigned by this layer for external raw construction, unknown/duplicate/
missing/mistyped literal fields, non-struct projection, unknown fields, and inaccessible fields.
Existing unavailable-name and non-type diagnostics remain authoritative where applicable. One
source-order representation alone was rejected because all downstream stages would have to rebuild
declaration order. Keeping only canonical order was rejected because inspectors and diagnostics would
lose the user's written structure.

### 3. HIR uses explicit nominal construction and projection operations

HIR gains a logical nominal type alongside bootstrap scalars. A `Construct` expression carries the
canonical nominal identity and its typed field expressions in declaration order while retaining
source provenance. A `Project` expression carries its typed subject, canonical field identity, access
result, and result type. Calls, parameters, results, bindings, and unavailable nodes use the same
logical type vocabulary.

Construction and projection remain semantic operations rather than being expanded to scalar tuples
in HIR. Early flattening was rejected because it would leak target realization into the
target-independent layer and make nominal ownership and diagnostics harder to express.

### 4. Ownership classifies bootstrap nominal values as move-only whole values

The ownership vocabulary gains an explicit move-only classification for every user-defined struct in
this slice. A bound nominal value requires `move` when a consuming let binding, call argument, or
return transfers it. Fresh literals and call results may flow directly because they do not imply a
copy from a reusable source binding. A whole move transfers the cleanup obligation and makes the
source unavailable. Reading a Copy scalar field does not consume the owner; `move value.field` is
always rejected as a partial move.

Ownership state is propagated per control-flow path and joined conservatively rather than computed
from one final moved set. Cleanup is planned for the values live on each structured exit. Local owners
are cleaned in reverse binding order, while fields within a live struct follow the language's
declaration-defined order recursively. Zero-runtime-action cleanup remains an explicit fact.

Implicitly copying nominal bindings was rejected because it would pre-empt the later `Copy`
validation design. Field-level ownership states were rejected for this slice because they would
silently introduce partial moves, replacement, and drop flags before those language features exist.

### 5. One compiler-owned aggregate ABI plan flattens logical structs into scalar lanes

The target layout catalog remains the physical authority for every nominal type. Reachability selects
the exact catalog entries needed by runtime instances; it never recalculates layout. Beside those
physical facts, the compiler derives an internal aggregate ABI shape by recursively flattening Copy
scalar leaf fields in canonical declaration-order paths. Each lane records its canonical field path
and selected scalar representation. An empty struct has zero lanes.

The plan applies uniformly to internal parameters, results, calls, locals, construction, projection,
and cleanup. It is encoded deterministically and consumed read-only by MIR lowering and both
backends. It does not define an external ABI: host-facing bootstrap entry points remain scalar.

Backend-selected aggregation was rejected because it would repeat policy and permit LLVM and Wasm to
disagree. Passing every aggregate indirectly through memory was rejected as unnecessary allocation
and pointer machinery for this bootstrap slice. WebAssembly GC structs were rejected because engine
types cannot become Silk layout authority. Recursive scalar lanes keep the logical model nominal
while using representations both current backends can realize.

### 6. MIR keeps logical aggregate values and explicit construct/project operations

MIR's type vocabulary widens to include canonical nominal types. A logical aggregate local is mapped
to the lane bundle selected by the target ABI plan, but MIR still addresses it as one value.
`Construct` defines a complete aggregate from declaration-ordered operands; `Project` reads a
canonical field path from an aggregate. Existing move, call, return, and drop operations operate on
the complete logical value.

The verifier checks nominal completion, exact construction fields, projection membership and result
type, lane-plan availability, move linearity, and aggregate call signature agreement. Deterministic
encoding includes nominal identities, canonical field paths, and lane representations. Representing
construction as an untyped list of moves was rejected because the verifier could not distinguish a
complete value from an accidental bundle.

### 7. Evaluation uses immutable canonical aggregate values

The evaluator gains an aggregate value containing the canonical nominal identity and immutable field
values in declaration order. Construction evaluates source initializers according to the language's
existing expression-order rule, then stores the complete canonical value; projection follows the
canonical field identity. Whole moves transfer the value and invalidate the source environment slot.
Calls and returns carry the logical aggregate without exposing lane realization.

Trace events describe construction, projection, transfer, and cleanup using stable nominal and field
identities with compact deterministic payloads. Reusing backend lane bundles in evaluation was
rejected because the interpreter is the semantic oracle and must not inherit representation-specific
behavior.

### 8. Backends realize the shared lane plan without adding policy

Native lowering may use LLVM aggregate SSA values or scalar temporaries internally, but function
signatures, call sites, field selection, and results must match the selected lane sequence exactly.
Direct WebAssembly lowering uses scalar parameters/results and locals, including multi-value internal
results where required by the plan. Zero-lane aggregates emit no payload while preserving logical
control flow and cleanup facts.

Both emitters validate their input plan and report typed unsupported or inconsistent-plan failures;
they do not re-run semantic discovery, calculate layout, reorder fields, or select a calling
convention. Differential driver fixtures compare evaluation, native execution, and Wasm execution for
the same aggregate-bearing internal call graph.

### 9. The facade is the sole inspector boundary

Immutable facade queries expose literal and projection syntax, semantic mapping, typed HIR,
ownership/cleanup, selected physical layouts and ABI lanes, MIR, evaluation traces, and emitted
representation summaries. The unified `/labs` registry adds struct-value presets and coordinated
panels with accessible text equivalents. No package exports a second inspector-specific compiler
model and no legacy standalone lab gains ownership of the feature.

## Risks / Trade-offs

- [Recursive lane flattening can grow signatures for deeply nested structs] -> Bound the bootstrap
  surface to finite completed nominal fields, reject recursive by-value declarations during type
  completion, and expose lane counts through analysis before emission.
- [Ownership joins and exit-specific cleanup replace the current all-Copy shortcut] -> Add focused
  branch, early-return, move-transfer, and nested-cleanup fixtures before enabling backend lowering.
- [LLVM and Wasm can drift in lane ordering or zero-sized handling] -> Give both emitters the same
  immutable ABI plan and run deterministic encoding plus three-engine parity tests.
- [Source-order evaluation and declaration-order storage can be accidentally conflated] -> Store both
  orders in semantic facts and test literals whose fields are written out of order with observable
  initializer traces.
- [Logical aggregate MIR mapped to physical lanes adds verifier complexity] -> Centralize nominal-to-
  lane lookup in the target plan actor and reject missing or mismatched entries before emission.
- [Aggregate traces can become noisy] -> Record stable identities and field paths, not duplicated
  nested value dumps, and let the facade expand details on demand.

## Migration Plan

This is an intentional pre-release data-model break with no compatibility layer.

1. Land syntax and semantic facts while aggregate runtime nodes remain unavailable downstream.
2. Widen the shared type vocabulary, implement HIR and path-sensitive ownership/cleanup, and add
   evaluator support.
3. Extend discovery and target planning with nominal reachability, catalog reuse, and ABI lanes.
4. Add aggregate MIR operations and verification, then consume the shared plan in native and Wasm
   emitters.
5. Publish facade queries and unified `/labs` presets, then enable end-to-end driver parity fixtures.
6. Remove scalar-only assumptions rather than keeping deprecated adapters.

Rollback is a normal revert of the change because no persisted user data or external protocol is
migrated. Deterministic fixtures make partial rollback detectable: stages that do not understand the
new logical type or ABI plan must fail explicitly rather than silently scalarizing it.
