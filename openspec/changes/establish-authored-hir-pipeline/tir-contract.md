# TIR contract (A3 / JUL-207 task 3.3)

This document fixes the final shape of the typed intermediate representation before any of it is
built. It replaces two schemas that exist today: the analyzed fact tree (`Elaboration.FunctionFact`,
`StatementFact`, `ExpressionFact`) and the tree `TirLowering` converts it into (`Tir.TirFunction`).
Neither survives as it is. The design follows the intended architecture and a later self-hosted
implementation, not the size of the change.

## Why neither current schema is the answer

|                            | Fact tree today                                                                  | TIR today                                                    |
| -------------------------- | -------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| Who reads it               | static evaluator, `LifetimeFlow`, completion, type hints, inspector, occurrences | ownership, instances, MIR lowering                           |
| What it knows              | references, call contracts, static values, scopes, captures                      | normalized executable operations, regions, borrow identities |
| What ties it to a revision | `DeclarationFact` objects, embedded `SourceSpan`s in diagnostics and identities  | `DeclarationFact` objects, a `SourceSpan` on every node      |

Both bodies hold objects and offsets that belong to one revision. That is the only reason
`SemanticRebinding` exists. A body that holds no such value needs no rebinding, and is also the body
a later disk cache can store.

## Principles

1. **One typed body per artifact.** An artifact is one semantic request against one authored owner
   (§ Artifact identity). Elaboration emits its TIR directly; there is no second tree and no
   conversion pass.
2. **Portable schema.** Every artifact is a value of a closed, versioned vocabulary with a field
   registry (as `AuthoredHir.fields` is) and a canonical codec derived from that registry. The schema
   and codec define the contract. Tagged records, arrays, strings, integers, `bigint`, booleans and
   explicit absence only: no functions, maps keyed by identity, symbols, or host objects.
3. **References are identities.** A declaration is a canonical id, a position an authored anchor, a
   local an artifact-local ordinal, a type a structural `Type` value, another artifact an
   `ArtifactId`.
4. **No spans and no revision.** A span exists only after a presentation resolves a location.
5. **Every semantic decision has one authoritative representation** (§ Node and table ownership).
6. **A published artifact is immutable.** Nothing writes into it afterwards, including the evaluator.

## Artifact identity

"Checking `f`" is not one thing. These are three distinct artifacts:

```text
check      @f                              the generic body, parameters symbolic
specialize @f<T = i32, static N = 4>       one semantic application
specialize @f<T = i32, static N = 8>       another
```

```text
ArtifactId = { owner:    AuthoredIdentity          which authored owner's body
               request:  Check | Specialize(Application)
               parent?:  ArtifactId }               set for hidden and generated bodies

Application = { typeArguments:   canonical generic-argument keys, declared order
                staticArguments: canonical static values, declared order      (no provenance)
                evidence:        canonical selected-evidence keys, declared order
                contractRow:     canonical row keys }
```

- **`Check`** elaborates the authored body once with type and static parameters symbolic. Static
  control flow that depends on them stays as structure (`StaticIf`, `StaticFor`).
- **`Specialize`** re-elaborates the same authored body with the application bound. It exists only
  when selection requires it (a static parameter, or static control flow): the existing
  `selectionReason`. Its body contains no static structure. Every part of the application that can
  change the result participates in identity, including evidence, because a `static if` may select
  on a conformance or on a type argument.
- **Pure type instantiation is not an artifact.** A generic body applied to `T = i32` with nothing
  static to select stays an _instance_: `InstanceKey = (ArtifactId, type substitution)`, handled by
  `Instances` by substitution over the one checked body. It is never re-checked.
- **Provenance never participates.** Two applications whose static arguments are equal values are
  the same artifact regardless of where those values were written.
- **Hidden and generated bodies** are their own artifacts, with their own authored or synthetic
  owner, and `parent` set to the artifact that produced them. The anonymous callable in
  `specialize @f<N = 4>` and the one in `specialize @f<N = 8>` have the same owner and different
  `parent`, so they are different artifacts and never share nodes. A generated effect runner adds
  its own runner key to a synthetic owner under its parent.

**Node references.** Inside an artifact a node is a bare `NodeId` and a local a bare `LocalId`. Any
reference that crosses an artifact boundary is `NodeRef = { artifact: ArtifactId, node: NodeId }`.
There is no global node numbering.

### Request identity versus reuse validity

Two separate things, deliberately:

```text
Request  = { profile/origin, ArtifactId }        finds a candidate. Pure identity; no content.

Validity = { header:   canonical semantic signature of the owner   (stable under alpha-rename)
             body:     authored body fingerprint                     (AuthoredEncoding body bytes)
             scope:    scope signature
             observed: Observation[] }                               recorded while building
```

`BodyQuery.request(request)` looks a candidate up by `Request`, then re-validates its `Validity`
against the current index: each `Observation` (a lookup with its answer, _including misses_; a
callee signature; a conformance answer; an evaluation's key and outcome key) is asked again and must
answer the same. All valid → the candidate object itself is returned. Anything else → rebuild. Equal
bytes alone never authorize reuse. This is the whole boundary: no general query engine, no
invalidation graph. A later engine or disk cache replaces the lookup and the store, not the split.

## Evaluation identity

A reusable representation of a function and a cached execution of that function are different
things. They have different identities and different stores.

```text
Body request        which typed body is needed?
                    { profile/origin, ArtifactId }                       → CheckedBody

Evaluation request  which body is executed, with which values, in which context?
EvaluationKey = { body:        ArtifactId                    the typed body that runs
                  instance:    canonical type substitution and selected evidence of this call,
                               when the body is generic and not already fixed by its application
                  arguments:   canonical value of every parameter, declared order
                  captures:    canonical value of every captured local, capture order
                  context:     compilation and target identity the evaluator can observe }
                                                                          → Outcome
```

Everything that can change the result participates; nothing else does. Call-site provenance, the
caller's identity and source positions are excluded, exactly as they are excluded from `ArtifactId`.
An evaluation never requires another TIR body: `next(1)` and `next(2)` run the _same_ checked body
under two keys.

```silk
static fn next(value: i32) -> i32 { return value + 1 }
```

```text
body        { owner: …/function=next#0, request: Check }        built once
evaluation  { body: ↑, arguments: [Integer(1 : i32)], … }  →  Value Integer(2 : i32)
evaluation  { body: ↑, arguments: [Integer(2 : i32)], … }  →  Value Integer(3 : i32)
```

A `Specialize` artifact already fixes its static arguments, so its evaluation key lists only what
is left: ordinary parameters, captures, instance and context.

**Two kinds of re-entry.** They are different conditions with different answers:

| Re-entered while in progress                                          | Meaning                                                   | Answer                                                                        |
| --------------------------------------------------------------------- | --------------------------------------------------------- | ----------------------------------------------------------------------------- |
| the same `ArtifactId` under **construction**                          | building this body needs this body: an availability cycle | the existing availability-cycle rejection                                     |
| the same `ArtifactId` under **evaluation**, different `EvaluationKey` | ordinary recursion (`fact(n)` calls `fact(n - 1)`)        | legal; bounded by the evaluation depth and step budgets                       |
| the same `EvaluationKey` under evaluation                             | this execution needs its own result                       | a non-terminating evaluation: a semantic rejection, not an availability cycle |

The session keeps two in-progress sets, one keyed by `ArtifactId` and one by `EvaluationKey`.

## Node and table ownership

**Rule:** if lowering or the evaluator needs it to execute the node, it is on the node. A large
payload is stored once in a table and the node holds an explicit reference to it. Anything that only
describes source, explains a decision, or serves an editor is supplementary.

| Decision                                                                                                                                                                                                   | Authoritative home                                                       |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| Which operation this is, fully resolved (`Call` target id, `InterfaceOperationCall` operation + witness, `BuiltinCall`, `Intrinsic`, `Construct` struct id, `Project` field id, variant / enum-member ids) | node                                                                     |
| Result type; operand order; region; place selectors; transfer (`Copy`/`Move`); borrow access                                                                                                               | node                                                                     |
| Every implicit conversion, as its own node (`UnionConvert`, receiver `ValueBorrow`, contextual integer typing, effect join conversion)                                                                     | node                                                                     |
| Call type arguments and substitution                                                                                                                                                                       | node (`Call.typeArguments`) — instances and lowering need them           |
| Selected evidence and provider selections (large)                                                                                                                                                          | `results.evidence[]`; node holds `evidence: EvidenceRef`                 |
| The application a static call selects                                                                                                                                                                      | node (`Call.application: ArtifactId`)                                    |
| Locals: kind, type, mutability, capture source                                                                                                                                                             | `function.locals[]`; nodes hold `LocalId`                                |
| Why a node is unavailable                                                                                                                                                                                  | node is `Unavailable { cause: CauseRef }`; payload in `results.causes[]` |
| A compile-time value the body depends on (selected arm, array length, residual literal)                                                                                                                    | node: it _is_ a literal, a selected branch, a length in a type           |
| Source occurrences for navigation                                                                                                                                                                          | `results.occurrences[]` (supplementary)                                  |
| Lexical scopes for completion                                                                                                                                                                              | `results.scopes[]` (supplementary)                                       |
| Lifetime regions, outlives constraints, borrow origins                                                                                                                                                     | `results.lifetimes` (analysis evidence)                                  |
| Diagnostics                                                                                                                                                                                                | `results.diagnostics[]` (supplementary)                                  |
| What this build depended on                                                                                                                                                                                | `validity.observed[]`                                                    |
| Provenance of static values written in this body                                                                                                                                                           | `results.provenance[]` (supplementary; § Text provenance)                |

There is no `resolutions` table and no `calls` table: the earlier draft put executable decisions in
tables, which would have given a call target two homes.

```text
CheckedBody
├─ id            ArtifactId
├─ validity      Validity
├─ function      TirFunction { contract, regions, locals[], body: Statement[] }
└─ results
   ├─ evidence[]      referenced by nodes          (authoritative payload)
   ├─ causes[]        referenced by nodes          (authoritative payload)
   ├─ occurrences[]   supplementary
   ├─ scopes[]        supplementary
   ├─ lifetimes       analysis evidence
   ├─ provenance[]    supplementary
   └─ diagnostics[]   supplementary
```

Every node is `{ id: NodeId, origin: Origin, … }`; expressions add `type`. `Origin` reuses the
authored vocabulary: `Authored(anchor)` or `Synthetic(anchor, role, occurrence)`.

### Complete input of each consumer

| Consumer                                                               | Complete input                                                                                       |
| ---------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| Analysis (`ExpressionAnalysis`, `StatementAnalysis`, `CallResolution`) | authored declaration, `SemanticContext`, declaration index, scope, `BodyBuilder`, evaluation session |
| `StaticEvaluation`                                                     | typed nodes, a read-only `BodyView` (locals, evidence, causes), an `Environment`, the session        |
| `Residualization`, `ModuleSelection`                                   | a `Request` for `Specialize(application)`; they hold the returned artifact and nothing else          |
| `LifetimeFlow`, `BodyControlFlow`, `TypeOutlives`                      | `function`, `results.lifetimes`                                                                      |
| `Ownership`, `CleanupPlan`, `ResidualOwnership`, `SuspensionOwnership` | `function`, `results.evidence`, declaration index (headers by id)                                    |
| `Instances`, `ExecutableOrigin`, `OpaqueRealization`                   | artifacts by `ArtifactId`, `results.evidence`, declaration index                                     |
| `FunctionLowering`, `Lower`, `EffectLowering`                          | `function`, `results.evidence`, layout; presentation registry only to stamp MIR provenance           |
| `SemanticOccurrence`                                                   | `results.occurrences` of each artifact, header occurrences from the declaration index                |
| `Completion`, `TypeHint`, `InspectorFlowModel`                         | `results.scopes`, `function.locals`, node types, `results.occurrences`                               |
| Publication (`Analysis`, `Frontend`)                                   | `results.diagnostics`, `results.provenance`, rejected evaluation outcomes, presentation registry     |

## Text provenance and diagnostic locations

Two coordinate systems exist and must never be mixed:

- **Value coordinates**: byte offsets into a _decoded_ static value. Slicing, concatenation and
  comparison happen here. They are revision-free.
- **Spelling coordinates**: byte offsets into the _source spelling_ of one literal. `"\x41"` is one
  value byte and four spelling bytes. They belong to presentation.

**Provenance is always in value coordinates.** A static text or bytes value may carry:

```text
Provenance = Segment[]                 ordered; covers the value, gaps allowed
Segment    = { value: Range            where in this value
               from:  Source }
Source     = Literal   { at: Anchor, range: Range }      decoded bytes of the literal at this anchor
           | Parameter { ordinal, range: Range }         decoded bytes of parameter `ordinal`
```

`Literal` names an authored anchor, not a node: an anchor already carries its module and owner, is
revision-free, and is what a presentation resolves. The rule for anything shared between call sites
(an evaluation outcome, a `Specialize` artifact) is: **caller-specific provenance is parameterized;
caller-independent provenance is kept as it is.** A literal written inside the callee's own body is
the same literal for every caller, so its `Literal` segment stays in the shared outcome. A literal
that arrived through an argument is caller-specific and appears only as `Parameter`.

Operations are total and mechanical: **slice** restricts segments to a range and rebases them;
**concat** appends the right operand's segments shifted by the left length; a value that was
computed rather than copied (a formatted number) has no segment for those bytes.

**Composition at a call.** Evaluating `g(a)` inside some body:

1. The outcome of `g` is shared. Its provenance mentions `Parameter`s of `g` and `Literal`s that do
   not depend on the caller.
2. The caller substitutes each `Parameter { ordinal: k, range }` with the segments of argument `k`'s
   provenance restricted to `range`, and **preserves every `Literal` segment unchanged**. The result
   is provenance in the _caller's_ terms.
3. In a `Check` body of an ordinary function that ends at `Literal`s. Inside another shared body it
   may mention that body's own `Parameter`s, resolved by _its_ callers in turn.

**Locations.**

```text
Location = At { anchor: Anchor, edge?: End } a node, or with `edge` the last byte of that node
         | In { parts: Source[],              a value range, as the ordered sources that cover it
                fallback: Anchor }            the reporting node, when no part resolves to a literal
```

`edge` exists because some reported positions have no node of their own: a missing return reports at
a block's closing brace, which is the last byte of the block. It is still relative to a node, so it
names no revision. A range that once ran from one token to another (`Enum.member`, a match arm from
its pattern to its body) is the span of the node that contains both, because presented spans are
trivia-free.

A rejected range can straddle segments (part of a callee literal and part of an argument), so a
value location is a list of sources, not one. `Parameter` sources appear only in `Specialize`
artifacts and in evaluation outcomes.

**Publication** runs once per revision, outside every artifact and outcome:

1. `At` → the presentation's span for the anchor; with `edge`, that span's last byte.
2. A `Literal` part → the presentation of _the anchor's own module_ (which may be the callee's, or a
   standard library module). The literal's presented spelling is decoded once to obtain its
   _spelling map_ (for each decoded byte, its spelling range — what `StaticText.decode` already
   computes as `sourceRanges`). The value range maps to the union of the spelling ranges of its
   bytes, offset by the literal's span start. The map is a function of the spelling, so it lives with
   presentation and is never stored in an artifact.
3. A `Parameter` part → for **each call site** that selected the shared result, substitute that
   call's argument provenance (from the caller's `results.provenance`) and continue with step 2 or,
   if the caller is itself shared, step 3 one level up. A part that cannot be resolved to a literal
   (the argument was computed) falls back to the call node's origin.
4. The first resolved part is the diagnostic's span; the remaining parts are related spans. A
   location with no `Parameter` part is caller-independent and is published once, naming the
   selecting call sites as related positions.

**Behaviour decision.** Today a shared residual failure is reported once, at the first caller in
canonical order. Under this contract every selected call site reports at its own argument. The
failure is one fact about the application; each call that selects it is a distinct authored mistake.
The pinned single-diagnostic expectation in `StaticText.test.ts` changes accordingly.

### Examples

**1. Spelling differs from decoded bytes.**

```silk
fn f() -> i32 { compileError(StaticText.slice("a\x41\u{e9}z", 1, 2)) }
```

```text
literal L   spelling  " a \x41 \u{e9} z "      decoded  61 41 c3 a9 7a   (5 bytes)
spelling map (decoded byte → spelling range, relative to the token)
            0→[1,2)  1→[2,6)  2→[6,12)  3→[6,12)  4→[12,13)

check @f    n2 StaticTextLiteral              provenance [ {value 0..5, from Literal(L, 0..5)} ]
            n1 Call slice(n2, 1, 2)           outcome    [ {value 0..1, from Parameter(0, 1..2)} ]
                                              composed   [ {value 0..1, from Literal(L, 1..2)} ]
            n0 CompileError(n1)               diagnostic SEM0177 at In[Literal(L, 1..2)]
publication Literal(L, 1..2) → spelling [2,6) → the four source bytes `\x41`
```

One decoded byte, four source bytes, and nothing in the artifact knows it.

**2. A substring forwarded through helpers.**

```silk
static fn inner(value: string) -> string { return StaticText.slice(value, 1, 4) }
static fn outer(value: string) -> string { return StaticText.slice(inner(value), 0, 2) }
fn reject(static template: string) -> i32 { compileError(outer(template)) }
pub fn main() -> i32 { return reject("aéz") }        // 61 c3 a9 7a
```

```text
outcome eval(check @inner, ["aéz"]) value "éz"   [ {0..3 from Parameter(0, 1..4)} ]
outcome eval(check @outer, ["aéz"]) inner(value) composed with value=Parameter(0, 0..4)
                                   → [ {0..3 from Parameter(0, 1..4)} ]; slice 0..2
                                   value "é"    [ {0..2 from Parameter(0, 1..3)} ]
artifact specialize @reject<"aéz"> n0 CompileError   diagnostic SEM0177 at In[Parameter(0, 1..3)]
check @main                        n1 Call reject  application = specialize @reject<"aéz">
                                   provenance row  n1.arg0 → [ {0..4 from Literal(M, 0..4)} ]
publication Parameter(0, 1..3) ∘ n1.arg0 → Literal(M, 1..3) → spelling of `é` in main's literal
```

No helper ever saw a source position; each only restricted ranges of its own parameter.

**3. Two calls, one residual, two reports.**

```silk
pub fn main() -> i32 {
  let first = reject("aéz")          // literal M1
  return reject("a\u{e9}z")          // literal M2: same value, different spelling
}
```

```text
both calls select   specialize @reject<"aéz">          one artifact, built once
its diagnostic      SEM0177 at In[Parameter(0, 1..3)]
check @main         n1 Call … provenance arg0 → Literal(M1, 0..4)
                    n4 Call … provenance arg0 → Literal(M2, 0..4)
publication         n1 → Literal(M1, 1..3) → the 2 source bytes `é`
                    n4 → Literal(M2, 1..3) → the 6 source bytes `\u{e9}`
```

Same value, same artifact, same value range; two reports, each at its own spelling.

**4. A literal owned by the callee.**

```silk
static fn tag() -> string { return "prefix" }                    // literal T in tag's body
fn f() -> i32 { compileError(StaticText.slice(tag(), 0, 3)) }
```

```text
outcome eval(check @tag, [])    value "prefix"   [ {0..6 from Literal(T, 0..6)} ]      shared as is
check @f    n1 Call tag         composed         [ {0..6 from Literal(T, 0..6)} ]      nothing to substitute
            n0 CompileError     diagnostic SEM0177 at In[Literal(T, 0..3)]
publication Literal(T, 0..3) → `pre` inside tag's own body, through tag's module presentation
```

**5. Callee literal and argument, mixed.**

```silk
static fn helper(value: string) -> string { return "prefix:" + value }     // literal H
fn g() -> i32 { compileError(StaticText.slice(helper("aéz"), 5, 10)) }     // literal M
```

```text
outcome eval(check @helper, ["aéz"])   value "prefix:aéz"
                                       [ {0..7  from Literal(H, 0..7)},
                                         {7..11 from Parameter(0, 0..4)} ]
check @g    n1 Call helper             composed: Literal kept, Parameter substituted
                                       [ {0..7  from Literal(H, 0..7)},
                                         {7..11 from Literal(M, 0..4)} ]
            slice 5..10                [ {0..2 from Literal(H, 5..7)}, {2..5 from Literal(M, 0..3)} ]
            n0 CompileError            diagnostic SEM0177 at In[Literal(H, 5..7), Literal(M, 0..3)]
publication span `x:` in helper's literal (helper's module presentation);
            related span `aé` in g's literal
```

## Construction, evaluation and publication

Elaboration needs the evaluator _while_ it builds (to select a `static if`, to fix an array length,
to reject a `compileError`). It must not need a finished `CheckedBody` of the body it is building,
and the evaluator must never write into a published one.

**Builder.** One `BodyBuilder` exists per artifact under construction and is private to that build.
It allocates `NodeId`/`LocalId`, appends finished nodes and table rows, and records observations.
Nodes are immutable values from the moment they are created; the builder only appends. It is never
cached, exported or shared between artifacts.

**What the evaluator consumes.** Its complete input is four things:

```text
evaluate(node, view, environment, session) → Outcome

BodyView (read-only)   local(LocalId)       → Local definition (kind, type, mutability, capture source)
                       evidence(EvidenceRef) → selected evidence payload
                       cause(CauseRef)       → unavailability cause
Environment            values of parameters, captures and locals evaluated so far; static bindings
Session                body requests, evaluation outcomes, budgets, the two in-progress sets
```

A node can hold `LocalId`, `EvidenceRef` and `CauseRef`, whose payloads are outside the subtree; the
view is how the evaluator reads them. It is an interface, not another representation. Over a
published artifact the view reads the frozen tables. During construction the builder supplies a view
over its **completed rows only**: rows are appended before any node that references them is created,
so every reference a finished node holds already resolves. The view exposes no way to write, does not
require the surrounding artifact to be finished, and never reaches a row that is still being built.

**Calls during evaluation.** A call to another static function makes two requests: a _body request_
for the callee's artifact (`check @callee`, or `specialize @callee<application>` when selection
requires it), which is published and immutable before any of its nodes run; then an _evaluation
request_ under the `EvaluationKey` built from that artifact and the argument values. A recorded
outcome answers it; otherwise the callee's nodes are evaluated through a view of the callee's
artifact in a fresh environment.

**Where results live.**

| Result                                         | Home                                                                                           |
| ---------------------------------------------- | ---------------------------------------------------------------------------------------------- |
| A value the body under construction depends on | becomes nodes of _that_ artifact before it is finished (selected branch, literal, type length) |
| The outcome of one execution                   | `session.outcomes[EvaluationKey]`: a separate product, never a field of an artifact            |
| What one call site passed                      | the caller's artifact: the `Call` node, and a `results.provenance` row for its arguments       |

So "static evaluation writes statics" means exactly: the evaluator returns values to the builder of
the artifact being built, and records outcomes in the session under evaluation keys. A node never has
different results for different executions, because results are not stored on nodes at all.

**Completed versus aborted.** A result that reports a problem in the source is a _completed_ result.
It is not a failure of the compiler.

|                         | Construction                                                                                                                                           | Evaluation                                                                                                                                     |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| **Completed, accepted** | artifact with no diagnostics; executable                                                                                                               | `Value { value, provenance }`                                                                                                                  |
| **Completed, rejected** | immutable artifact **with** diagnostics; damaged nodes are `Unavailable`, healthy structure is preserved; inspectable by tooling, **never executable** | `Rejected { failure, locations }`: `compileError`, a phase violation, a non-terminating evaluation; budget exhaustion only as § Budgets allows |
| **Aborted**             | cancellation, interruption, an internal defect: **no artifact, no partial artifact**                                                                   | cancellation or a transient or internal failure: **no outcome recorded**                                                                       |

A completed rejection is deterministic for its request, so it is published and recorded like any
other completed result; the residual `compileError` artifacts in the examples above are completed
rejections. An aborted request leaves nothing behind and can simply be asked again.

**Reuse policy is explicit.** Publication and reuse are separate questions; whether a completed,
diagnostic-bearing result may be reused is stated here, as part of `Validity`, not implied.

The authored HIR specification already says: _"Damaged owners MUST NOT be eligible for successful
semantic reuse or selected executable publication."_ This contract keeps that rule and makes its two
halves precise, rather than claiming a behaviour the current implementation has not been shown to
have (`BodyQuery` today applies one validity check and has no recovery-specific rule):

- **Reuse never upgrades a result.** A reused completed rejection is still a rejection. No artifact
  that carries an error diagnostic, an `Unavailable` node or an owner with recovery causes is ever
  admitted as a successfully checked body or selected for executable publication, whether it was
  just built or reused. That is what "successful semantic reuse" forbids, and it stays forbidden.
- **A completed rejection may be reused as a rejection** when everything it was derived from still
  holds: the owner's _canonical authored content_ (header and body encodings, which include the
  recovery causes and the retained healthy structure), its scope signature, and every recorded
  observation.
- **"Identical" means canonical authored content, never source bytes.** A presentation-only edit
  (whitespace, comments, moving the declaration) changes source bytes and leaves authored content
  equal: the rejection is reused and its diagnostics are published at the new positions. Any edit
  that changes the recovery structure changes the authored content.
- **Repair invalidates.** Repairing damaged source changes that owner's authored content, so its
  result fails validation and is rebuilt.
- **Healthy neighbours are independent.** Validity is per owner. An owner whose own content and
  observations still hold is reused regardless of damage elsewhere in the module, unless it observed
  the damaged owner, in which case its observation of that owner is what decides.
- **Nothing is reusable from an aborted request.**

Changing any of this later is a policy change, not a schema change.

**Budgets.** A budget limit is deterministic, but an exhaustion is not always a fact about the
evaluation that hit it. A nested evaluation can run out of what its callers left, and succeed when
asked again with a fresh allowance. The accounting is therefore defined so that neither caching nor
ordering can change what the language accepts:

```text
Allowance   { steps, callDepth, retainedValueBytes, residualNodes }   from the profile's limits
Cost        what one completed evaluation consumed, in the same units; depth relative to its entry
```

- **Root requests get a fresh allowance.** An evaluation requested by elaboration or selection
  starts with the full allowance. The limits and the accounting version are part of
  `EvaluationKey.context`, so they are part of every validated request.
- **Nested requests draw from their root.** They run against what remains.
- **An outcome records its cost.** A `Value` or a semantic `Rejected` recorded under a key means:
  _evaluated to completion, consuming `cost`_. Because accounting is additive and depth is relative,
  that cost is a function of the key alone.
- **A cache hit is charged like an execution.** The requester is charged the recorded cost, and the
  recorded relative depth is checked against the current depth. If what remains cannot pay, the
  requester exhausts exactly as it would have by executing. Cache warmth therefore never changes
  acceptance, only time.
- **Exhaustion is stored only where it is a function of the key.** A _root_ evaluation that exhausts
  a fresh full allowance is a deterministic fact about its key (which includes the limits): it is
  recorded as `Rejected(BudgetExhausted)` and reusable. A _nested_ evaluation that exhausts a
  remainder is a fact about its ancestors' spending: **nothing is recorded under its key**; the
  exhaustion is reported for the current attempt, and it surfaces as the root's outcome.
- **Completion under less implies completion under more.** A nested evaluation that _completes_
  within a remainder would complete identically with a full allowance, so its `Value` and cost are
  recorded normally.

```text
root  eval(@f, …)      fresh allowance 1000; spends 990; calls g
  nested eval(@g, …)   remaining 10; needs 40 → exhausts
                       nothing recorded for g; f is Rejected(BudgetExhausted), recorded for f
later root eval(@g, …) fresh allowance 1000 → Value, cost 40, recorded
later root eval(@h, …) spends 100; calls g → cache hit, charged 40, continues
```

**Publication.** `builder.finish()` validates (dense ids, references in range, closed vocabulary,
`Parameter` sources only where the artifact has parameters to name), freezes, fingerprints through
the canonical codec, and returns the `CheckedBody`. `BodyQuery` stores it under its `Request` only
then.

## Semantic occurrences

An occurrence is a fact about authored source, not about a node. Desugaring can produce several
nodes for one reference (a method call yields a receiver borrow and a call), and many references
never execute (a type annotation, a pattern's nominal, a call's written type argument).

```text
Occurrence = { at:      Anchor            the authored name token itself
               role:    Declaration | Value | Type | Field | Variant | Operation | Method | Import
               target:  Declaration(id) | Local(LocalId) | Field(id) | Variant(id)
                       | EnumMember(id) | ImportNamespace(module, spelling) | Builtin | Intrinsic
                       | Unavailable(CauseRef)
               node?:   NodeId            the primary executable node, when there is one
               import?: Anchor }          the import binding this use spends, by written spelling
```

`results.occurrences[]` is ordered by authored position and holds body-internal occurrences only.
Header occurrences (signature types, bounds, impl heads) belong to the declaration index. It is a
supplementary index: nothing reads it to execute, and no body is reconstructed from it.

## Body categories

| Category  | `ArtifactId`                                     | Notes                                                                           |
| --------- | ------------------------------------------------ | ------------------------------------------------------------------------------- |
| Ordinary  | `{ owner, Check }`                               | generic bodies keep static structure                                            |
| Static    | `{ owner, Check }`                               | `static fn` bodies are published like any other; today they exist only as facts |
| Residual  | `{ owner, Specialize(app) }`                     | no static structure left                                                        |
| Hidden    | `{ callable owner, request of parent, parent }`  | captures are `locals` with a capture source                                     |
| Generated | `{ synthetic owner, request of parent, parent }` | every origin is `Synthetic`                                                     |
| Malformed | any                                              | damaged nodes are `Unavailable`; healthy siblings stay; never executable        |
| Foreign   | none                                             | a contract and no body                                                          |

## Representative artifact

```silk
static fn limit() -> i32 { return 40 }
pub fn answer() -> i32 {
  let base = limit()
  return identity(base + 2)          // identity<T>(value: T) -> T from demo:app/Value
}
```

```text
artifact  { owner: demo:app/Main/function=answer#0, request: Check }
validity  header 9a1c…  body 5e07…  scope 11d2…
          observed  Lookup(scope, "limit")    → demo:app/Main::limit
                    Lookup(scope, "identity") → demo:app/Value::identity
                    Signature(demo:app/Value::identity) → 3f9c…
                    Evaluation({body: {…/function=limit#0, Check}, arguments: []}) → Integer(40 : i32)
function  contract fn() -> i32     regions [r0]
          locals   l0 base : i32 let
          n0 Bind l0 r0                                         origin body#0|statement#0
          n1   IntegerLiteral 40 : i32                          origin Synthetic(…|initializer#0, static-result)
          n2 Return r0                                          origin body#0|statement#1
          n3   Call demo:app/Value::identity<type i32> : i32    origin …|statement#1|value#0
          n4     BuiltinCall i32.Add : i32                      origin …|argument#0
          n5       LocalRead l0 : i32
          n6       IntegerLiteral 2 : i32
results   occurrences  limit@…|initializer#0|callee#0  Value → Declaration(demo:app/Main::limit)
                       base@…|statement#0|name#0       Declaration → Local(l0)
                       identity@…|value#0|callee#0     Value → Declaration(demo:app/Value::identity), node n3
                       base@…|argument#0|left#0        Value → Local(l0), node n5
          scopes       s0 { origin body#0, locals [l0] }
          diagnostics  []
```

The call target and its type arguments are on `n3`. The closed static call became the literal `n1`;
its reference survives as an occurrence with no node. Misspelling `identity` turns `n3` into
`Unavailable { cause: c0 }` with `causes[c0] = UnknownCallee`, an occurrence whose target is
`Unavailable(c0)`, the diagnostic `SEM0004 at At(…|callee#0)`, and the observation
`Lookup(scope, "identit") → Miss`.

## Implementation steps

Each step lands green, deletes what it replaces, and introduces nothing that a later step removes
except where stated.

1. **Locations.** `Location` and `Provenance` in value coordinates; `Diagnostic` generic over its
   location, so header and body products, cause identities and evaluator failures hold `Location`s;
   static text provenance as a literal's anchor plus value offsets; one publication point with the
   spelling map. Stages after TIR keep source coordinates until step 4, reading revision-free causes
   through one interim union. Two things wait for the step that owns them: span-derived identities
   are rebuilt from anchors with the other identities (step 2), and segment composition through
   slice, concat and calls with per-call-site reporting arrives with the node evaluator (step 5),
   because the fact evaluator's single-source side channel is deleted there.
2. **Identities and artifacts.** `ArtifactId`, `Application`, `NodeRef`, `EvaluationKey`; declarations, fields and
   members by id throughout TIR and cached products; `Request` separated from `Validity` in
   `BodyQuery`; a hit returns the cached object. **Delete `SemanticRebinding`.**
3. **The TIR schema.** Node and local ids, origins, resolved operations and explicit conversions on
   nodes, `evidence`/`causes` references, the supplementary tables, the field registry and canonical
   codec; static bodies published; goldens move to the source-free encoding.
4. **Whole-body consumers onto TIR.** Everything that runs on a finished body: `LifetimeFlow`,
   `BodyControlFlow`, `TypeOutlives`, `Ownership`, `OpaqueRealization`, `Instances`, then
   occurrences, completion, type hints, inspector, LSP and docgen callers. After this step only
   analysis and the evaluator read facts.
5. **The evaluator onto nodes.** `StaticEvaluation` interprets typed nodes through a read-only view,
   with an environment and a session; outcomes keyed by `EvaluationKey`, separate in-progress sets for
   construction and evaluation; `Residualization` and `ModuleSelection` request
   `Specialize` artifacts. This works before step 6 because `TirLowering` is already compositional
   per expression: during construction the analysis lowers the subexpression it has just checked and
   hands that node to the evaluator. The evaluator's input does not change again.
6. **Direct construction.** `BodyBuilder` emits nodes and rows from analysis. **Delete
   `FunctionFact`, `StatementFact`, `ExpressionFact`, `TirLowering`** and the fact half of
   `ResidualBody`. The per-expression lowering used in step 5 disappears here because analysis now
   produces those same nodes itself.
7. **Close the milestone.** Codec round-trip and fingerprint fixtures for every body category;
   `structuredClone` equality as a secondary check; spec sync; an inventory check that nothing
   imports a deleted schema.

**Order as implemented.** A cached fact tree embeds the declaration objects of its revision and can
only be reused by rebinding it, so `SemanticRebinding` could not go while facts were the cached
product. It went as soon as they were not: every stage after construction was moved to nodes and
body tables, `BodyQuery` then stored the checked unit of each declaration (its bodies, their tables
and located diagnostics), and `SemanticRebinding` was deleted in that same change. The
`Request`/`Validity` types, node ids and the span-derived identities (`BorrowId`,
`TemporaryOwnerId`, the site ids) still land with the schema in step 3; until then those identities
carry an authored anchor beside the span, which is what lets a cached body be presented again.

**What reuse still does to a cached body.** Two things, both temporary and both named here so they
are not mistaken for the design:

- _Renumbering._ A declaration id is a position in its module, so inserting a declaration renumbers
  the ones after it. A reused body is copied once with each declaration id replaced by its current
  one; a compiler-made body follows its enclosing declaration. Declaration ids keyed by authored
  identity make the copy unnecessary, and the cached object itself is then returned on every hit,
  as the `Validity` section requires.
- _Ownership positions._ Ownership proofs are reported in source coordinates. When a reused body
  only moved, each position of its proof follows the body's own presentation from the previous
  revision to this one; a position the body does not account for means the proof is checked again.
  Ownership results that name authored nodes make this a `present` like any other.

**What construction keeps private.** Analysis reasons about the subexpressions it has just checked:
their resolved references, contracts, written sub-tokens and recovery states. Typed nodes do not carry
all of that, and should not, because none of it is needed to execute or to reuse a body. So analysis
keeps working records while it builds a body. The boundary is strict and is what removes the
duplicate body:

- a working record never leaves construction: `Elaboration.Result` exposes checked bodies and their
  tables, and no stage after construction imports a record type. `Elaboration.records` is the one
  seam, for tests of construction and for the inspector, which shows construction itself;
- nothing caches a record: reuse stores the checked body only;
- everything a later stage or a tool needs is either on a node or in a table the body publishes
  (occurrences, inference rows, scopes and their locals, expression types, lifetimes, opaque-result
  evidence, generated aggregates, static structure, callable flow, diagnostics, provenance). A rule
  that spans bodies, such as the constrained-callable escape rule, is a join over per-body rows, so
  a reused body takes part without being read again;
- the evaluator reads nodes, also while a body is still being built.

**Presentation of a checked body.** Stages after construction still read source coordinates (loan
liveness, MIR provenance, debug locations). A checked body names authored nodes only. Once per
revision `Tir.present` stamps each node's span from its origin through the presentation registry;
the stamped span is presentation state, is never part of the portable schema, and is never read
from a cached body before it is stamped again. This is a pure function of origin and presentation,
which is what replaces `SemanticRebinding`. The milestone is complete only after
step 7. Persistent caching stays later work; what it needs already holds after step 3: a portable
schema, a canonical codec, revision-free content, and validity carried by the artifact.
