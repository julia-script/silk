# TIR contract (A3 / JUL-207 task 3.3)

This document fixes the final shape of the typed intermediate representation before any of it is
built. It replaces two schemas that exist today: the analyzed fact tree (`Elaboration.FunctionFact`,
`StatementFact`, `ExpressionFact`) and the tree `TirLowering` converts it into (`Tir.TirFunction`).
Neither survives as it is. The design follows the intended architecture and a later self-hosted
implementation, not the size of the change.

## Why neither current schema is the answer

| | Fact tree today | TIR today |
|---|---|---|
| Who reads it | static evaluator, `LifetimeFlow`, completion, type hints, inspector, occurrences | ownership, instances, MIR lowering |
| What it knows | references, call contracts, static values, scopes, captures | normalized executable operations, regions, borrow identities |
| What ties it to a revision | `DeclarationFact` objects, embedded `SourceSpan`s in diagnostics and identities | `DeclarationFact` objects, a `SourceSpan` on every node |

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
  static to select stays an *instance*: `InstanceKey = (ArtifactId, type substitution)`, handled by
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
against the current index: each `Observation` (a lookup with its answer, *including misses*; a
callee signature; a conformance answer; an application's outcome key) is asked again and must
answer the same. All valid → the candidate object itself is returned. Anything else → rebuild. Equal
bytes alone never authorize reuse. This is the whole boundary: no general query engine, no
invalidation graph. A later engine or disk cache replaces the lookup and the store, not the split.

## Node and table ownership

**Rule:** if lowering or the evaluator needs it to execute the node, it is on the node. A large
payload is stored once in a table and the node holds an explicit reference to it. Anything that only
describes source, explains a decision, or serves an editor is supplementary.

| Decision | Authoritative home |
|---|---|
| Which operation this is, fully resolved (`Call` target id, `InterfaceOperationCall` operation + witness, `BuiltinCall`, `Intrinsic`, `Construct` struct id, `Project` field id, variant / enum-member ids) | node |
| Result type; operand order; region; place selectors; transfer (`Copy`/`Move`); borrow access | node |
| Every implicit conversion, as its own node (`UnionConvert`, receiver `ValueBorrow`, contextual integer typing, effect join conversion) | node |
| Call type arguments and substitution | node (`Call.typeArguments`) — instances and lowering need them |
| Selected evidence and provider selections (large) | `results.evidence[]`; node holds `evidence: EvidenceRef` |
| The application a static call selects | node (`Call.application: ArtifactId`) |
| Locals: kind, type, mutability, capture source | `function.locals[]`; nodes hold `LocalId` |
| Why a node is unavailable | node is `Unavailable { cause: CauseRef }`; payload in `results.causes[]` |
| A compile-time value the body depends on (selected arm, array length, residual literal) | node: it *is* a literal, a selected branch, a length in a type |
| Source occurrences for navigation | `results.occurrences[]` (supplementary) |
| Lexical scopes for completion | `results.scopes[]` (supplementary) |
| Lifetime regions, outlives constraints, borrow origins | `results.lifetimes` (analysis evidence) |
| Diagnostics | `results.diagnostics[]` (supplementary) |
| What this build depended on | `validity.observed[]` |
| Provenance of static values written in this body | `results.provenance[]` (supplementary; § Text provenance) |

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

| Consumer | Complete input |
|---|---|
| Analysis (`ExpressionAnalysis`, `StatementAnalysis`, `CallResolution`) | authored declaration, `SemanticContext`, declaration index, scope, `BodyBuilder`, evaluation session |
| `StaticEvaluation` | typed nodes, an `Environment` (static bindings, local values), the evaluation session |
| `Residualization`, `ModuleSelection` | a `Request` for `Specialize(application)`; they hold the returned artifact and nothing else |
| `LifetimeFlow`, `BodyControlFlow`, `TypeOutlives` | `function`, `results.lifetimes` |
| `Ownership`, `CleanupPlan`, `ResidualOwnership`, `SuspensionOwnership` | `function`, `results.evidence`, declaration index (headers by id) |
| `Instances`, `ExecutableOrigin`, `OpaqueRealization` | artifacts by `ArtifactId`, `results.evidence`, declaration index |
| `FunctionLowering`, `Lower`, `EffectLowering` | `function`, `results.evidence`, layout; presentation registry only to stamp MIR provenance |
| `SemanticOccurrence` | `results.occurrences` of each artifact, header occurrences from the declaration index |
| `Completion`, `TypeHint`, `InspectorFlowModel` | `results.scopes`, `function.locals`, node types, `results.occurrences` |
| Publication (`Analysis`, `Frontend`) | `results.diagnostics`, `results.provenance`, application outcomes, presentation registry |

## Text provenance and diagnostic locations

Two coordinate systems exist and must never be mixed:

- **Value coordinates**: byte offsets into a *decoded* static value. Slicing, concatenation and
  comparison happen here. They are revision-free.
- **Spelling coordinates**: byte offsets into the *source spelling* of one literal. `"\x41"` is one
  value byte and four spelling bytes. They belong to presentation.

**Provenance is always in value coordinates.** A static text or bytes value may carry:

```text
Provenance = Segment[]                 ordered; covers the value, gaps allowed
Segment    = { value: Range            where in this value
               from:  Source }
Source     = Literal   { at: Anchor, range: Range }      decoded bytes of the literal at this anchor
           | Parameter { ordinal, range: Range }         decoded bytes of static parameter `ordinal`
```

`Literal` names an authored anchor, not a node: an anchor already carries its module and owner, is
revision-free, and is what a presentation resolves. `Parameter` is relative to the enclosing
application's parameters and is the only form a shared result may contain.

Operations are total and mechanical: **slice** restricts segments to a range and rebases them;
**concat** appends the right operand's segments shifted by the left length; a value that was
computed rather than copied (a formatted number) has no segment for those bytes.

**Composition at a call.** Evaluating `g(a)` inside some body:

1. The callee's outcome (§ Evaluation) is shared and its provenance mentions only `Parameter`.
2. The caller substitutes: each `Parameter { ordinal: k, range }` becomes the segments of argument
   `k`'s provenance restricted to `range`. The result is provenance in the *caller's* terms.
3. In a `Check` body that ends at `Literal`s. In a `Specialize` body it may still mention the
   caller's own `Parameter`s, and is resolved by *its* callers in turn.

**Locations.**

```text
Location = At        { origin: Origin }                       a node or authored position
         | Within    { at: Anchor, range: Range }             value range inside one literal
         | Through   { ordinal, range: Range }                value range inside a static parameter
```

`Through` appears only in artifacts and outcomes that are shared across call sites.

**Publication** runs once per revision, outside every artifact:

1. `At` → the presentation's span for the origin.
2. `Within` → the literal's presented spelling is decoded once to obtain its *spelling map* (for
   each decoded byte, its spelling range — what `StaticText.decode` already computes as
   `sourceRanges`). The value range maps to the union of the spelling ranges of its bytes, offset by
   the literal's span start. The map is a function of the spelling, so it lives with presentation and
   is never stored in an artifact.
3. `Through` → for **each call site** that selected the application, substitute that call's argument
   provenance (from the caller's `results.provenance`) and continue with step 2 or, if the caller is
   itself shared, step 3 one level up. A location that cannot be resolved to a literal (the argument
   was computed) falls back to the call node's origin.

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
            n0 CompileError(n1)               diagnostic SEM0177 at Within(L, 1..2)
publication Within(L, 1..2) → spelling [2,6) → the four source bytes `\x41`
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
outcome specialize @inner<"aéz">   value "éz"   [ {0..3 from Parameter(0, 1..4)} ]
outcome specialize @outer<"aéz">   inner(value) composed with value=Parameter(0, 0..4)
                                   → [ {0..3 from Parameter(0, 1..4)} ]; slice 0..2
                                   value "é"    [ {0..2 from Parameter(0, 1..3)} ]
artifact specialize @reject<"aéz"> n0 CompileError   diagnostic SEM0177 at Through(0, 1..3)
check @main                        n1 Call reject  application = specialize @reject<"aéz">
                                   provenance row  n1.arg0 → [ {0..4 from Literal(M, 0..4)} ]
publication Through(0, 1..3) ∘ n1.arg0 → Within(M, 1..3) → spelling of `é` in main's literal
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
its diagnostic      SEM0177 at Through(0, 1..3)
check @main         n1 Call … provenance arg0 → Literal(M1, 0..4)
                    n4 Call … provenance arg0 → Literal(M2, 0..4)
publication         n1 → Within(M1, 1..3) → the 2 source bytes `é`
                    n4 → Within(M2, 1..3) → the 6 source bytes `\u{e9}`
```

Same value, same artifact, same value range; two reports, each at its own spelling.

## Construction, evaluation and publication

Elaboration needs the evaluator *while* it builds (to select a `static if`, to fix an array length,
to reject a `compileError`). It must not need a finished `CheckedBody` of the body it is building,
and the evaluator must never write into a published one.

**Builder.** One `BodyBuilder` exists per artifact under construction and is private to that build.
It allocates `NodeId`/`LocalId`, appends finished nodes and table rows, and records observations.
Nodes are immutable values from the moment they are created; the builder only appends. It is never
cached, exported or shared between artifacts.

**What the evaluator consumes.** `evaluate(node, environment, session) → Outcome`. A finished typed
node (a subtree the builder has already produced), an `Environment` (the static bindings of a
`Specialize` request and the values of locals evaluated so far), and the session. It reads node
fields only. It never takes a `CheckedBody` as input for the body being built.

**Calls during evaluation.** A call to another static function asks the session for
`specialize @callee<application>` (or `check @callee` when nothing is static). That is a *different*
artifact; the session builds or reuses it through `BodyQuery` and it is published and immutable
before its nodes are evaluated in a fresh environment frame. The session owns the in-progress set by
`ArtifactId`, which is where recursion limits, budgets and availability cycles already live.

**Where results live.**

| Result | Home |
|---|---|
| A value the body under construction depends on | becomes nodes of *that* artifact before it is finished (selected branch, literal, type length) |
| The outcome of an application (`value`, provenance in `Parameter` terms, or failure with `Through` locations) | `session.outcomes[ArtifactId]`: a separate product keyed by artifact identity, never a field of the artifact |
| What one call site passed | the caller's artifact: the `Call` node's `application`, and a `results.provenance` row for its static arguments |

So "static evaluation writes statics" means exactly: the evaluator returns values to the builder of
the artifact being built, and records application outcomes in the session. One node never has
different results for different applications, because different applications are different
artifacts with their own nodes.

**Publication.** `builder.finish()` validates (dense ids, references in range, closed vocabulary,
no `Through` outside a `Specialize` artifact), freezes, fingerprints through the canonical codec, and
returns the `CheckedBody`. `BodyQuery` stores it under its `Request` only then. A failed or
interrupted build publishes nothing and leaves no reusable outcome.

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

| Category | `ArtifactId` | Notes |
|---|---|---|
| Ordinary | `{ owner, Check }` | generic bodies keep static structure |
| Static | `{ owner, Check }` | `static fn` bodies are published like any other; today they exist only as facts |
| Residual | `{ owner, Specialize(app) }` | no static structure left |
| Hidden | `{ callable owner, request of parent, parent }` | captures are `locals` with a capture source |
| Generated | `{ synthetic owner, request of parent, parent }` | every origin is `Synthetic` |
| Malformed | any | damaged nodes are `Unavailable`; healthy siblings stay; never executable |
| Foreign | none | a contract and no body |

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
                    Outcome({demo:app/Main/function=limit#0, Check}) → Integer(40 : i32)
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

1. **Locations and provenance.** `Location`, `Provenance` in value coordinates, composition at
   calls, one publication point with the spelling map, per-call-site reporting. `Diagnostic` becomes
   generic over its location so body products hold `Location`s. Span-derived identities rebuilt from
   anchors.
2. **Identities and artifacts.** `ArtifactId`, `Application`, `NodeRef`; declarations, fields and
   members by id throughout TIR and cached products; `Request` separated from `Validity` in
   `BodyQuery`; a hit returns the cached object. **Delete `SemanticRebinding`.**
3. **The TIR schema.** Node and local ids, origins, resolved operations and explicit conversions on
   nodes, `evidence`/`causes` references, the supplementary tables, the field registry and canonical
   codec; static bodies published; goldens move to the source-free encoding.
4. **Whole-body consumers onto TIR.** Everything that runs on a finished body: `LifetimeFlow`,
   `BodyControlFlow`, `TypeOutlives`, `Ownership`, `OpaqueRealization`, `Instances`, then
   occurrences, completion, type hints, inspector, LSP and docgen callers. After this step only
   analysis and the evaluator read facts.
5. **The evaluator onto nodes.** `StaticEvaluation` interprets typed nodes with an environment and a
   session; outcomes keyed by `ArtifactId`; `Residualization` and `ModuleSelection` request
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

Steps 1–2 remove rebinding. Step 6 removes the duplicate body. The milestone is complete only after
step 7. Persistent caching stays later work; what it needs already holds after step 3: a portable
schema, a canonical codec, revision-free content, and validity carried by the artifact.
