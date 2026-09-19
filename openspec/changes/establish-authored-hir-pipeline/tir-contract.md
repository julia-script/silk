# TIR contract (A3 / JUL-207 task 3.3)

This document fixes the final shape of the typed intermediate representation before any of it is
built. It replaces two schemas that exist today: the analyzed fact tree (`Elaboration.FunctionFact`,
`StatementFact`, `ExpressionFact`) and the tree `TirLowering` converts it into (`Tir.TirFunction`).
Neither survives as it is. Code is reused where it fits the contract below.

## Why neither current schema is the answer

| | Fact tree today | TIR today |
|---|---|---|
| Who reads it | static evaluator, `LifetimeFlow`, completion, type hints, inspector, occurrences | ownership, instances, MIR lowering |
| What it knows | references, call contracts, static values, scopes, captures | normalized executable operations, regions, borrow identities |
| What ties it to a revision | `DeclarationFact` objects, embedded `SourceSpan`s in diagnostics and identities | `DeclarationFact` objects, a `SourceSpan` on every node |

Both bodies hold objects and offsets that belong to one revision. That is the only reason
`SemanticRebinding` exists: a cached body has to be copied node by node to swap those values. A
body that holds no such value needs no rebinding, and is also the body a later disk cache can store.

## Principles

1. **One body per checked owner.** Elaboration emits TIR directly. There is no second tree and no
   conversion pass.
2. **Plain data only.** A TIR body and its tables contain tagged records, arrays, strings, numbers,
   `bigint`, booleans and `undefined`. No functions, `Map`/`WeakMap`, symbols, object identity, or
   references to `DeclarationFact`, syntax, source or presentation.
3. **References are identities.** A declaration is named by its canonical id, a position by an
   authored anchor, a local by an owner-local ordinal, a type by its structural `Type` value.
4. **No spans.** Every position is an anchor. A span exists only after a presentation resolves it:
   when diagnostics are published, when MIR provenance is written, when an editor asks.
5. **Executable facts live on the node; everything else lives in a table** keyed by node id. Lowering
   reads nodes. Tooling and analysis read tables. A consumer that needs neither pays for neither.
6. **Closed vocabulary with a field registry**, as `AuthoredHir.fields` already is, so canonical
   encoding and a future binary codec are derived from the schema rather than hand-written twice.

## The artifact

```text
CheckedBody                      one per checked owner; the unit BodyQuery caches
├─ owner        AuthoredIdentity              which authored owner this body checks
├─ key          BodyKey                       why this result may be reused (see Reuse)
├─ function     TirFunction                   the one typed body
│  ├─ declaration  CanonicalId | HiddenId     never a DeclarationFact
│  ├─ category     Ordinary | Static | Hidden | Generated | Residual
│  ├─ contract     Contract | Unavailable(cause)
│  ├─ regions      RegionId[]                 acyclic region order
│  ├─ locals       Local[]                    parameters, bindings, pattern bindings, captures
│  └─ body         Statement[]                typed tree; every node has `id` and `origin`
└─ results      BodyResults                   tables keyed by NodeId / LocalId
   ├─ resolutions  NodeId → Resolution        what a name, member, operator or call resolved to
   ├─ calls        NodeId → CallInstance      type arguments, substitution, evidence, mappings
   ├─ statics      NodeId → StaticResult      compile-time value and text origin
   ├─ scopes       Scope[]                    lexical scopes → LocalId[]; parent links
   ├─ lifetimes    LifetimeInput              regions, outlives constraints, borrow origins
   ├─ unavailable  NodeId → Cause             why a node or its contract is not executable
   ├─ diagnostics  BodyDiagnostic[]           located by anchor, never by span
   └─ observed     Observation[]              lookups this body depended on, including misses
```

`NodeId` and `LocalId` are owner-local ordinals assigned in authored evaluation order. They are
stable for an unchanged authored body, which is exactly the reuse condition, and they never appear
outside the owner they belong to.

### Node shape

Every statement and expression is:

```ts
interface Node {
  readonly id: NodeId            // owner-local ordinal, the table key
  readonly origin: Origin        // Authored(anchor) | Synthetic(anchor, role, occurrence)
  readonly type: Type            // expressions only; structural, already canonical
}
```

`Origin` reuses the authored HIR's own origin vocabulary, so a desugared node (an implicit return, a
receiver borrow, a `for` expansion) names the authored position it came from and its role.

The expression and statement vocabularies start from today's TIR operations, because those are the
normalized executable forms lowering already consumes (`Call`, `BuiltinCall`,
`InterfaceOperationCall`, `CallableSection`, `CallableApply`, `Construct`, `Project`, `IndexPlace`,
`ValueBorrow`, `SliceBorrow`, `Match`, `EffectBlock`, `Run`, `Replace`, `Write`, …). Three changes:

- `span` is removed from every node; `origin` replaces it.
- Every embedded `DeclarationFact`, `StructFact`, `FieldFact`, `EnumMemberFact` becomes its id
  (`CanonicalId`, `FieldId`, canonical enum-member id). Identities that were built from a span
  (`BorrowId.callSpan`, `TemporaryOwnerId`, anonymous aggregate identity) are built from the node id.
- The vocabulary gains what only facts could express, so static and tooling consumers need no
  second tree: `CompileError`, `StaticIf`/`StaticFor` (kept as authored structure in generic
  bodies, resolved in residual ones), and `Constant` reads.

### Result tables

| Table | Replaces | Example row |
|---|---|---|
| `resolutions` | `reference` / `target` / `path` fields on identifier, call, member, pattern and type facts | `n7 → Declaration(app/Value::identity)`, `n3 → Local(l0)`, `n9 → Field(app/Main::Counter#0)`, `n4 → Missing(cause SEM0004)` |
| `calls` | `CallContractFact`, `typeArguments`, `substitution`, `evidence`, `inferredProviderSelectors`, `mappings` | `n7 → { typeArguments: [i32], evidence: [], mappings: [p0←a0] }` |
| `statics` | `staticValue`, `staticTextSpan`, `staticTextOrigin`, `StaticText.data` | `n5 → { value: Text("aéz"), origin: Literal(n5, bytes 1..4) }` |
| `scopes` | `LexicalScopeFact` | `s1 → { parent: s0, origin: body#0, locals: [l2, l3] }` |
| `lifetimes` | `FunctionFact.lifetimeFlow.input`, `BodyLifetime` | regions by `NodeId`, `outlives` pairs, borrow roots as `LocalId` paths |
| `unavailable` | `Unavailable` contract reasons and causes scattered through facts | `n4 → UnknownCallee(SEM0004)` |
| `diagnostics` | `Diagnostic.span` / `relatedSpans` inside cached analyses | `{ code: SEM0004, reason, at: Authored(n4.origin), related: [] }` |
| `observed` | `BodyQuery` dependency and negative-lookup records | `Lookup(scope, "missing") → Miss`, `Signature(app/Value::identity) → <key>` |

A text origin is `Literal(node, byteRange)` or `Parameter(ordinal, byteRange)`: a position inside a
literal is an offset relative to that literal's node, resolved to a source offset only when a
diagnostic is published.

### Body categories

| Category | Owner | Produced when | Notes |
|---|---|---|---|
| Ordinary | the function's authored owner | every selected function, generic or not | generic bodies keep `StaticIf`/`StaticFor` as structure |
| Static | same | `static fn` — published like any other body | today these are dropped from TIR and exist only as facts |
| Hidden | the anonymous callable's own authored owner | an anonymous callable is checked | `HiddenId = (enclosing canonical id, site)`; captures are `locals` |
| Generated | synthetic owner under its origin declaration | effect runners, generated aggregates | `origin` is always `Synthetic` |
| Residual | the generic owner plus an application key | a declaration selected by static parameters or static control flow is applied | a new `CheckedBody` from the same authored body with static arguments bound; `StaticIf` resolved, no static structure left |
| Malformed | any | recovery left damage | healthy siblings stay; damaged nodes are `Unavailable` with a row in `unavailable`; never executable |
| Foreign | — | never | a foreign declaration has a contract and no body |

## Representative artifacts

Source (`demo:app/Main`), with `identity` imported from `demo:app/Value`:

```silk
static fn limit() -> i32 { return 40 }
pub fn answer() -> i32 {
  let base = limit()
  return identity(base + 2)
}
```

```text
tir.function demo:app/Main::answer  category=Ordinary
  contract  fn() -> i32
  regions   [r0]
  locals    l0 base : i32  let  origin=body#0|statement#0
  body
    n0 Bind l0 r0                              origin=body#0|statement#0
    n1   Call : i32                            origin=body#0|statement#0|initializer#0
    n2 Return r0                               origin=body#0|statement#1
    n3   Call : i32                            origin=body#0|statement#1|value#0
    n4     BuiltinCall i32.Add : i32           origin=…|arguments-list#0|argument#0
    n5       LocalRead l0 : i32                origin=…|argument#0|left#0
    n6       IntegerLiteral 2 : i32            origin=…|argument#0|right#0

results
  resolutions  n1 → Declaration(demo:app/Main::limit)
               n3 → Declaration(demo:app/Value::identity)
               n4 → Builtin(i32.Add)   n5 → Local(l0)
  calls        n1 → { typeArguments: [], evidence: [], mappings: [] }
               n3 → { typeArguments: [type i32], evidence: [], mappings: [p0←n4] }
  statics      n1 → { value: Integer(40 : i32) }
  scopes       s0 → { origin: body#0, locals: [l0] }
  observed     Lookup(scope, "limit") → demo:app/Main::limit
               Lookup(scope, "identity") → demo:app/Value::identity
               Signature(demo:app/Value::identity) → 3f9c…
  diagnostics  []
```

The same body after `identity` is misspelled as `identit`:

```text
    n3   Unavailable : —                       origin=body#0|statement#1|value#0
results
  resolutions  n3 → Missing
  unavailable  n3 → UnknownCallee
  diagnostics  { code: SEM0004, reason: UnknownFunction("identit"),
                 at: Authored(body#0|statement#1|value#0|callee#0), related: [] }
  observed     Lookup(scope, "identit") → Miss
```

Nothing in either artifact changes when a comment is inserted above `answer`: anchors do not move.
Only the presentation does, and the published diagnostic's span follows it.

A hidden body and its capture:

```silk
fn bump(counter: &mut Counter, by: i32) -> i32 {
  let add = fn(extra: i32) -> i32 { return counter.value + extra }
  return add(by)
}
```

```text
tir.function demo:app/Main::bump  category=Ordinary
  body
    n0 Bind l2 r0
    n1   CallableSection site=0 target=Hidden(demo:app/Main::bump, 0)
           captures [c0 ← ValueBorrow exclusive l0] : fn<'r0>(i32) -> i32
tir.function Hidden(demo:app/Main::bump, 0)  category=Hidden
  owner     demo:app/Main/function=bump#0/callable@anonymous#0
  locals    l0 extra : i32  parameter     l1 counter : &mut Counter  capture c0
```

A residual body. `fn reject(static template: string) -> i32 { compileError(outer(template)) }`
applied to `"aéz"`:

```text
tir.function demo:app/Main::reject  category=Residual
  application  { static: [Text("aéz")], typeArguments: [], evidence: [] }
  body
    n0 CompileError                           origin=body#0|statement#0
results
  statics      n0 → { value: Text("é"), origin: Parameter(0, bytes 1..3) }
  diagnostics  { code: SEM0177, at: TextOrigin(Parameter(0, bytes 1..3)) }
```

The caller's `calls` row for `reject("aéz")` maps parameter 0 to the literal node, so publication
resolves `Parameter(0, 1..3)` to bytes 1..3 of that literal in the caller's presentation.

## How each consumer uses it

| Consumer | Reads today | Reads in the contract |
|---|---|---|
| `ExpressionAnalysis`, `StatementAnalysis`, `CallResolution` | build facts | build TIR nodes and table rows directly through one `BodyBuilder`; they keep their checking logic |
| `TirLowering` | converts facts to TIR | **deleted**; its normalization rules move into the builder |
| `BodyQuery` | caches `{analysis, hidden}` facts, rebinds on reuse | caches `CheckedBody` by `BodyKey`; a hit returns the same object |
| `SemanticRebinding` | copies cached graphs | **deleted** |
| `StaticEvaluation` | interprets `ExpressionFact` | interprets TIR nodes; reads `resolutions`/`calls`; writes `statics` |
| `Residualization`, `ModuleSelection` | re-analyze with a static context, keep `{function, fact}` | request a `Residual` `CheckedBody`; `ResidualBody` holds only that |
| `LifetimeFlow`, `BodyControlFlow`, `TypeOutlives` | walk facts, key points by anchor | walk TIR; points are `NodeId`s; input is `results.lifetimes` |
| `Ownership`, `CleanupPlan`, `ResidualOwnership`, `SuspensionOwnership` | TIR plus fact lookups | TIR only; their output stays a separate product keyed by `NodeId` |
| `Instances`, `ExecutableOrigin`, `OpaqueRealization` | TIR plus `FunctionFact` lookups, `DeclarationFact` objects | TIR and ids; declarations resolved through `DeclarationIndex` when their header is needed |
| `FunctionLowering`, `Lower`, `EffectLowering`, MIR | TIR with spans | TIR with origins; provenance spans resolved through the presentation registry while lowering |
| `SemanticOccurrence` | walks facts and headers | walks TIR; one occurrence per `resolutions` row, positioned at the row's node origin |
| `Completion`, `TypeHint`, `InspectorFlowModel` | scopes, bindings and types from facts | `scopes`, `locals`, node `type`, `calls` |
| LSP `Document`, docgen `Project` | occurrences, presentations, diagnostics | unchanged surface; diagnostics arrive already published |
| `Analysis`, `Frontend` | assemble results and diagnostics | publish diagnostics once per revision: `BodyDiagnostic` + presentation registry → `Diagnostic` |

### Diagnostics

A body produces `BodyDiagnostic { code, reason, at: Location, related: [{label, at}] }`, where
`Location` is `Authored(anchor)`, `Synthetic(anchor, role)` or `TextOrigin(origin)`. The message
is derived from the reason. `Diagnostic` with a `SourceSpan` remains the public type; it is produced
in one place, when a snapshot is assembled for a revision, by resolving each location through the
presentation registry. A reused body therefore reports at the current position with no copying.

## Reuse

```text
BodyKey = { origin/profile, owner identity,
            canonical semantic signature,        (unchanged by an authored alpha-rename)
            authored body fingerprint,           (AuthoredEncoding body bytes)
            scope signature,
            observed[]  re-validated against the current index, misses included }
```

A hit returns the cached `CheckedBody` itself. Equal authored bytes alone never authorize reuse;
`observed` is what makes a new caller, a changed exported bound, a repaired missing member or an SCC
change recompute. The existing `ProjectAnalysis` witnesses keep their meaning and become simpler:
object identity of the body is the reuse witness, and a counter on the builder is the recompute
witness.

Persistent caching stays out of this milestone. What this milestone guarantees for it: a
`CheckedBody` round-trips through `structuredClone` and through its canonical encoding unchanged,
contains nothing that names a revision, and carries its own validity conditions in `key`.

## Implementation steps

Each step lands green, deletes what it replaces, and leaves no adapter behind it.

1. **Locations instead of spans in body products.** `BodyDiagnostic` and `TextOrigin` by anchor;
   one publication point; identities built from spans rebuilt from anchors. Removes the span half of
   rebinding.
2. **Identities instead of declaration objects.** TIR, call targets, struct/field/enum references
   and hidden ids become ids; consumers resolve headers through `DeclarationIndex`. `BodyQuery`
   reuses by key and returns the cached object. **Delete `SemanticRebinding`.**
3. **Node ids, origins and tables on TIR.** Introduce `NodeId`/`LocalId`, the field registry and
   `BodyResults`; publish `Static` bodies; canonical source-free `Tir.encode`; goldens move to it.
4. **Analysis consumers onto TIR.** `StaticEvaluation`, `Residualization`, `ModuleSelection`, then
   `LifetimeFlow`, `BodyControlFlow`, `TypeOutlives`, then `Ownership` and `OpaqueRealization`.
5. **Tooling onto TIR.** `SemanticOccurrence`, `Completion`, `TypeHint`, `InspectorFlowModel`,
   `ModuleTooling`, LSP and docgen callers.
6. **Elaboration emits TIR.** `BodyBuilder` replaces fact construction in the three analysis
   modules. **Delete `FunctionFact`, `StatementFact`, `ExpressionFact`, `TirLowering`** and the
   `fact` half of `ResidualBody`.
7. **Close the milestone.** Round-trip and encoding fixtures for `CheckedBody`; spec sync; inventory
   check that no module imports a deleted schema; archive-ready tasks.

Steps 1–2 remove rebinding. Step 6 removes the duplicate body. The milestone is complete only after
step 7.
