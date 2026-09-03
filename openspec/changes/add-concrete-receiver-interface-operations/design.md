## Context

See `proposal.md` for motivation. Three spellings already produce one `ResolvedInterfaceOperation`
reference and one `Hir.InterfaceOperationCall` node:

- a bounded generic receiver, `value.print()`, resolved in `ExpressionAnalysis.resolveMethodCandidate`
  from the receiver parameter's `ResolvedBound` list;
- the bare bound qualifier, `Printable.print(value)`, resolved in
  `CallResolution.boundOperationReference`;
- the applied qualified call, `Encodable<u32>.encode(&schema)`, resolved in
  `ExpressionAnalysis.resolveAppliedInterfaceProvider`.

Only the third proves conformance for a concrete provider; the first two carry a type parameter as
`provider` and defer the proof to specialization. A concrete nominal receiver reaches none of them:
`resolveMethodCandidate` consults `NameResolution.lookupAssociated`, which searches inherent
`impl Owner { ... }` members only, and falls through to `Missing`. `analyzeMethodCall` turns
`Missing` into the callable-field retry, which ends in `unknownProjectedField` (`SEM0027`).

Everything the concrete receiver needs therefore exists: `ConformanceProof.prove` for the proof,
`DeclarationFacts.interfaceApplication` for the substituted contract,
`CallResolution.interfaceOperationContract` for parameters and result, `synthesizeReceiver` for
adaptation, and `finishInterfaceOperationCall` for the shared fact. This change is a resolution-layer
addition, not a new representation.

## Goals / Non-Goals

**Goals:**

- Give a concrete receiver call the same witness, adaptation, specialization, failure channel, and
  requirement row as the qualified call it replaces.
- Keep candidate selection independent of arguments, expected result, source order, and declaration
  order, so a call either names one operation or is reported.
- Keep the lookup lexically bounded, so an interface the caller cannot name never contributes.
- Reuse one candidate query for resolution and for every tooling surface.

**Non-Goals:**

- Interface-backed bound method values (`let f = value.operation`), which need witness-carrying
  callable semantics. `JUL-92` owns that.
- Open applied nominal receivers needing conditional conformance proof from enclosing generic
  evidence.
- Wrapper dereference, receiver coercion, specialized inherent implementation heads, and runtime
  interface dispatch.
- Operations declaring their own type parameters, which remain unavailable through every spelling.
- Hover on an interface operation token, which returns no subject today for the already-shipped
  bounded-generic spelling as well. That gap is not specific to a concrete receiver, and fixing it
  for one spelling only would leave the two inconsistent. Hover on the receiver itself already
  reports the implemented contracts.
- A qualified spelling for a zero-type-parameter interface at a concrete call site. `Printable.print`
  resolves only inside a body bounded by `Printable`, and `Printable<>` does not parse, so an
  ambiguity between two such interfaces has no qualified recovery. This predates the change and
  belongs to the applied-interface-call capability.

## Decisions

### Reuse `ConformanceProof.implementedContracts` as the one candidate authority

`implementedContracts(index, requestingModule, provider)` already answers exactly the question this
feature asks, and it already enforces every filter the acceptance requires: the provider must be
nominal and `Type.isRuntimeConcrete`; the conformance must be `ValidConformance`, `Coherent`, and
`Terminating`; the ordinary `prove` must select that exact source declaration; the interface contract
must be `endpointVisible` from the requesting module; and `Copy` and `Drop` capabilities are excluded
by construction. It returns capabilities in a deterministic key order.

Making it the shared authority is what keeps resolution, completion, hover, identity, navigation, and
rename on one filter. Writing a second traversal over `index.modules[].conformances` was rejected:
two traversals drift, and the drift is invisible until an editor offers a member the resolver then
rejects.

The function is today only an editor query, so promoting it to a resolution authority is a real
change in its status. Its doc comment says so explicitly, and it is already written as an authority
rather than a heuristic: the comment states that matching a declared header is insufficient and that
the ordinary proof must select the exact declaration.

### Bound visibility by the interface declaration, not by the conformance

A conformance carries no module visibility. `ConformanceFact.visibility` is the literal `'Public'`,
because coherence is a whole-program property: a conformance is visible wherever its provider and
interface are. There is consequently no orphan rule to add, and global coherence already guarantees
that one provider/capability pair has at most one selectable witness.

The lexical bound the proposal requires is therefore the _interface_ declaration's visibility, which
`endpointVisible` applies to the contract returned by `contractByCapability`. A private interface in
another module contributes nothing, so an unrelated dependency cannot silently add or ambiguate a
receiver call by declaring a conformance the caller cannot name.

This is a narrower and more accurate rule than "interface applications semantically visible in the
caller's module," which the issue's wording suggested. Filtering conformances by module would
contradict the coherence model and would make the same program mean different things in different
modules.

### Place the fallback after every inherent outcome, not after `Missing` alone

The fallback runs only where `resolveMethodCandidate`'s nominal branch currently falls through to
`Missing` — that is, after `lookupAssociated` returns neither `Inherent`, `Inaccessible`, nor
`Duplicate`. An inaccessible or duplicate inherent member keeps failing as it does today.

This is what preserves the acceptance requirement that inherent results win or fail first. A private
inherent `print` must not become callable because the type also conforms to `Printable`, and a
duplicate inherent declaration must not have its diagnostic replaced by a successful interface call.
Reordering the fallback ahead of those outcomes would silently change which declaration a call means.

The callable-field precedence is untouched: it is tested before `resolveMethodCandidate` runs at all.
A receiver-less associated function keeps reporting `SEM0198`, because `NoReceiver` is an `Inherent`
outcome reached before the fallback.

### Return a new `Conformance` candidate rather than widening `Bound`

`resolveMethodCandidate` gains a `Conformance` variant carrying the finished
`ResolvedInterfaceOperation` reference with a concrete `provider`. It is kept distinct from `Bound`
because the two differ in proof status and in what the ambiguity diagnostic must say: `Bound` names
the type parameter's bounds, while `Conformance` names applied interfaces. `analyzeMethodCall` routes
both to `finishInterfaceOperationCall`, so they converge immediately after selection.

Reusing `Bound` was rejected because it would force one diagnostic to describe two different
ambiguities, and because `Bound`'s `provider` is a type parameter by construction.

### Diagnose ambiguity with a dedicated reason before argument analysis

`implementedContracts` returning two capabilities that both supply the operation is an ambiguity the
call cannot resolve, because arguments must not select an overload. It is reported at the member
token before argument checking, naming each qualified alternative so the author can write one.

`SEM0200` is the existing receiver-side ambiguity code and is reused, but its current message is
bound-specific ("declared by more than one bound of `T`"). The message and its structured reason gain
a concrete-receiver shape naming the receiver type and the applied interfaces. Keeping one code for
"this receiver call names more than one operation" matches how the code already reads to an author,
and the generated catalog gates the wording.

### Select the operation on the applied capability, and reject own-binder operations

For each visible implemented capability, the operation is looked up through
`DeclarationFacts.interfaceApplication(contract, capability, provider)` and
`CallResolution.interfaceOperationContract`, which is the same pair the qualified path uses after it
infers `Self`. Because `provider` is already concrete here, no `Self` inference is needed and the
`SEM0099`/`SEM0100` inference failures of the qualified form cannot arise.

`interfaceOperationContract` returns `undefined` for an operation declaring its own type parameters,
so those operations stay unavailable through the receiver spelling exactly as they are through the
other two, with no additional check.

An operation whose `receiverAccess` is `Unavailable` — one with no operand of the provider type — is
not a receiver operation and is filtered out, mirroring the generic-receiver branch. That is what
keeps a receiver-less interface operation from being callable on a value.

### Let the existing adaptation supply the loan or move

`analyzeMethodCall` already adapts the receiver against parameter zero of the selected candidate's
parameter list, and for an interface candidate that list is the applied contract's operand types with
`Self` substituted to the concrete provider. `synthesizeReceiver` therefore produces the shared loan,
exclusive loan, or move without any interface-specific branch.

This is the one place the receiver spelling is genuinely more convenient than the qualified form,
which requires the author to write `&`, `&mut`, or `move`. Because the adaptation is the same code
the inherent path uses, `&mut` on a non-`mut` binding, a conflicting loan, and a use-after-move all
report their ordinary diagnostics at the ordinary spans.

### Prove the change at the resolution tier and add no native leg

`Conformance` and the shipped `Bound` candidate converge immediately on one
`finishInterfaceOperationCall`, producing the same `ResolvedInterfaceOperation` reference and the
same `InterfaceOperationCall` node, and the receiver is adapted by the one existing
`synthesizeReceiver`. No production line below resolution changes, so a backend cannot disagree with
the evaluator for a reason this change introduced, and the existing applied-interface corpus entry
already runs an effectful interface operation natively.

The whole feature is therefore falsifiable at the analysis and evaluator tier, which is where its
coverage lives. Adding a differential-corpus program would buy a native compile-link-execute per run
for a claim the diff cannot break, against the repository rule that a native leg is for genuinely
target-specific lowering.

## Risks / Trade-offs

**Adding a member to every conforming type.** A concrete receiver now sees operations it did not see
before, so a name that was an error can become a call. It cannot change the meaning of an existing
successful call, because the fallback runs only where lookup previously failed outright. The
visibility bound keeps the new names to interfaces the caller can already name.

**`implementedContracts` cost on a failing lookup.** The query runs on every receiver call whose
member is not an inherent association — today an immediate error path. It iterates all modules'
conformances and calls `prove` per candidate. `prove` is memoized per `Index`, and the query runs
only after the inherent lookup has already failed, so it is off the successful-call path entirely.
If it ever shows up, the fix is memoizing the query per `(Index, module, provider)` rather than
narrowing the filter.

**Promoting an editor query to a resolution authority.** A future change to
`implementedContracts` for a tooling reason would now change what compiles. Its doc comment records
that it is the shared authority so that a later editor-shaped change is not made in ignorance of the
resolution contract.
