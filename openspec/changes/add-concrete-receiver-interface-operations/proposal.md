## Why

A generic receiver bounded by an interface already calls that interface's operations through
receiver syntax, and a concrete receiver can reach the same statically selected operation through
the qualified form `Printable.print(&document)`. Only the receiver spelling on a concrete value is
rejected, so `document.print()` reports an unknown member while the operation it names is fully
determined at compile time.

That asymmetry costs authors the receiver spelling exactly where a pipeline reads worst. Defining an
effectful pull `Stream` forces `Stream<u32, (), ()>.take(&mut range)` in place of `range.take()`,
repeating the applied interface and the receiver adaptation at every step of a chain whose whole
value is chaining. The operation, witness, and lowering are identical either way; only the surface
differs.

## What Changes

- Let a runtime-concrete nominal receiver call a receiver-bearing operation supplied by exactly one
  proved, coherent, source-visible interface application, selecting the same static witness and HIR
  the qualified call selects.
- Keep the existing lookup order intact: a callable field, then every inherent association outcome
  including inaccessible, duplicate, and receiver-less results. Interface fallback runs only when
  the inherent name is genuinely absent.
- Adapt the receiver through the one existing `Self`/`&Self`/`&mut Self` adaptation, so the receiver
  spelling gains the implicit loan or move the qualified form makes the author write.
- Diagnose two or more applicable applications before argument checking, naming the qualified
  alternatives; arguments, expected result, source order, and declaration order never select among
  candidates.
- Reject naming a supplied operation as a value with a diagnostic that says it must be called,
  rather than claiming the receiver has no such member.
- Offer the same candidates in completion under the resolver's own visibility, proof, precedence,
  and ambiguity filter, and resolve semantic identity and navigation to the selected operation.
- Reverse the prescriptive rule that a concrete receiver never reaches an interface operation.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-method-calls`: admit one proved, visible interface application as the receiver-syntax
  fallback after inherent lookup, with its precedence and ambiguity rules.
- `bootstrap-semantic-facts`: record the selected conformance on a concrete receiver call so
  identity and navigation resolve to the operation the call selected.
- `language-server-completion`: offer a concrete receiver's uniquely supplied interface operations
  under the resolver's own filter.

## Impact

The change is confined to receiver-call resolution and the tooling that reads its facts. It reuses
the existing conformance proof, interface application, operation contract, receiver adaptation, and
`InterfaceOperationCall` lowering unchanged, so HIR, MIR, ownership, evaluation, and both backends
need no new representation. It adds no runtime dispatch, dictionary, dereference, receiver coercion,
compiler-known standard-library actor, or external dependency.
