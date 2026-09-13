## Why

Generic call inference currently stops when a call parameter is known only through a supplied
provider value and the remaining type or row parameters appear in that provider's interface bound.
This makes ordinary scoped adapters such as
`C: BufferedContext<P, A, E, ?R>` unusable even when `C` has one coherent conformance whose applied
interface arguments determine every omitted parameter; JUL-192 needs this minimal semantic closure
before JUL-201 can carry its affine handler through a nominal context.

## What Changes

- Let a generic call use one unique, visible, coherent conformance of an already-known provider to
  fill unresolved arguments appearing in that provider parameter's direct interface constraint.
- Treat conformance evidence as monotonic supplied-argument constraint evidence: it may fill holes
  but never overwrite an explicit or operand-derived binding, and conflicting or non-unique
  evidence remains a deterministic inference failure.
- Preserve the existing prohibition on expected-result and later-use inference, unknown-provider
  enumeration, inverse row solving, runtime witness dictionaries, and backend polymorphism.
- Add structured semantic coverage and prescriptive language documentation for the rule and its
  ambiguity, conflict, and non-inference boundaries.
- Explicitly keep erased higher-ranked callable storage out of scope. JUL-201 will carry its handler
  through a nominal context interface whose concrete conformance exposes the complete static
  contract.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-type-generics`: extend supplied-argument generic call inference with the unique
  conformance of an already-known provider under a direct applied-interface constraint.

## Impact

- Semantic call inference and conformance-proof integration in the compiler, with no HIR, MIR,
  runtime representation, or backend change.
- Existing structured compiler acceptance tests for generic inference and interface conformance.
- Prescriptive generic-call/interface reference documentation.
- Unblocks the interface-only JUL-192 buffered context acquisition shape required by JUL-201; the
  dependent standard-library and HTTP changes remain in their own OpenSpec changes.
