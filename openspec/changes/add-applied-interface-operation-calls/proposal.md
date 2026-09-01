## Why

An interface operation can currently select an application only indirectly through a generic bound
or by inferring one unique conformance from its operands. When one provider implements multiple
applications of the same interface, callers must introduce a repetitive generic helper solely to
make the intended application explicit. Silk should expose that already-static choice directly.

## What Changes

- Allow a complete interface application to qualify an operation call or callable section, such as
  `Encodable<u32>.encode(&value)` and `&value |> Encodable<u32>.encode`.
- Preserve the token-identical `Path<Arguments>.member` shape neutrally until semantic resolution
  distinguishes an applied interface operation from an applied nominal-union member.
- Resolve the written interface arguments before inferring implicit `Self` from supplied operands or
  one unambiguous fallback generic bound, then select the same coherent static witness used by
  ordinary interface calls. Operand evidence takes precedence and all `Self` occurrences must
  agree.
- Preserve direct-call and pipeline equivalence for ordinary and effectful operations, including
  greedy `run` over an Effect-producing pipeline.
- Reject applied operations whose interface arguments are incomplete or whose provider cannot be
  determined, and keep every failed resolution out of instance discovery and lowering.
- Keep method-style `value.operation()` syntax, implicit borrowing, generated source-visible
  helpers, expected-result conformance selection, applied service-operation selection, and runtime
  interface values out of scope.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Parse and preserve a lossless applied-qualified member, then allow semantic
  resolution to use a complete generic interface application as the qualifier of an operation call
  or callable section without changing nominal-union syntax.
- `bootstrap-type-generics`: Select an explicitly applied interface operation by fixing the written
  interface arguments, inferring `Self` from operands or bounds, and choosing one static witness.
- `bootstrap-hir`: Carry the selected interface application and witness through canonical call and
  pipeline elaboration without introducing a new runtime dispatch form.

## Impact

The change affects expression parsing and formatting, qualified call resolution, generic and
conformance inference, callable-section elaboration, HIR construction, instance discovery, Effect
entry realization, diagnostics, and the prescriptive interface reference. It adds source syntax but
does not change runtime representation, conformance coherence, ownership adaptation, Effect
execution, package dependencies, or backend ABI.
