## Context

See proposal.md. After `add-inherent-impl-members`, an inherent member is an ordinary hidden
`FunctionDeclaration` fact with `associatedMember: { owner, receiver }`, reachable through
`NameResolution.lookupAssociated`. Interface operations already have a static call surface:
`Interface.op(...)` resolves through `boundOperationReference` in a generic body and through
`resolveAppliedInterfaceOperationTarget` for applied qualifiers, both ending in
`finishInterfaceOperationCall` with a `ConformanceProof` witness. Conformances are provider-local,
so every conformance of a nominal owner is discoverable from the owner's module facts.

`value.member(args)` parses today as `CallExpression(FieldProjectionExpression)`. In
`analyzeExpression`, the callee is analyzed first; a field projection with a callable type reaches
`finishCallableApplication`, and an unknown field reports the field diagnostic. The parser gives
`move` and `&` lower precedence than the projection chain: `move value.map(f)` is
`move (value.map(f))`, so receiver ownership cannot be written in front of a method call.

Argument analysis already adapts a written argument to an expected parameter (shared and exclusive
borrow arguments, moves, and ownership diagnostics) once the expected type is known.

## Goals / Non-Goals

**Goals:**

- One resolution routine for the value side that mirrors `lookupAssociated` on the type side and
  ends in the existing `finishDeclarationCall` / `finishInterfaceOperationCall`.
- Receiver adaptation expressed as "the receiver is argument zero with the declared expected type",
  so the ownership machinery for arguments handles it.

**Non-Goals:**

- Bound method values and their capture semantics.
- Auto-dereference, auto-borrow through `Box` or `Shared`, or any receiver coercion beyond the
  declared mode.
- Runtime interface values or dynamic dispatch.
- Multi-application selection through receiver evidence beyond what `Self` inference already does.

## Decisions

### Method calls are recognized at the call, not at the projection

`analyzeExpression` for a `CallExpression` whose callee is a `FieldProjectionExpression` first
resolves the subject's static type. If the subject's owner has a field of that name, the existing
callable-field path runs unchanged. Otherwise a new `resolveMethodCandidate(subjectType, name)`
runs: inherent receiver method via `lookupAssociated`, else interface candidates. Only when it
returns `Missing` does the existing unknown-field diagnostic fire. A bare `FieldProjectionExpression`
outside callee position never consults members; if the subject has no such field and the owner has
a receiver method of that name, the diagnostic is the dedicated "must be called" one.

Alternative: resolve `value.member` as a bound callable value and let the ordinary call path apply
it. Rejected: it would create the bound-method-value feature by accident, with capture semantics
the proposal explicitly defers.

### The receiver is argument zero

After a candidate is found, the receiver expression is analyzed through the same argument analysis
the explicit form uses, with parameter zero's declared type as the expected type. `&Self` and
`&mut Self` expected types cause the argument analyzer to take the loan it would take for a written
`&place` / `&mut place`; `Self` causes the move it would take for a written `move place` or an
rvalue. No new ownership rule exists; the diagnostics are the ordinary argument diagnostics. This is
why `move value.map(f)` is not needed: the declaration decides.

Alternative: require the explicit form for consuming receivers. Rejected because the parser makes
`(move value).map(f)` the only spelling, which defeats the purpose of the syntax.

### Interface candidates come from proven conformances or bounds, never from a synthesized member

For a concrete nominal receiver, candidates are the receiver operations named `name` across the
owner's coherent conformances (`ConformanceProof` search over the owner's declaring module). For a
type-parameter receiver, candidates are the receiver operations named `name` across the parameter's
declared bounds, reusing `boundOperationReference`. Exactly one candidate finishes through
`finishInterfaceOperationCall` with that witness; zero is `Missing`; more than one is a new
ambiguity diagnostic listing the interfaces. `Owner.op(...)` on the type side uses the same
candidate routine from `lookupAssociated`'s `Missing` branch, restricted to receiver-less
operations for `Owner.op()` without a receiver argument and to receiver operations otherwise.

Precedence is inherent-first because only the owner's module can declare inherent members, so a
dependency cannot change what `value.op()` means for a foreign type.

### Multi-application interfaces resolve only when receiver evidence is decisive

`Self` inference from the receiver operand already exists for explicit calls. The member form
supplies the receiver as the only operand, so an interface with several applications for one
provider is under-determined unless the operation's other arguments fix the application; the
under-determined case reports and points at the applied explicit form. No new inference.

### Presentation substitutes the receiver

Hover and completion on the value side present the member contract with parameter zero removed
and owner binders substituted from the receiver's static type; the type side keeps the complete
contract. Both are presentations of one declaration; navigation and rename already treat the
member as one identity after `add-inherent-impl-members`, and the receiver-syntax occurrence is
recorded with the same identity.

### Receiver-syntax occurrences carry a method role

A receiver-syntax call records its member token with the member's identity and a `Method`
occurrence role; the reference path has no qualifier token because the receiver is a value, and
hover reads the receiver's static type back from the projection at the token to substitute the
owner binders. Completion after `value.` infers the same substitution from the value's type
against parameter zero. A receiver whose type is unknown presents the declared binders.

## Risks / Trade-offs

- [Implicit receiver moves surprise readers used to explicit `move`] → the declared `self: Self`
  is visible in hover and completion labels, the use-after-move diagnostic is the ordinary one,
  and the explicit form stays available; documented in the reference as the one place ownership
  is declared rather than written.
- [Field/method ordering hides a method behind a same-named field on foreign types] → impossible
  for inherent members (rejected at declaration by `add-inherent-impl-members`); for interface
  operations the field wins and the explicit interface form remains.
- [Conformance search per call is costly] → conformances are provider-local facts already indexed
  by owner; memoize candidates per (owner, name) in the resolution seam.
- [Hover substitution needs the receiver's applied type] → the receiver is analyzed before the
  member; when its type is unavailable, fall back to the complete contract.
