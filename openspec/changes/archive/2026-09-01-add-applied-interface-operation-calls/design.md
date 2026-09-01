## Context

See [proposal.md](proposal.md) for motivation. The current parser attaches generic arguments after a
completed callee, as in `Actor.operation<T>(...)`; its only representation for arguments applied to
the qualifier before `.member` eagerly classifies the shape as a nominal-union member. Applied
interface operations and generic union members such as `Option<i32>.None` are token-identical until
the owner declaration resolves. Bound-operation resolution currently starts from a
bare interface qualifier, finds one matching bound, and produces a resolved reference containing
the capability, provider, operation contract, and result. Expression analysis then checks the call
and allocates an Effect witness site when required.

The compiler already has the downstream pieces this change should reuse: an applied interface
operation fact containing capability and provider, coherent conformance proof, executable witness
discovery, and static witness lowering. The observed `Effect entry lowering lost its constructor or
runner` failure shows that the currently recovered `Interface<Arguments>.operation` syntax can
escape the frontend without all of those facts. The new surface must therefore be represented
distinctly at syntax time and become executable only after complete semantic resolution.

## Goals / Non-Goals

**Goals:**

- Keep qualifier arguments distinct from operation-call arguments throughout syntax and semantics,
  while preserving one neutral applied-qualified-member shape for interface and nominal-union
  resolution.
- Reuse ordinary interface application, conformance proof, call-contract, callable-section, and
  witness-lowering machinery.
- Make direct and piped applied calls converge before HIR lowering.
- Preserve Effect constructor, runner, and source-site identity for effectful witnesses.
- Make unavailability a complete frontend result that realization cannot mistake for executable
  work.

**Non-Goals:**

- A general method or extension-member lookup system.
- Provider selection for operations whose operands and enclosing bounds contain no `Self` evidence.
- Runtime-polymorphic interface operation values.
- Operation-level generic parameters or expected-result-driven conformance inference.
- Applied service-operation selection.
- New conformance coherence, visibility, ownership adaptation, or backend dispatch rules.

## Decisions

### Represent applied qualified members neutrally and separately from call applications

The parser will recognize the contextual shape `Path<Arguments>.member` and retain a neutral,
lossless applied-qualified-member node with its arguments on the owner rather than in the call's
existing operation type-argument list. Semantic resolution will distinguish a nominal-union member
from an interface operation using the resolved owner declaration. An interface-qualified operation
remains callable, so a following argument list forms a direct call and the same expression may
appear on the right of `|>`. Existing nominal-union constructors, unit values, patterns, and
formatting keep their current meaning.

This distinction is necessary because `Interface<A>.operation(value)` fixes interface parameter
`A`, while `Actor.operation<A>(value)` applies a parameter owned by the operation declaration. The
syntax tree, formatter, semantic occurrence model, and diagnostics must never infer which meaning
was intended after discarding token position.

An alternative was to parse both spellings as one call-type-argument list and reinterpret it after
name resolution. That would make lossless formatting and malformed-syntax recovery depend on
semantic identity, and would preserve the current route by which an incomplete call reaches later
phases. It is rejected.

### Model the surface as application of an interface contract, not a generated helper

Resolution will first identify the interface (excluding services) and normalize the complete
written argument list.
It will then create a temporary inference slot for implicit `Self`, substitute the fixed interface
arguments into the selected operation contract, and analyze supplied operands against that
contract. All declared operand occurrences of `Self` participate and must agree. Operand-derived
evidence wins whenever present; only when operands do not determine `Self` may one unambiguous
applicable bound in the enclosing generic declaration supply it. Expected results do not
participate.

Once `Self` is known, the resolver will construct the existing applied-operation fact and prove the
complete `Self: Interface<Arguments>` goal. The resulting static interface-operation call reference will carry the same
capability, provider, substituted contract, operation identity, and witness evidence used by the
existing bound-operation path. Bound-only semantic and HIR names will be generalized where they now
represent both enclosing-bound and explicitly applied calls.

This is equivalent in expressive power to a compiler-known constrained helper, but no declaration
is synthesized. Generating a function would add a second public symbol, visibility and naming
questions, duplicate documentation, and a second specialization identity for one operation. The
interface operation itself remains the sole declaration and observability identity.

### Complete pipeline sections through the existing leading-argument mechanism

`Interface<Arguments>.operation` may remain temporarily open only while its surrounding expression
can supply the omitted leading operand. A pipeline supplies its fully elaborated left value once,
then completes provider inference and ordinary call analysis exactly as the equivalent direct call.
The section must retain the explicit qualifier application while open; after completion, direct and
piped forms carry the same applied-operation and witness identities.

An unresolved section may not be stored or cross an executable boundary. Existing finite-callable
specialization rules should reject it at the same boundary as any other callable with unresolved
generic evidence rather than creating an interface-specific closure representation.

### Reuse canonical HIR interface evidence and static witness discovery

No new runtime or MIR operation will be introduced. A successfully resolved call will populate the
existing HIR interface-operation evidence with normalized capability arguments, provider,
operation, substituted contract, and source provenance. Executable-origin discovery will walk that
evidence and register the selected conformance target. Witness lowering will then use the existing
coherent proof and static dispatch path.

For an `effect fn` witness, semantic analysis must allocate the witness Effect site only after the
applied call is available. HIR must retain that site, and executable discovery must register both
the concrete witness implementation and the matching Effect constructor and runner before entry
assembly. Direct and pipeline syntax must not create different Effect identities for the same call
site shape.

### Treat resolution failure as unavailable before realization

Availability requires all of the following: valid qualifier syntax, a visible interface, complete
and kind-correct interface arguments, a known operation, a provider determined without result
inference, a compatible call contract, and one proven conformance. Failure of any condition produces
an unavailable expression with its source diagnostic and no executable or Effect site.

Realization should consume only available call facts. This both enforces the normal phase contract
and closes the observed crash path; lowering is not responsible for reconstructing a missing
application or converting semantic absence into a diagnostic.

### Keep the first surface explicitly qualified

The change adds only `Interface<Arguments>.operation`. It does not add `value.operation()`, implicit
receiver borrowing, or imported-interface member lookup. Those features have distinct ambiguity,
ownership, and discoverability policies and can be evaluated later without changing this call's
meaning.

## Risks / Trade-offs

- **[Contextual `<...>.` parsing conflicts with relational recovery]** → Recognize the qualifier
  application only in the bounded qualified-callee shape and add damaged-delimiter recovery cases
  alongside valid direct and pipeline forms.
- **[Applied interface syntax regresses generic nominal unions]** → Preserve one neutral lossless
  applied-member node, resolve its owner semantically, and pin `Option<i32>.None`-style values,
  constructors, patterns, and formatting in regression tests.
- **[A second interface-call resolver drifts from bound calls]** → Share contract application,
  call checking, proof, and call-reference construction; vary only how interface arguments and
  `Self` evidence are seeded.
- **[Pipeline sections accidentally become runtime-polymorphic]** → Retain the open provider only
  inside ordinary section inference and require it to close before a binding or executable instance
  is admitted.
- **[Effect calls again reach lowering without runner identity]** → Gate witness Effect-site and
  executable-origin publication on the same complete availability result and add the reported
  failure shape as a realization regression.
- **[Multiple `Self` occurrences disagree]** → Use ordinary call-inference conflict reporting and
  retain every operand origin; do not let conformance search choose among inconsistent operands.

## Migration Plan

The syntax is additive and existing bare bound-operation and actor-call spellings remain valid.
Implementation can land atomically across syntax, semantics, HIR, tests, formatter output, and the
prescriptive reference because the repository has no compatibility obligation to preserve the
currently crashing partial interpretation. Rollback consists of reverting the change; no stored
data, package format, runtime ABI, or generated migration is involved.
