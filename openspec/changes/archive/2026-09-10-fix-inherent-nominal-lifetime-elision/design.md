## Context

At db05ef34, nominal/function headers replay `DeclarationLifetime` after arity discovery, but inherent heads remain in their collection form. The holder head resolves to an `Applied` failure with SEM0051; completion publishes neither its member nor a closed Self. The user-facing call then reports SEM0010. Frontend-only explicit/elided probes reproduce the distinction in milliseconds.

## Goals / Non-Goals

**Goals:** one canonical lifetime-elaboration path for inherent heads, accurate member binders, and preserved Self semantics.

**Non-Goals:** infer relationships from bodies, change Self to mean a newly instantiated output, admit borrowed Effect outcomes, or implement JUL-117.

## Decisions

- Retain the head's declaration lifetime context and replay it using the completed nominal arity. Reuse the existing header elaborator rather than teaching name lookup to fabricate a missing member.
- Finalize the head before its members. Inherited implicit binders used through Self join the member contract; a member returning an omitted nominal result keeps its own input/output binder instead. Allocate member binders with the ambient owner binders present so their presentation names remain distinct.
- Validate whole-family arity after elision, retaining the syntactic argument-to-binder check. Preserve the resolver's normalized nominal arguments instead of reconstructing them in written order.
- Keep Self fixed to the impl owner. The current unresolved-Self diagnostic is defective, but the unconnected borrowed input remains invalid after closure.

## Risks / Trade-offs

- Binder replay could discard or accidentally quantify an owner lifetime → assert member binder identity and resolved Self, including explicit lifetime use without Self.
- Relaxed syntactic arity could admit specialized heads → retain focused rejected-head controls and validate completed kinds/arity.
- Tests could pay for realization unnecessarily → use one frontend snapshot per program in the existing inherent-impl test file; no backend or native test is added.
