## Context

Borrow roots and callable sections expose limitations of current syntax-shaped ownership facts. The migration needs stable place identity and one delayed-value lifetime analysis shared by Effects and callable values.

## Goals / Non-Goals

**Goals:** stable temporary/subplace roots; local borrow values; generalized trailing sections; last-use loan shortening; engine parity.

**Non-goals:** non-lexical escaping references, relaxed exclusive access, implicit heap promotion, argument holes, or arbitrary currying that reorders arguments.

## Decisions

1. Materialize addressable temporaries and subplaces with compiler-owned stable logical owner IDs.
2. Carry a provenance chain in semantic facts and MIR places rather than recognizing arrays or names specially.
3. Represent a section by the source callable plus an ordered list of supplied trailing arguments and the remaining leading-parameter contract.
4. Reuse one last-use analysis for Effect runs and callable invocations, with escape/storage as conservative lifetime barriers.
5. Preserve affine moves and loans when a section is staged; no supplied argument is duplicated.

## Risks / Trade-offs

- Stable roots can increase local storage lifetime but never beyond lexical validity.
- Generalized sections expand representation combinations; specialization remains finite.

## Migration Plan

Add owner/place IDs and provenance, migrate borrow checking and views, generalize section HIR/MIR, implement engines, unify last-use analysis, migrate tests/diagnostics, then delete named-root and unary-section exceptions.

## Open Questions

None at language level; exact stack-slot placement remains backend-private.
