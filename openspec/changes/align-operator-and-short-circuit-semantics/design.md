## Context

Operator selection is partly name-based, while short-circuit right operands are rejected by a purity pass instead of flowing through ordinary conditional analysis. Both are expression-layer privileges to remove.

## Goals / Non-Goals

**Goals:** explicit operator declarations; ordinary conformance dispatch; heterogeneous signatures; ordinary branch analysis for short circuit.

**Non-goals:** dynamic multimethods, user-defined precedence, overload ranking, implicit conversions, or eager boolean operands.

## Decisions

1. Record an optional closed operator marker on interface operations and validate allowed arity/shape.
2. Resolve an operator by ordinary visible conformance evidence; spelling does not participate.
3. Carry selected operation identity and substituted signature into HIR before specialization.
4. Lower `&&` and `||` as existing conditional control with a boolean join.
5. Run standard path-local ownership, Effect, and cleanup analysis on the right region.

## Risks / Trade-offs

- Removing name privilege breaks existing intrinsic mappings until their interface operations receive explicit markers.
- Multiple visible conformances remain a coherence error rather than an overload-ranking opportunity.

## Migration Plan

Add markers and validation, migrate builtin operator contracts, migrate selection and HIR/MIR, remove name lookup and impurity checks, align engines, update diagnostics/tests/docs.

## Open Questions

The future set of supported operator tokens can expand through separate proposals without changing dispatch semantics.
