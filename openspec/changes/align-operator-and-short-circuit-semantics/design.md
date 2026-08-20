## Context

Operator selection is partly name-based, while short-circuit right operands are rejected by a purity pass instead of flowing through ordinary conditional analysis. Both are expression-layer privileges to remove.

## Goals / Non-Goals

**Goals:** explicit operator declarations; ordinary conformance dispatch; heterogeneous signatures; ordinary branch analysis for short circuit.

**Non-goals:** dynamic multimethods, user-defined precedence, overload ranking, implicit conversions, or eager boolean operands.

## Decisions

1. Parse the contextual form `operator <token> fn` on contract operations and retain its token as
   optional semantic metadata. Only interfaces may use it. The closed eligible set is unary
   `-`, `!`, `~` and eager binary `*`, `/`, `%`, `+`, `-`, `&`, `^`, `|`, `<`, `<=`, `>`, `>=`,
   `==`, `!=`; arity disambiguates unary and binary `-`.
2. Resolve an operator by ordinary visible conformance evidence; spelling does not participate.
3. Carry the selected operation as the same applied interface contract and witness question used by
   a named bound-operation call; concrete user types and bounded parameters share that path.
4. Lower `&&` and `||` as existing conditional control with a boolean join.
5. Run standard path-local ownership, Effect, and cleanup analysis on the right region.
6. Retire `SEM0096`. Use stable declaration, no-applicable-operation, and ambiguity diagnostics for
   invalid operator contracts and selection failures; ordinary operand diagnostics remain attached
   to operands once one operation is selected.

## Risks / Trade-offs

- Removing name privilege breaks existing intrinsic mappings until their interface operations receive explicit markers.
- Multiple visible conformances remain a coherence error rather than an overload-ranking opportunity.

## Migration Plan

Add markers and validation, migrate builtin operator contracts, migrate selection and HIR/MIR, remove name lookup and impurity checks, align engines, update diagnostics/tests/docs.

## Open Questions

The future set of supported operator tokens can expand through separate proposals without changing dispatch semantics.
