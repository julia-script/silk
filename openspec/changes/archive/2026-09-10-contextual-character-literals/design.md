## Context

See proposal.md for motivation. Expression analysis, primitive constant analysis and static syntax
evaluation independently handle character literals. Existing integer facts already carry exact
values through static evaluation and lowering. `char` lowering intentionally remains character-only.

## Goals / Non-Goals

**Goals:** select literal types before value construction and keep one integer execution path.

**Non-goals:** implicit value conversions, new encodings, enum construction, global inference,
changing string traversal or permitting arithmetic on already-typed `char` values.

## Decisions

1. Decode Unicode first. For a concrete integer expectation, normalize the decoded scalar to the
   existing integer fact and static-value representation. Keep default character facts unchanged.
   Widening character HIR/MIR instead would create a second integer path and risk hardcoded char
   lowering silently restoring the wrong type.
2. Reuse existing expected-type propagation. Exclude bare character literals from choosing the
   operator's declared operand type, alongside numeric literals, so a typed operand wins in either
   order. Preserve the first-literal fallback if no operand supplies a declared type. Do not add a
   new inference engine or retroactively retype bindings.
3. Extend primitive constant selection and syntax-based static evaluation explicitly. The former
   emits ordinary Integer constant values; the latter admits IntegerValue through existing exact
   validation. This covers entry paths that do not call ordinary expression analysis.
4. Generalize SEM0002 with explicit selected type and exact decimal bounds, updating both current
   callers and generated diagnostic documentation. A new character-only diagnostic would duplicate
   the same range failure. Bounds are serialized strings to avoid lossy numeric representation.
5. Replace contradictory literal tests, share semantic programs across assertions, extend existing
   static primitive tests, and use structural MIR plus the shared native corpus for execution
   evidence. No new feature-specific native compilation test is needed.

## Risks / Trade-offs

- Scalar numbers can be mistaken for UTF-8 bytes → show `'é'` as integer 233 explicitly in reference.
- A leaf-only edit can miss constant/static paths → cover each entry path at its cheapest tier.
- Defaulted mixed literal operators retain order-dependent selection → document existing fallback;
  broader inference remains a separate language decision.
- Diagnostic payload changes affect generated text → regenerate the catalog and verify consumers.

## Migration Plan

Update the compiler, reference and contradictory tests together on the issue branch, then run the
required checks and independent reviews. No compatibility path or deployment migration is needed.
