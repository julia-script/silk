# Context

`BodyQuery` already preserves dependency-sensitive artifacts between editor revisions, and
`Evaluation` already owns deterministic value-sensitive caching and budgets. Replacing either store
would regress established behavior.

# Decisions

- `SemanticQuery` owns only within-session request coordination. `CheckBody` executes inside that
  boundary and delegates cross-revision validation/presentation to `BodyQuery`.
- Nested name/header/evaluation reads publish observations into the active `CheckBody` request.
- `Semantic.evaluate` delegates to `Evaluation`; its key remains the compilation identity, source
  identity, declaration, type/evidence/row arguments, and static values. A semantic wrapper never
  aliases different values or bypasses budget charging.
- `checkBodyFresh` creates an isolated session and bypasses the reusable body store.
- Active reservations are released by the shared query runtime after success or defects; evaluation
  keeps its existing pending-cycle and deterministic failure behavior.

# Risks

- Double caching could skip evaluation accounting. Avoided by keeping evaluation cache ownership in
  `Evaluation` and using the semantic layer only to observe/delegate.
- Session memoization could hide body presentation changes. Cross-revision sessions are always
  fresh, while `BodyQuery` remains responsible for presenting retained artifacts at current spans.
