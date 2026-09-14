## Context

The compiler already indexes coherent conditional conformance heads, validates mapped operation
witnesses in `DeclarationCompletion`, and defers concrete conformance proof to
`ConformanceProof`. It also has known-provider inference for generic calls. The missing path appears
when the selected provider remains generic and the conditional requirement is itself one of the
enclosing declaration's bounds: the source conformance is valid, but selection asks the concrete
proof engine for a specialization it intentionally cannot close yet. Inline Effect-polymorphic
witnesses can additionally leave a target requirement-row binder unresolved during declaration
validation.

The reduced regression uses two interfaces, `Inner<P, A, E ? R>` and
`Outer<P, A, E ? R>`, plus `Context<P, A, E, R, H>`. The conditional Outer conformance requires
Inner for H and maps an operation returning `A ! E ? R`. A generic caller already bounded by Inner
must be accepted without turning either interface into a runtime service.

## Goals / Non-Goals

**Goals:**

- Admit the exact symbolic conditional conformance only when its substituted requirements are
  already declared assumptions of the enclosing generic body.
- Infer ordinary, failure-row, and requirement-row witness target binders from the complete
  interface operation contract.
- Prove definitionally known open requirement subsets such as `R | Q` within the same normalized
  `R | Q` source row without solving for either parameter.
- Preserve the current concrete proof tree, coherence, termination, diagnostics, and static
  specialization model.

**Non-Goals:**

- No runtime dictionaries, witness values, service slots, or backend dispatch.
- No expected-result-based generic inference and no weakening of exact Effect row matching.
- No inverse row solving or proof through unresolved member-well-formed obligations.
- No relaxation of conditional-head overlap, structural descent, or concrete proof requirements.
- No implicit `Self`, `where` syntax, conditional inherent members, or callable-storage escape
  hatch.

## Decisions

### Treat declared bounds as symbolic proof assumptions

When call resolution has a known generic provider, match visible conditional conformance heads
using the existing kind-aware open-generic substitution. The candidate may retain parameters owned
by the enclosing declaration, but the match may bind only the callee's still-open parameters.
Before invoking concrete proof search, substitute each conditional requirement and require an exact
corresponding declared bound in the enclosing generic context. Retain the unresolved conformance
selection in HIR so concrete instance discovery still builds the canonical proof tree.

This keeps `ConformanceProof`'s closed-goal invariant intact. Extending its memoized concrete proof
objects with open variables would blur the boundary between checked generic assumptions and
reachable monomorphic evidence.

### Infer witness binders from the complete mapped contract

Use the same generic-argument inference machinery over receiver, parameters, success, failure, and
requirement rows when validating an inline or mapped witness. A binder that remains absent from all
contract positions stays an error; a conflicting or wrong-kind row binding keeps the existing
deterministic diagnostic.

This is preferable to special-casing Effect rows or accepting a stronger witness contract, both of
which could make requirement subtraction unsound.

### Reuse forward-only normalized requirement subset proofs

After independently inferred row binders are substituted, an Effect call may still compare open
rows whose union operands are definitionally identical. A direct exclusion bound on a context row
is checked only after known-provider inference has independently fixed that row. Reuse
`RowAlgebra.isKnownSubset` for the remaining structural check only when neither row retains
member-well-formed obligations. The helper proves only exact operands, concrete finite subsets, and
unions assembled from them; it never inverts a row, binds a parameter, or reasons backward through
`Without`.

This keeps the generic server adapter exact while leaving genuinely unknown row relationships to
the existing deterministic diagnostic.

### Reuse one reduced fixture at the cheapest semantic tiers

Add the minimal declaration/call source to the existing conditional-conformance focused test. One
analysis snapshot should assert clean diagnostics, generic HIR retention, and the selected concrete
witness after realization. Negative variants should alter only the promised handler bound or row
argument and assert exact diagnostic code/span. No new backend pass or per-feature binary test is
needed because the change affects static selection rather than code generation.

## Risks / Trade-offs

- **[Risk] Symbolic assumption matching accidentally accepts a merely unifying bound.** → Require
  canonical exact interface/provider equality after substitution, including normalized rows and
  access-qualified requirement keys.
- **[Risk] Generic and concrete proof paths select different conformance declarations.** → Retain
  the symbolic declaration identity in HIR and verify concrete instance discovery selects the same
  canonical source conformance.
- **[Risk] Broader witness inference changes existing diagnostics.** → Limit new evidence to the
  mapped operation's declared contract positions and retain existing unresolved/conflicting/wrong-
  kind failures outside that evidence.
- **[Risk] A structural subset proof bypasses capability well-formedness.** → Require both rows
  to have no outstanding member-well-formed obligations before applying the forward-only proof.

## Migration Plan

No source migration is required. Existing rejected programs remain rejected unless they already
carry the exact conditional bound needed to justify the symbolic conformance. The change can be
rolled back with the frontend selection patch and its focused regression; emitted runtime formats
and public APIs are unchanged.
