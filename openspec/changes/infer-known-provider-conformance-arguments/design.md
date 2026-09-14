## Context

See [proposal.md](./proposal.md) for motivation. Ordinary argument inference currently builds a
partial substitution and then checks interface constraints. A constraint such as
`C: BufferedContext<P, A, E, ?R>` therefore cannot help when a supplied argument has fixed `C` but
`P`, `A`, `E`, and `R` remain unresolved, even though the conformance index already contains the
only coherent applied `BufferedContext` fact for that concrete provider.

Conformance discovery already owns visibility, locality, overlap, conditional-proof termination,
and witness completeness. The inference change must consume those facts without creating a second
conformance model or changing the static representation of interface calls.

## Goals / Non-Goals

**Goals:**

- Complete unresolved generic call arguments from a direct interface constraint once that
  constraint's provider is fully known from explicit or supplied-operand evidence.
- Keep inference monotonic, deterministic, kind-correct, and independent of declaration/import
  order.
- Reuse the existing coherent conformance model and preserve the selected proof for later
  constraint checking and specialization.

**Non-Goals:**

- Inferring a provider by searching all conformances, or inferring from an expected result, an
  assignment, or a later use.
- Solving membership, subset, difference, or other row constraints backwards.
- Adding first-class existential or erased higher-ranked callables, witness dictionaries, runtime
  interface dispatch, or new HIR/MIR/backend representations.
- Encoding JUL-201's handler as a stored callable. Its concrete context actor will implement a
  nominal context interface and invoke the owned handler inside that statically selected witness.

## Decisions

### Run a bounded known-provider conformance phase inside call inference

After explicit-prefix and supplied-operand inference reaches its ordinary partial substitution,
analysis will inspect direct interface constraints whose provider parameter now substitutes to a
fully known type. It will query the existing conformance index by that provider and canonical
interface identity, prove the one applicable visible conformance, and unify the conformance's
applied interface arguments with the constraint application.

The phase uses a monotonic worklist: a successful proof may fill unresolved call binders and make
another direct constraint's provider fully known, but no step may clear or replace a binding. The
worklist terminates because each successful step binds at least one member of the finite call-binder
set; a step that binds nothing is not re-enqueued.

This is preferable to teaching ordinary operand unification about conformances because a
conformance is constraint evidence, not a runtime conversion or a structural relationship between
the provider argument and an operand type.

### Require a ground provider and one proven conformance

The rule activates only when the provider is already fully determined without this phase. Lookup
may inspect candidates indexed for that concrete provider and interface, but it never enumerates
providers or uses the unresolved interface arguments to choose a provider. Existing coherence,
visibility, overlap, and conditional-proof rules decide whether exactly one conformance is usable.

Missing proof leaves the interface constraint unsatisfied. More than one applicable proof is an
ambiguity. Either outcome follows the existing deterministic constraint-diagnostic path rather
than choosing by declaration or import order.

This ground-provider restriction is the smallest rule needed for
`C: BufferedContext<P, A, E, ?R>` and avoids general bidirectional logic-programming behavior.

### Unify evidence without changing its priority

Explicit generic arguments and direct operand inference remain primary evidence. Conformance
arguments fill only unresolved binders. If unification encounters an already-bound type or row, it
must agree exactly under the existing kind-aware equality and row-normalization rules; otherwise
analysis reports both responsible origins and publishes no specialization.

The selected conformance proof is retained as ordinary static evidence so the later constraint and
specialization phases validate the same fact. No witness becomes a runtime operand and no callable
or Effect representation changes.

### Prove the rule at the semantic tier

One shared structured-analysis source will cover a positive type/failure/requirement-row inference,
agreement with earlier operand evidence, conflict, absent or ambiguous proof, and refusal to infer
an unknown provider or use a result context. These are semantic claims and require no native or
WebAssembly compilation. A small realization using the real buffered context declaration may guard
the dependent public signature without introducing a backend-specific execution leg.

The language reference will describe known-provider conformance evidence next to supplied-argument
call inference and reaffirm that interfaces remain compile-time-only.

## Risks / Trade-offs

- **[Risk] A partially open conformance query could accidentally become global provider search.**
  → Require the substituted provider to be ground before lookup and key discovery by its nominal
  identity plus the canonical interface identity.
- **[Risk] Constraint order could change inferred results.** → Use a monotonic worklist and require
  all prior bindings to agree, so order affects neither the final substitution nor diagnostics.
- **[Risk] Conditional conformances could make the new phase recurse indefinitely.** → Delegate
  proof to the existing terminating conformance engine and keep the outer worklist bounded by the
  finite unresolved call binders.
- **[Risk] The feature could be mistaken for general result-directed or inverse-row inference.** →
  Add negative semantic cases for those boundaries and state them normatively in the delta spec.

## Migration Plan

1. Land the compiler semantic rule, structured regression coverage, and reference documentation.
2. Revalidate JUL-192's interface-only `BufferedContext` acquisition without changing its runtime
   representation.
3. Have JUL-201 define a nominal context actor and conformance that owns and invokes its affine
   handler, then call the JUL-192 acquisition API with that context value.

The repository is green-field, so there is no compatibility path or dual inference mode. If the
compiler change must be reverted, dependent JUL-192/JUL-201 uses remain blocked until the semantic
rule is corrected; no erased-callable fallback is introduced.
