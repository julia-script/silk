## Context

Silk indexes parametric conformances and can specialize existing witnesses, but generic wrappers
cannot declare that their own conformance depends on a parameter's conformance. Arbitrary recursive
proof search would threaten coherence and termination, so the admitted form is deliberately
conservative.

## Goals / Non-Goals

**Goals:**

- Admit bounded generic conformances with deterministic static witnesses.
- Make overlap decidable at declaration time and proof search structurally terminating.
- Produce useful proof and cycle diagnostics.

**Non-Goals:**

- Specialization by negative bounds, overlapping instances, interface inheritance, runtime
  dictionaries, coinductive proofs, or compiler-configurable semantic fuel.
- Implicit `Self` or `where` syntax.

## Decisions

### Put requirements in `impl<...>`

Reuse the generic parameter list for type, row, representation, and interface-bound parameters.
Interface applications continue to state the provider explicitly and the provider must equal the
`for` type in the conformance head.

### Reject possible overlap by head unification

At indexing, alpha-normalize and conservatively ask whether provider/interface heads may unify.
Ignore bounds. Types use first-order unification; normalized closed rows compare canonically; open
rows use finite row unification or conservatively overlap; representation variables overlap when a
common admissible concrete representation cannot be disproved. False-positive rejection is preferred
to runtime-dependent coherence.

### Enforce strict provider descent

For every requirement, its provider must be a strict structural subterm of the `for` provider,
generic-variable occurrences across the goal cannot increase, and ground non-provider arguments may
only remain unchanged. This accepts `MappedSchema<S>` requiring `S` and nested `OptionalSchema<S>`
while rejecting peer, equal, and growing providers. Active-goal cycle detection is defensive recovery,
not the termination proof.

### Prove at concrete specialization

HIR retains a canonical interface goal and conformance candidates. Reachable instance discovery
substitutes all concrete arguments, follows requirements to base witnesses, memoizes completed proofs,
and emits one direct witness target. In-progress goals cannot satisfy themselves.

## Risks / Trade-offs

- [Conservative overlap rejects useful disjoint cases] → Require distinct wrapper heads rather than
  making coherence depend on an evolving proof graph.
- [Structural descent is less expressive than general logic programming] → Keep the first feature
  finite and explain rejected sizes in diagnostics.
- [Damaged facts create spurious cycles] → Preserve unavailable states and use active-stack traces for
  recovery without treating them as valid witnesses.

## Migration Plan

1. Parse/index bounded impl headers and explicit requirements.
2. Add kind-aware may-overlap and termination validation.
3. Add canonical proof goals, memoization, and diagnostics.
4. Integrate HIR questions and concrete witness discovery.
5. Verify no MIR/runtime dictionary is introduced.

Rollback rejects bounded impl declarations while existing unconditional conformances remain unchanged.
