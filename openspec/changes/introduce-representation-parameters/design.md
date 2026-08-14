## Context

Callable and Effect values already carry compiler-private construction identities through direct
higher-order specialization. Nominal applications, however, store only ordinary type arguments, so
placing the same values behind fields loses the identity before ownership and layout can act. This
change builds the static semantic substrate while leaving all runtime storage diagnostics intact.

## Goals / Non-Goals

**Goals:**

- Introduce a first-class generic kind for callable and Effect representations.
- Give nominal applications one deterministic vector of kinded generic arguments.
- Preserve representation facts to the concrete specialization boundary.
- Make incompatible joins and missing representations fail before MIR.

**Non-Goals:**

- Layout or execute stored callable or Effect fields.
- Add opaque result binders, runtime erasure, interface dictionaries, or compiler-known library actors.
- Make representation-bearing nominals structurally Copy.

## Decisions

### Separate representation identity, required bound, and admissibility

Model a representation parameter as a declaration-owned binder plus a callable or Effect bound.
Model a concrete argument as an exact identity, an opaque family instance supplied by a later
change, or an open parameter reference. A represented field use combines the argument with its
substituted required bound and an admissibility proof. The raw identity never substitutes into an
ordinary `Type` slot.

This preserves one equality identity when a reusable function is admitted under `fn` and `once fn`.
Embedding the use bound into identity was rejected because contextual weakening would create two
representations for one target and environment.

### Generalize nominal applications to kinded argument vectors

Replace the type-only nominal argument collection with an ordered generic-argument vector whose
entries are checked against declaration binders. Type, failure-row, requirement-row, and
representation arguments retain distinct tagged forms, equality, ordering, substitution, and
encoding. Existing type-only consumers receive a filtered ordinary-type substitution only where
their contract genuinely excludes other kinds.

### Infer representation arguments at construction

Each representation-bearing initializer contributes one candidate argument. Repeated uses of the
same binder unify; disagreement is a source error at the first conflicting field. Nested nominals
forward their complete arguments rather than re-deriving identities from syntax.

### Resolve before layout and MIR

Syntax, semantic facts, and generic HIR may retain open representation parameters. Reachable
instance specialization must resolve every open argument and attach a resolved field representation
or explicit unavailable reason. Layout and MIR reject any residue. Runtime fences remain active in
this proposal, so the new substrate can land without accidental partial execution.

### Use canonical structure, never presentation text

Equality and keys use tagged structural data and stable declaration or HIR-site identities. Source
paths and spans are diagnostic presentation only. Encoders use deterministic definition ordering and
back-references; digest-based symbols must verify structural equality rather than trust collisions.

## Risks / Trade-offs

- [Generic-argument changes touch many consumers] → Audit every nominal argument traversal and add
  wrong-kind recovery fixtures before changing runtime behavior.
- [Deep nested identities amplify key size] → Preserve a DAG-shaped logical model and characterize
  growth before requiring hash-consing.
- [Frontend support can appear more complete than it is] → Keep callable and Effect storage fences
  unchanged until their dedicated proposals pass every engine.
- [Diagnostics expose enormous types] → Show the first divergent representation and make full
  canonical detail secondary.

## Migration Plan

1. Add syntax and declaration facts behind unavailable recovery.
2. Introduce tagged generic arguments and adapt equality, encoding, inspectors, and semantic surfaces.
3. Add represented-value substitution and construction inference.
4. Preserve arguments through generic HIR and instance discovery.
5. Assert that every layout/MIR path still rejects representation-bearing fields until later changes.

Rollback removes the new syntax/facts while existing storage fences continue to protect execution.
