## Context

Representation parameters make concrete identities part of complete nominal contracts. Named
callable items can sometimes expose an exact identity, but capturing sections and Effects are
construction-site values whose implementation must remain private across public result boundaries.

## Goals / Non-Goals

**Goals:**

- Provide exact and opaque source contracts for representation-dependent results.
- Keep opaque equality stable across body-only edits while invalidating changed realizations.
- Support generic producers without runtime existential packaging.

**Non-Goals:**

- Hide multiple runtime representations behind one result.
- Add uniform closures, allocation, reflection, or indirect calls.
- Make local construction-site identities directly source-nameable.

## Decisions

### Restrict `typeof` to exact named callable items

Resolve `typeof(item)` during declaration analysis only after overload and generic specialization are
complete. The item must be sufficiently visible for the containing contract. Sections, locals,
Effect sites, partial generics, and private leaks use an opaque result or fail. This keeps exact
source identity deterministic and navigable.

### Make `some` a contextual scoped result binder

Parse `some<F: Contract> Result` only in result-binder position. The binder scopes over the entire
result so one opaque representation can occur multiple times. `some` hides one static family, not a
runtime package.

### Separate family key, public signature, instance, and realization

Use `(producer canonical identity, binder ordinal)` as the stable family key. Add normalized public
bound, result occurrences, and enclosing binder kinds to the public semantic signature. Apply every
enclosing concrete type, row, and representation argument to obtain a family instance. The private
realization then records target or runner, captures, access, cleanup, and suspendability.

This separates source equality from body invalidation. Changing only a captured value preserves the
public instance; changing the bound changes the public signature; changing target or capture shape
changes private fingerprints and invalidates lowering.

### Publish private realization dependencies across modules

Export compiler-internal opaque definition records separately from public module semantic surfaces.
Importers may specialize and invalidate from these records but tooling outside the producer reveals
only the opaque origin and bound.

### Require one finite realization per producer specialization

Analyze every reachable return for one opaque binder and specialization. They must unify to one
representation. Recursive returns may reuse a realization established by a local construction;
realization-only recursion and inline self-layout cycles fail.

## Risks / Trade-offs

- [Stable public key can leave stale layout caches] → Track body/target and layout/access/cleanup
  fingerprints as separate mandatory dependencies.
- [Opaque privacy can impede debugging] → Present producer origin and public bound while keeping
  captures and private target hidden.
- [Generic families multiply instances] → Key them by normalized enclosing arguments and reuse
  identical specializations deterministically.

## Migration Plan

1. Add parsing, formatting, and damaged recovery for `typeof` and contextual `some`.
2. Add family/public-signature facts and visibility validation.
3. Add producer-body realization unification and recursion checks.
4. Publish private realization dependency records and invalidation fingerprints.
5. Add inspector, navigation, and fresh-process invalidation fixtures.

The feature can be rolled back by rejecting the new result forms; no runtime ABI is introduced.
