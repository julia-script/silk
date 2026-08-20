## Context

Construction authorization and generic inference currently consult different module and field facts. The stabilized model can use the resolved field list as the sole source for both visibility and constraints.

## Goals / Non-Goals

**Goals:** field-based construction; private-field fences; complete forward inference; deterministic diagnostics.

**Non-goals:** positional constructors, default field values, reflection, inference from future uses, or private-field bypass.

## Decisions

1. Resolve every named initializer to a canonical field and validate that field's visibility.
2. Require every construction-required field exactly once and preserve declaration-defined layout order independently from source initializer order.
3. Collect generic constraints from all supplied fields after any explicit type-argument prefix.
4. Solve constraints together and report all conflicting sources rather than taking the first.
5. Publish resolved fields and inferred arguments as semantic facts used by HIR and tooling.

## Risks / Trade-offs

- External code may begin constructing types whose authors intended to protect but exposed every field; private fields are the deliberate language fence.
- Better inference may replace some ambiguity diagnostics with successful construction.

## Migration Plan

Unify field resolution, replace module authorization, add full constraint collection, publish semantic facts, update completion/signature help, migrate tests/specs, and remove the old gate.

## Open Questions

Defaulted or omitted optional fields remain future struct-design work.
