## Why

Services are currently represented through duplicated provider and service-specific conformance paths even though the confirmed language defines a service as an ordinary interface that passed one dependency-eligibility check. Divergent identity and witness rules make operation syntax, `Self`, conformances, and generic specialization unpredictable.

## What Changes

- Give every interface and service operation the same implicit `Self` model, static operation contract, witness identity, and conformance validation.
- Restrict service-specific compiler behavior to one declaration-time eligibility check and requirement-row use.
- Admit mixed inline and mapped conformance operations, conjunctions of bounds, and ordinary static operation calls.
- Enforce provider-module locality, endpoint visibility, overlap rejection, termination, and static proof consistently.
- Delete duplicated provider identities and name-based service witness paths.

## Capabilities

### Modified Capabilities

- `bootstrap-service-declarations`: define dependency eligibility as the only extra service classification.
- `bootstrap-complete-interface-contracts`: unify operation bodies, mappings, `Self`, visibility, and witness completeness.
- `bootstrap-type-generics`: support bound conjunction and one static specialization path for interfaces and services.
- `bootstrap-declaration-index`: record one canonical conformance identity and enforce coherence boundaries.

## Impact

Depends on `enforce-return-contract-soundness`. It supersedes the narrower assumptions in completed interface-witness changes without preserving compatibility shims. It affects parsing facts, indexing, conformance validation, specialization, static calls, diagnostics, and tests; it adds no dynamic dispatch or runtime vtable.
