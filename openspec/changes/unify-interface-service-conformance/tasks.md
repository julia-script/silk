## 1. Canonical contracts

- [x] 1.1 Define one interface/service contract fact with implicit `Self`, ordered operations, and a service-eligibility bit.
- [x] 1.2 Migrate bounds, static operation calls, service declarations, and existing interface declarations to that fact.
- [x] 1.3 Remove duplicate provider and service-specific witness identities.

## 2. Conformance and coherence

- [x] 2.1 Resolve mixed inline and mapped operations into one complete witness table.
- [x] 2.2 Add bound conjunction and substitute the same contract for concrete and generic static calls.
- [x] 2.3 Enforce provider-module locality, endpoint visibility, overlap rejection, termination, and static proof.
- [x] 2.4 Restrict dependency-row admission to the single service-eligibility check.

## 3. Verification and reconciliation

- [x] 3.1 Migrate completed interface witness/bound call-surface behavior into the unified path and delete narrower branches.
- [x] 3.2 Add user interface, service, mixed operation, conjunction, visibility, overlap, and locality tests.
- [x] 3.3 Update diagnostics, semantic inspectors, canonical specs, docs, and reconciliation evidence.
- [x] 3.4 Run typecheck, Biome, focused analysis/evaluation tests, full tests, and `pnpm check`.
