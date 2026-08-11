## ADDED Requirements

### Requirement: Completion keeps intrinsics explicit

Qualified completion on `Intrinsic` SHALL offer the deterministic catalog of compiler primitives
with concrete signatures and safety markers. Ordinary expression and actor completion SHALL offer
visible source APIs and MUST NOT leak unqualified intrinsic operations or former compiler-known
actor members. Standard-library APIs SHALL remain the preferred completion path outside the sealed
namespace.

#### Scenario: Complete integer operations

- **WHEN** completion is requested after `Intrinsic.` and after an ordinary integer API qualifier
- **THEN** the first result set contains concrete primitives and the second contains source-defined numeric operations
