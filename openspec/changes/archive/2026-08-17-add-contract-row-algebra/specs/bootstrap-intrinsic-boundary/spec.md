## ADDED Requirements

### Requirement: Row-transforming intrinsics expose canonical callable contracts

Every source-callable row-transforming primitive SHALL declare one canonical `CallableContract`
containing binder kinds/order, fixed parameter modes, parameter and result types, constraints,
capture relationships, and availability. Inventory rendering, signature help, explicit generic
arguments, ordinary call admission, and diagnostic labels SHALL consume that same contract.

Shared, exclusive, and owned requirement binding SHALL be separate sealed operations with selected
requirement row first and `Without<R, S>` results. The sealed post-contract hook SHALL require
constraint evidence and may only validate mode-appropriate place/move legality, record captures,
and construct proof-bearing HIR. It SHALL NOT enumerate candidates, infer access or roles, subtract
rows, or reconstruct an Effect result type.

#### Scenario: Admit source and intrinsic calls through one contract

- **WHEN** an ordinary Silk wrapper and a sealed binding operation have equivalent callable contracts
- **THEN** both calls produce the same generic substitution, wanted, evidence, result row, and diagnostic identity through the common call path

#### Scenario: Reject a hook call without evidence

- **WHEN** a binding post-contract hook is invoked without assumed or concrete proof of its provider-selection wanted
- **THEN** the hook is structurally unavailable rather than performing its own candidate search

#### Scenario: Keep the intrinsic inventory auditable

- **WHEN** intrinsic inventory documentation is generated
- **THEN** fixed modes, selected-row-first binders, constraints, result difference, and availability are rendered from the canonical contracts
