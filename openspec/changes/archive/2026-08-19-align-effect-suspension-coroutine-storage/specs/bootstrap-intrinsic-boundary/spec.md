## MODIFIED Requirements

### Requirement: One target-neutral suspension primitive is admitted

The sealed `Intrinsic` namespace SHALL contain exactly one safe Effect suspension operation whose
contract transfers one deferred Effect to the compiler-owned execution boundary and later returns
that Effect's typed outcome. The operation SHALL preserve generic success, failure, and requirement
rows exactly and MUST NOT request a source allocator or report private execution-storage exhaustion
as a typed failure. It MUST NOT expose a continuation type, callback ABI, scheduler, fiber, pending
token, target address, execution-stack allocator, or backend frame layout.

#### Scenario: Audit the suspension seam

- **WHEN** the deterministic intrinsic catalog and its consumers are inspected
- **THEN** exactly one suspension operation is present with evaluator, LLVM, and Wasm availability, exact channel preservation, and no public continuation-management or allocation operations

#### Scenario: Give a same-named function no privilege

- **WHEN** user source defines `Effect.suspend` or another function with the same spelling as the canonical wrapper
- **THEN** it receives ordinary function behavior unless its body explicitly calls the sealed intrinsic

#### Scenario: Keep execution storage out of the intrinsic contract

- **WHEN** tooling renders the suspension intrinsic's canonical callable contract
- **THEN** the result contains only the deferred child's `A ! E ? R` channels and no `OutOfMemory` or `Allocator` contribution
