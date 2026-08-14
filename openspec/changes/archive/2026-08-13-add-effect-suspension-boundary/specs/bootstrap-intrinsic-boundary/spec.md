## ADDED Requirements

### Requirement: One target-neutral suspension primitive is admitted

The sealed `Intrinsic` namespace SHALL contain exactly one safe Effect suspension operation whose
contract transfers one deferred Effect to the compiler-owned execution boundary and later returns
that Effect's typed outcome. The operation SHALL preserve generic success, failure, and requirement
rows while explicitly adding `OutOfMemory` and exclusive `Allocator` access for continuation
storage. It MUST NOT expose a continuation type, callback ABI, scheduler, fiber, pending token,
target address, or backend frame layout.

#### Scenario: Audit the suspension seam

- **WHEN** the deterministic intrinsic catalog and its consumers are inspected
- **THEN** exactly one suspension operation is present with evaluator, LLVM, and Wasm availability and no public continuation-management operations

#### Scenario: Give a same-named function no privilege

- **WHEN** user source defines `Effect.suspend` or another function with the same spelling as the canonical wrapper
- **THEN** it receives ordinary function behavior unless its body explicitly calls the sealed intrinsic

### Requirement: The public suspension API remains ordinary Silk

The canonical `Effect.suspend` operation SHALL be a shipped ordinary Silk declaration over the
single suspension intrinsic. Generic lifting, row composition, documentation, imports, navigation,
and reusable composition policy MUST remain in source and MUST NOT be selected by standard-library
module identity.

#### Scenario: Navigate to Effect.suspend

- **WHEN** tooling resolves a call to canonical `Effect.suspend`
- **THEN** it navigates to shipped Silk source whose only compiler privilege is its explicit sealed intrinsic call
