## ADDED Requirements

### Requirement: External parking exposes only Wake, wake, and park

The sealed intrinsic catalog SHALL add opaque affine `Wake`, synchronous consuming `wake(Wake) ->
()`, and effectful unit-returning `park<G,F>(F) -> ()` where `F` is one NonParking
`once fn(Wake) -> G` registration callback. The operations SHALL expose no callback
representation inside Wake, payload channel, scheduler token, explicit cancellation operation,
destroy operation, allocator, timer, queue, Deferred, Fiber, Coroutine, or program-entry policy.
Every phase and target SHALL agree on safety, affinity, local-transfer, reachability, and callback
metadata.

#### Scenario: Audit the wake and park inventory

- **WHEN** intrinsic declarations are compared with semantic, HIR, MIR, evaluator, and backend branches
- **THEN** all phases expose exactly Wake, wake, and park with matching contracts and no actor-shaped primitive

#### Scenario: Admit an affine registration callback

- **WHEN** a park registration callback owns affine source state
- **THEN** the intrinsic inventory accepts it as a NonParking `once fn`, invokes it at most once, and exposes no reusable-call requirement

#### Scenario: Rename a source waiter actor

- **WHEN** a Deferred-shaped or timer-shaped ordinary source actor is renamed without changing intrinsic calls
- **THEN** registration, notification, and cancellation behavior remains unchanged

#### Scenario: Keep source payload out of Wake

- **WHEN** one waiter stores task identity or result data beside Wake
- **THEN** inspection shows the payload remains ordinary source state and Wake retains only readiness authority

#### Scenario: Reject an explicit intrinsic cancel

- **WHEN** implementation attempts to add source-callable Wake cancellation or Execution destroy for prompt unlinking
- **THEN** the boundary audit rejects it because affine drop plus ordinary guard cleanup already provide the selected contract

## MODIFIED Requirements

### Requirement: One target-neutral suspension primitive is admitted

The sealed `Intrinsic` namespace SHALL contain exactly one safe nested-Effect suspension operation
whose contract transfers one deferred Effect to the compiler-owned nested execution boundary and
later returns that Effect's typed outcome. The operation SHALL preserve generic success, failure,
and requirement rows exactly and MUST NOT request a source allocator or report private execution-
storage exhaustion as a typed failure. It MUST NOT expose a continuation type, callback ABI,
scheduler, fiber, pending token, target address, execution-stack allocator, or backend frame layout.
This exact-one constraint applies to nested child-transfer suspension; the separately cataloged
`Intrinsic.park` operation supplies external-wake relinquishment only inside explicit Execution
ownership and MUST NOT be counted or substituted as a nested-transfer operation.

#### Scenario: Audit the suspension seam

- **WHEN** the deterministic intrinsic catalog and its consumers are inspected
- **THEN** exactly one nested-transfer suspension operation is present with evaluator, LLVM, and Wasm availability, exact channel preservation, and no public continuation-management or allocation operations, while external parking remains a distinct explicit-Execution operation

#### Scenario: Give a same-named function no privilege

- **WHEN** user source defines `Effect.suspend` or another function with the same spelling as the canonical wrapper
- **THEN** it receives ordinary function behavior unless its body explicitly calls the sealed intrinsic

#### Scenario: Keep execution storage out of the intrinsic contract

- **WHEN** tooling renders the nested suspension intrinsic's canonical callable contract
- **THEN** the result contains only the deferred child's `A ! E ? R` channels and no `OutOfMemoryError`, `Allocator`, Execution, or Wake contribution
