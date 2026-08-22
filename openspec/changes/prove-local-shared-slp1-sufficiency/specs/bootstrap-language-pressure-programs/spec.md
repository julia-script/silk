## ADDED Requirements

### Requirement: Local shared ownership removes the SLP-0001 shared-state wall

The repository SHALL contain one readable ordinary-Silk pressure program in which multiple dormant
callbacks retain explicit cloned handles to one fixed-capacity ready inbox and producer/waiter actors
retain cloned handles to one Deferred-style value-and-waiter state. Registration, publication, and
enqueue operations SHALL use short local shared mutation callbacks. Every readiness callback or
other external executable SHALL be moved out of shared state and invoked only after access is
restored. The witness MUST NOT require a compiler-known Shared, queue, Deferred, Scheduler, callback
registry, or execution actor.

The witness SHALL agree across evaluation, native LLVM, and direct WebAssembly on enqueue order,
one-time publication, dormant callback cleanup, unpublished affine value cleanup, strong-count
transitions, and final allocation release. Its findings report SHALL distinguish the removed
shared-state wall from the execution-transfer and parking work that remains owned by SLP-0001.

#### Scenario: Enqueue from two dormant callbacks

- **WHEN** two independently retained callbacks run sequentially through cloned handles to one ready inbox
- **THEN** the inbox contains both identifiers in source execution order without either callback retaining an exclusive lexical borrow

#### Scenario: Publish after extracting waiters

- **WHEN** one producer publishes an affine value to several registered waiters
- **THEN** publication moves the callbacks out under one short mutation, restores access, invokes each callback afterward, and publishes the value only once

#### Scenario: Drop unpublished state exactly once

- **WHEN** the last Deferred-style handle is dropped while it contains an unpublished affine value or unconsumed callback
- **THEN** ordinary local-shared cleanup destroys every retained owner exactly once before releasing the state allocation

#### Scenario: Drop one dormant callback early

- **WHEN** a dormant callback holding an inbox clone is dropped before it runs
- **THEN** its handle decrements without releasing the inbox while another handle remains, and final cleanup still balances one acquisition and release

#### Scenario: Agree across engines

- **WHEN** the ready-inbox and Deferred-style acceptance cases run through evaluation, native LLVM, and direct Wasm
- **THEN** every engine reports identical logical results, callback order, payload cleanup, count transitions, and allocation release order

#### Scenario: Rename every witness actor

- **WHEN** the source-level inbox, Deferred-style state, and wrappers are renamed without changing their ordinary operations
- **THEN** semantic facts, verified MIR, and engine behavior remain equivalent with no actor-specific compiler branch

#### Scenario: Record the remaining SLP-0001 boundary

- **WHEN** the pressure findings are finalized
- **THEN** they mark local shared state sufficient while leaving execution transfer, parking, and wake-order policy to the separate SLP-0001 handoff
