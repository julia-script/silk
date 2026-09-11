## MODIFIED Requirements

### Requirement: Runtime specialization is finite and monomorphic

Runtime instance discovery SHALL key each generic function by its canonical declaration and
normalized runtime-relevant concrete type arguments with semantic lifetime arguments erased, record
the key before following dependencies, and require every ordinary recursive generic call to
preserve its current runtime-relevant type arguments. MIR and LLVM emission MUST receive only
concrete monomorphic instances and MUST NOT require runtime generic dictionaries or type
descriptors.

Instance discovery MAY follow a generic call with changed ordinary type arguments only while
executing cleanup selected by the concrete owner's cleanup plan. The cleanup work item SHALL carry
the exact selected cleanup target or hook and a finite measure derived from that plan. Every changed
ordinary type vector SHALL be an exact proper subterm of the preceding vector; a downstream helper
MAY preserve the current vector but SHALL NOT introduce another change unless it is the next strict
descent. Provider-owner cleanup SHALL use the same rule. The cleanup measure SHALL NOT propagate to
unrelated calls, and semantic lifetime arguments SHALL remain erased from comparisons.

#### Scenario: Discover two concrete instances

- **WHEN** the entry reaches `identity<i32>` and `identity<Token>`
- **THEN** discovery records exactly two deterministic instance keys and lowering produces two concrete MIR functions

#### Scenario: Terminate ordinary generic recursion

- **WHEN** `walk<T>` recursively calls `walk<T>`
- **THEN** discovery reuses the already recorded instance key rather than expanding a new instance

#### Scenario: Discover nested cleanup by strict descent

- **WHEN** the selected cleanup plan for an owner reaches Drop for a proper nested generic field type through its exact cleanup hook and helper calls
- **THEN** discovery follows the finite proper-subterm sequence and emits every required concrete cleanup instance

#### Scenario: Discover nested provider-owner cleanup

- **WHEN** a selected lexical provider owns a generic value whose cleanup reaches a proper nested field type
- **THEN** provider cleanup follows the same cleanup-plan-derived strict descent and remains finite

#### Scenario: Reject polymorphic recursion

- **WHEN** an ordinary recursive call changes `T` to `[T; 1]` outside an exact selected cleanup plan
- **THEN** analysis rejects the call before instance discovery can expand indefinitely

#### Scenario: Erase different caller lifetimes

- **WHEN** two calls differ only in source owners or inferred regions
- **THEN** they share runtime instance keys, layout identities, and backend symbols without lifetime tokens, reference counting, or borrowing allocation
