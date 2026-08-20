# bootstrap-nominal-callable-storage Specification

## Purpose

Define statically specialized nominal storage for named and capturing callable values across
ownership, layout, lowering, cleanup, and all executable engines.

## Requirements

### Requirement: Nominals store concrete callable representations inline

A callable-bounded representation field SHALL become layoutable when the complete nominal identity
contains one concrete callable representation. Layout and lowering SHALL use that representation's
target, ordered capture environment, access, and cleanup rather than the structural callable contract.

#### Scenario: Store a capturing section

- **WHEN** a nominal construction stores a section with a Copy capture
- **THEN** the complete nominal has one finite inline capture layout and one static call target

### Requirement: Aggregate access preserves callable invocation modes

Shared aggregate access SHALL permit only `fn`, exclusive aggregate access SHALL permit `fn` and
`mut fn`, and consuming whole-owner access SHALL permit `fn`, `mut fn`, and `once fn`. A weaker
aggregate access MUST NOT invoke a stronger callable mode.

#### Scenario: Reuse a shared callable through a borrow

- **WHEN** a shared-borrowed parser stores reusable `fn(Arguments) -> A`
- **THEN** repeated parsing invokes the static target without consuming the parser

#### Scenario: Reject take-only invocation through a shared borrow

- **WHEN** a shared-borrowed parser stores `once fn(Arguments) -> A`
- **THEN** ownership rejects invocation and identifies the required whole-owner take access

### Requirement: Representation-bearing nominals use ordinary aggregate ownership

A nominal containing a callable representation field SHALL derive Copy, affine ownership, moves,
partial-move rejection, and cleanup from its admitted `impl Copy` and the concrete realized capture
fields. A callable representation with only Copy snapshots or shared borrows MAY participate in a
Copy aggregate; an owned affine or exclusive capture SHALL keep the aggregate affine. Take-once
invocation SHALL consume the complete aggregate, and access validation remains independent.

#### Scenario: Copy a reusable stored callable

- **WHEN** an aggregate validly implements `Copy` and its concrete callable realization contains only Copy captures
- **THEN** an ordinary read duplicates the complete aggregate and neither source has cleanup

#### Scenario: Reject direct affine callable extraction

- **WHEN** source attempts to move an affine callable field out of an otherwise live aggregate
- **THEN** ownership reports the ordinary aggregate partial-move diagnostic

### Requirement: Callable captures clean exactly once

Uninvoked, shared-invoked, exclusively invoked, and consuming callable environments SHALL clean each
live owned capture exactly once across success, typed failure, whole-value movement, and scope exit.
Scoped captures MUST NOT escape through an enclosing nominal.

#### Scenario: Drop an uncalled stored callable

- **WHEN** a move-only nominal containing an uncalled owned-capture section leaves scope
- **THEN** every live capture is cleaned exactly once

### Requirement: Callable storage has cross-engine static parity

HIR, MIR, evaluator, native LLVM, and direct WebAssembly SHALL preserve the same aggregate field
paths, concrete target, capture layout, access checks, result, and cleanup trace. Direct Wasm MUST
NOT add a function table or `call_indirect` for this capability.

#### Scenario: Execute one stored callable through every engine

- **WHEN** the same stored-callable acceptance program is evaluated, compiled natively, and emitted
  as direct WebAssembly
- **THEN** all engines agree and structural inspection finds only direct targets

### Requirement: Storage fences retire only for proven paths

`SEM0103` SHALL remain for every reachable stored-callable path whose representation is unknown or
unsupported by ownership, layout, MIR, or any executable engine. A known unequal join SHALL use the
representation-join diagnostic instead of pretending the field has no layout.

#### Scenario: Keep an unsupported path fenced

- **WHEN** frontend analysis knows a callable identity but one backend cannot realize its environment
- **THEN** compilation retains the pre-MIR fence rather than producing partial support
