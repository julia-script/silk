# bootstrap-independent-execution-pressure Specification

## Purpose

Defines connected ordinary-Silk evidence for source-owned scheduling, event waits, alternate-owner
reuse, static pay-for-use, publication safety, and the compiler–library privilege boundary.

## Requirements

### Requirement: Ordinary source implements the driving execution and wake cases

A checked-in ordinary-Silk pressure suite SHALL implement: deferred owner-selected first activation;
a source-owned one-shot result awaited while another execution progresses; a same-thread timer
registration and reactor; dropping a Dormant outer execution before the timer fires; and a
Coroutine-shaped owner that yields through source payload state and Wake. The suite SHALL use only
the general Execution/Wake intrinsics plus ordinary Layout, Allocation, Allocator, Effect, and the
SLP-0002 local Shared capability. It MUST NOT establish canonical Scheduler, Fiber, Deferred, Timer,
or Coroutine APIs.

#### Scenario: Choose first activation in homogeneous owner storage

- **WHEN** a source owner erases two distinct exact Effect body representations into the same `Execution<TaskOutput>` type, stores both in one homogeneous task owner, publishes them Initial, and selects the second first
- **THEN** the first body remains unrun and every supported target observes the owner-selected activation order through one homogeneous storage contract

#### Scenario: Await a source-owned result

- **WHEN** a waiter parks on source Deferred-shaped state and a producer publishes `42`
- **THEN** the producer extracts and consumes Wake after shared access, the owner later resumes the waiter, and the connected program completes with `42`

#### Scenario: Drive a same-thread timer

- **WHEN** a detached child owns its Timer provider, fallible timer registration preparation succeeds before park, and the explicit outer driver polls its retained reactor
- **THEN** reactor readiness propagates through source state, a sibling may progress first, the outer execution later completes with reified success or declared ordinary failure data, and Timer.sleep gains no Scheduler or Fiber requirement

#### Scenario: Fail timer preparation before parking

- **WHEN** ordinary timer allocation or system registration preparation fails
- **THEN** the declared source failure is observed before park, no Wake registration or external relinquishment begins, and no Scheduler/Fiber or compiler privilege is introduced

#### Scenario: Cancel before timer readiness

- **WHEN** the explicit driver drops the Dormant outer Execution before firing the timer
- **THEN** cascading source cleanup cancels outer and child waits, a later timer Wake is a consuming no-op, and no outer readiness or redrive occurs

#### Scenario: Reuse the substrate from a Coroutine-shaped owner

- **WHEN** ordinary source stores yielded payload and Wake in a shared port and repeatedly signals then drives one Execution
- **THEN** it observes the chosen yield sequence and final value using the same package, drive, park, wake, resume, and destroy machinery with no Scheduler-specific compiler path

#### Scenario: Drop the Coroutine-shaped owner while yielded

- **WHEN** the owner drops its Execution while the shared port retains a yielded payload and Wake
- **THEN** guard, frame, port payload, and endpoint state clean exactly once, the retained Wake is a safe consuming no-op, final authority releases the package, and no Scheduler-specific path runs

### Requirement: The source owner remains acyclic and closed-leaf only

The Scheduler-shaped pressure owner SHALL keep task ownership and readiness routing distinct: the
TaskStore SHALL own each Execution, while an Execution endpoint SHALL own only detached ReadyInbox
routing state and TaskId, never the TaskStore or Scheduler provider that owns it. A child passed to
the owner SHALL have every requirement eliminated with owned Detached providers before Execution
construction. A child that still requires Scheduler or Allocator, including a nested join against
the owning Scheduler, MUST fail the closed/Detached boundary; the intrinsic MUST NOT inherit,
extend, or clone providers automatically.

#### Scenario: Preserve acyclic endpoint routing

- **WHEN** a task Execution parks, becomes eligible, completes, or is cancelled
- **THEN** endpoint routing reaches the ReadyInbox without retaining the TaskStore owner, and final cancellation can release every strong source handle

#### Scenario: Reject implicit nested provider inheritance

- **WHEN** source attempts to schedule a child Effect that still requires Scheduler or Allocator for a nested join
- **THEN** the closed/Detached construction boundary rejects it with the retained requirement/provider cause and no automatic provider inheritance

### Requirement: Source publication is all-or-nothing and wake-time allocation free

Every source task witness SHALL reserve task and readiness capacity, construct its fixed endpoint,
procure the exact combined package, and initialize the Execution before observational publication.
Failure at any pre-publication allocation ordinal SHALL clean body, result state, endpoint,
reservation, and package exactly once and SHALL publish no task. A waiter allocation performed by a
later join/await operation SHALL fail that operation through its declared ordinary channel while
leaving already published tasks valid and owned. Park, wake, endpoint notification, and task-specific ready
identity publication SHALL allocate nothing and introduce no hidden failure. Unknown callbacks
MUST run only after any `Shared` access ends.

#### Scenario: Fail each construction ordinal

- **WHEN** the source allocator fails at each pre-publication task, result, reservation, shared-state, or package allocation exercised by the witness
- **THEN** no partial task becomes observable and every prior affine value and allocation is cleaned exactly once

#### Scenario: Fail a waiter allocation after task publication

- **WHEN** a later join or await operation cannot allocate its ordinary waiter node after tasks are published
- **THEN** that operation returns its declared allocation failure, begins no park, and leaves every published task and Execution valid for owner cleanup or later progress

#### Scenario: Publish one complete Initial task

- **WHEN** every reservation and package allocation succeeds
- **THEN** the owner publishes one Initial Execution together with its source identity and can later choose its first activation

#### Scenario: Notify without allocation

- **WHEN** a parked condition becomes ready after task publication
- **THEN** source extracts Wake, ends Shared access, consumes Wake, and publishes one pre-reserved task identity without allocator access or failure

#### Scenario: Discard a stale identity

- **WHEN** readiness was queued and the corresponding Eligible Execution is destroyed before selection
- **THEN** the source owner consumes its ordinary tombstone/identity state without accessing freed Execution storage or invoking compiler policy

### Requirement: Static artifacts prove five pay-for-use configurations

Committed structural evidence SHALL distinguish: ordinary direct execution with no suspension
runtime; ordinary nested-only execution with the existing LIFO machinery and no independent owner or
Wake; explicit non-suspending Execution with one owned erased body package and no readiness state;
explicit nested-only Execution with that package plus internal LIFO frames but no Wake,
notification, or dormant-owner state; and explicit external-park Execution with execution-owned
continuation and Wake state. A dynamic
branch that does not park SHALL still use the external tier when parking remains statically
reachable. Importing or naming source policy actors MUST select no tier.

#### Scenario: Inspect a direct program

- **WHEN** a program uses ordinary `run` and reaches no suspension
- **THEN** its native/LLVM-to-Wasm artifacts contain no suspension runner, Execution package, Wake cell, scheduler, or atomic support

#### Scenario: Inspect a nested-only program

- **WHEN** a program uses `Effect.suspend` but no explicit Execution or external park
- **THEN** artifacts retain bounded-stack nested transfer and contain no independent execution owner, Wake, ready queue, or atomic support

#### Scenario: Keep local Shared affinity separate from direct execution machinery

- **WHEN** an ordinary direct program captures a local Shared handle and reaches no suspension
- **THEN** semantic inspection reports `LocalExecution` while native/LLVM-to-Wasm artifacts omit the independent Execution owner, package, Wake, and external-parking runtime and retain only any separately required local-Shared slice

#### Scenario: Keep local Shared affinity separate from nested execution machinery

- **WHEN** an ordinary nested-only program captures a local Shared handle and uses `Effect.suspend` without explicit Execution or external park
- **THEN** semantic inspection reports `LocalExecution` while artifacts retain only local-Shared and bounded-stack nested-transfer machinery and omit the independent owner, package, Wake, notification, and dormant-owner state

#### Scenario: Inspect an explicit non-parking execution

- **WHEN** a closed direct body is explicitly packaged with the zero-sized endpoint
- **THEN** artifacts contain the owned erased-body lifecycle package but omit wake and dormant-continuation state

#### Scenario: Inspect an explicit nested-only execution

- **WHEN** an explicitly packaged body reaches nested `Effect.suspend` but cannot reach park
- **THEN** native and LLVM-generated WebAssembly complete it through one owner drive using internal LIFO transfers, retain the package plus nested frames, and omit Wake, notification, dormant-owner, and atomic support

#### Scenario: Inspect an external-park execution

- **WHEN** the explicit body can statically reach park even though one runtime path completes directly
- **THEN** artifacts contain the external-park package tier for that specialization and no owner-side source function becomes parkable through drive

### Requirement: Compiler privilege remains actor neutral

Semantic, HIR, MIR, and LLVM inventories SHALL contain no branch keyed by the
spelling of Execution safe wrappers, Allocator, OutOfMemoryError, Scheduler, Fiber, Deferred, Timer,
Coroutine, ready inbox, task store, reactor, or result actor. Renaming every pressure actor SHALL
preserve intrinsic facts and behavior. The checked-in findings SHALL distinguish substrate evidence
from deferred canonical concurrency, Coroutine, implicit-root, and parallel-memory decisions.

#### Scenario: Rename all witness actors

- **WHEN** an equivalent pressure fixture renames every source policy actor
- **THEN** normalized semantic facts, MIR transitions, engine outcomes, and intrinsic inventories remain equivalent apart from ordinary source identities

#### Scenario: Audit compiler branches

- **WHEN** implementation inventories source-name comparisons and intrinsic dispatch across all compiler and engine phases
- **THEN** only sealed Intrinsic identities grant privilege and no library declaration is recognized by spelling

#### Scenario: Preserve deferred decisions

- **WHEN** findings document nested provider inheritance, structured cancellation, implicit entry ownership, or cross-thread Wake delivery
- **THEN** they identify the dependent SLP boundary rather than adding fallback compiler behavior or canonical APIs to this change

### Requirement: The selected capability point remains distinct from its smaller alternative

Evidence SHALL record that an eager-start owner-sweep execution with fatal runtime packaging is a
coherent rejected alternative, not a compatibility fallback. The realized substrate SHALL preserve
the three selected guarantees together: Initial owner-controlled activation, task-specific push
readiness, and recoverable caller-funded package admission. No implementation path MAY silently
replace one guarantee with eager start, legal dormant polling/sweeps, or hidden fatal packaging.

#### Scenario: Distinguish eager start from Initial ownership

- **WHEN** a task is published before any drive
- **THEN** its body remains unrun, proving the implementation did not substitute eager-to-first-relinquishment semantics

#### Scenario: Distinguish push readiness from owner sweeping

- **WHEN** one of many dormant tasks becomes ready
- **THEN** its Wake publishes exactly that task's source identity through pre-reserved routing and structural evidence contains no owner-wide dormant scan

#### Scenario: Distinguish recoverable admission from fatal packaging

- **WHEN** the exact package allocation is refused before task publication
- **THEN** ordinary typed failure and rollback occur without starting the body or terminating through runtime stack exhaustion

### Requirement: User documentation preserves the selected execution distinctions

Language and standard-library documentation SHALL distinguish compiler-private representation from
source-visible relinquishment, nested child-completion transfer from external-wake parking, Initial
ownership from eager start, and package admission failure from fatal later stack growth. It SHALL
document that a forgotten cancelled Wake retains the complete inert combined Allocation after all
values are cleaned, and SHALL not present pressure actors as canonical APIs.

#### Scenario: Read the execution model documentation

- **WHEN** a user reads the updated Effect suspension and intrinsic/runtime boundary documentation
- **THEN** the direct, nested, explicit-owned, and external-park behaviors and their failure/resource boundaries are stated without exposing a public continuation or canonical Scheduler
