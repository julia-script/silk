## Purpose

Selected hosted source owns startup, process-input lifetimes and reporting policy.

## ADDED Requirements

### Requirement: Transparent finalization preserves diagnostic ownership

Finalization SHALL run an infallible unit finalizer after protected success or typed
failure and before exposing that outcome. It SHALL preserve the original payload,
diagnostic origin and owner instead of producing a replacement failure or selecting
it as a recovery cause. Public policy SHALL remain in source, using only a
target-neutral composition primitive where ordinary propagation cannot express it.

#### Scenario: Failed protected execution and suspended finalizer

- **WHEN** the protected Effect fails and its finalizer suspends before completing
- **THEN** the original failure payload and context remain owned until finalization
  completes, propagate unchanged afterwards, and are released once if the parked
  composition is destroyed

#### Scenario: Trap during execution

- **WHEN** either protected execution or finalization traps
- **THEN** no typed recovery or guaranteed finalizer execution is introduced

### Requirement: Source entry selection

Default/custom/none profiles SHALL independently select source runtime and application roots. Source SHALL implement the platform C entry ABI and exit conversion; libraries SHALL acquire no accidental entry. Shared Wasm entry/export consumers SHALL migrate without obsolete symbol adapters.

#### Scenario: Two profiles

- **WHEN** one project selects different source runtime roots
- **THEN** each artifact and cache identity reflects only its selected entry/requirements

#### Scenario: Application execution boundary

- **WHEN** the hosted runtime invokes an application returning integer, unit or an Effect producing unit with at most a mutable HostInput requirement
- **THEN** ordinary source specialization supplies process inputs and result conversion, admits nested transfer, and requires the complete application call to satisfy NonParking; external parking requires an explicit owner in application source

#### Scenario: Bootstrap failure

- **WHEN** owned process-input capture or application Execution creation fails
- **THEN** source drops the typed failure payload and requests policy 1 through a zero-capacity observer, preserving that status when reporting cannot allocate or write

#### Scenario: Standalone WebAssembly entry

- **WHEN** the LLVM-to-WebAssembly execution profile selects its source runtime
- **THEN** the host calls an explicit source C main export with no arguments, source converts integer/unit/Effect outcomes to exit status and owns nested execution, all service requirements are supplied by application source, and no diagnostic observer or host input service is implicitly added

### Requirement: Complete process inputs

Source SHALL implement argument count/indexing, environment lookup and cwd with byte preservation, absence versus empty distinction, short-buffer/full-length behavior, committed prefixes and explicit borrow/copy lifetimes. Private argument globals and all four host-input intrinsics SHALL be deleted.

#### Scenario: Short input buffer

- **WHEN** a process-input result exceeds the caller buffer
- **THEN** the result reports its full length, preserves the committed prefix and leaves the tail untouched

### Requirement: Terminal reporting ownership

Source SHALL own report state, bounded formatting/output, allocation-failure behavior and exit policy, with explicit instance/reentrancy ownership. Compiler semantics SHALL retain failure identity/origin/logical frames/causes and machine traps. Typed failure payload cleanup SHALL precede terminal reporting. Fatal traps SHALL gain neither typed recovery nor guaranteed reporting/cleanup.

#### Scenario: Failed report output

- **WHEN** terminal reporting cannot allocate or write
- **THEN** the declared bounded terminal policy applies without replacing typed-failure cleanup or promising trap unwinding

#### Scenario: Independent diagnostic lifetimes

- **WHEN** observed execution suspends, runs cleanup or reenters an independent library invocation
- **THEN** its outcome-owned identity, origin and retained context remain distinct, and cancellation releases context before the owning observer state

#### Scenario: Independently owned execution outlives its driver

- **WHEN** an Execution is driven inside a diagnostic observation and remains parked after that observation completes
- **THEN** its body retains no borrowed observer or selected cause from the driving caller, observation installed inside the body follows its own continuation lifetime, and outcome callbacks use the driving caller's context

#### Scenario: Selected recovery cause

- **WHEN** a selected failure handler is applied and its returned Effect executes
- **THEN** only that scope retains the protected failure as a cause, a failing handler attaches it once, and a successful handler releases it

#### Scenario: Optional report storage exhausted

- **WHEN** source cannot retain additional diagnostic frames or causes
- **THEN** the primary semantic identity and origin remain available for bounded allocation-free output, and no hidden process-global history is substituted
