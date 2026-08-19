# Bootstrap Intrinsic Boundary Specification

## Purpose

Define the smallest explicit compiler primitive surface from which Silk source can build its
standard library, services, and portable application APIs without hidden name-based privilege.
## Requirements
### Requirement: Callable compiler primitives occupy one sealed namespace

Every source-callable operation selected by compiler identity SHALL be a qualified member of the
compiler-sealed `Intrinsic` namespace. No service, interface, standard-library actor, or ordinary
source declaration name outside that namespace MAY select special elaboration, HIR, MIR,
evaluation, or backend behavior. Language syntax and primitive type identities are not callable
intrinsics merely because the compiler implements them.

#### Scenario: Recognize one explicit intrinsic

- **WHEN** source calls a declared member of `Intrinsic`
- **THEN** analysis records its canonical intrinsic identity and every execution engine applies the same primitive contract

#### Scenario: Reject hidden name privilege

- **WHEN** an ordinary source actor or operation has the same spelling as a former compiler-known abstraction
- **THEN** it resolves as ordinary source and receives no compiler behavior from its spelling

### Requirement: Compiler features obey the minimal intrinsic rule

A compiler feature SHALL add only the smallest target-neutral primitive contract sufficient for
canonical Silk source to implement the intended public abstraction. Validation, generic lifting,
service policy, presentation policy, and reusable safe composition MUST remain in source whenever
the primitive surface makes that possible. Each admitted intrinsic MUST have a shipped source
consumer or be required directly by language syntax, safety, representation, or backend lowering.

#### Scenario: Reject an abstraction-shaped intrinsic

- **WHEN** a proposed intrinsic duplicates behavior that can be expressed from existing primitives in ordinary Silk source
- **THEN** the intrinsic catalog and its verification reject the new compiler-known operation

#### Scenario: Add one irreducible primitive

- **WHEN** a standard-library implementation cannot perform a required representation or platform operation in Silk
- **THEN** the compiler exposes only that primitive and the public API remains a source declaration over it

### Requirement: Intrinsic safety is explicit and local

Membership in `Intrinsic` SHALL NOT by itself imply safety or unsafety. An intrinsic whose caller
must establish a memory, ownership, pointer, initializedness, or platform ABI invariant MUST require
an explicit unsafe boundary. A primitive whose complete contract preserves safe-code guarantees
SHALL remain callable from safe code even when it can return a typed failure or trap.

#### Scenario: Call a safe scalar primitive

- **WHEN** a standard-library numeric wrapper calls a concrete checked or trapping scalar intrinsic
- **THEN** the call requires no unsafe boundary and preserves the intrinsic's value, result, or trap contract

#### Scenario: Reject unchecked storage outside unsafe

- **WHEN** source calls an intrinsic that reads unproved initialized storage outside an unsafe boundary
- **THEN** analysis rejects the call at that boundary

### Requirement: The intrinsic inventory is closed and auditable

The compiler SHALL publish one deterministic catalog of every intrinsic operation, signature,
safety classification, semantic operation, and supported target. Completion, hover, analysis,
evaluation, HIR, MIR, LLVM, and direct WebAssembly MUST consume that catalog or verified derived
data. Private host imports SHALL be traceable to catalog operations but SHALL NOT become additional
public intrinsic spellings.

#### Scenario: Compare the catalog with compiler branches

- **WHEN** verification scans compiler operation identities, lowering branches, and host imports
- **THEN** every callable compiler primitive maps to one catalog member and no unregistered public operation remains

#### Scenario: Preserve engine parity

- **WHEN** an accepted intrinsic program succeeds, fails, or traps
- **THEN** logical evaluation, native LLVM, and direct WebAssembly agree on its observable outcome

### Requirement: Raw buffers expose only minimal unsafe initialized views

The sealed `Intrinsic` namespace SHALL provide target-neutral unsafe operations that form shared and
exclusive slices from a live `RawBuffer`, an offset, and a length within its initialized extent. The
operations MUST NOT recognize `Vector` or any other source-defined owner, allocate, copy, resize, or
decide public collection policy. Ordinary Silk wrappers SHALL establish the initialized-range and
aliasing preconditions before invoking them.

#### Scenario: Form a shared initialized view

- **WHEN** an ordinary source wrapper proves that an offset and length are within a live raw buffer's initialized extent
- **THEN** the shared intrinsic returns a view over exactly that range without allocation or copying

#### Scenario: Form an exclusive initialized view

- **WHEN** an ordinary source wrapper holds exclusive access to a live raw buffer and supplies a valid initialized range
- **THEN** the exclusive intrinsic returns the corresponding exclusive view without naming the wrapper type

#### Scenario: Keep collection policy out of the compiler

- **WHEN** a source-defined owner uses the raw-buffer view operations
- **THEN** semantic analysis and lowering depend only on `RawBuffer` and slice invariants, not on the owner's declaration spelling

### Requirement: Intrinsic availability metadata is sealed and auditable

The canonical intrinsic inventory SHALL record a normalized supported-target set for every callable
operation and SHALL expose that data to executable planning, tooling, and inventory tests. Ordinary
source declarations MUST NOT override, infer, or attach compiler target privilege by spelling.

#### Scenario: Audit one restricted operation

- **WHEN** the intrinsic inventory is encoded for tests or tooling
- **THEN** the operation's canonical identity and sorted supported-target set appear in deterministic form

#### Scenario: Refuse source-defined target privilege

- **WHEN** ordinary source declares a function or service using the same name as a restricted intrinsic
- **THEN** it remains an ordinary declaration with no compiler availability metadata

### Requirement: OS filesystem privilege is handle-level and sealed

The `Intrinsic` namespace SHALL contain only the unsafe file open/read/write, directory open/next,
path inspection, directory creation, file removal, directory removal, and generic consuming close
operations required to build an OS provider. Their signatures SHALL use primitive scalars, slices,
output parameters, `Option`, `bool`, and opaque `OsHandle`; they MUST NOT use a source-defined
filesystem service or domain value.

#### Scenario: Build a source provider from low-level calls

- **WHEN** canonical `OsFileSystem` implements a whole-file read
- **THEN** it composes open, repeated read, and consuming close rather than invoking a compiler-known whole-file operation

#### Scenario: Keep portable operations ordinary

- **WHEN** another source-defined provider implements `FileSystem.readFile`
- **THEN** it can satisfy the service without invoking any OS intrinsic or receiving name-based compiler treatment

### Requirement: OS intrinsic invariants are explicit and unsafe

Each OS intrinsic contract SHALL document handle kind and liveness, initialized input/output ranges,
root-confinement inputs, retry behavior, consuming behavior, and target availability. Semantic
analysis SHALL reject safe-call syntax for these operations while preserving the sealed catalog as
the only source-callable compiler privilege.

#### Scenario: Require unsafe acknowledgement

- **WHEN** ordinary source calls an OS handle operation outside the language's unsafe call form
- **THEN** semantic analysis rejects the call even if its argument types otherwise match

#### Scenario: Consume close regardless of outcome

- **WHEN** consuming close reports failure
- **THEN** the source handle is nevertheless dead and cannot be retried or dropped as a live resource

### Requirement: String intrinsics expose only view primitives

The sealed `Intrinsic` namespace SHALL expose only the target-neutral primitives needed to preserve
the abstract `string` representation and implement its compiler-selected operators: unchecked
formation from a live UTF-8 byte view, immutable UTF-8 byte viewing, encoded byte length, and exact
equality. Unchecked formation MUST require an unsafe boundary; inspection and exact equality of
already valid strings SHALL remain safe. The intrinsics MUST NOT allocate, normalize, compare with
locale policy, traverse graphemes, recognize stdlib `String`, or decide owned storage behavior.
Each string intrinsic SHALL declare normalized availability for every current execution target,
and backend preparation SHALL reject reachable use for any target absent from that declaration
before target layout and MIR lowering.

#### Scenario: Audit the intrinsic catalog

- **WHEN** tooling enumerates the deterministic intrinsic catalog after this change
- **THEN** it finds the narrow string view operations with exact signatures, unsafe classification,
  normalized evaluator/native/Wasm availability, and no owning-String operation

#### Scenario: Use safe string inspection

- **WHEN** ordinary stdlib code asks an existing valid `string` for its bytes and byte length
- **THEN** it may call the inspection primitives outside unsafe because they cannot violate text or ownership invariants

### Requirement: One unsafe byte-input primitive is admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only byte-input operation taking an
exclusive byte buffer plus explicit reason and native-code outputs and returning `Option<usize>`. A
present count SHALL be the exact transferred byte count, zero SHALL mean the end of input, and an
absent result SHALL write the normalized low-level reason and native code. The compiler MUST NOT
construct or recognize `ReadOutcome`, `StreamReadFailure`, or the `StandardInput` service, and MUST
NOT admit a second input operation for buffering, decoding, or terminal control.

#### Scenario: Report a refused read

- **WHEN** the host refuses a standard-input read
- **THEN** the intrinsic returns `None` and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report the end of input

- **WHEN** the host reports that no further bytes will arrive
- **THEN** the intrinsic returns a zero count rather than a failure, and the library decides what that means

### Requirement: Two unsafe child-process primitives are admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only execution operation taking an
executable path, an argument block, an environment block, and a working-directory block as byte
slices plus explicit termination, capture-length, reason, and native-code outputs, and one unsafe
native-only capture operation taking a stream selector, an offset, and an exclusive byte buffer and
returning `Option<usize>`. The argument and environment blocks SHALL be NUL-terminated entry blocks,
and an empty working-directory block SHALL mean the caller's own directory. A successful execution
SHALL retain exactly one capture until the next execution replaces it. The compiler MUST NOT
construct or recognize `ProcessRequest`, `ProcessOutcome`, `ProcessFailure`, or the `ChildProcess`
service, and MUST NOT admit further operations for shells, streaming, or signal delivery.

#### Scenario: Report a failure to start

- **WHEN** the host cannot start the requested program
- **THEN** the execution operation reports failure and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report a nonzero exit code as success

- **WHEN** a child runs to completion and returns a nonzero code
- **THEN** the execution operation succeeds and reports that code as data, leaving the meaning to the library

#### Scenario: Copy one completed capture

- **WHEN** a capture reads the retained result of the immediately preceding execution
- **THEN** it commits the requested prefix into the caller's buffer and reports the exact transferred byte count

### Requirement: Four unsafe process-input primitives are admitted

The sealed `Intrinsic` namespace SHALL expose four unsafe native-only process-input operations: an
argument count with an exclusive `usize` output returning `bool`, and argument, environment-value,
and working-directory lookups each taking an exclusive byte buffer plus explicit reason and
native-code outputs and returning `Option<usize>`. A present count SHALL be the value's complete byte
length with the prefix that fits copied into the buffer; an absent result SHALL write the normalized
low-level reason and native code, where the not-found reason means the value does not exist. The
compiler MUST NOT construct or recognize `HostInputFailure` or the `HostInput` service, and MUST NOT
admit an operation that sets an environment variable, changes the working directory, or parses
arguments.

#### Scenario: Report a value longer than the buffer

- **WHEN** the host holds a value longer than the buffer the caller supplied
- **THEN** the intrinsic copies the prefix that fits and reports the complete byte length, without a separate buffer-too-small protocol

#### Scenario: Report an absent value

- **WHEN** an argument index is past the last argument or an environment name is unset
- **THEN** the intrinsic returns `None` with the not-found reason and the library decides what that means

#### Scenario: Report a refused lookup

- **WHEN** the host cannot answer a lookup at all
- **THEN** the intrinsic reports the normalized reason and native code without constructing a standard-library value

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

### Requirement: The public suspension API remains ordinary Silk

The canonical `Effect.suspend` operation SHALL be a shipped ordinary Silk declaration over the
single suspension intrinsic. Generic lifting, row composition, documentation, imports, navigation,
and reusable composition policy MUST remain in source and MUST NOT be selected by standard-library
module identity.

#### Scenario: Navigate to Effect.suspend

- **WHEN** tooling resolves a call to canonical `Effect.suspend`
- **THEN** it navigates to shipped Silk source whose only compiler privilege is its explicit sealed intrinsic call

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
