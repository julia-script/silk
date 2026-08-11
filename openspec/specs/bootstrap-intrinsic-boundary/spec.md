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
