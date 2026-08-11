## Purpose

Define the smallest explicit compiler primitive surface from which Silk source can build its
standard library, services, and portable application APIs without hidden name-based privilege.

## ADDED Requirements

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
