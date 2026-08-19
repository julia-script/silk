# SLP-0007: Explicit unsafe contracts and sealed intrinsics

SLP: 0007
Status: Candidate
Revision: 12
Author: Julia Ortiz
Created: 2026-08-19
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0003, SLP-0004, SLP-0005, SLP-0006
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Silk makes unsafety an explicit source contract rather than a relaxed checking
mode. A lexical `unsafe { ... }` boundary acknowledges the additional obligations of unsafe
operations but preserves ordinary type, ownership, lifetime, Effect, and visibility checking. The
sealed `Intrinsic` namespace is the only source-callable compiler-privilege surface; each intrinsic
has one auditable signature, safety classification, invariant, and supported-target set. Ordinary
Silk code builds safe policy and abstractions above that surface, and unsupported target-specific
intrinsics are diagnosed only when they enter the selected executable closure.

## Problem and evidence

Silk already parses lexical unsafe blocks, classifies individual intrinsics as safe or unsafe,
records supported execution targets in one catalog, and rejects unsupported reachable calls before
lowering. It also relies on ordinary Silk wrappers to build strings, allocation owners, collections,
filesystems, processes, and host input from narrow primitives.

The programmer-facing model is not gathered in one place. More importantly, current source can
acknowledge an unsafe intrinsic call but cannot declare an ordinary reusable function whose caller
must uphold an unsafe precondition. Without that boundary, a library must either duplicate raw
intrinsic calls at every use or hide an unverified caller obligation behind a function that appears
safe. The language also needs one answer for what `unsafe` permits, which checks remain active, how
Effect and ownership contracts behave, and when target restrictions reject an otherwise valid
program.

## Driving examples: current and desired

### Case: Encapsulate an unchecked primitive without pretending the wrapper is safe

#### Intent

Expose a reusable low-level UTF-8 construction operation whose caller must prove that the supplied
bytes are valid UTF-8.

#### Current Silk

```silk
fn localUnchecked(bytes: &[u8]) -> string {
  unsafe {
    return Intrinsic.stringFromUtf8Unchecked(bytes)
  }
}
```

The intrinsic call is explicit, but `localUnchecked` has an ordinary safe signature. Nothing in its
call surface tells another module that calling it transfers the UTF-8 proof obligation.

#### Desired Silk

```silk,ignore
pub unsafe fn fromUtf8Unchecked(bytes: &[u8]) -> string {
  unsafe {
    return Intrinsic.stringFromUtf8Unchecked(bytes)
  }
}

pub fn fromUtf8(bytes: &[u8]) -> Result<string, InvalidUtf8Error> {
  if Utf8.isValid(bytes) {
    return Result.succeed(unsafe {
      String.fromUtf8Unchecked(bytes)
    })
  }
  return Result.fail(InvalidUtf8Error {})
}
```

#### Observable result

Calling `fromUtf8Unchecked` requires a lexical unsafe boundary even though it is ordinary Silk
source. Calling `fromUtf8` does not: the wrapper checks the invariant before entering its small
unsafe region. Neither function gains hidden compiler identity.

#### Boundary case

```silk,ignore
let text = String.fromUtf8Unchecked(bytes)
// invalid: the unsafe contract is called outside `unsafe { ... }`
```

The declaration carries the caller obligation, while the body remains safe-by-default and still
requires nested lexical boundaries around the unsafe operations it performs.

### Case: Keep compiler privilege narrow and target availability pay-for-use

#### Intent

Ship portable source that contains a native adapter without rejecting a Wasm executable that never
uses that adapter.

#### Current Silk

The intrinsic catalog already records safety and normalized evaluator, LLVM, and Wasm availability.
Executable planning validates target-restricted operations after reachability.

#### Desired Silk

```silk,ignore
fn nativeAdapter(input: &[u8], output: &mut [u8]) -> usize {
  unsafe {
    return Intrinsic.osHostWorkingDirectory(output, reason, code)
  }
}

pub fn portableMain() -> i32 {
  return 42
}
```

The example is schematic; exact OS intrinsic operands remain a realization detail rather than the
public lesson.

#### Observable result

A Wasm build rooted at `portableMain` succeeds and contains no native host import. If the selected
entry reaches `nativeAdapter`, planning reports the unavailable intrinsic and requested target
before MIR or backend failure.

#### Boundary case

Merely importing or type-checking the module containing `nativeAdapter` does not make the intrinsic
reachable and does not reject the portable program.

## Goals and non-goals

### Goals

- Define the programmer-visible meaning and scope of `unsafe`.
- Define how ordinary Silk declarations expose unsafe caller contracts.
- Preserve all ordinary static checks inside unsafe code.
- Define the sealed `Intrinsic` namespace as the sole source-callable compiler privilege.
- Define per-operation safety, invariants, and reachable-only target availability.
- Keep validation, policy, safe wrappers, services, and public domain types in ordinary Silk.

### Non-goals

- Stabilize a user-facing C FFI or public ABI.
- Inventory every intrinsic operation in the language proposal.
- Define the complete standard library or platform-service APIs.
- Add unchecked variants of safe language syntax merely for performance.
- Promise cleanup, recovery, or stack unwinding after undefined behavior or a fatal trap.
- Add target-conditional compilation, feature flags, or source reflection.

## Current language model

`unsafe { ... }` is a statement block. Semantic analysis records its lexical span and admits calls
whose canonical intrinsic catalog entry is marked unsafe only inside that span. Safe intrinsics do
not require the boundary. Unsafe blocks otherwise flow through ordinary elaboration, ownership,
lowering, semantic facts, formatting, and tooling.

Every intrinsic is a compiler-owned member of `Intrinsic` with catalog metadata for its signature,
safety, invariant where relevant, admission reason, source consumer, compiler operation identity,
supported execution targets, and optional private host import. Name-equivalent ordinary source
receives no privilege. Target validation considers only retained executable intrinsic calls.

The current parser has no ordinary `unsafe fn` declaration form. Its rendered intrinsic signatures
may say `unsafe fn`, but that is compiler catalog presentation rather than a source-declarable
contract.

## Proposed language model

Unsafe code means “the programmer is taking responsibility for named additional invariants,” not
“the compiler stops checking.” Ordinary source may propagate caller obligations through an explicit
`unsafe fn` contract, but the declaration does not create an unsafe body scope. Each implementation
still marks the local operations whose obligations it accepts.

Target availability belongs to the selected executable, not the loaded source graph. A restricted
intrinsic is valid source on every target but is not executable on targets absent from its catalog
entry. Public target policy remains ordinary module and service design above that primitive fact.

## Worked language experience

For one invocation, `unsafe operation(arguments)` produces the call's ordinary result and grants
permission only to that direct call, not to calls nested in its arguments. `unsafe { ... }` remains
a statement region for several operations and does not gain special trailing-expression semantics.

An unsafe effect function is acknowledged when its call constructs the lazy Effect. The returned
Effect is ordinary and `run` requires no second unsafe marker. The caller's construction obligation
covers the value's complete lifetime and every execution allowed by its explicit run-access mode;
unsafe neither makes an Effect eager nor adds a fourth Effect channel.

Unsafe qualifies individual ordinary, effectful, reusable, mutable, and consuming callable
contracts. Safe callables may satisfy unsafe-facing contracts because they require fewer caller
obligations; unsafe callables cannot satisfy a promised-safe contract. Generic bounds,
representations, interfaces, and services preserve that direction. Declarations and conformances do
not gain ambient unsafe variants.

`Intrinsic` is a globally available sealed compiler namespace. Every module may call it; the
standard library receives no path-based trust or exclusive access. Each operation owns one canonical
contract and independent safety classification, and every unsafe entry states its exact caller
invariant. Admission requires the smallest irreducible machine, representation, safety, execution,
or private-host primitive plus a real source consumer or direct language necessity. Convenience and
abstraction-shaped compiler APIs are rejected.

Target availability is validated only after concrete specialization and executable reachability.
Loaded but unreachable restricted primitives are valid and contribute no artifact support. A
reachable unsupported intrinsic is one deterministic compile-time compatibility error before
layout or lowering, never a typed failure, trap, service requirement, or runtime fallback. Ordinary
runtime branches remain part of executable behavior, and the first stable model adds no target
conditional source or reflection.

Safe Silk retains ordinary values, typed failures, and defined fatal traps as its complete outcome
model. Undefined behavior begins only after a stated unsafe precondition is violated. Entering an
unsafe region does not otherwise weaken semantics. Optional debug checks may detect a violation and
trap, but no program may depend on that detection or on cleanup after undefined behavior.

Unsafe never suspends initialization, ownership, borrowing, liveness, invocation access, or cleanup
checking. A low-level operation that performs an otherwise unprovable state transition exposes that
transition through its explicit contract so ordinary checking can resume after the call. Correct
unsafe code receives normal cleanup on success and typed failure; unsafe blocks do not suppress
`Drop`. Fatal traps and violated unsafe contracts retain their respective no-cleanup and
no-guarantee boundaries.

Partial application constructs a callable rather than invoking its body, so partially applying an
unsafe operation is safe and produces another unsafe callable. The application that supplies the
final missing arguments requires acknowledgement. Captured arguments still undergo ordinary move
and lifetime checking when the partial application is formed.

Unsafe declarations use ordinary module and visibility rules. Source authors should state the
caller-owned preconditions in an attached nonempty `# Safety` documentation section. Missing safety
documentation is an LSP warning rather than a compiler error: the compiler enforces invocation
acknowledgement independently, while hover and generated documentation surface the prose contract.

A safe wrapper is ordinary Silk that establishes an unsafe precondition through validation or
structural guarantees, then chooses its own ordinary value, typed-failure, trap, or unsafe caller
contract. The compiler verifies the acknowledgement but does not prove the wrapper's safety
argument or privilege it by identity. Direct intrinsic calls remain legal, and names such as
`Unchecked` or `Raw` have no semantic force.

## Semantic sketch

- Unsafe permission is lexical and never inferred from function names, modules, or types.
- Unsafe permission admits only operations whose contracts explicitly require it.
- Ordinary source may declare `unsafe fn`; its callable contract preserves the caller obligation.
- An `unsafe fn` body remains safe-by-default and locally marks every unsafe operation it invokes.
- `unsafe call(...)` acknowledges one direct invocation and composes in an ordinary expression;
  `unsafe { ... }` acknowledges a multi-statement region without becoming a value-producing block.
- Calling an unsafe effect function acknowledges construction of one ordinary Effect; its eventual
  `run` has no second unsafe rule, and repeatability remains explicit in the Effect contract.
- Unsafety qualifies one callable operation. A safe implementation may satisfy an unsafe contract,
  but an unsafe implementation cannot satisfy a safe contract or erase its obligation through a
  generic, representation, interface, or service boundary.
- `Intrinsic` is globally available but sealed; only its qualified canonical members receive
  compiler identity, and standard-library source has no additional calling privilege.
- Each intrinsic has one cross-phase contract and must be the smallest irreducible primitive with a
  real source consumer or direct language necessity.
- Target availability is checked over the selected executable closure. Unreachable restricted
  primitives are free; reachable unsupported calls are compile-time errors before lowering.
- Undefined behavior is confined to violated explicit unsafe contracts. Correct unsafe code remains
  specified; debug detection may trap but is not a portable guarantee.
- Unsafe permission does not suspend initialization, ownership, borrowing, liveness, invocation
  access, or cleanup. Raw transitions require explicit operation contracts with trackable
  post-state.
- Partial application of an unsafe callable is safe and preserves the unsafe qualifier; only the
  arity-satisfying invocation requires acknowledgement.
- Unsafe declarations use ordinary visibility. Their caller preconditions belong in a `# Safety`
  documentation section whose absence is an LSP warning, not a compiler error.
- Safe wrappers are ordinary source proofs without compiler identity. They establish preconditions,
  choose their own ordinary outcome contract, and are unsound if a safe surface admits a violated
  invariant; direct intrinsic calls remain legal.
- Type checking, initialization, ownership, borrows, moves, cleanup, Effect channels, visibility,
  generic bounds, and conformance rules remain active inside unsafe code.
- `Intrinsic` is sealed and has one auditable compiler-owned operation catalog.
- Each unsafe operation states the invariant the caller assumes responsibility for.
- Ordinary source wrappers may validate an invariant and expose a safe API.
- A target restriction is checked only for a reachable intrinsic call in the selected executable.
- An unreachable restricted operation contributes no host import or backend support.

## Compiler–standard library boundary

### Compiler necessity

Ordinary Silk cannot directly perform machine memory access, create values from unproved
representation invariants, invoke private host ABI operations, or lower certain scalar primitives.
The compiler must own those irreducible operations and enforce where unsafe acknowledgement and
target support are required.

### Smallest target-neutral primitive

One sealed catalog entry per irreducible operation: canonical identity, exact generic and value
contract, safety bit and invariant, supported-target set, semantic operation, and any private host
binding. Unsafe syntax grants lexical permission to call an unsafe contract; it does not encode
library policy.

### Standard-library construction

Ordinary Silk code owns validation, domain errors, safe wrappers, owner types, cleanup, services,
provider implementations, buffering, retries, text policy, filesystem policy, and API composition.
An intrinsic must not name those source abstractions merely to save wrapper code.

### Privilege audit

Compiler-known standard-library actors, name-based hooks outside `Intrinsic`, whole-service
intrinsics, automatic unsafe permission, and target checks on unloaded execution paths are broader
than required. User-declared unsafe functions preserve honest caller obligations but grant no
compiler privilege and no ambient permission to their bodies.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Resolved | `unsafe fn`, direct invocation acknowledgement, statement blocks, ordinary declaration visibility, and sealed namespace resolution are defined. |
| Types and abstraction | Affected | Unsafe callable contracts may need exact/generic/opaque representation behavior without becoming a new runtime type. |
| Execution contracts | Affected | Ordinary and effect functions need the same caller obligation; unsafe does not alter typed failures or traps. |
| Ownership and resources | Resolved | Unsafe admits named invariants while preserving moves, borrows, liveness, initialization state, and ordinary cleanup for correct execution. |
| Runtime and targets | Affected | Catalog target sets, reachable-only validation, host imports, evaluator/native/Wasm parity, and undefined-behavior boundaries matter. |
| Compiler | Affected | Parsing, semantic admission, catalog verification, reachability, HIR/MIR, evaluation, and backends participate. |
| Standard library | Resolved | Safe wrappers and platform providers remain ordinary source over minimal intrinsics; no wrapper is privileged, registered, or inferred by name. |
| Tooling and diagnostics | Resolved | Compiler diagnostics identify missing acknowledgement or unsupported targets; LSP hover exposes safety contracts and warns when source unsafe documentation is absent. |
| Learning and use | Resolved | Teach unsafe as acknowledgement of one stated caller obligation, distinct from recoverable typed failure, defined fatal trap, and violated-contract undefined behavior. |

## Scope cohesion

Unsafe syntax, intrinsic privilege, and target availability currently form one proposed boundary:
unsafe states who owns a primitive invariant, `Intrinsic` identifies the operations that may carry
compiler privilege, and target metadata states where those operations can execute. Split target
availability only if it develops source-level target selection independent of intrinsic use.

## Complexity and subtraction budget

Prefer one lexical permission mechanism and one sealed catalog. Do not add unsafe modules, unsafe
types, ambient modes, capability tokens, name-based compiler hooks, target annotations on ordinary
source, or a second intrinsic namespace without a driving case.

## Surface displacement

Potentially adds source-declarable unsafe function contracts. Consolidates scattered unsafe and
target rules into one reference. Removes any remaining compiler-known library operation by spelling
and any eager target rejection based only on loaded source.

## Drawbacks and risks

- User-declared unsafe functions make library unsoundness possible when authors claim an
  insufficient contract or use unsafe primitives incorrectly.
- Requiring nested unsafe blocks inside unsafe functions adds ceremony but keeps dangerous
  operations locally visible.
- Reachable-only availability can make target errors appear only after entry selection, although
  tooling can still report non-blocking portability information earlier.
- An overgrown intrinsic inventory would undermine the ordinary-Silk standard-library model even
  if every entry is technically cataloged.

## Alternatives and prior art

### Status quo

Keep unsafe blocks only and permit only compiler-owned intrinsics to require them. This is smaller
but cannot honestly expose reusable source-defined caller obligations.

### Smaller primitive or library solution

Make every source wrapper safe and document preconditions in prose. This requires no declaration
syntax but lets invalid callers enter code that claims safe-code guarantees; documentation cannot
participate in checking or tooling.

### Strongest competing language model

Treat an unsafe function body as an ambient unchecked region and permit unsafe declarations,
modules, types, implementations, and target annotations. This is expressive but widens the amount
of code whose local obligations are invisible and creates several overlapping privilege systems.

## Falsifiers and acceptance blockers

- A realistic low-level library cannot preserve an unsafe caller obligation through generic or
  higher-order APIs without a substantially different type model.
- Keeping ordinary checks active inside unsafe code prevents a necessary representation operation
  rather than merely requiring a narrower intrinsic.
- Reachability cannot determine target-specific intrinsic use before target-dependent lowering.
- A single sealed catalog cannot express the safety and target contracts needed by evaluator,
  native, and Wasm without backend-specific source semantics.

## Open realization questions

- Whether ordinary source ever needs target annotations beyond intrinsic availability.
- Which current compiler-known operations violate the sealed-namespace rule and must move to source
  wrappers or syntax-owned semantics.

## Future directions

Public FFI, stable ABI declarations, address spaces, volatile and atomic memory operations,
concurrency, target-conditional source, sanitizer contracts, capability-based unsafe permissions,
and proof-carrying safe wrappers remain separate future work.

## OpenSpec realization map

None while the Draft is establishing its unsafe contract and privilege model.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-19 | Created the Draft around lexical unsafe acknowledgement, a sealed auditable intrinsic catalog, reachable-only target availability, and the unresolved need for source-declared unsafe contracts. |
| 2 | 2026-08-19 | Added source-declared `unsafe fn` as a caller-obligation contract while keeping its body safe-by-default and requiring explicit local boundaries around the unsafe operations it invokes. |
| 3 | 2026-08-19 | Added a direct `unsafe call(...)` expression for one invocation while retaining `unsafe { ... }` as a statement-only region; unsafe permission does not extend into nested argument calls. |
| 4 | 2026-08-19 | Required acknowledgement when an unsafe effect function constructs its ordinary lazy Effect, with no unsafe Effect channel or second acknowledgement at `run`; run access states whether the invariant must support one or many executions. |
| 5 | 2026-08-19 | Made unsafety an operation-level callable qualifier preserved by generics, representations, interfaces, and services; safe implementations may satisfy unsafe contracts, while unsafe implementations cannot satisfy safe contracts. |
| 6 | 2026-08-19 | Made the sealed `Intrinsic` namespace directly available to every module without standard-library privilege, required per-operation canonical contracts and unsafe invariants, and limited admission to the smallest irreducible primitive with a real source consumer or direct language necessity. |
| 7 | 2026-08-19 | Made intrinsic target availability reachable-only and compile-time: unreachable restricted operations impose no artifact cost, while each reachable unsupported operation/target pair is rejected deterministically before layout or lowering. |
| 8 | 2026-08-19 | Confined undefined behavior to violations of explicit unsafe contracts; safe Silk retains typed failures and fatal traps as its complete abnormal-outcome model, while optional debug detection does not become a portable trap guarantee. |
| 9 | 2026-08-19 | Kept initialization, ownership, borrowing, liveness, invocation access, and cleanup active inside unsafe code; low-level state changes require explicit contracts with trackable post-state, and correct unsafe code retains ordinary cleanup. |
| 10 | 2026-08-19 | Made partial application of an unsafe callable safe while preserving its qualifier; acknowledgement occurs only at the arity-satisfying invocation, and captured arguments retain ordinary ownership and lifetime checks. |
| 11 | 2026-08-19 | Kept unsafe declarations under ordinary visibility and standardized a `# Safety` documentation section; missing safety documentation is an LSP warning rather than a compiler error. |
| 12 | 2026-08-19 | Defined safe wrappers as ordinary, unprivileged Silk proofs over unsafe contracts; direct intrinsic use remains legal, wrapper outcomes stay explicit, and naming conventions have no semantic force. |
