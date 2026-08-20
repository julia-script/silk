# Unsafe code, intrinsics, and targets

Unsafe code is Silk code that explicitly accepts responsibility for invariants the compiler cannot
prove. It is not a mode that disables ordinary language checks. Intrinsics are the sealed primitive
operations from which ordinary Silk source builds safe language and standard-library abstractions.

This page is a stabilization workbench. Individual rule statuses record their review with the
language author; the proposal remains Draft until its whole-language review is accepted.

## Terminology

- **Safe code** is code outside an unsafe boundary. It cannot cause undefined behavior when its
  dependencies satisfy their safe contracts.
- An **unsafe boundary** is an explicit lexical region that permits calls with additional caller
  obligations.
- An **unsafe contract** states the invariant a caller must establish before invoking an operation.
- An **intrinsic** is a source-callable compiler primitive with canonical identity in the sealed
  `Intrinsic` namespace.
- A **safe wrapper** validates or structurally guarantees an unsafe precondition and exposes an
  ordinary safe operation.
- **Undefined behavior** means execution has violated an unsafe contract and the language makes no
  further behavioral guarantee.
- A **trap** is a defined fatal termination. It is not undefined behavior or a typed failure.
- **Target availability** is the set of execution targets on which one intrinsic has defined
  behavior and lowering.
- The **executable closure** is the set of declarations and intrinsic calls reachable from the
  selected entry for one concrete program and target.

## Safety outcomes

### SAFETY-001 — Only violating an explicit unsafe contract permits undefined behavior

**Status:** Confirmed

Safe Silk has ordinary values, typed failures, and defined fatal traps as its complete outcome
model. Undefined behavior enters only when execution violates a stated precondition of an unsafe
operation.

```silk,ignore
let value = values[index]
// invalid index: defined fatal trap

let text = unsafe Intrinsic.stringFromUtf8Unchecked(bytes)
// bytes that are not valid UTF-8: violated unsafe contract, undefined behavior
```

Entering an unsafe boundary is not itself undefined behavior and does not make surrounding code
unspecified. When every unsafe contract is upheld, the program retains the semantics promised by
its ordinary and intrinsic operations.

Debug builds, interpreters, sanitizers, or a particular target may detect some contract violations
and trap. That detection is permitted but is not a portable semantic guarantee. Invalid pointers,
aliasing, initializedness, layouts, or private ABI state cannot in general be converted into a
guaranteed trap without maintaining and checking additional runtime proof.

**Boundary:** A typed failure is expected recoverable data in an Effect contract. A fatal trap is a
defined abnormal termination that cannot be caught and promises no cleanup. Undefined behavior
makes no guarantee about results, termination, cleanup, diagnostics, or subsequent execution. It is
not a hidden failure channel and cannot be caught.

If a caller satisfies an unsafe source function's declared preconditions but its implementation
still violates an unsafe invariant, that function is unsound. Safe code cannot cause undefined
behavior unless an unsafe dependency exposes an unsound safe API or the compiler/runtime violates
its own safe-language contract.

**Diagnostics:** No compile-time diagnostic can be required for a dynamic contract violation the
compiler cannot prove. Proven violations should be diagnosed when possible; optional runtime checks
may trap with invariant context. Tooling must not describe such checks as a guarantee of release
behavior.

**Current compiler:** Partially aligned. Unsafe intrinsics carry invariant text and safe operations
retain defined trap behavior. The language documentation has not previously separated detected
debug violations from the portable undefined-behavior boundary this explicitly.

**Evidence:** [fatal traps](typed-failures.md#fail-012--fatal-trap-is-unrecoverable-and-promises-no-cleanup),
[safe-code vocabulary](../../CONTEXT.md),
[intrinsic unsafe invariants](../../packages/compiler/test/fixtures/intrinsic-inventory.json).

## Unsafe boundaries and contracts

### UNSAFE-001 — An unsafe boundary permits obligations; it does not disable checking

**Status:** Confirmed

`unsafe { ... }` explicitly accepts responsibility for the unsafe operations lexically contained
by its block. Type checking, definite initialization, ownership, moves, borrows, cleanup, Effect
contracts, visibility, generic bounds, and conformance checking remain active inside it.

```silk
fn fromUtf8Unchecked(bytes: &[u8]) -> string {
  unsafe {
    return Intrinsic.stringFromUtf8Unchecked(bytes)
  }
}
```

The block permits the unchecked construction call. It does not prove the bytes valid, make an
invalid borrow live longer, duplicate an affine value, handle an Effect, or make a private name
visible.

**Boundary:** Unsafe permission is lexical. It does not propagate into a called ordinary function,
another module, a callback that may execute later, or code merely dominated by an earlier unsafe
block. Nesting an unsafe block is valid but adds no stronger permission.

**Diagnostics:** Calling an unsafe operation outside a boundary reports `SEM0082` at the call and
names the operation whose invariant requires acknowledgement. Other invalid code inside the block
retains its ordinary primary diagnostic.

**Current compiler:** Aligned. The parser represents both unsafe statement blocks and single-call
acknowledgements, and semantic analysis applies the same invocation rule to intrinsic and ordinary
source callables.

**Evidence:** [intrinsic boundary specification](../../openspec/specs/bootstrap-intrinsic-boundary/spec.md),
[unsafe parser](../../packages/compiler/src/Parser.ts),
[unsafe-call diagnostics](../../packages/compiler/test/StringIntrinsics.test.ts).

### UNSAFE-002 — Ordinary source may declare a caller-owned unsafe contract

**Status:** Confirmed

Proposed syntax:

```silk,ignore
pub unsafe fn fromUtf8Unchecked(bytes: &[u8]) -> string {
  unsafe {
    return Intrinsic.stringFromUtf8Unchecked(bytes)
  }
}
```

Calling an `unsafe fn` requires a lexical unsafe boundary whether the implementation is a
compiler intrinsic or ordinary Silk source. The declaration would not receive compiler privilege;
it would only preserve an invariant obligation across an abstraction boundary.

The body of an `unsafe fn` remains safe-by-default. Every unsafe operation it performs still needs
an explicit local unsafe boundary. The declaration and the block answer different questions:

- `unsafe fn` says that the caller must establish the declared preconditions.
- `unsafe { ... }` says where an implementation accepts responsibility for one or more unsafe
  operations it invokes.

This keeps unsafe implementation sites visible when a large function changes and prevents an
unsafe public contract from silently suppressing checks throughout its body.

Unsafe is part of the function's callable contract. Taking, storing, passing, specializing, or
returning the function does not erase the call requirement. A safe callable parameter cannot accept
an unsafe callable, while a parameter explicitly accepting an unsafe callable preserves the
requirement at its eventual invocation.

**Boundary:** Merely mentioning or moving an unsafe callable does not perform an unsafe operation;
invoking it does. An unsafe function may call ordinary safe functions without a boundary. It cannot
declare that its caller owns an obligation and then expose an ordinary safe callable type that
forgets that obligation.

**Diagnostics:** Calling an unsafe source function outside an unsafe boundary uses the same
missing-boundary diagnostic as an unsafe intrinsic and names the selected function. Assigning or
passing it where a safe callable is required reports a callable-contract mismatch at the value
transfer.

**Current compiler:** Aligned. Ordinary and effectful source declarations, callable values,
serialized module surfaces, and tooling presentations preserve the unsafe-call qualifier.

### UNSAFE-003 — `unsafe call(...)` marks one invocation; `unsafe { ... }` marks a statement region

**Status:** Confirmed

The prefix form acknowledges exactly one unsafe function or intrinsic invocation and produces that
call's ordinary result:

```silk,ignore
let text = unsafe String.fromUtf8Unchecked(bytes)
let value = unsafe Intrinsic.rawBufferRead(buffer, index)
```

It is not a general unchecked-expression operator. Permission attaches to the directly prefixed
call and does not extend into its argument expressions:

```silk,ignore
let value = unsafe outer(unsafe inner())
```

If both calls are unsafe, both markers are required. An ordinary safe call may be prefixed only if
the language permits redundant acknowledgement; the first stable model diagnoses it as unnecessary
so `unsafe` continues to identify a real obligation rather than becoming decorative.

The block form marks every directly evaluated unsafe invocation within one statement region:

```silk
unsafe {
  let text = Intrinsic.stringFromUtf8Unchecked(bytes)
  consume(move text)
}
```

`unsafe { ... }` remains a statement block. It does not introduce trailing-expression results or a
special block-return rule. Use the single-call form when an unsafe result must participate in a
larger expression, binding, argument, or return.

**Boundary:** Permission from either form is lexical, not dynamic. It does not enter an ordinary
called function or a callback that executes later. The single-call form does not cover sibling,
receiver, argument, pipeline, or later invocations merely because they occur in the same enclosing
expression.

**Diagnostics:** Prefixing a non-call expression reports that `unsafe` must select one invocation.
Calling an unsafe operation without either form reports the missing-boundary diagnostic. A nested
unsafe call inside an argument receives its own diagnostic even when the outer call is prefixed.
Tooling may remove a redundant nested block whose complete region is already unsafe, but nesting is
not a compiler error.

**Current compiler:** Aligned. The statement block grants lexical authority, while the prefix grants
authority only to its directly wrapped complete call and not to nested argument calls.

**Evidence:** [unsafe block parser](../../packages/compiler/src/Parser.ts),
[unsafe semantic admission](../../packages/compiler/src/Elaboration.ts),
[unsafe formatting](../../packages/compiler/src/Formatter.ts).

### UNSAFE-004 — An unsafe effect function is acknowledged when its Effect is constructed

**Status:** Confirmed

An unsafe effect function requires acknowledgement at its call, exactly as an unsafe ordinary
function does. Calling it constructs one ordinary lazy Effect; running that valid Effect later does
not require another unsafe marker.

```silk,ignore
pub unsafe effect fn readUnchecked(
  pointer: RawPointer<u8>
) -> u8 {
  unsafe {
    return Intrinsic.pointerRead(pointer)
  }
}

let pending = unsafe Device.readUnchecked(pointer)
let value = run pending
```

The caller accepts responsibility for the returned Effect's validity over its complete lifetime
and every run its contract permits. If its invariant can be upheld only for one execution, the
function must return or construct a consuming `once Effect`; unsafe does not silently change run
access. Borrowed captures retain their ordinary lexical lifetime restrictions.

The effect function body remains lazy and safe-by-default. Its local unsafe operations still need
an `unsafe` call marker or block when the body eventually executes. The call-site marker
acknowledges the source function's construction contract; it does not eagerly execute the body.

**Boundary:** Silk has no `unsafe Effect` channel or unsafe form of `run`. Effect combinators,
storage, provision, failure handling, and execution preserve the ordinary success, failure,
requirement, representation, run-access, and cleanup contracts. An unsafe constructor cannot rely
on safe code to revalidate an undocumented condition at each run.

**Diagnostics:** Calling an unsafe effect function without acknowledgement reports the ordinary
missing-boundary diagnostic at construction. Running the resulting Effect receives only the normal
Effect, ownership, and execution-boundary diagnostics.

**Current compiler:** Aligned. Source and intrinsic unsafe Effect constructors are acknowledged when
called, and the resulting ordinary Effect runs without a second unsafe marker.

**Evidence:** [intrinsic Effect catalog](../../packages/compiler/src/Intrinsic.ts),
[OS intrinsic acceptance](../../packages/compiler/test/IntrinsicCatalog.test.ts),
[Effect construction and execution](effects-and-execution.md).

### UNSAFE-005 — Unsafety qualifies individual callable contracts and is compatible only toward safer implementations

**Status:** Confirmed

`unsafe` is part of an individual callable's use contract. It composes with invocation mode and
ordinary or effectful function kind:

```silk,ignore
unsafe fn(RawPointer<u8>) -> u8
unsafe mut fn(Buffer) -> ()
unsafe once fn(Handle) -> Result<(), CloseError>
```

Taking, moving, storing, passing, specializing, or returning an unsafe callable is safe. Invoking
it requires acknowledgement. Generic bounds and exact or opaque representation parameters preserve
the qualifier:

```silk,ignore
fn invoke<F: unsafe fn(RawPointer<u8>) -> u8>(
  operation: F,
  pointer: RawPointer<u8>
) -> u8 {
  return unsafe operation(pointer)
}
```

A safe callable may satisfy an unsafe callable contract because it requires fewer caller
obligations. The reverse is invalid. Code checked against the unsafe contract still requires
acknowledgement even when one later specialization selects a safe implementation.

Interfaces and services mark individual operations, not entire declarations or conformances:

```silk,ignore
interface RawAccess {
  unsafe fn read(pointer: RawPointer<u8>) -> u8
}
```

A safe function may implement that operation; callers continue to observe the interface's unsafe
contract. An unsafe function cannot implement an operation declared safe. The same compatibility
rule applies after substituting `Self`, generic arguments, Effect channels, ownership modes, and
service requirements.

**Boundary:** Silk has no `unsafe interface`, `unsafe service`, `unsafe impl`, `unsafe module`, or
ambient unsafe type. Unsafety neither implies nor changes ownership mode, Effect construction,
failure, requirements, visibility, target availability, or conformance coherence.

**Diagnostics:** Assigning, passing, returning, or mapping an unsafe callable where a safe contract
is required reports a callable-contract mismatch at the transfer or implementation. The diagnostic
identifies safety before reporting secondary representation details. Invoking through a statically
unsafe generic, interface, or service contract requires acknowledgement even when a selected safe
implementation is known later.

**Current compiler:** Aligned. Source callable types, generic substitution, interface and service
operations, conformance checking, and representation parameters preserve the qualifier. A safe
witness may satisfy an unsafe operation; the reverse is rejected.

**Evidence:** [callable contracts](functions-callables-and-control-flow.md),
[interface operation compatibility](generics-interfaces-and-specialization.md#impl-003--each-implementation-must-satisfy-the-substituted-operation-contract),
[intrinsic catalog](../../packages/compiler/src/Intrinsic.ts).

### UNSAFE-006 — Unsafe permission does not suspend ownership, borrowing, or cleanup rules

**Status:** Confirmed

An unsafe boundary permits only the additional caller obligations declared by the unsafe operations
it contains. Definite initialization, ownership, moves, borrowing, aliasing of safe references,
liveness, invocation access, and cleanup remain checked normally.

```silk,ignore
unsafe {
  consume(move value)
  use(value) // invalid: `value` was moved
}
```

Unsafe is not a general “trust me” escape from a rejected safe program. A raw operation that
performs a state transition ordinary Silk cannot prove must be an explicit unsafe intrinsic or
unsafe source operation. Its signature and postcondition must expose enough semantic state for the
compiler to resume ordinary checking after the call.

```silk,ignore
let value = unsafe Intrinsic.readInitialized(pointer)
use(&value) // `value` is an ordinary initialized value from here
```

Constructing a safe reference from a raw pointer similarly requires an unsafe operation whose
contract states the validity, lifetime, alignment, and aliasing obligations. Once constructed, the
reference follows the ordinary borrow rules. Unsafe code cannot reuse a moved or dead value, forge
a longer safe lifetime through syntax alone, or create simultaneously usable safe references that
violate their aliasing contracts.

Correct unsafe code receives ordinary cleanup on successful return and typed failure. Entering an
unsafe block does not suppress `Drop` or weaken cleanup ordering. Fatal traps retain their existing
no-cleanup guarantee. After an unsafe contract is violated, behavior is undefined and therefore no
cleanup behavior is promised.

**Boundary:** Some low-level operations necessarily change initializedness, ownership, or raw
resource state. The compiler supports those transitions through their canonical operation
contracts; the unsafe boundary merely acknowledges the unproved precondition. It does not make an
untracked transition visible by convention or comments.

**Diagnostics:** Invalid moves, borrows, liveness, initialization, and invocation access inside an
unsafe region use their ordinary diagnostics. A diagnostic should not suggest adding `unsafe` when
no unsafe operation exists that expresses the required transition.

**Current compiler:** Largely aligned for lexical unsafe blocks: ordinary semantic and ownership
analysis remains active inside them. The intrinsic audit must verify that every raw state transition
has a precise post-state rather than relying on an ambient unsafe exemption.

**Evidence:** [ownership and borrowing](ownership-and-borrowing.md),
[fatal-trap cleanup boundary](typed-failures.md#fail-012--fatal-trap-is-unrecoverable-and-promises-no-cleanup),
[intrinsic boundary specification](../../openspec/specs/bootstrap-intrinsic-boundary/spec.md).

### UNSAFE-007 — Partial application preserves unsafety but does not invoke it

**Status:** Confirmed

Partially applying an unsafe callable is safe because it constructs another callable without
executing the unsafe operation. The resulting callable preserves the unsafe qualifier, and only
the application that supplies its final missing arguments requires acknowledgement.

```silk,ignore
unsafe fn readAt(pointer: RawPointer<u8>, offset: usize) -> u8 {
  unsafe {
    return Intrinsic.pointerReadOffset(pointer, offset)
  }
}

let readAtFour = readAt(4)                 // unsafe fn(RawPointer<u8>) -> u8
let byte = unsafe readAtFour(pointer)      // complete invocation crosses the unsafe contract
```

The same rule applies to source functions, intrinsic operations, generic callables, interface and
service operations, and exact or opaque callable representations. Each additional partial
application remains safe until the callable's arity is satisfied.

Trailing arguments captured by a partial application still follow their ordinary ownership and
lifetime rules when the callable is formed. The unsafe precondition must hold when the completed
invocation executes. In particular, capturing a raw pointer in a trailing parameter does not
promise that it will still be valid later; satisfying that condition remains the eventual caller's
obligation.

```silk,ignore
let readAtFour = unsafe readAt(4)
// invalid: no unsafe operation is invoked by this partial application
```

**Boundary:** This rule relies on Silk's ordinary leading-argument partial application. A call that
fully satisfies the remaining arity invokes the callable even if its result is itself another
callable; result shape does not retroactively make that invocation partial.

**Diagnostics:** An unsafe callable invoked without acknowledgement reports the missing-boundary
diagnostic at its complete application. Prefixing a partial application with `unsafe` reports that
the acknowledgement is misplaced because the operation has not been invoked. Ownership and
lifetime errors in captured arguments retain their ordinary diagnostics.

**Current compiler:** Aligned. Partial application preserves the unsafe qualifier without requiring
acknowledgement, and every complete invocation through the resulting callable requires one block or
single-call marker.

**Evidence:** [partial application](functions-callables-and-control-flow.md),
[callable ownership](ownership-and-borrowing.md),
[intrinsic catalog](../../packages/compiler/src/Intrinsic.ts).

### UNSAFE-008 — Unsafe declarations use ordinary visibility and document their caller contract

**Status:** Confirmed

An unsafe source function follows the same module, visibility, import, qualification, and naming
rules as any other function. `unsafe` does not imply `pub`, grant access to private names, or allow
the declaration to escape its module without an ordinary export.

The attached documentation should state the caller-owned preconditions in a nonempty `# Safety`
section:

```silk,ignore
/// Constructs text without validating its encoding.
///
/// # Safety
///
/// `bytes` must contain valid UTF-8 for the complete duration of this call.
pub unsafe fn fromUtf8Unchecked(bytes: &[u8]) -> string {
  unsafe {
    return Intrinsic.stringFromUtf8Unchecked(bytes)
  }
}
```

This documentation convention is tooling and style policy, not a condition for compiling valid
Silk. The LSP warns when an unsafe source declaration has no nonempty `# Safety` section and may
offer a scaffold. Hover, signature help, completion details, and generated documentation prominently
show the unsafe qualifier and safety section when present.

Intrinsic operations have no source declaration, so their mandatory catalog invariant supplies the
same tooling information. Hover and missing-boundary diagnostics show that invariant directly.

**Boundary:** The compiler enforces that an unsafe callable is acknowledged at invocation whether
or not its documentation is present or correct. Documentation cannot make a safe declaration
unsafe, grant intrinsic privilege, or change a callable contract. An incomplete or incorrect safety
description may make a library unsound, but it does not create a second type-level contract
language.

**Diagnostics:** Missing acknowledgement remains a compiler error. Missing or empty `# Safety`
documentation is an LSP warning and does not fail parsing, semantic analysis, execution, or a
compiler-only build. Ordinary visibility and import errors retain their ordinary diagnostics.

**Current compiler:** Partially aligned. Documentation comments already attach to declarations and
flow into editor information, while ordinary source-declared unsafe functions and the corresponding
LSP warning do not yet exist.

**Evidence:** [module and visibility rules](modules-names-and-visibility.md),
[documentation block model](../../packages/compiler/src/DocBlock.ts),
[editor documentation tests](../../packages/compiler/test/EditorIntelligence.test.ts).

## Sealed intrinsic boundary

### INTR-001 — `Intrinsic` is sealed compiler identity available to every source module

**Status:** Confirmed

`Intrinsic` is a globally available compiler namespace. It requires no import, and every Silk
module may call its members under their individual contracts.

```silk,ignore
let previous = Intrinsic.replace(place, value)
let text = unsafe Intrinsic.stringFromUtf8Unchecked(bytes)
```

The namespace is sealed. Source cannot declare, import as, alias as, shadow, reopen, extend, or
implement `Intrinsic`. Its operations have canonical compiler identities independent of source
paths or packages.

An ordinary actor or function with a matching member spelling receives no privilege:

```silk,ignore
fn stringFromUtf8Unchecked(bytes: &[u8]) -> string {
  // ordinary Silk source
}
```

Only the qualified sealed identity selects intrinsic behavior. The compiler never recognizes an
ordinary standard-library actor, service, interface, provider, type, or function by spelling.

**Boundary:** The shipped standard library has no special permission to call `Intrinsic`. Restricting
it by package, path, signature, build mode, or toolchain provenance would introduce a second hidden
trust system. Tooling and style guidance may prefer a safe public wrapper where one exists, but the
compiler does not reject direct calls merely because they occur outside the standard library.

**Diagnostics:** Declaring or importing a binding named `Intrinsic` reports a reserved-binding
collision. An unknown member reports an unknown-intrinsic diagnostic and does not search ordinary
modules for a fallback. A same-named ordinary operation under another qualifier resolves normally
and receives no intrinsic-specific diagnostic or lowering.

**Current compiler:** Largely aligned. Intrinsic operations carry canonical identities and scalar
actor spellings now resolve to shipped source wrappers. The complete catalog and every remaining
name-based compiler branch still require reconciliation against this rule.

**Evidence:** [intrinsic boundary specification](../../openspec/specs/bootstrap-intrinsic-boundary/spec.md),
[intrinsic catalog tests](../../packages/compiler/test/IntrinsicCatalog.test.ts),
[minimal compiler privilege](../../AGENTS.md#minimal-compiler-privilege).

### INTR-002 — Safety belongs to each intrinsic contract, not to the namespace

**Status:** Confirmed

Every intrinsic has one canonical callable contract covering its generic parameters, value
parameters, ownership modes, result, Effect channels where present, invocation or run access,
safety classification, caller invariant when unsafe, and supported-target set.

A safe intrinsic is callable from safe code. An unsafe intrinsic requires UNSAFE-003's call marker
or block. Namespace membership alone neither makes an operation unsafe nor makes its failures,
traps, or target restrictions implicit.

Unsafe intrinsic invariants must name the exact obligation the compiler cannot prove—for example,
initialized extent, live handle kind, valid UTF-8, non-aliasing access, or private ABI layout. “May
be unsafe” and “caller must use correctly” are not sufficient contracts.

The same canonical contract drives name resolution, generic inference, signature help, hover,
diagnostics, HIR, MIR, evaluation, target validation, and backend lowering. Those consumers cannot
independently reconstruct or weaken it.

**Boundary:** An intrinsic may still have defined typed failures or traps. Unsafe marks only the
additional precondition whose violation permits undefined behavior; it is not another failure
channel or a promise that every bad runtime condition becomes undefined behavior.

**Diagnostics:** A contract mismatch uses the ordinary call, ownership, generic, or Effect
diagnostic. A missing unsafe acknowledgement names the invariant-bearing operation. Catalog
verification rejects an unsafe operation without a concrete invariant, an operation without a
target, or divergent contracts across compiler phases.

**Current compiler:** Aligned in architecture. The deterministic catalog records signatures,
safety, unsafe invariants, consumers, compiler identities, targets, and private host imports, and
tests compare the checked inventory with accepted semantic calls.

**Evidence:** [intrinsic inventory](../../packages/compiler/test/fixtures/intrinsic-inventory.json),
[catalog implementation](../../packages/compiler/src/Intrinsic.ts),
[catalog verification](../../packages/compiler/test/IntrinsicCatalog.test.ts).

### INTR-003 — An intrinsic must be the smallest irreducible primitive with a real consumer

**Status:** Confirmed

The compiler admits an intrinsic only when ordinary Silk cannot express a required machine,
representation, language-safety, execution-boundary, or private-host operation. The operation must
be the smallest contract from which ordinary source can build the desired abstraction.

Every intrinsic must have either a shipped ordinary-Silk consumer or a direct necessity imposed by
language syntax, safety, representation, or lowering. Convenience, fewer wrapper lines, faster
initial implementation, or an existing compiler branch are not admission reasons.

Validation, domain errors, generic selection, provider types, conformance policy, retries,
buffering, allocation strategy, text policy, filesystem policy, diagnostics, and reusable safe
composition remain ordinary Silk whenever the primitive contract makes them expressible.

For example, private OS operations expose handles, primitive scalars, byte views, explicit output
storage, and low-level outcomes. They do not construct `FileError`, `ProcessOutcome`, `HostInput`,
or a service implementation. Source providers translate the primitive result into those public
domain values.

**Boundary:** Target-restricted does not mean abstraction-shaped. A native-only intrinsic remains a
narrow semantic primitive with explicit availability; it does not become permission to expose an
entire native service or platform object through the compiler.

**Diagnostics and audit:** This is primarily a catalog and release invariant rather than a
program-error condition. The deterministic inventory records the admission reason and source
consumer. Repository verification rejects missing consumers, unregistered compiler branches,
unregistered public host imports, or duplicate abstraction-shaped operations.

**Current compiler:** Partially aligned. The inventory records admission and consumer metadata and
the bootstrap intrinsic specification has repeatedly replaced domain-shaped operations with narrow
primitives. A stabilization audit must still remove any survivor that cannot meet this rule.

**Evidence:** [minimal intrinsic requirement](../../openspec/specs/bootstrap-intrinsic-boundary/spec.md),
[current deterministic inventory](../../packages/compiler/test/IntrinsicCatalog.test.ts),
[minimum runtime direction](../../wayfinder/bootstrap-language/issues/07-minimum-runtime-and-standard-library.md).

### INTR-004 — Safe wrappers are ordinary Silk proofs over unsafe contracts

**Status:** Confirmed

A safe wrapper may call an unsafe operation only after validation or ordinary type, ownership, and
construction rules establish its preconditions:

```silk,ignore
pub fn fromUtf8(bytes: &[u8]) -> Result<string, InvalidUtf8Error> {
  if Utf8.isValid(bytes) {
    let text = unsafe Intrinsic.stringFromUtf8Unchecked(bytes)
    return Result.succeed(move text)
  }
  return Result.fail(InvalidUtf8Error {})
}
```

The wrapper is ordinary Silk and receives no compiler identity, trust, or privilege from its name,
module, package, path, or relationship to an intrinsic. The compiler checks the explicit unsafe
acknowledgement and every ordinary static rule, but it does not prove that the wrapper's validation
logically establishes the documented unsafe invariant. A wrapper that exposes invalid input to the
operation through a safe signature is unsound.

The wrapper chooses its public outcome according to its own ordinary contract. It may return a
typed failure for recoverable invalid input, rely on an already-defined fatal trap for a violated
language precondition, or preserve a caller obligation through an unsafe function. No outcome or
error channel is inferred from the intrinsic it calls.

Calling `Intrinsic` directly remains legal in every module. Tooling may suggest an existing safe
wrapper when useful, but direct intrinsic access is not itself a warning or error. `Unchecked`,
`Raw`, and `Unsafe` are style signals only; the callable's actual `unsafe` qualifier determines
whether acknowledgement is required.

**Boundary:** A safe wrapper may combine several intrinsics, and several wrappers may expose
different policies over one intrinsic. No registry identifies an approved wrapper, and an
intrinsic does not nominate one privileged public actor.

**Diagnostics:** An unsafe call in a wrapper without acknowledgement reports the ordinary
missing-boundary error. Invalid wrapper logic receives only the diagnostics justified by ordinary
Silk analysis; the compiler does not claim to have proven its semantic safety argument. LSP wrapper
suggestions are optional actions, not correctness diagnostics.

**Current compiler:** Architecturally aligned. Shipped source wrappers call narrow intrinsics from
ordinary Silk and the compiler resolves only the intrinsic identity. A stabilization audit must
remove any remaining name-based wrapper privilege and ensure direct intrinsic calls are treated
uniformly.

**Evidence:** [minimal compiler privilege](../../AGENTS.md#minimal-compiler-privilege),
[intrinsic boundary specification](../../openspec/specs/bootstrap-intrinsic-boundary/spec.md),
[standard-library source](../../packages/compiler/stdlib).

## Target availability

### TARGET-001 — Intrinsic availability is checked only for the selected executable closure

**Status:** Confirmed

Parsing, importing, resolving, and type-checking a target-restricted intrinsic is valid on every
target. After generic specialization and executable reachability select one concrete program, every
retained intrinsic call must support the requested execution target.

```silk,ignore
fn nativeWorkingDirectory(...) -> ... {
  return unsafe Intrinsic.osHostWorkingDirectory(...)
}

pub fn portableMain() -> i32 {
  return 42
}
```

A Wasm executable rooted at `portableMain` is valid because it cannot reach the native operation.
Changing the selected entry or a static call edge so that `nativeWorkingDirectory` becomes
executable makes the same intrinsic call a compile-time target error.

Ordinary runtime control flow does not hide a call from availability:

```silk,ignore
if flag {
  nativeWorkingDirectory(...)
}
```

When `flag` is runtime data, both branches belong to the executable behavior and the intrinsic must
support the target. Availability cannot depend on an optimizer deciding to delete source behavior.

**Boundary:** Loading a module, importing a wrapper, mentioning it in documentation, or retaining
an unreachable generic declaration does not select its intrinsic calls. Invalid syntax, types,
generic arguments, ownership, or unsafe acknowledgement are diagnosed before target selection and
do not produce secondary availability errors.

**Diagnostics:** Each distinct unavailable intrinsic/target pair reports one deterministic error
at its first reachable call, naming both. Additional reachable sites may appear as related notes.
The error occurs before layout, MIR, evaluation, or backend emission; no internal backend failure
may substitute for it.

**Current compiler:** Aligned. Target selection consumes the concrete intrinsic calls retained by
instance discovery, deduplicates unavailable diagnostics by canonical operation identity, and runs
before evaluation or emission.

**Evidence:** [intrinsic target availability specification](../../openspec/specs/bootstrap-intrinsic-target-availability/spec.md),
[availability selection](../../packages/compiler/src/IntrinsicAvailability.ts),
[target-availability tests](../../packages/compiler/test/IntrinsicTargetAvailability.test.ts).

### TARGET-002 — Unreachable target-specific primitives have no artifact cost

**Status:** Confirmed

An intrinsic absent from the selected executable closure contributes no host import, runtime helper,
adapter, symbol, table entry, or backend support module to the emitted artifact. Target-specific
code is pay-for-use even when its source declaration belongs to a loaded module.

Equivalent source closure, concrete specializations, entry, and target produce deterministic
retained inventories and diagnostics. Import order, filesystem traversal, prior builds, and backend
discovery order do not affect the result.

**Boundary:** This rule does not promise that two different source programs have identical binary
layout or size. It promises only that an unreachable restricted intrinsic cannot force its own
support machinery into the artifact.

**Diagnostics:** No programmer diagnostic applies when the intrinsic is unreachable. Release and
backend inventory verification reject an artifact that contains support or imports for an absent
operation.

**Current compiler:** Aligned in the validated intrinsic inventory and backend preparation model.

**Evidence:** [pay-for-use target requirement](../../openspec/specs/bootstrap-intrinsic-target-availability/spec.md),
[intrinsic availability selection](../../packages/compiler/src/IntrinsicAvailability.ts),
[intrinsic inventory tests](../../packages/compiler/test/IntrinsicCatalog.test.ts).

### TARGET-003 — Target unavailability is a compile-time compatibility error

**Status:** Confirmed

A reachable intrinsic outside its supported-target set makes the selected executable invalid. The
condition is not a typed failure, service requirement, fatal trap, runtime feature probe, or dynamic
fallback.

Ordinary wrapper functions need no target annotation: their reachable primitive calls determine
compatibility transitively. Ordinary source cannot add to, subtract from, or override an intrinsic's
compiler-owned target set.

This first stable model adds no target-conditional source, target predicates, target reflection, or
runtime inspection. Portable public APIs use ordinary source abstractions and target-appropriate
providers selected outside this rule; future conditional compilation requires its own design.

**Boundary:** An intrinsic may return a typed failure for a supported target's ordinary operational
failure. That does not make absence of the intrinsic implementation recoverable on an unsupported
target.

**Diagnostics:** The availability error names the canonical intrinsic and requested target and may
suggest selecting a supported target or removing the reachable dependency. It does not suggest
catching a failure, adding an unsafe block, or providing a service.

**Current compiler:** Aligned for evaluator, LLVM, and Wasm availability classes. Finer target-triple
availability, if needed, must refine the same compile-time rule rather than create runtime fallback.

**Evidence:** [target diagnostic](../../packages/compiler/src/Diagnostic.ts),
[target selection](../../packages/compiler/src/Target.ts),
[target availability specification](../../openspec/specs/bootstrap-intrinsic-target-availability/spec.md).
