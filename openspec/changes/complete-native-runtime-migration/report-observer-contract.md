# Lexical diagnostic observation

This is the selected design for replacing the generated reporting state. It is an
implementation contract, not evidence of complete observer admission. The formatter, lexical observer, owned failure-context transport and public source
context owner, default source startup and removal of generated reporting are implemented.
The final admission audit remains open.

## Boundary and ownership

Add a sealed `Intrinsic.observeDiagnostics` operation that constructs an Effect
from an owned observer state, a callback and a protected Effect. The protected
Effect must have an empty failure row. Its success type and requirement row are
preserved. This restriction makes the observation lifetime explicit: the protected
source composition handles every typed failure before releasing its observer.
It does not restrict failure rows of calls inside the protected Effect.

The callback accepts a mutable borrow of the state, an event discriminator, two
opaque word-sized context handles and two immutable strings. It is an ordinary
`fn<'static>` returning a word-sized context handle, with an `Intrinsic.NonParking`
representation bound. It may run infallible, requirement-free Effects in its body;
it does not return a lazy Effect for the compiler to execute afterward. Its
executed closure must not suspend or recursively observe diagnostics. Validate
that closure, including indirect source calls and destructors, before lowering.
Creating a lazy Effect is not by itself evidence of executing its body.

The primitive has type parameters `S, A, ?R, F` and takes owned state `S`, the
represented callback `F`, and `once Effect<A ? R>`, in that order. It returns
`once Effect<A ? R>`. Source wrappers pass explicit primitive type arguments
`<S, A, R, F>` and use ordinary generic-call inference at their public boundary.
The callback's state borrow is universally quantified for each invocation.

The frontend catalog, row checks and a post-specialization direct-execution check
are now implemented. NonParking alone is insufficient because it permits nested
transfers. DiagnosticObservation rejects both NestedTransfer and ExternalPark,
as well as an unavailable callback summary, with SEM0216 at the incoming call.
Final suspension summaries include explicit drops, structured-exit cleanup and
assignment cleanup in their owning execution regions. A deferred body's cleanup
does not execute merely because that Effect is constructed.

The execution graph also marks observation entry and propagates it through callees
and cleanup. SEM0216 rejects a callback whose executed closure can enter another
observation. Construction registers a deferred node without entering it. Dropping
an unstarted Effect follows owned captures' cleanup, without executing its body.
Regressions distinguish these cases, including call cycles and parameter cleanup.

Native callback dispatch, persistent scope descriptors and owned failure-context
transport are implemented. Complete indirect and composite cleanup coverage remains
part of the final admission audit.

Continuation transport must update both private storage layouts together. Relay
frames begin with parent and resume-control pointers, followed by an observer word
and two six-word borrowed cause records when the module retains observation. The
first record preserves the incoming invocation cause for resume; the second records
the active cause for cancellation cleanup. Transfer storage has eighteen words: child
control, head, append position, execution owner, storage owner, observer, a
six-word borrowed current cause record and a six-word owned completed-result record. ContinuationTransfer
defines the header size shared by NativeProgram, ExecutionPackage and initialization.
The observer never reuses either owner word.

The child and resume adapters insert the observer and by-value cause arguments after source lanes and
before transfer/frame/path arguments, matching NativeDeclare. A resumed frame loads
its saved lexical observer; an originating child receives the current observer.
Scope descriptors and callback captures belonging to a suspended invocation need
frame storage with stable offsets across all its states. Their state pointer must
refer to the existing affine owner's stable address; the descriptor does not own a
second state. Cancellation must use the selected scope while protected owners are
released, then restore its predecessor before dropping observer captures and state.
The runtime fixtures cover resumed fatal dispatch and cancellation cleanup. The
suspension fixture crosses both the public Effect.suspend boundary and a direct
Intrinsic.suspendEffect call before performing its result computation.

The callback uses the ordinary internal Silk calling convention. No C callback,
foreign function-pointer conversion, platform struct or compiler-known library
declaration is introduced. Compiler privilege belongs only to the intrinsic and
the semantic event protocol. Source owns the state representation, handles,
allocation, limits, formatter, descriptor selection and exit policy.

The constructed Effect owns its state. Moving, dropping without starting,
suspending, resuming and cancelling it follow the existing owned Effect and
execution-storage lifetimes. The observer's mutable borrow cannot escape a
callback. The compiler disables observation during a callback and its cleanup;
a trap there takes the bare fatal path. This is an invocation property, not a
process-global recursion flag.

An ordinary foreign C entry starts without an observer. Reentering a library
through C therefore cannot borrow or overwrite another invocation's observer.
An explicit observation within that entry owns another state. Nested lexical
observations similarly own distinct states; the inner protected Effect handles
its failures before its observer is destroyed.

An independently owned Execution also starts a fresh diagnostic invocation. Its
initial body call receives no observer borrowed from the driving caller, whether
the body is synchronous or suspendable. The body can remain parked after the
driver and its observation have returned, so inheriting that pointer would create
a dangling scope. Install observation inside the body to retain its state in the
body's continuation frames. Completion/suspension callbacks still run in the driving
caller's context. The same independent boundary must clear incoming selected causes
through the implemented borrowed-cause ABI. Ordinary nested Effect.suspend retains its
enclosing observation through the existing continuation chain.

## Semantic contexts

A diagnostic context handle is meaningful only to its owning observer. Zero means
that no optional context was retained. The compiler must not dereference, compare
for ordering, serialize, or interpret a nonzero handle. Immutable semantic strings
remain compiler-owned and valid for the artifact's lifetime.

Use these events and discriminants. The event lane is `u8`, both handle lanes and
the returned handle are `usize`, and both text lanes are `string<'static>`. Unknown event
values are not emitted by the compiler. The source callback must not retain its
mutable state borrow, but may retain the artifact-lifetime semantic strings.

| Event         | Input handles                                | Strings                            | Result and ownership                                         |
| ------------- | -------------------------------------------- | ---------------------------------- | ------------------------------------------------------------ |
| Produce (0)   | zero, zero                                   | canonical failure identity, origin | One owned context, or zero if source declines retention      |
| Propagate (1) | borrowed primary, zero                       | logical caller frame, empty        | One owned replacement; input remains owned by caller         |
| WithCause (2) | borrowed primary, borrowed protected failure | empty, empty                       | One owned replacement; both inputs remain owned by caller    |
| Retain (3)    | borrowed context, zero                       | empty, empty                       | One additional owned reference, possibly the same handle     |
| Release (4)   | owned context, zero                          | empty, empty                       | Consumes reference; returns zero                             |
| Unhandled (5) | borrowed context, zero                       | primary identity, primary origin   | Best-effort terminal output; returns source policy result    |
| Fatal (6)     | borrowed current cause, zero                 | trap reason, trap origin           | Best-effort fatal output; return is followed by machine trap |

The primary identity and origin must survive source declining optional storage.
They travel as immutable semantic values beside the optional handle. A source
implementation may omit outward frames and causes when its bound is exhausted;
it must not relabel the primary failure. The default source observer records
truncation and emits a bounded marker when output permits. A zero handle is legal
for every event and release of zero is a no-op.

Propagate and WithCause produce persistent contexts. They cannot mutate a borrowed
input: that input may still belong to another live outcome or recovery scope.
Source may implement persistence with bounded reference-counted nodes. All
references, including links between nodes, must be released. An append-only
history of handled failures is not an acceptable implementation.

## Compiler transport

The internal call context carries an optional observer reference and a separately
owned failure context. No mutable module or thread-local diagnostic state remains.
Each nested call has its own returned failure context. Calls made by cleanup use
separate result slots and cannot overwrite the outcome being unwound.

Only a failing return transfers a produced context to its caller. A successful
return releases any temporary context. Propagation adds the logical caller frame
without replacing identity or origin. Handles live across suspension only in an
owned continuation field governed by execution storage; no resumed code may refer
to a departed native stack slot. Cancellation releases those fields before the
observer state owner is destroyed.

Catch lowering explicitly delimits the selected handler's application and the
execution of its returned Effect. That scope retains the protected failure as a
cause. A failing handler attaches it once with WithCause; a successful handler
releases it. A nonselected member propagates unchanged. Reifying an outcome into
ordinary source data does not install an ambient cause. These rules replace the
existing inference from the presence of CatchEffect anywhere in a function.

The sealed `Intrinsic.observeUnhandled() -> usize` operation is usable within a
selected failure handler. It exposes no payload and observes that handler's retained semantic
context. The source entry first destroys its owned failure payload and then calls
this operation; normal source sequencing enforces TERM-011. The operation invokes
Unhandled and returns its source policy result. Calling it without a selected
failure context is diagnosed when statically provable and otherwise yields no
report; it never invents a failure or reads a previously handled one.
Runtime lowering, native Unhandled dispatch and a conservative static absence proof
are implemented. The proof starts from selected-handler application and execution,
then follows the retained execution graph. It diagnoses unreachable terminal sites
with SEM0217, including sites reached through ordinary helpers and lazy Effects.
A helper reachable both with and without a selected failure remains admitted. An
unresolved recovery target or selected callable execution prevents an absence proof.
A fresh observation cuts inherited-context edges to its protected execution while
retaining edges for argument construction and owner cleanup after scope exit.
An independently started Execution body has no inherited-context edge. Drive
completion/suspension callbacks, park registration and shared-access callbacks do
inherit the invoking context. Dynamically selected readiness endpoints are
conservatively included among possible callback targets.

Native admission exercises one helper from selected recovery and five disabled
contexts: after the handler has completed, inside a fresh observation across
suspension, at ordinary C entry, inside the observer callback, and inside an
independent Execution across suspension. Every disabled call returns zero without
another Unhandled event. Where a primary is retained, subsequent terminal reporting
still observes the original handle, identity and origin. The fresh observer is
destroyed once without receiving any event. These checks cover both debug and
optimized Darwin ARM64 and GNU Linux x86-64/ARM64 execution.

Fatal invokes the observer at most once, then executes the irreducible trap. It
does not return a catchable outcome, initiate structured cleanup, or guarantee
output after corrupted state. Disabled observation retains the bare trap and
does not retain source report allocation or descriptor dependencies.

## Default source policy

Construct report storage before observing application execution. Allocate one
explicitly bounded source-owned pool through the admitted storage capability;
do not add a second allocator bridge or environment-controlled compiler limit.
If pool acquisition fails, retain an allocation-free writer state and emit the
primary semantic identity/origin with truncation when terminal output is possible.
Output failure commits at most the already written prefix and prevents subsequent
writes, as implemented by NativeReport. Failure to report cannot replace the
application outcome or undo completed payload cleanup.

The selected entry supplies pool, frame and byte bounds and converts the source
observer result to its process policy. Custom source can choose different bounds
or disable observation. A runtime catalog entry alone neither installs an
observer nor retains its implementation in a library artifact.

## Admission evidence still required

- Exact intrinsic signatures, event discriminants and callback closure diagnostics.
- MIR ownership checks for context transfer, selected-handler scopes and suspended
  context fields; no public Effect outcome payload-layout change by accident.
- Source pool accounting under exhaustion, persistent sharing and repeated handled
  failures, using the existing storage counting fixture where appropriate.
- The existing cleanup-preserves-primary regression under the new transport, plus
  selected versus nonselected recovery, nested recovery and suspended recovery.
- Independent reentrant library entry, nested observation and cancellation with
  all observer/context allocations released.
- Native enabled/disabled trap and output-failure lanes; the shared Wasm entry
  migration preserves bare traps without requiring a Wasm reporter.
- Source and artifact absence of old report globals, generated report output and
  generated main/silk_main adaptation after migration.

## Bounded source context owner

`silk/native_diagnostics` supplies the public NativeDiagnostics actor. Its make
operation accepts a NativeReport writer and an explicit node capacity. Its observe
operation transfers that owner into one lazy lexical observation. Merely making
or dropping an unstarted observation does not execute the protected computation.
The body handles its typed failures; terminal recovery drops its payload before
calling Intrinsic.observeUnhandled. The observer returns policy 1 even if output
fails. Fatal events write best-effort output before the compiler's machine trap.

The owner allocates one bounded reservation through ExecutionStorage, containing
persistent nodes and a traversal stack. Zero capacity performs no allocation.
Checked capacity overflow or either allocation refusal leaves a writer with no
optional storage; immutable primary identity and origin still come from the
compiler. There is no additional allocator bridge or process-global state.

Nodes retain artifact-lifetime semantic strings and immutable primary/cause links.
Release consumes references iteratively. Only the transition to zero puts a node
on the pending list, which uses dead nodes' next links; destruction neither
allocates nor recurses. Fully released slots return to the free list. The private
Context destructor checks that all references were consumed before its reservation
and accounting owner are released.

Rendering visits primary frames before recovery causes and their frames. It
finds successive immutable frames by scanning the bounded chain and uses the
reserved cursor stack for nested causes. No traversal uses native recursion or
allocates storage. Stack depth cannot exceed the acyclic pool's node capacity.
Missing primary context retains the supplied identity and origin before truncation;
a missing cause truncates without inventing an identity. Output refusal stops
traversal. NativeReport emits at most one bounded truncation marker.

The context conformance fixture uses real compiler-produced failures, selected
recovery causes and outward frames through this public actor. Its independent C
receiver checks full reports, zero/tiny capacities, allocation refusal at each
site, repeated handled failures followed by an identical terminal report, writer
limits and partial-output failure. Dropping an unstarted observation and cancelling
a parked observation must release every allocation. The allocator poisons freed
memory to expose access after release. Cancellation covers both dropping and
notifying the owned Wake during frame destruction. Child processes capture actual
fatal reports with and without a selected cause and must terminate by machine trap.

Default source entry is installed; full migration evidence remains a separate
obligation. The compact context evidence records only the actual executed cases.

## MIR scope implementation

Running the primitive now lowers to DiagnosticScope with owned state and callback
locals, separate cleanup plans, a nested protected execution and its result shape.
Argument construction occurs before entering that execution. Shared MIR visitors,
cleanup normalization and ownership liveness traverse the nested body. The capture
regression checks distinct protected bodies, state cleanup and absence of a second
ordinary drop of the scope's state.

Control expansion now lowers DiagnosticScope to explicit EnterDiagnosticScope and
LeaveDiagnosticScope operations. The protected execution is reachable between
them. Normal completion moves its result, leaves the observation and destroys the
callback followed by state; trap paths do not receive structured cleanup. The state
is an address root because callbacks borrow it mutably. Native emission installs
synchronous descriptors on the stack and suspended descriptors in verified frames.

A suspended protected-body regression retains the affine state with its Drop hook
and the represented callback's captured value in the relay. Success restores both;
the failure plan releases the state. Native fixtures now exercise normal completion, post-resume fatal dispatch and
cancellation, including owned context release through the separate continuation
transport described below.

## Internal observer argument implementation

Modules retaining a diagnostic scope now declare a hidden observer pointer and by-value
six-field cause aggregate after each ordinary function's source lanes and before
any suspension-control lanes.
NativeDiagnosticContext owns an invocation-local slot initialized from that input;
ordinary calls load the current reference at the call site. Synchronous calls and
initial suspension-aware calls forward that reference. Naked machine functions do
not receive an added argument. C export thunks retain their public signature and
pass null observation and a zero cause aggregate to the private implementation,
establishing a fresh observation boundary.

Resume thunks restore the observer saved in each relay frame. Child thunks load
the originating observer from the sixth transfer word. Initial independently owned
Execution body calls pass null observation and zero cause. Release helpers accept
both hidden inputs and restore the observer and active cause retained in each
cancelled frame; storage bootstrap calls also clear both inputs to avoid a
storage-dependent reporting cycle. Resume loads the original incoming cause; child
calls load the active cause from the transfer header. These copies borrow their
reference from the owning outcome; this ABI does not acquire a new pool reference.
Selected-recovery lookup reads its matching metadata slot. Failure production and
returned results populate that slot through explicit reference transfers.

## Synchronous dispatch implementation

Each synchronous scope preallocates a private ten-word descriptor in its
invocation: dispatch function, mutable state address, represented capture storage
previous observer and six-word previous cause. Enter fills it and selects it; leave restores the previous
reference and cause before normal source cleanup. Descriptor allocation occurs at function
entry, so repeated scope entry does not grow the machine stack. Suspended scopes
reserve ten words after the maximum retained payload; acquisition and reuse bind
the descriptor to that persistent address. Captures borrow the callback owner's
concrete storage. Cancellation restores each frame's observer, leaves a scope before
its first owner cleanup, and restores the caller's observer after releasing frames.

The internal adapter accepts capture storage followed by the callback's source
lanes. It reconstructs positional captures using the same NativeCallable operation
as ordinary calls, then invokes the admitted callback with a null observer and zero cause aggregate.
Fatal sites dispatch event 6 with artifact-lifetime reason/origin strings and the
active borrowed cause handle, then execute the irreducible trap. The handle is
selected only when its originating observer matches the current observer; a
foreign pool handle is omitted. Dispatch loads the observer once, and does not
retain or release the borrowed cause. Null observation takes the bare path.
Borrowed cause aggregates cross internal calls and continuation boundaries.
Owned result transport and authored failure production are connected. The suspended
fixture exercises nonzero selected causes and a replacement failure with an attached
cause. Propagation frames, allocation-boundary production and terminal observation
are exercised by the integrated native fixtures.

Six native debug/optimized lanes cover Darwin ARM64 and GNU x86-64/ARM64. The C
parent checks normal owner destruction, fatal output, captured callback arguments,
callback traps, C reentry, disabled observation, nesting and output refusal. The
source object has no generated runtime dependencies. Exact evidence is in
report-observer-conformance.json. Default entry is ordinary selected source and establishes its observer inside the
owned application Execution. No generated entry path remains.

## Native failure value emitter

NativeDiagnosticFailure represents one owned reference as six physical fields:
originating observer, opaque handle, identity pointer/length and origin pointer/length.
Pack/unpack do not retain; they are transfer operations. Produce can return handle
zero while retaining immutable identity/origin. Retain and Propagate return another
owned value without consuming the input. Release and Unhandled dispatch through the
stored observer rather than the invocation's current lexical selection. WithCause
borrows both inputs and rejects different observer owners before dispatching handles.

The emitter operations have a 32/64-bit structural regression for owner selection
and metadata preservation and are connected to Effect outcomes and call results.
The old generated failure globals and artifact-level Termination tables are deleted;
lexical transport is the sole diagnostic path.

## Selected recovery boundaries in MIR

Catch lowering now attaches `Execution.recoveryOutcome` only to the selected
failure handler's region graph. The graph includes conversion of the selected
payload, ordinary callable application, and execution of the returned Effect.
The protected-success branch and nonselected propagation do not acquire that
scope. The referenced local is the original CatchEffect outcome, not its extracted
source payload. Normalization preserves this association and MIR text records it.

Control expansion carries the ordered active outcomes in each LinearBlock's
`recoveryOutcomes`. Normal completion joins outside that scope. A DiagnosticScope
body starts a fresh `recoveryBoundary` and clears locally inherited outcomes;
its LeaveDiagnosticScope and owner drops occupy a separate block in the enclosing
context. The explicit boundary also distinguishes clearing an incoming call cause
from merely having no locally selected handler. MIR verification checks the
referenced outcome producer and the enclosing selection, rejecting a recovery
annotation on the success branch, an unrelated matched payload, a guard, a
short-circuit body or a fresh observation body.

Native failure values, incoming handler causes, continuation retention and
cancellation release consume these ownership boundaries. Payload liveness is not
extended merely to retain a diagnostic reference. The generated entry path and its
legacy caught globals are deleted.

## Private result representation implementation

NativeResult separates source lane values from an optional private diagnostic
aggregate. Its packer checks both source lane count and metadata presence; a
caller cannot accidentally omit required metadata or pass metadata to a shape
that cannot carry it. Unpacking uses the same source ordering for synchronous
results and skips the status field for suspension-step results. The private
aggregate is a final field, separate from the source payload's layout.

When a module retains diagnostic observation, each potentially failing Effect
runner now declares the trailing metadata field in both its synchronous and step
signatures. Infallible results and public C signatures retain their source shape.
NativeCall preserves the complete NativeResult. Ordinary, callable, static and
composite Effect consumers transfer metadata into their outcome slot before
exposing payload lanes. Source-only consumers reject an unexpected metadata field.

Completion takes the returned outcome's reference before releasing other slots and
its invocation frame. Suspension child/resume adapters publish metadata into the
transfer's separate completed-result slot. A resumed caller or driver moves it out
and clears the slot before subsequent invocation or storage teardown. Independent
Execution completion consumes its result metadata when delivering its payload to
the outcome callback. Its body begins with disabled observation, separately from
the callback's driving observation scope.

The 32/64-bit seeded fixture checks nonempty metadata across synchronous and step
forms, publication and consuming clear in transfer storage, and rejection of
missing, extra or discarded fields. Native fixtures exercise the active ABI with
nonempty source-produced metadata and same-observer cause attachment, including
propagation frames, allocation-boundary production, bounded-pool integration and
selected-handler exit timing. Default source entry is integrated; the final migration
audit and repository checks remain open.

## Outcome storage and cleanup implementation

Each potentially failing internal outcome has a separate six-word metadata slot.
Invocations without a resumable frame initialize stack slots, including wrappers
that only transfer execution to a child. Resumable invocations reserve persistent
slots after the frame's scope descriptors. New frame acquisition clears
those slots, while resume binds the existing addresses without clearing them.
The target verifier checks their exact ordering, offsets and maximum frame size,
and MIR encoding exposes the planned fields. Source payload layout is unchanged.

Selected recovery blocks borrow their annotated outcome's slot. An unselected
block uses the invocation cause, while a fresh observation boundary starts empty.
Moving a metadata reference clears its slot. Replacement publishes the incoming
reference before releasing the previous one through its own observer. Completion
releases remaining slots. Scope exit releases matching observer-owned slots before
observer-state cleanup; cancellation does the same and releases the remaining
slots before freeing the frame. These operations never release a borrowed input
cause merely because an invocation ends.

Authored nominal and union-valued Fail operations now produce source handles and
attach a borrowed selected cause when its observer matches. The producer releases
its superseded primary reference after creating the replacement. Static identity
and origin text are emitted independently of the generated host-report policy.
Scope and invocation cleanup release the resulting references through their owners.
Observer dispatch invalidates cached addressable values; NativeFunction reloads
them at the enclosing operation join, after destination stores, avoiding cached
values defined in only one callback branch.

The suspended native fixture allocates nonzero handles in source state, checks
normal owner destruction has no live handles, and checks one live handle at a
fatal selected recovery (none at unrelated fatal sites). It verifies the original
cause and replacement cause after another suspension, including both union arms.
Its small counting observer verifies the compiler protocol independently of the
public NativeDiagnostics pool. The context fixture exercises the public owner
with compiler-produced handles, refusal fallback, reclamation, cancellation and
terminal/fatal output. Default entry is integrated; the complete cleanup audit
remains a separate admission obligation.
