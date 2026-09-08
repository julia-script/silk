# Native report source conformance

Build the compiler, then run `node packages/compiler/conformance/native-report/run.mjs`
with the SILK_SUPPLY tool, target, SDK/sysroot, GCC and container variables used by
the native-process runner. The runner requires LLVM 22.1.8, checks pinned headers,
rejects LTO and executes both debug and optimized lanes on each admitted target.

An independent C caller invokes the source formatter through an ordinary C export.
Its write implementation checks descriptor 2 and captures exact bytes. Every success
uses partial writes of at most three bytes. Seven cases cover typed failure and
cause formatting, a distinct fatal classification, a byte budget too small for the
first fragment, failure after a committed prefix, EINTR followed by success, zero
progress and caller-frame exhaustion. The latter emits one truncation marker and
prevents later cause output. All cases check both exact output and completion state.

A second source fixture owns a bounded persistent context pool through ExecutionStorage.
Its independent C receiver checks both allocation-failure ordinals, complete release,
shared primary/frame/cause nodes, two links to one child, capacity exhaustion and
100 slot-reuse cycles. Rendering checks primary and outward-frame order before
recovery causes, missing-context fallback, frame and byte limits, and partial output
followed by EIO. Repeated truncation writes nothing further. Reporting performs no
heap allocations beyond the two pool-construction acquisitions; release is iterative,
while rendering recursion is bounded by the selected pool capacity. Its source
callback implements Produce, Propagate, WithCause, Retain, Release, Unhandled and
Fatal. Rendering tests invoke that protocol, check Release returns zero, and prove
that terminal policy remains unchanged by output failure. Fatal cases cover both
absence of a cause and a retained primary/frame/cause chain. The callback also
passes the intrinsic's static NonParking and ownership admission. Compiler emission
of typed-failure events remains separate integration work.

The observer fixture enters the compiler's lexical diagnostic scope through source
C exports. A separate C parent verifies normal state destruction and forked fatal
paths: callback invocation with a captured argument, callback traps, independent
C reentry, disabled observation, nested scopes and output refusal. Fatal children
must terminate by a machine-trap signal; output is checked through a pipe. This
fixture admits synchronous fatal dispatch, not owned failure-context transport.

The suspended observer fixture first suspends a protected failure and its selected
Effect.catchAll handler, whose deferred child borrows the failure payload. It then
repeats those cases through public Effect.suspend, direct Intrinsic.suspendEffect
and caller-funded Execution.make/drive. Both debug and optimized runs must preserve
the recovered divisor and report division by zero. The source observer allocates
nonzero handles, checks replacement-cause operands, and tracks live references.
Normal destruction requires zero live references. Fatal recovery checks exactly
one live reference and the expected original or replacement cause handle; an
unrelated fatal requires zero. Both variants of a union-valued failure enter
separate suspended recovery arms. The cancellation fixture checks one-time
cleanup, reporting from a guard destructor, and leaving a scope before its state
destructor.

The suspended fixture also checks automatic outward-frame events. Ordinary failed
recovery emits the recoverInput and Effect.catchAll frames. An unselected selective
catch preserves its original outcome's diagnostic owner and adds Effect.catch and
selectiveFailure before those two frames. The C receiver verifies frame order and
the final nonzero cause handle after another suspension. Each replacement releases
its predecessor; the observer still requires exactly one live reference at the
fatal recovery. This checks transport independently of the bounded pool adapter.

Two allocation cases request a valid layout whose alignment padding overflows the
target word. They require the compiler allocation boundary to produce the sealed
Intrinsic.StorageFailure identity. Recovery suspends after that production, then
either traps with the original nonzero handle or returns normally with no live
references at observer destruction. Because production precedes the first transfer,
these cases also detect continuation spills that overwrite callback-mutated state
with values cached before the call.
The normal case uses a direct intrinsic catch and checks zero live references from
the next source operation, before its enclosing function returns. This distinguishes
recovery-completion release from delayed function-exit cleanup. A C counter also
requires that checkpoint to execute exactly once.

Two payload-cleanup cases destroy a primary failure inside its selected handler.
The destructor produces and handles CleanupNoise through a direct intrinsic catch.
The source observer checks both failure identities and the cause-combination
operands. An independent C checkpoint requires all three produced handles to have
been observed and only the primary reference to remain when payload cleanup ends.
After another suspension, the fatal case still reports that original handle; the
normal case checks zero references before subsequent source work. Both require
exactly one payload destruction.

Terminal variants destroy the primary payload, suspend, and call
Intrinsic.observeUnhandled. The C receiver requires completed payload cleanup and
the original live handle before the Unhandled callback. Source reporting must name
CleanupPrimary and its original origin. A second variant closes the output descriptor
before formatting; both return the observer's policy value 42, release the retained
reference, and destroy the observer normally. Static terminal-context rejection and
dynamic-absence admission remain separate integration work.

Cancellation also retains a parked Execution after the driving observer has been
destroyed, then drops it. Its body starts without borrowing the driver's observer;
the guard's fatal cleanup takes the bare path. A synchronous independent body has
the same boundary. A fatal outcome callback still uses the driving observer.
The C receiver checks observer destruction before escaped cancellation, forbids
observer events from both independent bodies, and checks the callback's fatal output.

Only the Execution fixtures may retain malloc/free for their explicit system
allocator. Other source objects retain no generated runtime symbol. Object inspection checks
architecture and the write relocation. Evidence includes pinned tools and headers,
source and artifact identities, C compilation, object inspection, disassembly and
actual execution. Required platform CI uploads `.scratch/native-report`.

The pool is a candidate source implementation retained in the conformance fixture,
not an installed default observer. Compiler failure-context transport, payload
cleanup ordering and default entry selection remain JUL-130 integration work.
The standalone direct-suspend-regression.silk is also part of the shared native
acceptance corpus: direct Intrinsic.suspendEffect must retain the following division
in its caller. The primitive owns a separate generated runner for its terminal
transfer origin.
