# Reporting context and cleanup contract

This contract records the semantic obligations implemented by the source-owned reporting
transport. The exact observer ABI is in report-observer-contract.md. Lexical transport, bounded
source ownership, default startup and deletion of the generated report state are implemented;
final migration admission remains open.

## Outcome ownership

A failure's diagnostic identity, origin, outward logical frames and recovery causes
belong to its outcome. They do not belong to its payload fields or to whichever
failure most recently executed in the process. Propagation preserves this context
and appends the propagating logical frame. It does not replace the failure origin.

Cleanup may execute ordinary source code, including an Effect that fails and is
successfully recovered. Such an inner outcome cannot replace the outcome whose
unwinding caused cleanup. This applies both to owners released during propagation
and to the failure payload destroyed at terminal entry. The latter still completes
before any terminal report bytes are written (TERM-011).

If cleanup traps, the trap remains fatal. Neither preservation nor reporting grants
trap recovery, resumed unwinding or a guarantee that output is possible (TERM-008).

## Recovery ownership

The protected failure becomes a cause only while its selected recovery handler is
being applied and executed. A failing handler returns its own primary context with
that protected context retained as cause. A successful handler discards the handled
context. A nonselected failure propagates without acquiring a new cause. Merely
reifying an outcome is not permission to install an ambient cause indefinitely.

Lowering makes both operands before executing the protected Effect.
EffectLowering.lowerEffectCatch marks the selected handler's application and run
with the original caught outcome. That lexical recovery marker supplies a borrowed
cause through calls and suspension; merely executing CatchEffect does not select a
cause. Normal recovery completion releases the protected outcome before subsequent
source operations, after transferring the handler's success result. Failure returns
and cancellation release their remaining owned outcomes through their exit paths.
An unselected selective-catch arm transfers the original outcome's metadata with
an outward frame and does not enter recovery.

## Required transport properties

- Each call's returned failure context remains distinct from contexts produced by
  cleanup, callbacks and reentrant library entry.
- Suspending execution retains its context through the admitted execution-storage
  lifetime. Resume cannot refer to a departed native stack frame.
- Source owns report storage, bounds, exhaustion behavior, formatting and output.
  Immutable semantic identities and provenance remain compiler-owned.
- A disabled reporter retains bare traps and does not acquire report allocation or
  output dependencies. Fatal reporting cannot recursively invoke the observer.
- Context release follows handled, propagated, cancelled and terminal outcomes;
  the implementation cannot grow an unreclaimed history of already handled failures.

## Executed cleanup regression

The shared native corpus case native-termination-cleanup-preserves-primary creates
Primary in primary(), then propagates through main. Both a live Guard destructor
and Primary's payload destructor execute and recover Noise. The expected terminal
report retains Primary, primary's origin and main's logical frame, with no Noise
origin or cause. It uses the existing differential native corpus instead of adding
a per-feature compile/link test.

The historical regression named Primary but gave noise() as its origin and lost main's frame.
The replacement associates diagnostic context with owned outcomes, so cleanup's recovered Noise
cannot overwrite Primary. The temporary global-save/restore helper and the generated globals
it protected are deleted. Current hosted-start and startup-fault fixtures independently exercise
payload cleanup before output, suspended propagation and nested entry with poisoned frees.
