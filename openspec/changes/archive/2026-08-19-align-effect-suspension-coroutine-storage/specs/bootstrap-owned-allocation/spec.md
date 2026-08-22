## REMOVED Requirements

### Requirement: Continuation storage uses explicit typed allocation

**Reason**: [SUSP-006](../../../../../../docs/language/effect-suspension.md#susp-006--execution-stack-exhaustion-is-a-fatal-trap)
classifies coroutine frames as compiler-owned execution-stack storage. Their placement is not a
source allocation, and exhaustion is a fatal trap rather than `OutOfMemory`.

**Migration**: Remove continuation requests from the `Allocator` service, remove allocator-visible
request/refusal/release events and failure ordinals, and realize one reusable private frame per
active suspendable invocation.

### Requirement: Continuation storage releases through captured authority

**Reason**: Coroutine frames are owned and released by the private execution stack, so no frame
captures reclaim authority from a source allocator.

**Migration**: Release completed frames through their compiler-owned execution owner and retain no
allocator provider, loan, reclaim token, or source-visible release event.
