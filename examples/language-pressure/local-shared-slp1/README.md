# Local shared SLP-1 pressure program

This executable pressure program asks whether SLP-0002's allocation-backed `Shared<T>` is enough
to build the shared-state portion of an SLP-0001-style deferred result in ordinary Silk.

The witness allocates exactly two shared blocks: a fixed-capacity ready inbox and a
`DeferredState<Payload, F>`. Three waiters register real once-callable values under short exclusive
accesses. A producer moves one affine payload into the state, extracts the callbacks, restores the
state, and only then invokes them. Each waiter subsequently observes that same retained payload
through its state handle under a shared borrow, and the inbox records deterministic order `1, 2, 3`.

The program also drops an unrun publication Effect, an unrun observation Effect, an uncalled stored
callable, an unpublished affine payload, and the final state and inbox owners. The focused tests
sweep allocation ordinals 0, 1, and completion through structural compiler evidence, compare
normalized semantic and MIR structure against an actor-renamed fixture, and register success,
renamed, and boundary cases in the independently pinned native corpus. A successful result of `42` is
returned only after order, publication, payload/callback cleanup, and final-state lifecycle checks;
quota recovery returns the distinct value `142`.

This is evidence, not a canonical deferred-result API. It deliberately does not transfer
execution, park work, choose a wake order, or introduce a scheduler. Those remain SLP-0001
questions. See [findings.md](findings.md) for the classified results.
