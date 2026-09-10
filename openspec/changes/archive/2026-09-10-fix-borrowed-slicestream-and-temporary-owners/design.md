## Context

See proposal.md. At e0b30a93 the explicit/named/local-view/qualified control compiles and consumes 10. Isolated changes reproduce SEM0051, OWN0010, OWN0019 and the native reference-read repeated-layout crash. Existing declaration header replay handles inherent heads; existing TemporaryRoot identities and loan/cleanup machinery already model temporary borrows. The native reference-place loop assumes element projection always follows inline repeated storage, which fails after a slice field.

## Goals / Non-Goals

**Goals:** reuse canonical declaration binders, concrete place loans, lexical owners and slice operations. Make the existing abstraction contracts agree across source spellings and phases.

**Non-Goals:** Stream API design, arbitrary expression lifetime extension, altered Self output defaults, relaxed borrow conflicts, trap unwinding, lifetime-driven specialization or a second temporary representation.

## Decisions

1. Replay conformance headers after nominal lifetime arity is known, retaining implicit impl binders in operation signatures and closing Self against the completed owner. Reuse declaration lifetime elaboration; do not patch missing argument counts in conformance lookup or infer relationships from implementation bodies.
2. Preserve distinct wrapper and stored-data provenance. Synthesized receiver borrows use the same root and adaptation as written borrows; an exclusive receiver loan does not upgrade the shared loan retained by a slice field. Suppressing conflicts globally would admit genuinely overlapping access and is rejected.
3. Normalize projection through each slice descriptor into the existing slice access path during Effect lowering. Materialize the descriptor as a local before evaluating its index, retaining its pointer, runtime length and element type together. This also handles dynamic array prefixes and nested slice projections without native special cases. Treating the descriptor as inline repeated storage or deriving a constant bound from the wrapper layout is invalid.
4. Borrowed array producers in a binding initializer materialize one hidden local owner at their original evaluation point. The owner belongs to the current lexical block/selected branch/loop iteration and is retained by ordinary dependent loans, including aggregate and delayed-value propagation. Extend the existing temporary identity into an ordinary local declaration/storage lifetime; never merely suppress OWN0019. Preserve earlier argument effects and conditional evaluation, rather than hoisting every producer before the initializer. Existing expected-type analysis determines element types; moving storage must not add new contextual conversions.
5. Hidden locals participate in ordinary initialization, cleanup and coroutine liveness. Dependent owners/loans end before backing storage cleanup; partially initialized arrays and early exits use established cleanup recipes. Suspension and interruption retain/drop storage like an explicitly named local. Function-local references cannot be returned in aggregates or Effects. Fatal traps retain no-unwind behavior.

## Risks / Trade-offs

- Binder replay can lose explicit bounds or capture a method-local lifetime → compare explicit/elided contracts and retain ordinary type-argument negative controls.
- Receiver provenance can accidentally relax wrapper conflicts → test live wrapper loans and backing-owner mutation alongside repeated successful calls.
- Descriptor projection can read a wrong length or pointer → assert structural slice bounds and consume native results including an out-of-bounds boundary.
- Hidden-owner hoisting can reorder side effects or leak iteration storage → preserve original evaluation points and verify branch/loop/early-exit cleanup plus suspension interruption.
- Lifecycle tests can inflate the compiler critical path → share analysis snapshots, extend existing test files, consolidate runtime claims in the shared native corpus and measure focused base/head cost.

## Migration Plan

One green-field compiler change updates all affected representations, consumers, tests and reference text. No compatibility path or migration is retained. Deliver only after required checks and two independent final-diff reviews.
