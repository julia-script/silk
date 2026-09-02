## 1. Combinator

- [x] 1.1 Add `Effect.ensuring` to canonical `silk/effect` source, reifying the protected Effect
      with `Effect.result` before running the finalizer so the finalizer exits after the protected
      Effect's local cleanup rather than on its propagation path.
- [x] 1.2 Type the finalizer `once Effect<() ! never ? S>` and return `A ! E ? R | S`, so the
      original outcome is preserved and only the requirement row widens.
- [x] 1.3 Regenerate the compiler-shipped standard-library source table.

## 2. Acceptance

- [x] 2.1 Assert the finalizer runs after a success and the success value reaches the caller
      unchanged.
- [x] 2.2 Assert the finalizer runs after a typed failure and the recovery handler receives that
      same failure with its payload intact.
- [x] 2.3 Assert the order against local cleanup on an allocation trace, using a protected Effect
      holding one owner and a finalizer holding two so the two bodies' events are distinguishable
      in either order.
- [x] 2.4 Assert an owner acquired inside the protected body is released exactly once across a
      failing run, and that acquires equal releases once the finalizer has contributed its own —
      the leak `bootstrap-ownership` fixed for generic bodies, which this combinator was named as
      the one that would reintroduce.
- [x] 2.5 Assert a fallible release recovered into `() ! never` composes and still preserves the
      protected Effect's outcome.
- [x] 2.6 Assert a trap inside the protected Effect blocks the run without producing any finalizer
      events.

## 3. Documentation

- [x] 3.1 Regenerate the standard-library documentation page.
