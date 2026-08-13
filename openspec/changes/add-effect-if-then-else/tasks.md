## 1. Combinator

- [x] 1.1 Add `Effect.ifThenElse` to canonical `silk/effects` source, taking the two branches as
      suspended `once fn() -> Effect<...>` arms and invoking only the arm the condition selects.
- [x] 1.2 Release the unselected arm with an explicit `drop move`, so the affine obligation for the
      arm that is never invoked is discharged in the source.
- [x] 1.3 Return `A ! E | F ? R | S`, so the caller discharges whatever either branch could need
      without knowing which one is selected.
- [x] 1.4 Name it `ifThenElse`, because `if` is lexed unconditionally as a keyword and the
      declaration itself cannot be spelled `if`.
- [x] 1.5 Regenerate the compiler-shipped standard-library source table.

## 2. Acceptance

- [x] 2.1 Assert the true arm runs and none of the false arm's effects are performed, counting
      service calls through a `&mut Counter` provider.
- [x] 2.2 Assert the same in the opposite polarity.
- [x] 2.3 Assert the unselected arm is never *constructed*, not merely never run — the reworded #98
      criterion — using ordinary `fn` arms whose bodies allocate at invocation, so the allocation
      trace distinguishes an arm that was built from one that was not. Both polarities.
- [x] 2.4 Assert the requirement rows of both arms are unioned and both are dischargeable, in both
      polarities.
- [x] 2.5 Assert the selected arm's failure propagates through the unioned failure row, and that
      selecting the infallible arm produces no failure.
- [x] 2.6 Discharge the reworded cleanup criterion by asserting that a capturing value is rejected
      where a zero-arity arm is required, since no arm can own anything to leak.
- [x] 2.7 Assert the three engines — evaluator, Wasm, and native — agree on every selecting case.

## 3. Documentation

- [x] 3.1 Regenerate the standard-library documentation page.
