## 1. Portable Contract

- [x] 1.1 Add the canonical `silk/standard_input` module with the `StandardInput` service, its one
      blocking `read` operation, and the `StreamReadFailure` typed failure plus its constructor.
- [x] 1.2 Add `ReadOutcome` as an ordinary struct holding `Filled { count } | EndOfInput`, with
      `filled`, `endOfInput`, `count`, and `isEndOfInput` accessors and the `receive` service effect.
- [x] 1.3 Register the module and its prelude aliases in the standard-library manifest and
      regenerate the compiler-shipped source table.

## 2. Intrinsic Boundary

- [x] 2.1 Add the unsafe native-only `Intrinsic.osStandardInputRead` catalog entry with its
      `Option<usize>` result, reason and native-code outputs, and stated invariant.
- [x] 2.2 Add its HIR operation identity and record its standard-library consumer in the checked
      intrinsic inventory.
- [x] 2.3 Confirm target-restricted availability rejects a reachable read on direct Wasm through the
      existing generic mechanism.

## 3. Evaluation Host

- [x] 3.1 Define the injected evaluator standard-input provider with one capacity-bounded read and
      an in-memory implementation supporting scripted bytes, short commits, and host failure.
- [x] 3.2 Evaluate the read against that provider, committing the exact transferred prefix and
      normalizing a host error to the low-level reason convention.
- [x] 3.3 Block a reachable read that has no injected host instead of inventing empty input.

## 4. Native Provider and Runtime

- [x] 4.1 Add the ordinary-source `OsStandardInput` provider translating `Some(0)` to `EndOfInput`
      and `None` to `StreamReadFailure`.
- [x] 4.2 Add the reachable-only native runtime symbol reading the process standard-input descriptor
      with interrupt retry and stable failure mapping.
- [x] 4.3 Prove the read lowers to exactly one OS call and links only its own runtime symbol.

## 5. Acceptance and Documentation

- [x] 5.1 Add tests reading a known byte sequence from an in-source test provider and asserting the
      committed count.
- [x] 5.2 Add tests asserting the end of input is `EndOfInput` data reached without the failure
      channel, and that a partial read reports its true count rather than the buffer length.
- [x] 5.3 Add tests routing both an in-source provider error and a native host error to
      `StreamReadFailure`.
- [x] 5.4 Update the standard-library README and the project glossary, and regenerate the standard
      library and diagnostic documentation pages.
