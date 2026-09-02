## 1. Portable Contract

- [x] 1.1 Add the canonical `silk/child_process` module with the `ChildProcess` service, its one
      blocking `execute` operation, and the `ProcessError` typed failure with its closed
      stage/reason pair, optional provider detail, and constructors.
- [x] 1.2 Add `ProcessRequest` carrying an executable path, ordered argument bytes, an exact
      environment that starts empty, and an optional working directory, with `addArgument`,
      `setVariable`, and borrowed accessors.
- [x] 1.3 Add `ProcessOutcome` as an ordinary struct holding `Exited { code output errors } |
Signaled { signal output errors }`, with `exited`, `signaled`, `isSignaled`, `exitCode`,
      `terminatingSignal`, `outputBytes`, and `errorBytes` accessors and the `submit` service effect.
- [x] 1.4 Register the module and its prelude aliases in the standard-library manifest and
      regenerate the compiler-shipped source table.

## 2. Path Byte Escape

- [x] 2.1 Add `Path.fromBytes`, applying every existing normalization rule to exact platform bytes.
- [x] 2.2 Add `Path.rawBytes`, the lossless borrowed view, leaving `Path.view` and `Path.name`
      checked and unchanged.

## 3. Intrinsic Boundary

- [x] 3.1 Add the unsafe native-only `Intrinsic.osProcessExecute` catalog entry with its byte-block
      inputs, termination and capture-length outputs, reason and native-code outputs, and stated
      invariant.
- [x] 3.2 Add the unsafe native-only `Intrinsic.osProcessCapture` catalog entry with its stream
      selector, offset, `Option<usize>` result, and stated invariant.
- [x] 3.3 Add their HIR operation identities and record their standard-library consumers in the
      checked intrinsic inventory.
- [x] 3.4 Confirm target-restricted availability rejects a reachable execution on direct Wasm
      through the existing generic mechanism.

## 4. Evaluation Host

- [x] 4.1 Define the injected evaluator child-process provider taking one structured request and
      returning an exit, a signal, or a host failure, with an in-memory implementation that scripts
      outcomes and records every request.
- [x] 4.2 Evaluate the execution against that provider, splitting the entry blocks into entries,
      rejecting a malformed low-level request, and normalizing a host error to the low-level reason
      convention.
- [x] 4.3 Evaluate the capture against the retained result with a stream selector and an offset.
- [x] 4.4 Block a reachable execution that has no injected host instead of inventing an outcome.

## 5. Native Provider and Runtime

- [x] 5.1 Add the ordinary-source `OsChildProcess` provider translating the termination selector to
      `Exited` or `Signaled` and an absent result to `ProcessError`.
- [x] 5.2 Add the reachable-only native runtime symbols: one that forks, closes the child's standard
      input, reports a failure to start through a close-on-exec channel, drains both streams
      concurrently, and waits; and one that copies the retained capture.
- [x] 5.3 Prove the execution lowers to the two OS calls and links only its own runtime symbols.

## 6. Acceptance and Documentation

- [x] 6.1 Add tests running a program that exits zero and asserting the captured output and errors.
- [x] 6.2 Add tests asserting a nonzero exit code is result data and that a signal is distinguished
      from an exit code.
- [x] 6.3 Add a test asserting a missing executable is a typed process failure that retains its
      platform code.
- [x] 6.4 Add a test asserting the service presents no environment by default and passes exact bytes,
      including bytes that are not well-formed text, for what it was given.
- [x] 6.5 Update the standard-library README and the project glossary, and regenerate the standard
      library and diagnostic documentation pages.
