## 1. Portable Contract

- [x] 1.1 Add the canonical `silk/host_input` module with the `HostInput` service, its four
      read-only operations, and the `HostInputError` typed failure plus its constructor.
- [x] 1.2 Add the `arguments` collector, the `variableNamed` textual-name lookup, and the checked
      `text` view over raw host bytes.
- [x] 1.3 Register the module and its prelude alias in the standard-library manifest and regenerate
      the compiler-shipped source table.

## 2. Intrinsic Boundary

- [x] 2.1 Add the unsafe native-only `Intrinsic.osHostArgumentCount`, `Intrinsic.osHostArgument`,
      `Intrinsic.osHostVariable`, and `Intrinsic.osHostWorkingDirectory` catalog entries with their
      results, reason and native-code outputs, and stated invariants.
- [x] 2.2 Add their HIR operation identities and record their standard-library consumers in the
      checked intrinsic inventory.
- [x] 2.3 Confirm target-restricted availability rejects a reachable lookup on direct Wasm through
      the existing generic mechanism.

## 3. Evaluation Host

- [x] 3.1 Define the injected evaluator host-input provider with argument-count, argument, variable,
      and working-directory lookups, and an in-memory implementation supporting a scripted command
      line, environment, working directory, and host failure.
- [x] 3.2 Evaluate the lookups against that provider, committing the prefix that fits, reporting the
      complete byte length, and normalizing absence to the not-found reason.
- [x] 3.3 Block a reachable lookup that has no injected host instead of inventing empty input.

## 4. Native Entry and Runtime

- [x] 4.1 Change the native entry point to receive `argc` and `argv` and store them for the
      host-input runtime, keeping every termination status and report byte unchanged.
- [x] 4.2 Add the reachable-only native runtime symbols reading the stored command line, the process
      environment block by raw bytes, and the working directory with growth retry.
- [x] 4.3 Add the ordinary-source `OsHostInput` provider translating a short buffer into one exact
      second pass, the not-found reason into `None`, and any other reason into `HostInputError`.

## 5. Acceptance and Documentation

- [x] 5.1 Add tests returning three arguments in order from a pure in-source provider and asserting
      the argument count.
- [x] 5.2 Add tests asserting a missing variable and an argument past the end are absence reached
      without the failure channel, and that a value which is not valid UTF-8 survives byte for byte
      while its checked textual view refuses it.
- [x] 5.3 Add tests routing both an in-source provider error and a native host error to
      `HostInputError`, and proving the lookups lower to their own reachable-only runtime symbols
      and are rejected on direct Wasm.
- [x] 5.4 Add an end-to-end test compiling a native program that derives its exit status from the
      real command line and environment, and assert a program that reads no host input keeps
      statuses `0`, `1`, and `2` with arguments present.
- [x] 5.5 Update the standard-library README and the project glossary, and regenerate the standard
      library documentation page.
