## Why

A self-hosted Silk compiler must invoke Clang and the linker itself, and Silk cannot run another
program at all. `OsRuntime.ts` has no spawn, fork, or exec of any kind: every OS intrinsic it offers
is a file-system operation. That is a hard stage-2 self-hosting blocker.

`apps/docs/content/language/glossary.md` already fixes the shape — an executable path, ordered arguments, an optional working
directory, an exact environment, closed standard input, owned captured output, exit codes
distinguished from signal termination, nonzero exit as result data, and no shell command string —
so what remains is a representation question and the missing intrinsics.

The representation question is settled. Arguments and environment values are exact platform bytes
(#43, option 2: byte-level escape). On POSIX these are arbitrary byte strings, so anything narrower
would make it impossible to hand a received argument back to a child unchanged. `Path` gains a
raw-bytes constructor and view for the same reason; `string`-typed views stay checked, and no second
text type is introduced.

## What Changes

- Add a portable `ChildProcess` service with one blocking `execute(request: &ProcessRequest)`
  operation that runs a child to completion and owns everything it wrote.
- Add `ProcessRequest`, carrying an executable path, ordered argument bytes, an exact environment,
  and an optional working directory. Arguments and environment values are `&[u8]`, and the
  environment starts empty rather than inheriting the caller's.
- Add `ProcessOutcome` as `Exited { code output errors }` or `Signaled { signal output errors }`, so
  a nonzero exit code is outcome data and a terminating signal can never be read as an exit code.
- Add `ProcessError` for a failure to start, to wait, or to capture only, carrying a closed
  stage/reason pair plus an optional numeric provider detail.
- Add `OsChildProcess`, an ordinary-source native provider using two new unsafe intrinsics:
  `Intrinsic.osProcessExecute` runs one child and reports its termination and capture lengths, and
  `Intrinsic.osProcessCapture` copies the retained capture into caller storage.
- Add the evaluator's explicit child-process host, separate from the OS filesystem and
  standard-input hosts, and the reachable-only native runtime symbols.
- Add `Path.fromBytes` and `Path.rawBytes`, the byte-level escape the service needs for an
  executable path and a working directory.

## Capabilities

### New Capabilities

- `bootstrap-child-process`: the portable execution contract, its request and outcome shapes, its
  typed failure, and the native provider's target behavior.

### Modified Capabilities

- `bootstrap-intrinsic-boundary`: admit two unsafe child-process primitives under `Intrinsic`,
  reporting through the existing reason and native-code convention.
- `bootstrap-evaluation`: accept an injected child-process host separate from the OS filesystem and
  standard-input hosts, and block a reachable execution that has no host.
- `bootstrap-silk-stdlib`: ship `ChildProcess` and `OsChildProcess` as separate canonical modules
  and keep native mechanisms out of the portable signature.
- `bootstrap-file-system`: admit exact platform bytes as a `Path` construction input and view,
  keeping every existing normalization rule and leaving the `string` views checked.

## Impact

The change affects the intrinsic inventory, HIR and MIR operation identity, evaluator host
configuration, native runtime shims and linking, standard-library source and manifest, generated
standard-library documentation, and acceptance tests. It adds no shell, no asynchronous or
concurrent execution, no pipes, streaming, or interactive child input, no process groups, signal
delivery, or job control, no `PATH` search, no ambient default provider, and no second text type.
