## Why

A self-hosted Silk compiler must receive source paths and flags from its command line, and Silk
cannot read one. The native entry point emits `int main(void)`, so `argc` and `argv` never reach the
program, and no service gives a program its arguments, an environment value, or its working
directory. This is a stage-2 self-hosting blocker.

Two things had to be settled before the surface could be written, and both now are:

- **Representation.** #43 selected the byte-level escape: argv entries and environment values are
  raw bytes, exactly as the process received them, and checked `string` views layer on top. On a
  POSIX host neither is required to be UTF-8, and a program must be able to read a value it cannot
  decode and pass it through unchanged. No second text type is introduced.
- **The entry shape.** The runtime must hold the command line for a provider to read, so the native
  entry becomes `main(int argc, char **argv)`. Silk `main` keeps its zero-parameter,
  empty-requirement-row shape: arguments reach a program through a service, never through the entry
  signature.

## What Changes

- Add a portable `HostInput` service with `argumentCount`, `argument(index)`, `variable(name)`, and
  `workingDirectory`. It reads only: there is no operation that sets a variable or changes the
  working directory.
- Return raw bytes from every value-returning operation, and add `text`, the checked fallible
  textual view, so undecodable input stays readable and passable rather than being replaced.
- Report an index at or past the argument count, and an unset variable name, as `None`. Only a host
  that cannot answer at all is `HostInputFailure`.
- Add `arguments`, which collects the complete command line in order above the service.
- Add `OsHostInput`, an ordinary-source native provider reading the process command line,
  environment block, and working directory through four new unsafe `Intrinsic` operations that reuse
  the `Option<usize>` plus reason and native-code shape `os_filesystem` already uses.
- Change the native entry point to `main(int argc, char **argv)`, which stores the command line for
  the host-input runtime before running the user entry. The exit-code contract is unchanged: `0` for
  success, `1` for a reported typed failure, `2` for a failed standard-error write.
- Add the evaluator's injected host-input host, separate from the OS filesystem and standard-input
  hosts, so a scripted command line, environment, and working directory are trivially testable.

## Capabilities

### New Capabilities

- `bootstrap-host-input`: the portable process-input contract, its byte representation and checked
  textual view, its absence-versus-failure split, and the native provider's target behavior.

### Modified Capabilities

- `bootstrap-entry-termination`: the native entry receives the process command line and holds it for
  the host-input runtime, while Silk `main` keeps its zero-parameter shape and every termination
  status is unchanged.
- `bootstrap-intrinsic-boundary`: admit four unsafe process-input primitives under `Intrinsic`,
  reporting through the existing `Option<usize>` plus reason and native-code convention.
- `bootstrap-evaluation`: accept an injected host-input host separate from the other hosts, and
  block a reachable lookup that has no host.
- `bootstrap-silk-stdlib`: ship `HostInput` and `OsHostInput` as separate canonical modules and keep
  native mechanisms out of the portable signature.

## Impact

The change affects the intrinsic inventory, HIR and MIR operation identity, evaluator host
configuration, the native runtime shim and its entry signature, standard-library source and
manifest, generated standard-library documentation, and acceptance tests. It adds no argument
parsing or flag grammar, no environment mutation, no working-directory mutation, no `OsString` or
`PlatformPath` type, no Wasm entry change, and no change to any existing exit status.
