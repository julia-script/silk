## Why

Silk cannot read a byte. `StandardStreams` writes only, and its exclusion is scoped to that one
service: it "does not imply terminal control, color detection, flushing, logging, or interactive
input" (`CONTEXT.md`). Two properties of the existing contract make an added read impossible rather
than merely inconvenient:

- Its `destination` is a `bool`. `stdout()` is `false` and `stderr()` is `true`; a two-value type
  has no third value for a read direction, and a second parameter would burden every write.
- Its write is all-or-nothing: `writeAll` must commit the complete sequence or return a typed
  stream failure. A read is inherently partial — the host may commit fewer bytes than the caller
  asked for, and input ends — so a read needs its own outcome shape.

Both point at a separate service rather than an extension of `StandardStreams`.

## What Changes

- Add a portable `StandardInput` service with one blocking `read(buffer: &mut [u8])` operation that
  fills a prefix of the caller's buffer.
- Add a `ReadOutcome` of `Filled { count }` and `EndOfInput`. The count is the exact number of
  committed bytes and may be less than the buffer length; the end of input is outcome data, not a
  typed failure.
- Add `StreamReadError` for a host error only, so a caller that drains input never handles a
  failure to reach the end.
- Add `OsStandardInput`, an ordinary-source native provider reading the process standard-input
  descriptor through one new unsafe `Intrinsic.osStandardInputRead`, following the same
  reason-and-native-code OS boundary `os_filesystem` already uses.
- Add the evaluator's explicit standard-input host and the reachable-only native runtime symbol.
- Leave `StandardStreams` untouched: same service, same `bool` destination, same typed failure.

## Capabilities

### New Capabilities

- `bootstrap-standard-input`: the portable byte-input contract, its partial-read and end-of-input
  outcome, its typed host failure, and the native provider's target behavior.

### Modified Capabilities

- `bootstrap-intrinsic-boundary`: admit one unsafe byte-input primitive under `Intrinsic`, reporting
  through the existing `Option<usize>` plus reason and native-code convention.
- `bootstrap-evaluation`: accept an injected standard-input host separate from the OS filesystem
  host, and block a reachable read that has no host.
- `bootstrap-silk-stdlib`: ship `StandardInput` and `OsStandardInput` as separate canonical modules
  and keep native mechanisms out of the portable signature.

## Impact

The change affects the intrinsic inventory, HIR and MIR operation identity, evaluator host
configuration, native runtime shims and linking, standard-library source and manifest, generated
standard-library documentation, and acceptance tests. It adds no terminal control, raw mode, color
detection, line editing, prompt handling, non-blocking or asynchronous read, hosted-Wasm input ABI,
ambient default provider, or change to `StandardStreams`.
