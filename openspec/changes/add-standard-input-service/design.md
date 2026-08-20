## Context

`StandardStreams` is the only host byte boundary Silk has. It writes to stdout and stderr and
reports `StreamWriteError`. Reading is not a missing parameter on that service; it is a different
shape of operation, and the two constraints below decide the design rather than leaving it open.

## Goals / Non-Goals

Goals:

- One portable byte-input contract that a pure in-source provider can implement without a host.
- An outcome that distinguishes a short read from the end of input from a host error.
- A native provider built from the smallest possible compiler primitive, in ordinary Silk source.

Non-Goals: terminal control, raw mode, color detection, line editing, prompt handling, non-blocking
or asynchronous reads, a hosted-Wasm input ABI, a default provider, and any change to
`StandardStreams`.

## Decisions

### A separate service, not a third destination

`core.silk` defines `stdout()` as `false` and `stderr()` as `true`. The destination type is `bool`,
so it holds exactly two values and neither can name a read. Widening it to a three-value type or
adding a direction parameter would change every existing write call for the benefit of an operation
with a different result type, a different failure meaning, and a different partiality contract. The
services stay separate, and `StandardStreams` is unchanged.

### The outcome is data, and the end of input is one of its members

`writeAll` commits everything or fails. A read cannot promise that: the host decides how many bytes
it hands back, and input ends. So `read` returns

```silk
pub struct ReadOutcome {
  pub value: Filled | EndOfInput
}
```

`Filled { count }` carries the exact committed count, which may be less than the buffer length and
is never greater. Bytes past the committed prefix are untouched.

`EndOfInput` is a member of the outcome rather than a failure because reaching the end is the
ordinary way input finishes. A loop that drains input to completion therefore has no failure to
handle on its normal path, and `StreamReadError` keeps its meaning: the host could not perform the
read. This mirrors `Option` and `Result` in the standard library, which are ordinary structs holding
a structural union narrowed by `match`, rather than a compiler-known outcome type.

### The native provider uses the OS intrinsic pattern

`Intrinsic.osStandardInputRead(output: &mut [u8], reason: &mut i32, nativeCode: &mut u32)` returns
`Effect<Option<usize>>`, exactly the shape `Intrinsic.osFileRead` already uses. `Some(n)` is a
committed transfer of `n` bytes, `Some(0)` is the end of input, and `None` writes the normalized
reason and native code. Reusing the shape means the operation inherits the existing unsafe
acknowledgement, native-only target restriction, reachable-only runtime linking, and Wasm rejection
without new compiler machinery.

The intrinsic takes no handle. Standard input is a process-wide descriptor rather than a resource
the caller opened, so there is no close obligation to transfer and `OsStandardInput` owns no state.

### The evaluator host is separate from the OS filesystem host

The evaluator's OS filesystem host is a filesystem contract: opens, handles, directories, and paths.
Standard input has none of those, so it gets its own injected provider with one `read(capacity)`
operation. A reachable read with no injected host blocks rather than inventing empty input, matching
how a missing `StandardStreams` provider blocks rather than discarding output.

## Risks / Trade-offs

- A caller must loop to fill a buffer completely. That is inherent to a partial read; a
  fill-completely helper can be added later above this boundary without changing the service.
- Native reads block. A non-blocking or asynchronous surface is deliberately deferred rather than
  approximated.
- Direct Wasm has no standard input. The read is native-only and rejected on Wasm, exactly as the
  OS filesystem operations are, rather than given an invented host import.
