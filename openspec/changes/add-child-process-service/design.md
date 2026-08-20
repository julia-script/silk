## Context

Silk has ten OS intrinsics and every one of them is a file-system operation. Running another program
needs both a portable contract and new compiler primitives, and the primitives are constrained: an
intrinsic parameter is a scalar, a byte slice, a mutable scalar output, or an OS handle. A child's
request is a variable-length list of variable-length byte strings, and its captured output has a
size nobody knows until the child has finished. Those two facts decide most of this design.

## Goals / Non-Goals

Goals:

- One portable execution contract that a pure in-source provider can implement without a host.
- A request that can carry any argument or environment value the platform can carry.
- An outcome that separates an exit code from a terminating signal, and both from a typed failure.
- A native provider built from the smallest possible compiler primitives, in ordinary Silk source.

Non-Goals: a shell, `PATH` search, asynchronous or concurrent execution, pipes, streaming output,
interactive child input, process groups, signal delivery, job control, a default provider, and a
second text type.

## Decisions

### Arguments and environment values are exact bytes

Settled by #43, option 2. A POSIX argument or environment value is an arbitrary NUL-free byte
string, so a `string`-typed request could not hand a received argument back to a child unchanged —
which is exactly what a compiler driver does when it forwards a path it was given. `addArgument` and
`setVariable` therefore take `&[u8]`.

`Path` gains the same escape rather than growing a sibling type: `Path.fromBytes` constructs from
exact platform bytes and `Path.rawBytes` views them. Both keep every existing normalization rule —
absolute, no NUL, no `.`, `..`, empty component, or trailing separator — and lift only the
well-formed-text requirement. `Path.view` and `Path.name` are unchanged, so a `string` view of a
path is exactly as trustworthy as it was.

### The request is a builder over one NUL-terminated block per list

`ProcessRequest` stores arguments and environment entries as one `Bytes` block of NUL-terminated
entries plus a count, which is the representation `execve` wants and the only one an intrinsic can
receive. An argument may not contain NUL, so the terminator is unambiguous and the encoding is
lossless. `addArgument` and `setVariable` append; the entries keep the order they were added.

The environment starts empty. A child sees no variable this request did not name, which makes
"inherit my environment" an explicit act rather than the default, and makes an execution's inputs
completely visible in the request value.

The child's `argv[0]` is the program path. That is the platform convention and Rust's `Command`
convention; the request's own arguments follow it.

### The outcome names how the child ended

```silk
pub struct ProcessOutcome {
  pub value: Exited | Signaled
}
```

An exit code and a signal number are different facts about a child, and folding them into one
integer with a sentinel is how "exit status 139" happens. They are separate members, so
`exitCode(outcome)` is `None` for a signaled child and no caller can read a signal number as a
return value.

A nonzero exit code is data inside `Exited`. A compiler driver runs a tool that fails as a matter of
course, and reading that tool's code and its diagnostics is the ordinary path, not recovery. Only a
failure to start, to wait, or to capture is `ProcessError`, which carries which of those three
stages failed.

### Two intrinsics, because the capture has an unknown size

`Intrinsic.osProcessExecute` runs the child to completion, drains both streams into storage the
boundary retains, and reports the termination status, the termination code, and the exact captured
lengths. `Intrinsic.osProcessCapture` copies from the retained capture into caller-owned storage
using a stream selector and an offset, in the `Option<usize>` transfer shape `Intrinsic.osFileRead`
already uses.

The alternative — one intrinsic writing directly into caller buffers — cannot work: the buffer has
to be sized before the child runs, and the child has already run by the time a too-small buffer is
discovered, so a short buffer would silently truncate. Splitting execution from the copy keeps the
result complete without a handle type. It is deliberately not a handle: the service is blocking and
single-child by contract, so exactly one capture is live at a time, and adding a third `OsHandle`
kind would push process state through the filesystem host that owns handle closing.

The pairing is the intrinsics' stated invariant: a capture reads the immediately preceding execute's
result, and the next execute replaces it. Both are unsafe, native-only, and reachable-only, so they
inherit the existing acknowledgement, Wasm rejection, and runtime-linking behavior with no new
compiler machinery.

### The native runtime drains both streams concurrently

The runtime shim forks, redirects the child's standard input from `/dev/null`, and reports a failure
to start through a close-on-exec channel, so a missing executable is `ENOENT` at the boundary rather
than exit status 127 from the child. It then polls both pipes until each reaches its end before
waiting for the child. Draining one pipe to completion first would deadlock against a child that
fills the other.

### The evaluator host is separate from the OS filesystem host

The OS filesystem host is a filesystem contract: opens, handles, directories, and paths. An
execution has none of those, so it gets its own injected provider taking one structured request and
returning one structured result. The evaluator splits the NUL-terminated blocks back into entries at
that boundary, so a test asserts on the request the child would have received rather than on a byte
block. A reachable execution with no injected host blocks rather than inventing an outcome.

## Risks / Trade-offs

- The retained capture is boundary state rather than an owned handle. The invariant is stated on
  both intrinsics, and the only public path to them runs execute and its captures inside one service
  call, so no ordinary program can interleave two executions.
- Output is captured whole, in memory. Streaming output is deliberately deferred rather than
  approximated; a compiler driver's tool output is bounded in practice.
- The service takes an absolute executable path and never searches `PATH`. Resolving a tool name is
  the caller's decision, and doing it here would silently depend on an environment the request is
  explicit about not inheriting.
- Direct Wasm has no processes. Both intrinsics are native-only and rejected on Wasm, exactly as the
  OS filesystem operations are, rather than given invented host imports.
