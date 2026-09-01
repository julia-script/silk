## Context

Silk has a write-only standard-streams boundary, a byte-input boundary, and a confined filesystem
boundary. It has no way to learn what the process was started with. The native entry is
`int main(void)`, so even the runtime does not hold the command line. A self-hosted compiler needs
all three of argv, environment, and working directory before it can be driven from a shell.

## Goals / Non-Goals

Goals:

- One portable read-only process-input contract a pure in-source provider can implement without a
  host.
- A representation that can carry any value a POSIX process can actually receive.
- A native entry that holds the command line without changing Silk `main` or any exit status.

Non-Goals: argument parsing and flag grammar, environment or working-directory mutation, a second
text type, a Wasm entry change, an ambient default provider, and any change to the termination
contract.

## Decisions

### Values are raw bytes, and the textual view is checked

#43 decided this for `HostInput` and `ChildProcess` together. On POSIX, argv entries and environment
values are arbitrary NUL-terminated byte strings; the kernel enforces no encoding. A program that
receives a path it cannot decode must still be able to open it and hand it to a child unchanged, so
the primary view has to be bytes. `text(values: &[u8]) -> Result<string, InvalidUtf8>` layers the
common case on top and stays fallible; a failure leaves the bytes untouched and readable. No
`OsString` or `PlatformPath` type is introduced, and option 3 stays open: if a Windows port demands
distinct types they can be layered on later, with ill-formed UTF-16 mapping to bytes as WTF-8.

### Absence is data, and only a broken host is a failure

An index at or past `argumentCount` and an unset variable name are `None`. Asking whether a variable
is set is an ordinary question with an ordinary negative answer, so a program that probes the
environment handles no failure on that path — the same reasoning that makes `EndOfInput` outcome
data rather than a typed failure in `StandardInput`. `HostInputError` keeps one meaning: the host
could not answer at all.

The working directory has no absent case — a running process always has one — so it returns `Bytes`
directly and a host that cannot answer is a failure.

### The working directory is bytes, not a `Path`

`Path` is normalized UTF-8 that is always absolute _inside the selected provider's namespace_, never
consults a process working directory, and rejects lexical root escape. The process working directory
is none of those things: it is a host-absolute location outside any provider root, and it may not be
valid UTF-8. Returning it as a `Path` would either lie about the namespace or discard values a
program is entitled to read, so it is returned as raw bytes and a program that wants a `Path` builds
one explicitly through the existing checked constructors.

### The native provider uses the OS intrinsic pattern

Four unsafe operations mirror `Intrinsic.osFileRead`'s shape:

```silk
Intrinsic.osHostArgumentCount(count: &mut usize, reason: &mut i32, nativeCode: &mut u32) -> Effect<bool>
Intrinsic.osHostArgument(index: usize, output: &mut [u8], reason: &mut i32, nativeCode: &mut u32) -> Effect<Option<usize>>
Intrinsic.osHostVariable(name: &[u8], output: &mut [u8], reason: &mut i32, nativeCode: &mut u32) -> Effect<Option<usize>>
Intrinsic.osHostWorkingDirectory(output: &mut [u8], reason: &mut i32, nativeCode: &mut u32) -> Effect<Option<usize>>
```

`Some(n)` reports the value's _complete_ byte length and copies the prefix that fits, so one
undersized buffer costs exactly one extra pass with an exactly sized one rather than a separate
buffer-too-small protocol. `None` with the not-found reason is absence; `None` with any other reason
is a host error. Reusing the shape means the operations inherit the existing unsafe acknowledgement,
native-only target restriction, reachable-only runtime linking, and Wasm rejection without new
compiler machinery.

The environment is scanned by raw bytes over the process environment block rather than through
`getenv`, so a name or value that is not valid UTF-8 is matched and returned exactly as received.

### The entry holds the command line; Silk `main` does not take it

The runtime shim's `main` becomes `main(int argc, char **argv)` and stores both into
compiler-owned storage before calling `silk_main`. That is the whole change to the entry: the
adapter's tag branch, its report bytes, and its `0`/`1`/`2` statuses are untouched, and a program
that never reads host input pays two stores and no syscall. Silk `main` keeps its zero-parameter,
empty-requirement-row shape, so arguments reach a program through a service that can be replaced in
a test, rather than through a signature that could not be.

Direct WebAssembly is unaffected: it has no shim, keeps `--no-entry`, and rejects a reachable
host-input read through the existing native-only restriction rather than inventing a host import.

## Risks / Trade-offs

- A `Vector<Bytes>` of arguments is consumed by moving its elements out, since owned bytes cannot be
  borrowed out of a sequence element today. Callers that want one argument should ask for it by
  index instead of collecting.
- Reading a long value costs two host calls. The alternative — a required-capacity out-parameter —
  adds a protocol reason for a case that costs one extra memcpy.
- The environment scan is linear in the number of variables. A process environment is small, and a
  cached view is a caller's decision, not the boundary's.
