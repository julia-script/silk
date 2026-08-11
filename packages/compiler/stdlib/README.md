# Silk standard library sources

The files under `silk/` are the canonical compiler-shipped Silk modules. The generated TypeScript
manifest is only a build artifact; editors and language-server definition results point back to
these source files.

`Logger` is the portable semantic logging boundary. Each `Effect.log` or `Effect.logAt` invocation
supplies one complete borrowed UTF-8 message; callers cannot append fragments to an open event.
Providers own formatting, allocation, destinations, buffering, and physical writes. The initial
`StdoutLogger` can forward message bytes directly, while `InMemoryLogger` copies messages only
because it retains ordered observations. The bootstrap in-memory implementation is deliberately
bounded to eight events and 64 total message bytes; capacity exhaustion is a deterministic
`LogError`, not an ambient allocator requirement.

`StandardStreams` is the lower-level process-output boundary. It writes immutable byte views to
stdout or stderr and reports `StreamWriteFailure`; it does not invent log levels or Logger
requirements. It is also not the future `Stream`/`Sink` model, which will own composition, flow,
buffering, and backpressure.

`StandardStreams` and `Logger` remain explicit Effect requirements until a
general default-provider mechanism exists; neither logging nor process output receives an ambient
exception.

`Path` and `FileSystem` are the portable whole-file boundary. A `Path` is owned, normalized UTF-8
inside the selected provider namespace: it is always absolute, never consults a process working
directory, rejects NUL and lexical root escape, and exposes lexical borrowed views plus an allocated
owned parent. Root's name is an empty view because the conservative returned-borrow subset cannot
wrap a borrow in `Option`; `Path.isRoot` distinguishes it unambiguously.

`FileSystem` has seven mutable service operations: complete reads and writes, minimal metadata,
deterministically ordered immediate listings, one-directory creation, one-file removal, and empty-
directory removal. Writes receive one complete borrowed value but providers may use any internal
chunking; physical atomic replacement and rollback are not promised. `FileError` is allocation-free
and carries a closed operation/reason pair plus an optional numeric provider detail. `exists`,
`createDirectoriesRecursively`, and `writeFileWithParents` are ordinary source composition.

The portable module contains no provider or platform ABI. Native applications may later choose the
separate confined `OsFileSystem`; browser and direct-Wasm programs can provide ordinary virtual
implementations, and programs that never use FileSystem emit no filesystem imports.
