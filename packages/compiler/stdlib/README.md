# Silk standard library sources

The files under `silk/` are the canonical compiler-shipped Silk modules. The generated TypeScript
manifest is only a build artifact; editors and language-server definition results point back to
these source files.

## `string` and `String`

Lowercase `string` is the built-in immutable view of a complete, valid UTF-8 byte sequence. Text
literals have this type. A runtime `string` borrows its backing storage; it does not allocate or own
the bytes. `string` has no indexing operation and no generic `.length` property.

Uppercase `String` is the ordinary `silk.string.String` struct. It owns private `Bytes` storage and
uses the active `Allocator` for operations that allocate. The compiler does not recognize `String`
by spelling or give it privileged storage behavior.

The `silk.string` module exposes these operations:

| Operation | Result and cost |
| --- | --- |
| `fromUtf8(values: &[u8])` | Validates the complete view without allocation and returns `Result<string, InvalidUtf8>`. The error contains the first invalid byte offset. |
| `utf8Bytes(value: string)` | Borrows the immutable UTF-8 encoding as `&[u8]` without allocation. |
| `byteLength(value: string)` | Returns the UTF-8 byte length without allocation. |
| `make()` | Constructs an empty owned `String` without allocation. |
| `copy(value: string)` | Allocates and copies into an owned `String`; fails with `OutOfMemory`. |
| `append(self: &mut String, value: string)` | Allocates replacement storage and appends complete text; fails with `OutOfMemory` without changing `self`. |
| `view(self: &String)` | Borrows the complete owned contents as `string` without allocation. |
| `ownedUtf8Bytes(self: &String)` | Borrows the owned value's immutable bytes without allocation. |
| `ownedByteLength(self: &String)` | Returns the owned value's initialized byte length without allocation. |
| `scalarCursor()` and `nextScalar(value, cursor)` | Traverse Unicode scalar values without allocation. Each step carries its starting byte offset and the next opaque cursor. |

Conversions between `string`, `String`, and `&[u8]` are explicit. Passing text to a byte-oriented
API requires `utf8Bytes`; retaining text requires `copy`; borrowing owned text requires `view`.
There is no implicit allocation, validation, ownership transfer, or byte-view conversion.

`fromUtf8` is the safe byte-to-text boundary. `Intrinsic.stringFromUtf8Unchecked` is sealed compiler
surface and is callable only inside `unsafe`; its caller must establish that the entire borrowed
view is valid UTF-8 and remains alive for the returned view's lifetime. Invalid input is outside
the intrinsic's safe contract and does not become a recoverable validation error.

The `==` and `!=` operators on `string` compare the exact UTF-8 bytes. They do not normalize Unicode,
fold case, segment graphemes, or compare locale-sensitive text. Scalar traversal is explicit, and
neither scalar nor grapheme indexing is available.

`Logger` is the portable semantic logging boundary. Each `Effect.log` or `Effect.logAt` invocation
supplies one complete borrowed `string` message; callers cannot append fragments to an open event.
Providers own formatting, allocation, destinations, buffering, and physical writes. The initial
`StdoutLogger` explicitly borrows UTF-8 bytes at the `StandardStreams` boundary, while
`InMemoryLogger` copies message bytes only
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
directory, rejects NUL and lexical root escape, accepts textual construction and resolution inputs,
and exposes borrowed `string` views plus an allocated owned parent. Root's name is empty text because
the conservative returned-borrow subset cannot
wrap a borrow in `Option`; `Path.isRoot` distinguishes it unambiguously.

`Path.make`, `Path.join`, and `Path.resolve` accept `string`; `Path.view` and `Path.name` return
borrowed `string`. The explicitly named `Path.joinUtf8` is reserved for provider boundaries such
as native directory enumeration: it validates arbitrary bytes before admitting them as portable
path text.

`FileSystem` has seven mutable service operations: complete reads and writes, minimal metadata,
deterministically ordered immediate listings, one-directory creation, one-file removal, and empty-
directory removal. Writes receive one complete borrowed value but providers may use any internal
chunking; physical atomic replacement and rollback are not promised. `FileError` is allocation-free
and carries a closed operation/reason pair plus an optional numeric provider detail. `exists`,
`createDirectoriesRecursively`, and `writeFileWithParents` are ordinary source composition.

The portable module contains no provider or platform ABI. Native applications may later choose the
separate confined `OsFileSystem`; browser and direct-Wasm programs can provide ordinary virtual
implementations, and programs that never use FileSystem emit no filesystem imports.
