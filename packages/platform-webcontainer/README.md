# `@silklang/platform-webcontainer`

Effect-native access to the [WebContainer API](https://webcontainers.io/): one scoped browser
runtime, the standard Effect `FileSystem` service, WebContainer-native processes, and typed event
streams. The package is application-agnostic; it contains no React, editor, compiler, or demo
state.

## Install

```sh
pnpm add @silklang/platform-webcontainer effect
```

The package can be imported during SSR because imports and layer construction do not touch browser
globals. Acquiring `WebContainer.layer()` must happen in a supported browser.

## One shared runtime layer

WebContainer permits one active runtime. Construct the layer once and reuse that exact layer value
throughout the graph:

```ts
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as WebContainer from '@silklang/platform-webcontainer/WebContainer'
import * as WebContainerFileSystem from '@silklang/platform-webcontainer/WebContainerFileSystem'

const runtimeLayer = WebContainer.layer({ workdirName: 'playground' })
const fileSystemLayer = WebContainerFileSystem.layer.pipe(Layer.provide(runtimeLayer))
const applicationLayer = Layer.merge(runtimeLayer, fileSystemLayer)

const program = Effect.gen(function* () {
  yield* WebContainer.mount({
    'package.json': { file: { contents: '{"type":"module"}' } },
  })
  const fs = yield* FileSystem.FileSystem
  yield* fs.writeFileString('hello.ts', 'console.log("hello")')
})

Effect.runPromise(program.pipe(Effect.provide(applicationLayer)))
```

`WebContainer.layer` boots when acquired and tears down when its scope closes. Mount, export,
preview configuration, filesystem primitives, spawning, and subscription setup expose typed Effects
instead of raw promises or thrown values.

## Processes and events

Processes preserve WebContainer semantics: combined terminal output, terminal input, integer exit,
kill, and resize. They intentionally do not invent PIDs, POSIX signals, or separate stdout/stderr.

```ts
import * as Effect from 'effect/Effect'
import * as Stream from 'effect/Stream'
import * as WebContainer from '@silklang/platform-webcontainer/WebContainer'
import * as WebContainerProcess from '@silklang/platform-webcontainer/WebContainerProcess'

const run = Effect.gen(function* () {
  const process = yield* WebContainer.spawn('node', ['-e', 'console.log(42)'])
  const output = yield* Stream.runCollect(process.output)
  const exitCode = yield* WebContainerProcess.awaitExit(process)
  return { output: output.join(''), exitCode }
})
```

Process output is single-consumer because the underlying Web stream has one reader lock. The input
sink writes in order and releases its writer lock after success, failure, or interruption. A running
process is killed when its scope closes; a process that has already exited is left alone.

The runtime exposes separate lazy streams for `port`, `server-ready`, internal error, and preview
messages. Every consumer gets an independent callback subscription and an unbounded queue preserving
callback order. Do not abandon a running event stream without interrupting its fiber: the upstream
API has no backpressure protocol, so an unconsumed subscription can grow in memory.

## FileSystem compatibility

`WebContainerFileSystem.layer` provides Effect's standard `FileSystem.FileSystem` service.

| Classification | Operations                                                                                                                                                                    |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Native         | `access`, `exists`, `makeDirectory`, `readDirectory`, `readFile`, `readFileString`, `remove`, `rename`, `watch`, `writeFile`, `writeFileString`                               |
| Derived        | `copy`, `copyFile`, `makeTempDirectory`, `makeTempDirectoryScoped`, `makeTempFile`, `makeTempFileScoped`, recursive `readDirectory`, `realPath`, `sink`, `stream`, `truncate` |
| Approximated   | `stat`                                                                                                                                                                        |
| Unsupported    | `chmod`, `chown`, `glob`, `link`, `open`, `readLink`, `symlink`, `utimes`                                                                                                     |

Unsupported operations fail immediately with a typed `PlatformError` whose description identifies
the unsupported capability. They do not pretend the path is missing.

Derived operations have these deliberate limits:

- `stat`, `access`, and `exists` answer from directory listings alone and never read file
  contents, so tree walks scale with entry count. File/directory type is accurate; file byte
  sizes are a stable `0` approximation (use `readFile(...).byteLength` for an accurate size).
  Mode and device are stable zero values; timestamps, inode, ownership, link count, block size,
  and block count are absent.
- `watch` adapts WebContainer's native watcher. `change` notifications become `Update` events;
  Node-style `rename` notifications are classified as `Create` or `Remove` through a parent
  directory listing, so a path recreated between the notification and the probe can be reported
  as `Create` for what was momentarily a removal. Consumers reconcile on the next event.
- `stream` reads the whole file once, then emits the selected offset/limit range in bounded chunks.
- `sink` buffers input chunks in order and performs one whole-file write on completion.
- Copy, stat, temporary-name allocation, append, and exclusive-write behavior are derived from
  multiple WebContainer calls and are not atomic under concurrent mutation.
- `realPath` performs lexical POSIX normalization relative to the WebContainer working directory
  and validates existence; it cannot resolve symlinks.

## Browser and hosting requirements

Serve production pages over HTTPS and configure the page hosting WebContainer with:

```text
Cross-Origin-Embedder-Policy: require-corp
Cross-Origin-Opener-Policy: same-origin
```

Chromium is the primary supported target. Browser privacy controls—especially third-party-cookie or
aggressive tracking protection—can prevent boot. The package reports those failures through
`WebContainerError`; it does not mutate response headers or browser settings.

## Application runtimes

This package owns WebContainer resources only through Effect scopes. If a UI framework needs to run
Effects from callbacks, create one `ManagedRuntime` from the shared application layer at the
application edge and dispose it when the page root unmounts. Do not create a runtime per component
or per callback. The package deliberately does not import React or own a `ManagedRuntime`.

## Public actors

- `WebContainer`: scoped service, mount/export/preview/spawn operations, metadata, and event streams.
- `WebContainerError`: semantic typed failures and safe external-failure translation.
- `WebContainerEvent`: typed runtime event values and the lazy callback-to-stream adapter.
- `WebContainerFileSystem`: standard Effect filesystem layer and error normalization.
- `WebContainerProcess`: combined-output process value, stream/sink adapters, and process controls.

Each actor is available as an explicit package subpath and as a namespace from the root entrypoint.
