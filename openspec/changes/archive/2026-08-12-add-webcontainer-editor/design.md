## Context

See `proposal.md` for motivation. The docs application is a Next.js App Router application with an existing `/labs` client workbench and one route-local Atom registry. It does not currently depend on xterm.js or `@silk-lang/platform-webcontainer`, and its Next configuration does not set cross-origin isolation headers.

The WebContainer package already supplies three boundaries this change needs:

- `WebContainer.layer` lazily acquires and scopes the one permitted browser runtime.
- `WebContainerFileSystem.layer` derives Effect's standard `FileSystem` service from that runtime.
- `WebContainer.spawn` returns a scoped process with combined output, an input sink, exit, kill, and resize operations.

The package requires consumers to construct one WebContainer layer value and reuse that exact value throughout the layer graph. Process output is single-consumer. The repository Atom styleguide additionally requires module-scope atom identity, one runtime per bounded context, one registry at the application root, explicit browser-only SSR behavior, stream atoms for push data, `runtime.fn` for event-driven mutations, and exhaustive `AsyncResult` rendering.

## Goals / Non-Goals

**Goals:**

- Establish an editor application boundary that future filesystem consumers can reuse without changing composition.
- Keep WebContainer runtime, filesystem, process, and Atom lifecycles aligned under one scope.
- Adapt xterm's imperative DOM surface to Effect streams and mutations without placing application state in React hooks.
- Make strict development remounts, route unmounts, boot failures, and unsupported hosting configurations safe and visible.
- Preserve direct test substitution at the shared layer boundary.

**Non-Goals:**

- Add a source editor, file tree, preview iframe, package manager UI, persistence, collaboration, or multiple terminal tabs in this change.
- Introduce a second runtime for the future features above.
- Expose the raw WebContainer instance, raw promises, xterm objects, or manual process teardown as shared application state.
- Change the public API or behavioral contracts of `@silk-lang/platform-webcontainer` unless implementation proves its current actors insufficient.
- Support separate stdout and stderr, operating-system signals, PIDs, or other semantics WebContainer does not provide.

## Decisions

### 1. Compose one editor layer from one WebContainer layer value

Create an `EditorEnvironment` actor at module scope in the docs editor area. It constructs one `WebContainer.layer` value, provides that exact value to `WebContainerFileSystem.layer`, merges the runtime and standard filesystem services, and exports one Atom runtime built from the merged layer.

Conceptually:

```ts
const webContainerLayer = WebContainer.layer({ workdirName: 'silk-editor' })
const fileSystemLayer = WebContainerFileSystem.layer.pipe(Layer.provide(webContainerLayer))
const editorLayer = Layer.merge(webContainerLayer, fileSystemLayer)

export const runtime = Atom.runtime(editorLayer)
```

"Globally available" means available to every Effect program and atom inside the editor application's registry-scoped service graph. It does not mean an eagerly acquired module singleton, an implicit browser global, or a `ManagedRuntime` stored in React state. Future actors import `EditorEnvironment.runtime` and define their module-scope atoms from it; Effect programs inside those atoms resolve `FileSystem.FileSystem` normally.

The environment is placed above all `/editor` content with one `RegistryProvider`, preferably in the editor route layout so future child routes inherit it. There are no nested registries.

**Alternatives considered:**

- A runtime or filesystem per component was rejected because it creates competing WebContainer boots and divergent filesystems.
- A React context carrying a WebContainer or filesystem object was rejected because it bypasses Layer injection, typed acquisition, Atom runtime memoization, and test replacement.
- A module-level `ManagedRuntime` was rejected because acquisition would no longer follow the editor registry and route lifecycle.

### 2. Add a scoped `TerminalSession` adapter actor

`TerminalSession` owns the application-specific bridge between xterm events and the package's `WebContainerProcess`. Its scoped constructor:

1. Spawns one interactive `jsh` process from the shared environment.
2. Creates one scoped queue for terminal input.
3. Runs one continuous queue stream into the process input sink, holding a single writer for the session.
4. Exposes the process output stream, an input enqueue operation, and a resize operation while keeping the process representation private.

The continuous input pump is important: invoking the process sink independently for every xterm `onData` callback would repeatedly acquire the underlying writer and could race under fast input. Queue insertion preserves callback order, and scope interruption shuts down the input fiber and releases the writer.

The package's process scope remains authoritative for termination. `TerminalSession` does not duplicate kill bookkeeping or add manual cleanup branches.

**Alternatives considered:**

- Calling the input sink once per keystroke was rejected because writer acquisition is session-shaped, not keystroke-shaped.
- Letting the React component spawn and own the process was rejected because failure, interruption, service resolution, and cleanup would escape the Atom/Effect lifecycle.

### 3. Represent the session with module-scope atoms

Define all atoms and combinators once at module scope:

- A session atom acquires `TerminalSession` through `EditorEnvironment.runtime` and exposes typed `AsyncResult` state.
- A stream atom is the only consumer of combined process output and emits each chunk to subscribers.
- Input and resize are `runtime.fn` mutation atoms. Their session reads are intentionally invocation-time snapshots; they are not expected to react automatically.
- Browser-only effectful atoms use `Atom.withServerValueInitial` so server rendering yields the same initial state as the first client render without acquiring WebContainer.
- Diagnostic atoms receive stable labels.

The session and output atoms use the default lifetime rather than `Atom.keepAlive`. They remain mounted while the terminal surface consumes them and are collected after the editor unmounts. The route-level registry's deferred Strict Mode disposal and atom idle grace prevent a development remount from immediately forking an abandoned application environment.

Tests replace `EditorEnvironment.runtime.layer` with a deterministic layer through `Atom.initialValue` in a fresh registry. They do not branch atom definitions on environment variables or module-mock the atom module.

**Alternatives considered:**

- `keepAlive` was rejected because navigating away must release the shell and runtime.
- Atoms created inside the terminal component or a custom hook were rejected because render-time identity would fork state and subscriptions.
- Mirroring session state into `useState` was rejected because it would lose typed waiting/failure semantics and create two sources of truth.

### 4. Keep xterm as a DOM-owned adapter, not shared state

The terminal component creates `Terminal` and `FitAddon` only after its host element mounts, imports the xterm stylesheet, and disposes both through the component's DOM lifecycle. A ref holds the renderer because it is an imperative view handle, not application state.

The component uses narrow Atom hooks:

- `useAtomValue` for session `AsyncResult` rendering.
- `useAtomSubscribe` with a referentially stable callback to forward every output chunk to `terminal.write` without a React render per chunk.
- `useAtomSet` for input and resize operations.

`terminal.onData` enqueues input. A `ResizeObserver` asks `FitAddon` to fit the host and forwards only positive resulting columns and rows. All xterm subscriptions, the observer, and renderer are disposed together on unmount.

This use of a React effect is limited to attaching and detaching an imperative DOM resource. It does not fetch data, run WebContainer promises, mirror atom state, or own the process.

**Alternatives considered:**

- Storing the xterm instance in an atom was rejected because it is local to one DOM node, non-serializable, and has no shared or derived application meaning.
- Writing output through component state was rejected because it adds a render for every chunk and duplicates xterm's own terminal buffer.

### 5. Make boot and process states explicit in the UI

The editor renders the session `AsyncResult` exhaustively. Initial/waiting state produces an in-surface boot indicator; success reveals the active terminal; typed failure produces an actionable error panel. Refresh or waiting behavior does not erase an existing terminal buffer.

No component calls `Effect.runPromise` from xterm callbacks. Input and resize flow through mutation atoms; errors remain in their typed result state or are observed deliberately when an imperative outcome is required.

### 6. Isolate only the editor document

Configure Next.js response headers for `/editor` (and future editor child paths if the route layout introduces them):

- `Cross-Origin-Embedder-Policy: require-corp`
- `Cross-Origin-Opener-Policy: same-origin`

The WebContainer boot mode must match the response policy. Headers live in Next configuration so development and production use the same contract; Vercel configuration does not become a second source of truth. Scoping isolation to the editor avoids changing popup/opener and cross-origin resource behavior for unrelated docs pages.

### 7. Test behavior at the narrowest boundary

Most behavior is tested without React:

- Acquire the editor runtime with a counted test WebContainer service and verify runtime plus `FileSystem` consumers share one backing store and one acquisition.
- Test `TerminalSession` with a fake process: ordered input, ordered output, resize forwarding, and release on interruption.
- Test session/output/mutation atoms through fresh registries and layer injection, including initial, success, and typed failure states.
- Test the terminal DOM adapter with a fake terminal port where practical; reserve a browser smoke test for xterm layout and a live WebContainer only if the existing test environment can run it reliably.
- Validate route existence and isolation header configuration.

React tests do not create a `ManagedRuntime`, call `Effect.runPromise` per case, or share a registry across cases.

## Risks / Trade-offs

- [Cross-origin isolation can block third-party resources or alter opener behavior] → Apply headers only to the editor route, keep boot mode aligned, and surface configuration failures visibly.
- [WebContainer permits only one active runtime] → Export one editor layer and Atom runtime, forbid component-local layers, and test acquisition count.
- [Process output accepts one consumer] → Centralize consumption in one stream atom and fan out only after the registry receives each chunk.
- [Fast xterm input can contend for the process writer] → Hold one scoped input sink through a queue-backed stream rather than running the sink per callback.
- [Atom idle disposal could end a shell unexpectedly] → Keep the session atom mounted for the terminal surface's entire lifetime and test unmount/remount behavior; do not rely on unobserved writes.
- [A route-scoped environment does not survive navigation away from `/editor`] → Treat that as the intended lifecycle for this first slice; future persistence must serialize files or snapshots explicitly rather than leaking a live runtime.
- [xterm is an imperative third-party renderer] → Confine it to one adapter component and keep all runtime/process state behind Effect actors and atoms.

## Migration Plan

1. Add dependencies, the editor application environment, and deterministic test layers without routing traffic to them.
2. Add `/editor`, its provider boundary, isolation headers, and the terminal adapter.
3. Verify typecheck, formatting, unit tests, production build, response headers, and a supported-browser interactive shell smoke test.
4. Roll back by removing the additive route and its header rule; no existing docs route or persisted user data requires migration.
