## Context

The repository is a pnpm/Turbo workspace on Effect v4 beta with actor-oriented public modules, explicit package subpath exports, typed external boundaries, scoped resources, and `@effect/vitest` tests. Existing source-resolution code already consumes Effect's standard `FileSystem` and `Path` services, so a WebContainer-backed platform layer can integrate without compiler-specific changes.

WebContainer is browser-only at execution time. Boot is asynchronous and expensive, only one instance may be active concurrently, and teardown invalidates its filesystem, processes, and subscriptions. Its filesystem exposes a Node-like but deliberately small promise API. Its process model exposes a combined text output stream, a text input stream, exit, parameterless kill, and terminal resize rather than POSIX process semantics. Its runtime events are callback subscriptions returning unsubscribe functions.

See `proposal.md` for motivation and the four delta specs for behavioral requirements.

## Goals / Non-Goals

**Goals:**

- Make every WebContainer call used by the package cross one typed Effect boundary.
- Make the runtime, processes, stream writers, event subscriptions, and scoped temporary entries resource-safe under all Effect exits.
- Provide the standard Effect `FileSystem` service for the subset that can be implemented honestly.
- Preserve WebContainer-native process and event semantics instead of synthesizing unavailable POSIX behavior.
- Keep package import and layer construction safe in SSR or non-browser module graphs; browser state is touched only during live-layer acquisition.
- Make live infrastructure replaceable with deterministic service values in unit tests.

**Non-Goals:**

- React hooks, UI state, editor models, compiler workflows, demo content, or `ManagedRuntime` ownership.
- A drop-in implementation of Effect's generic child-process spawner.
- Emulating filesystem permissions, ownership, links, native watching, or random-access file descriptors.
- Polling to pretend an unavailable native capability exists.
- Wrapping StackBlitz authentication, commercial API-key configuration, or page-global OAuth redirects in this first change.
- Supporting server-side execution; the package may be imported during SSR, but its live layer must be acquired in a supported browser environment.

## Decisions

### 1. Publish a platform package with concept-oriented public actors

Create `packages/platform-webcontainer` as `@silklang/platform-webcontainer` with these public modules:

```text
src/
├── WebContainer.ts
├── WebContainerError.ts
├── WebContainerEvent.ts
├── WebContainerFileSystem.ts
├── WebContainerProcess.ts
└── index.ts
```

`index.ts` re-exports actors as namespaces, while `package.json` exposes each actor as an explicit subpath. Consumers should normally use deep public imports such as `@silklang/platform-webcontainer/WebContainer`.

`WebContainer.ts` is the sole owner of runtime-value imports from `@webcontainer/api`. It defines the capability-bearing service, its live scoped layer, and the thin adapters around raw boot, mount, export, filesystem, process, preview, and subscription calls. The sibling actor modules define safe package values and transformations; they do not call the external API directly. Type-only re-exports needed for inputs such as filesystem trees are allowed, but raw instances are never service fields or public results.

Alternative considered: expose one large imperative wrapper class. Rejected because it would bolt Effect onto an imperative core, obscure requirements and failure channels, and encourage manual disposal.

Alternative considered: make the package only a filesystem adapter. Rejected because boot, process, and event lifecycles are the harder boundaries and would later force a second competing owner for the same WebContainer instance.

### 2. Model the runtime as one scoped Effect service

`WebContainer.WebContainer` is a `Context.Service` with a stable package identifier. Its service shape contains immutable `path` and `workdir` metadata and Effect-native capabilities for mount, export, preview configuration, filesystem primitives, process spawning, and event streams.

`WebContainer.layer(options)` is a scoped live layer. Acquisition wraps `WebContainer.boot(options)` with `Effect.tryPromise`; release invokes `teardown()` through an uninterruptible finalizer that cannot replace the original exit. Layer construction is pure and does not eagerly read `window`, `crossOriginIsolated`, or other browser globals.

Layer memoization is the concurrency boundary. Documentation and examples build the layer once and share that same value across `WebContainerFileSystem.layer`, application effects, and any runtime bridge. The package does not introduce a global singleton or hidden cache because those would outlive scopes and make teardown nondeterministic.

Alternative considered: cache a process-global promise. Rejected because failed acquisition, scope closure, hot reload, tests, and a later reboot would all become ambiguous.

### 3. Use one semantic WebContainer error family

`WebContainerError.WebContainerError` is a public tagged error carrying:

- `operation`: a stable actor operation name;
- `message`: contextual human-readable text;
- `reason`: a discriminated union of `InvalidInput`, `InvalidState`, and `WrappedFailure`;
- relevant semantic details such as a path or command where useful.

Only `WrappedFailure` carries JavaScript causal ancestry. Invalid terminal dimensions and package-detected lifecycle misuse use semantic reasons without causes. Rejected promises and throws at the external boundary are translated once and never exposed as `unknown` in a public Effect channel.

Filesystem adapter operations translate external failures again at the standard service boundary into Effect `PlatformError`. A narrow error classifier reads Node-like error evidence using safe property checks and maps known codes or messages to `NotFound`, `AlreadyExists`, `PermissionDenied`, `InvalidData`, `Busy`, or `Unknown`. It never casts an arbitrary rejection to a Node error. Unsupported operations use `Unknown` with an explicit unsupported description because Effect v4's `PlatformError` reason set has no `NotSupported` variant.

Alternative considered: expose raw WebContainer errors directly. Rejected because their shape is not a stable recovery contract and would couple consumers to an implementation dependency.

### 4. Expose safe runtime capabilities, not the raw instance

The public service exposes thin Effect capabilities rather than an escape hatch. Mount accepts the upstream filesystem-tree and snapshot input types. Export uses overloads or discriminated request/result types so JSON produces a tree and binary or ZIP formats produce bytes without a consumer cast. Preview script configuration mirrors supported source and attribute options.

The internal filesystem capability is already effectful and includes only the primitive WebContainer operations needed by `WebContainerFileSystem`. Process spawning immediately converts the raw process into `WebContainerProcess.Process`. Event registration immediately converts callbacks into scoped streams. Nothing outside `WebContainer.ts` needs the external runtime class.

Alternative considered: expose an `unsafeRaw` getter for completeness. Rejected because it would defeat typed failures and scoped ownership. A future capability missing from the wrapper should be added as an actor operation.

### 5. Implement `FileSystem` explicitly with a published compatibility matrix

`WebContainerFileSystem.layer` requires `WebContainer.WebContainer` and provides `FileSystem.FileSystem`. It constructs the service explicitly with `FileSystem.FileSystem.of` rather than `FileSystem.make`, because Effect's `make` derives streams and sinks from random-access `open`, which WebContainer cannot implement faithfully.

The implementation groups operations as follows:

| Classification | Operations                                                                                                                                     |
| -------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| Native         | `makeDirectory`, `readDirectory`, `readFile`, `readFileString`, `writeFile`, `writeFileString`, `rename`, `remove`, `access`, `exists`         |
| Derived        | `copy`, `copyFile`, temporary files/directories, scoped temporary entries, recursive directory reads, `realPath`, `truncate`, `stream`, `sink` |
| Approximated   | `stat`                                                                                                                                         |
| Unsupported    | `chmod`, `chown`, `glob`, `link`, `open`, `readLink`, `symlink`, `utimes`, `watch`                                                             |

Derived copy walks directories using typed directory entries and copies file bytes. Temporary names use Effect's random capability captured while constructing the layer and scoped variants remove their entries with brackets. `realPath` performs POSIX normalization relative to the runtime work directory and verifies existence; it cannot resolve symlinks because symlinks are unavailable. `truncate` reads, slices or zero-extends, and rewrites a file. Streams read the file once and emit bounded chunks honoring offsets and byte limits. Sinks collect input chunks in order and write once when the sink completes, so documentation calls out whole-file buffering and the absence of concurrent append semantics.

`stat` inspects the parent directory with file types and reads a file to calculate byte size. It returns accurate `File` or `Directory` type, file size, absent optional timestamps and identifiers, and stable documented neutral values for required but unavailable fields. The package does not fabricate host ownership or timestamps.

Unsupported operations fail immediately in the `PlatformError` channel. The README includes the same matrix plus details about buffering, synthetic stat fields, POSIX paths, and races inherent in deriving stat and copy from multiple calls.

Alternative considered: use `FileSystem.makeNoop` and override the available methods. Rejected because its defaults conflate unsupported operations with not-found results or defects, which would be dishonest and difficult to recover from.

Alternative considered: emulate open handles, links, watch, and glob. Rejected for this change because correct cursor concurrency, metadata, event delivery, and glob semantics would add substantial machinery without support from WebContainer.

### 6. Preserve the WebContainer-native process model

`WebContainerProcess.Process` is a data interface with Effect-valued capabilities and sibling operations. It contains:

- `exit`: an Effect yielding the integer exit code;
- `output`: a single-consumer `Stream<string, WebContainerError>` for combined terminal output;
- `input`: a `Sink<void, string, never, WebContainerError>`;
- `kill`: an Effect operation;
- `resize`: an Effect operation validating terminal dimensions first.

`WebContainer.spawn` is a named Effect operation returning `Process` in `Scope`. It uses `Effect.acquireRelease`: acquire awaits the raw spawn promise and builds the process value; release checks completion state and requests kill only while still running. Kill and resize wrap synchronous external calls with `Effect.try`. Exit wraps the raw exit promise with `Effect.tryPromise`. Nonzero exit codes stay in the success channel.

The output stream adapts the browser `ReadableStream<string>` and releases its reader lock on completion or interruption. It is explicitly documented as single-consumer because a Web stream cannot be read by multiple locked readers. The input sink acquires a writer, writes strings sequentially, and always releases its lock. Output disabled at spawn maps to an empty stream.

Alternative considered: implement Effect's `ChildProcessSpawner`. Rejected because WebContainer lacks PID, separate stdout and stderr, signals, additional file descriptors, and ref/unref. Supplying placeholders would violate that service's contract.

### 7. Convert callbacks to per-subscriber scoped streams

`WebContainerEvent` defines one data type per runtime event plus typed preview-message variants. A small internal stream constructor accepts an event-specific subscribe function, installs the listener on stream consumption, publishes events to a per-subscriber unbounded queue in callback order, and invokes the returned unsubscribe function in a finalizer.

Separate stream properties are exposed for port, server-ready, internal-error, and preview-message events. Internal runtime errors remain event values rather than failing the stream, because the upstream API reports them as observable runtime events and continuing events may still be relevant. Each stream consumption registers independently; one subscriber ending cannot affect another.

An unbounded queue is chosen because the upstream listener is synchronous and provides no backpressure protocol. Documentation warns consumers not to leave an unconsumed event stream running indefinitely. Runtime-scope closure interrupts or finalizes dependent stream consumers before teardown through normal scope ownership.

Alternative considered: one shared hub created at boot. Rejected because it registers every listener eagerly, complicates shutdown ordering, and introduces a hidden long-lived buffer even when no event is consumed.

### 8. Keep runtime bridges and hosting requirements at the application edge

The package exports layers and Effects only. A React application that needs callbacks may build one `ManagedRuntime` from the shared layer and dispose it when the page root unmounts, but the package does not import React or construct that bridge.

The README documents WebContainer hosting prerequisites: supported browsers, cross-origin isolation and COOP/COEP configuration, and known browser privacy settings. The live layer reports boot failures as typed errors rather than trying to mutate server headers or browser settings.

### 9. Test the boundary at two levels

Unit tests use `@effect/vitest`. A replaceable service value and fake primitive capabilities verify lifecycle finalizers, error translation, derived filesystem behavior, process stream and writer cleanup, and event unsubscribe behavior without mocking globals or booting WebContainer. Effect tests use `it.effect` or shared `it.layer` graphs.

A browser integration suite uses Vitest Browser Mode with the Playwright Chromium provider and a test server configured with the required isolation headers. It boots one live WebContainer per scoped suite, exercises mount/read/write/export, runs a small command, verifies output and exit, observes a runtime event where deterministic, and then verifies teardown by allowing the scope to close. The browser suite is a distinct script so local unit tests remain fast; CI and the package release checklist run it in a browser-capable job.

Public type tests pin export overloads, Effect error and requirement channels, and the absence of raw-promise APIs. Release-candidate validation checks package contents and every explicit subpath export.

## Risks / Trade-offs

- **[Only one WebContainer can be active concurrently]** → Build and share one layer value, document the rule prominently, and test that derived layers do not boot independently.
- **[Effect `FileSystem` is broader than WebContainer's filesystem]** → Fail unsupported operations explicitly and publish an exhaustive compatibility matrix.
- **[Derived filesystem operations are non-atomic and may buffer whole files]** → Document the limitation, keep implementations deterministic, and avoid claiming host-filesystem performance or concurrency guarantees.
- **[WebContainer error objects may change across versions]** → Depend only on guarded evidence, map unknown failures conservatively, and preserve causal ancestry.
- **[Browser promises cannot always be canceled underneath Effect interruption]** → Stop awaiting promptly, run scoped cleanup, and document that an underlying mount, export, or boot may finish after the waiting fiber is interrupted.
- **[Process output is a lockable, single-consumer Web stream]** → Document single consumption and release reader locks reliably; add an explicit broadcast combinator later only if a real use case requires it.
- **[Synchronous event callbacks have no backpressure]** → Use per-subscriber queues, register lazily, unsubscribe deterministically, and warn against abandoned consumers.
- **[Live integration tests require browser infrastructure and isolation headers]** → Keep them in a dedicated script and CI job while retaining comprehensive fake-boundary unit tests.
- **[SSR bundlers may evaluate package modules]** → Keep module evaluation free of boot and browser-global access; only live-layer acquisition requires the browser.

## Migration Plan

1. Add the package scaffold, explicit exports, TypeScript configuration, dependency declarations, and release-candidate metadata without changing existing packages.
2. Add the error actor and scoped runtime service with test replacement points.
3. Add filesystem, process, and event actors and their unit tests.
4. Add browser integration configuration, live smoke coverage, README usage, compatibility tables, and hosting prerequisites.
5. Run repository checks followed by release-candidate validation before publishing the new package.

No consumer migration is required because this is a new package and no existing API changes. Before publication, rollback consists of removing the unpublished package and workspace wiring. After publication but before stable adoption, breaking corrections are allowed by the repository's alpha-stage policy and should be released with a changeset.
