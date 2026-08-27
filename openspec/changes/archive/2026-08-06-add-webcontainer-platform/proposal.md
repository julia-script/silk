## Why

Browser applications need an Effect-native way to own and use the WebContainer runtime without leaking throwing calls, bare promises, callback subscriptions, or manual resource cleanup into application code. A reusable platform package will let any Effect program run an in-browser Node.js environment while remaining independent of a particular language, compiler, editor, or demonstration.

## What Changes

- Add a publishable `@silklang/platform-webcontainer` package that owns the `@webcontainer/api` boundary.
- Add a scoped WebContainer runtime service that boots one container, tears it down with its enclosing scope, and exposes mount, export, runtime metadata, and preview configuration as typed Effects.
- Add `WebContainerFileSystem`, an implementation layer for Effect's `FileSystem.FileSystem` service with documented native, derived, partial, and unsupported capabilities.
- Add a WebContainer-native process actor for spawning commands, consuming combined output, writing terminal input, awaiting exit, killing processes, and resizing terminals with scoped cleanup.
- Add scoped Effect streams for WebContainer port, server-ready, internal-error, and preview-message events, including automatic listener unsubscription.
- Add a semantic `WebContainerError` family for runtime operations and translate filesystem failures into Effect `PlatformError` values at the `FileSystem` boundary.
- Add public subpath exports, package documentation, capability tables, unit tests with a replaceable test boundary, browser integration tests, and release-candidate coverage.
- Keep framework bridges and product-specific behavior out of the package; consumers own React integration, `ManagedRuntime` construction, compiler orchestration, editor state, and page configuration.

## Capabilities

### New Capabilities

- `webcontainer-runtime`: Scoped WebContainer acquisition, teardown, mount/export operations, runtime metadata, preview configuration, and typed boundary failures.
- `webcontainer-filesystem`: Effect `FileSystem` provisioning over the WebContainer virtual filesystem with explicit compatibility semantics.
- `webcontainer-processes`: Scoped command execution and interaction through a WebContainer-native process model.
- `webcontainer-events`: Resource-safe Effect streams for WebContainer runtime and preview events.

### Modified Capabilities

None.

## Impact

- Adds `packages/platform-webcontainer` and the public package `@silklang/platform-webcontainer`.
- Adds `@webcontainer/api` as the package's external runtime dependency and `effect` as its Effect dependency.
- Adds browser-specific build, test, and documentation requirements to the pnpm/Turbo workspace and release-candidate validation.
- Introduces a reusable browser platform layer that existing consumers of `FileSystem.FileSystem`, including source resolution code, can use without a WebContainer-specific dependency.
- Does not change existing compiler, CLI, LLVM, or WebAssembly APIs.
