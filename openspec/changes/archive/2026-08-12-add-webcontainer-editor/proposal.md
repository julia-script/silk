## Why

Silk's documentation site needs a browser-native execution surface where users can work with a real terminal today and grow into an editor, file tree, and preview workflow without replacing its runtime foundation later. The existing Effect-native WebContainer package already supplies the required scoped runtime, process, and standard `FileSystem` boundaries, but the docs app does not compose or expose them.

## What Changes

- Add a dedicated `/editor` route to the docs application.
- Add an interactive xterm.js terminal backed by a scoped `jsh` process from `@silklang/platform-webcontainer`.
- Establish one editor application environment that constructs a single WebContainer layer and derives both the WebContainer runtime service and Effect's standard `FileSystem` service from that exact layer value.
- Make the shared application environment available through one module-scope Atom runtime so the terminal and future editor, file-tree, package-management, and preview actors resolve the same filesystem and WebContainer instance.
- Model boot, session, process output, input, resize, failure, and disposal through Effect scopes and Atom state rather than raw WebContainer promises or component-owned application state.
- Configure the `/editor` document for the cross-origin isolation headers required by WebContainer.
- Add focused tests for application-layer sharing, terminal session lifecycle, Atom behavior, and route configuration.

## Capabilities

### New Capabilities

- `docs-editor-environment`: Defines the docs editor's single scoped WebContainer application environment and globally shared Effect `FileSystem` service.
- `docs-editor-terminal`: Defines the `/editor` route and its interactive, resource-safe xterm.js terminal session.

### Modified Capabilities

None. The existing WebContainer runtime, filesystem, and process contracts already provide the package-level behavior this application consumes.

## Impact

- Affects `apps/docs` routing, client UI, styles, Next.js response headers, tests, and package dependencies.
- Adds workspace consumption of `@silklang/platform-webcontainer` plus `@xterm/xterm` and `@xterm/addon-fit`.
- Introduces an editor-scoped Atom runtime and application composition layer that future editor features must reuse instead of booting independent WebContainers or constructing parallel filesystem services.
- Does not change the public API of `@silklang/platform-webcontainer` unless implementation reveals a missing capability that cannot be expressed through its current runtime, process, and filesystem actors.
