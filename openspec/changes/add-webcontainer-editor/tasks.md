## 1. Editor Application Setup

- [x] 1.1 Add `@silk-effect/platform-webcontainer`, `@xterm/xterm`, and `@xterm/addon-fit` to the docs app and update the lockfile.
- [x] 1.2 Add route-scoped COOP/COEP response headers for `/editor` and editor child paths in the Next.js configuration.
- [x] 1.3 Scaffold the `/editor` route and one route-level `RegistryProvider` boundary that future editor child content will inherit.

## 2. Shared Editor Environment

- [x] 2.1 Create the `EditorEnvironment` actor with one module-scope WebContainer layer value, derive `FileSystem.FileSystem` from that exact value, and export one Atom runtime over the merged application layer.
- [x] 2.2 Add deterministic test-layer support through Atom runtime layer injection without environment branches, module mocks, nested registries, or component-owned runtimes.
- [x] 2.3 Test that runtime and standard `FileSystem` consumers acquire one environment and observe the same backing filesystem, including a process-written file read through `FileSystem`.
- [x] 2.4 Test that importing and server-rendering the editor environment does not access browser globals or boot WebContainer.

## 3. Terminal Session and Atoms

- [x] 3.1 Create the `TerminalSession` actor that scopes one `jsh` process and continuously feeds a queue-backed input stream into its process input sink.
- [x] 3.2 Expose session output, ordered input enqueueing, and validated resize operations while leaving process termination to the package's scoped process lifecycle.
- [x] 3.3 Define module-scope session, output-stream, input-mutation, and resize-mutation atoms from `EditorEnvironment.runtime`, with stable labels and browser-safe server initial values.
- [x] 3.4 Test ordered input and output, single output consumption, resize forwarding, typed acquisition failure, interruption cleanup, and default atom lifetime through fresh registries.

## 4. Xterm Editor Surface

- [x] 4.1 Build the DOM-owned xterm adapter with `Terminal`, `FitAddon`, the xterm stylesheet, and one scoped component lifecycle for renderer, event, and resize-observer disposal.
- [x] 4.2 Connect xterm output through a stable `useAtomSubscribe` callback and connect input and resize through write-only mutation hooks without component-level Effect execution or mirrored terminal state.
- [x] 4.3 Render session `AsyncResult` exhaustively so boot progress, the active terminal, and actionable typed failures remain visible without blanking an existing terminal buffer.
- [x] 4.4 Style `/editor` as a usable full-page terminal surface whose container produces positive fitted rows and columns and can later accommodate editor and file-tree panes.
- [x] 4.5 Add focused adapter and route tests for renderer disposal, remount safety, error presentation, route existence, and isolation-header configuration.

## 5. Verification

- [x] 5.1 Run `pnpm typecheck`, fix all failures introduced by the change, and record any unrelated pre-existing failures.
- [x] 5.2 Run `pnpm exec biome check .` and `pnpm test`, fixing all change-related failures.
- [x] 5.3 Run the docs production build and verify that `/editor` is emitted without server-side WebContainer acquisition.
- [x] 5.4 In a supported browser, verify the `/editor` response is cross-origin isolated and smoke-test command input, ordered output, fitting, resizing, file creation, and cleanup after navigating away.
- [x] 5.5 Run `pnpm check` before handoff and report the exact status of every required check.
