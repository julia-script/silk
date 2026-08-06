## 1. Package Foundation

- [x] 1.1 Create `packages/platform-webcontainer` with package metadata, license, TypeScript build and test configuration, source and test directories, and workspace scripts matching the repository conventions.
- [x] 1.2 Add `@webcontainer/api`, `effect`, `@effect/vitest`, and browser-test dependencies at compatible versions and update the pnpm lockfile.
- [x] 1.3 Add the explicit root and actor subpath exports for `WebContainer`, `WebContainerError`, `WebContainerEvent`, `WebContainerFileSystem`, and `WebContainerProcess`.
- [x] 1.4 Add the namespace-exporting public `src/index.ts` and public type tests that verify every subpath resolves without exposing raw promises or an unknown error channel.

## 2. Error and Boundary Models

- [x] 2.1 Implement the `WebContainerError` actor with `InvalidInput`, `InvalidState`, and `WrappedFailure` reasons, operation names, contextual details, and causal ancestry only for wrapped failures.
- [x] 2.2 Add safe constructors and guards for translating thrown and rejected external failures without casts, non-null assertions, or an exposed `unknown` error channel.
- [x] 2.3 Add unit tests for error construction, messages, reason discrimination, and cause preservation.

## 3. Scoped Runtime Service

- [x] 3.1 Define the `WebContainer` service contract, boot option model, immutable runtime metadata, effectful filesystem primitives, process spawning, and typed event stream capabilities.
- [x] 3.2 Implement the live scoped layer so acquisition wraps WebContainer boot, release tears down exactly once after every exit, and constructing the layer performs no browser-side work.
- [x] 3.3 Implement typed mount operations for filesystem trees and binary snapshots with optional mount points.
- [x] 3.4 Implement format-preserving export operations for JSON trees, binary snapshots, and ZIP bytes.
- [x] 3.5 Implement preview-script configuration and expose runtime path and working-directory metadata.
- [x] 3.6 Add fake-boundary tests for successful acquisition, boot failure, lazy layer construction, shared service use, and teardown after success, typed failure, defect, and interruption.
- [x] 3.7 Add runtime operation tests for option forwarding, metadata, mount inputs and failures, export result typing and failures, and preview configuration.

## 4. Runtime Event Streams

- [x] 4.1 Implement typed `WebContainerEvent` models for port, server-ready, internal-error, and every supported preview-message variant.
- [x] 4.2 Implement the lazy per-subscriber callback-to-stream adapter with ordered delivery and exactly-once unsubscribe finalization.
- [x] 4.3 Wire the four independent event streams into the live runtime service without eagerly registering listeners.
- [x] 4.4 Add tests for event field preservation, ordering, independent subscribers, interruption cleanup, normal completion cleanup, and runtime-scope shutdown.

## 5. WebContainer Process Actor

- [x] 5.1 Implement the `WebContainerProcess` data actor with exit, combined output, terminal input, kill, and resize capabilities and no unsupported POSIX fields.
- [x] 5.2 Adapt the Web `ReadableStream<string>` into a single-consumer Effect stream that releases its reader lock on completion, failure, and interruption and becomes empty when output is disabled.
- [x] 5.3 Adapt the Web `WritableStream<string>` into an ordered Effect sink that releases its writer lock on completion, failure, and interruption.
- [x] 5.4 Implement scoped spawning with command options, typed spawn failures, integer exit results, dimension validation, synchronous kill and resize boundaries, and kill-on-release for active processes.
- [x] 5.5 Add process tests for option forwarding, combined output ordering, disabled output, ordered input, reader and writer cleanup, zero and nonzero exits, invalid resize rejection, and process release after interruption.

## 6. Native Filesystem Adapter

- [x] 6.1 Implement guarded filesystem error classification and translation to Effect `PlatformError`, including not-found, already-exists, permission, invalid-data, busy, and unknown reasons.
- [x] 6.2 Implement `WebContainerFileSystem.layer` to provide the standard `FileSystem` service from the shared runtime and POSIX path services.
- [x] 6.3 Implement native directory creation, directory reading, byte and string reads, byte and string writes, rename, removal, access, and existence operations with supported options.
- [x] 6.4 Implement explicit unsupported failures for permissions, ownership, globbing, links, open handles, timestamps, and watch streams.
- [x] 6.5 Implement approximated stat results with accurate entry type and file size plus stable neutral or absent values for unavailable metadata.
- [x] 6.6 Add tests for every native operation, recognized and unknown error translation, stat approximations, and every unsupported operation's typed failure.

## 7. Derived Filesystem Operations

- [x] 7.1 Implement recursive directory listing and recursive file or directory copy with overwrite behavior and byte preservation.
- [x] 7.2 Implement temporary file and directory creation with collision retry and scoped removal after success, failure, defect, and interruption.
- [x] 7.3 Implement lexical POSIX real-path resolution relative to the WebContainer working directory with existence validation.
- [x] 7.4 Implement truncate with shrinking, zero-extension, and default zero-length behavior.
- [x] 7.5 Implement whole-file buffered streams honoring offset, byte-limit, and chunk-size options.
- [x] 7.6 Implement whole-file buffered sinks that preserve input chunk order and supported write options.
- [x] 7.7 Add tests for recursive traversal and copy, temporary-name collision and cleanup, real paths, truncation, stream ranges and chunking, sink ordering, and buffering-related error propagation.

## 8. Browser Integration and Documentation

- [x] 8.1 Configure a dedicated Vitest Browser Mode project with Playwright Chromium and the COOP/COEP headers required to boot WebContainer.
- [x] 8.2 Add a scoped live-browser smoke suite covering boot, mount, standard filesystem reads and writes, export, command output and exit, one deterministic event path, and teardown.
- [x] 8.3 Add CI wiring for the browser-capable test job while keeping ordinary package unit tests independent of a live WebContainer boot.
- [x] 8.4 Write the package README with installation, one-shared-layer usage, runtime and process examples, application-edge `ManagedRuntime` guidance, hosting prerequisites, browser limitations, and SSR-safe import expectations.
- [x] 8.5 Publish the exhaustive filesystem compatibility matrix and document synthetic stat fields, whole-file buffering, non-atomic derived operations, single-consumer process output, and event queue behavior.
- [x] 8.6 Add public API documentation and examples for all actor subpaths, then add a changeset for the new publishable package.
- [x] 8.7 Extend release-candidate coverage to validate the package contents, generated declarations, root entrypoint, and every explicit subpath export.

## 9. Verification

- [x] 9.1 Run `pnpm typecheck` and fix all package, test, and public type errors.
- [x] 9.2 Run `pnpm exec biome check .` and fix all formatting, import, and lint failures.
- [x] 9.3 Run `pnpm test`, including the package unit tests, and fix all failures.
- [x] 9.4 Run the dedicated live browser integration test command in a browser-capable environment and record any host prerequisite failures separately from code failures.
- [x] 9.5 Run `pnpm check` for the complete repository validation gate.
- [x] 9.6 Run `pnpm release:candidate` and verify the new package's published contents and exports.
