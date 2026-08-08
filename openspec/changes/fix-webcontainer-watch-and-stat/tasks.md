# Tasks: WebContainer watch support and cheap stat

## 1. Primitive watch boundary

- [ ] 1.1 Add `watch(path, options?)` to the `FileSystem` interface in `src/WebContainer.ts`,
      returning a stream of raw `{ event: 'rename' | 'change', filename: string }` notifications
      with `WebContainerError` failures
- [ ] 1.2 Implement it in `makeFileSystem` over `raw.fs.watch` using the
      `Stream.callback` + `acquireRelease` pattern (normalize `Uint8Array` filenames, close the
      watcher on stream end/interrupt)
- [ ] 1.3 Unit-test registration, emission order, and watcher close via the test double

## 2. Standard service watch

- [ ] 2.1 Replace the failing `watch` in `src/WebContainerFileSystem.ts` with the mapped stream:
      `'change'` → `Update`; `'rename'` → parent-listing probe → `Create`/`Remove`
- [ ] 2.2 Fail `NotFound` before registration when the watched path does not exist
- [ ] 2.3 Unit-test mapping (change, create, remove), missing-path failure, and lifecycle

## 3. Stat without content reads

- [ ] 3.1 Make `stat` report `Size(0)` for files and drop its `readFile`; confirm
      `exists`/`access` no longer touch contents
- [ ] 3.2 Update affected unit tests (stat size expectations, VirtualFileSystem read counters
      proving no content reads on stat/exists)

## 4. Test double and browser coverage

- [ ] 4.1 Extend `test/support/VirtualFileSystem.ts` with a watcher registry and manual emit
      helper
- [ ] 4.2 Add one browser test: watch a directory, spawn a process that writes a file, assert a
      Create/Update event arrives; assert watcher closes on interruption

## 5. Docs and release

- [ ] 5.1 Update the README compatibility table (watch native, stat approximation now
      listing-only with zero sizes) and the rename-race note
- [ ] 5.2 Update the package changeset to mention watch support and the stat behavior change
- [ ] 5.3 Run package typecheck, unit tests, and browser tests; run `openspec validate
      fix-webcontainer-watch-and-stat --strict`
