import { assert, it } from '@effect/vitest'
import * as WebContainer from '@silk-lang/platform-webcontainer/WebContainer'
import * as WebContainerProcess from '@silk-lang/platform-webcontainer/WebContainerProcess'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Sink from 'effect/Sink'
import * as Stream from 'effect/Stream'
import * as EditorEnvironment from '../app/editor/EditorEnvironment'
import * as VirtualFileSystem from '../../../packages/platform-webcontainer/test/support/VirtualFileSystem.js'

it.effect('shares one WebContainer between process and standard FileSystem consumers', () =>
  Effect.gen(function* () {
    const virtual = VirtualFileSystem.make()
    let acquisitions = 0
    let releases = 0

    const service = WebContainer.make({
      ...virtual.service,
      spawn: Effect.fnUntraced(function* () {
        yield* virtual.service.fileSystem.writeFileString(
          'created-by-terminal.txt',
          'shared filesystem',
        )
        return WebContainerProcess.make({
          exit: Effect.succeed(0),
          output: Stream.empty,
          input: Sink.drain,
          kill: Effect.void,
          resize: () => Effect.void,
        })
      }),
    })

    const countedWebContainerLayer = Layer.effect(
      WebContainer.WebContainer,
      Effect.acquireRelease(
        Effect.sync(() => {
          acquisitions += 1
          return service
        }),
        () =>
          Effect.sync(() => {
            releases += 1
          }),
      ),
    )

    const contents = yield* Effect.gen(function* () {
      yield* WebContainer.spawn('test-shell')
      const fs = yield* FileSystem.FileSystem
      return yield* fs.readFileString('created-by-terminal.txt')
    }).pipe(Effect.provide(EditorEnvironment.makeLayer(countedWebContainerLayer)))

    assert.strictEqual(contents, 'shared filesystem')
    assert.strictEqual(acquisitions, 1)
    assert.strictEqual(releases, 1)
  }),
)

it('constructs the live environment without browser acquisition', () => {
  assert.isDefined(EditorEnvironment.webContainerLayer)
  assert.isDefined(EditorEnvironment.layer)
  assert.isDefined(EditorEnvironment.runtime)
})
