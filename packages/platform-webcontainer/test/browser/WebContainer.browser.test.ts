import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as Stream from 'effect/Stream'
import * as WebContainer from '../../src/WebContainer.js'
import * as WebContainerFileSystem from '../../src/WebContainerFileSystem.js'
import * as WebContainerProcess from '../../src/WebContainerProcess.js'

it.effect('boots one runtime and exercises its live capabilities', () => {
  const runtimeLayer = WebContainer.layer({
    coep: 'require-corp',
    workdirName: 'silk-effect-smoke',
  })
  const fileSystemLayer = WebContainerFileSystem.layer.pipe(Layer.provide(runtimeLayer))
  const applicationLayer = Layer.merge(runtimeLayer, fileSystemLayer)

  return Effect.gen(function* () {
    const runtime = yield* WebContainer.WebContainer
    const fs = yield* FileSystem.FileSystem
    yield* WebContainer.mount({
      'package.json': {
        file: { contents: '{"name":"webcontainer-smoke","type":"module"}' },
      },
      project: { directory: {} },
      'mounted.txt': { file: { contents: 'mounted' } },
    })
    assert.strictEqual(yield* fs.readFileString('mounted.txt'), 'mounted')
    yield* fs.writeFileString('written.txt', 'written through Effect FileSystem')
    assert.strictEqual(yield* fs.readFileString('written.txt'), 'written through Effect FileSystem')
    const exported = yield* WebContainer.exportJson('.')
    assert.property(exported, 'written.txt')

    const watched = yield* fs.watch('project').pipe(
      Stream.filter((watchEvent) => watchEvent.path.endsWith('from-process.txt')),
      Stream.take(1),
      Stream.runHead,
      Effect.forkScoped,
    )
    const writer = yield* WebContainer.spawn('node', [
      '-e',
      "require('node:fs').writeFileSync('project/from-process.txt', 'watched')",
    ])
    assert.strictEqual(yield* WebContainerProcess.awaitExit(writer), 0)
    const watchEvent = yield* Fiber.join(watched)
    assert.isTrue(Option.isSome(watchEvent))
    if (Option.isSome(watchEvent)) {
      assert.oneOf(watchEvent.value._tag, ['Create', 'Update'])
    }

    const command = yield* WebContainer.spawn(
      'node',
      ['-e', "process.stdout.write(process.env.SMOKE + ':' + process.cwd())"],
      {
        cwd: 'project',
        env: { SMOKE: 'webcontainer output' },
        terminal: { cols: 80, rows: 24 },
      },
    )
    const output = yield* command.output.pipe(Stream.runCollect)
    assert.match(output.join(''), /^webcontainer output:.*\/project$/)
    assert.strictEqual(yield* WebContainerProcess.awaitExit(command), 0)
    yield* WebContainer.setPreviewScript('/preview-smoke.js', { type: 'module', defer: true })

    const openedPort = yield* runtime.portEvents.pipe(
      Stream.filter((event) => event.status === 'open' && event.port === 3456),
      Stream.take(1),
      Stream.runHead,
      Effect.forkScoped,
    )
    yield* WebContainer.spawn('node', [
      '-e',
      "require('node:http').createServer((_request, response) => response.end('ok')).listen(3456)",
    ])
    const event = yield* Fiber.join(openedPort)
    assert.isTrue(Option.isSome(event))
    if (Option.isSome(event)) {
      assert.strictEqual(event.value.port, 3456)
      assert.strictEqual(event.value.status, 'open')
    }
  }).pipe(Effect.provide(applicationLayer))
})
