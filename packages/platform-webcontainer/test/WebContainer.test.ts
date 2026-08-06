import { assert, it } from '@effect/vitest'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as WebContainer from '../src/WebContainer.js'
import * as WebContainerError from '../src/WebContainerError.js'
import * as WebContainerService from './support/WebContainerService.js'

it.effect('constructs a live layer without acquiring it', () =>
  Effect.sync(() => {
    let boots = 0
    const boot: WebContainer.Boot = () => {
      boots += 1
      return Effect.succeed({
        service: WebContainerService.make(),
        teardown: Effect.void,
      })
    }
    WebContainer.layerWith(boot)
    assert.strictEqual(boots, 0)
  }),
)

it.effect('shares one acquired service and tears it down after success', () =>
  Effect.gen(function* () {
    let boots = 0
    let teardowns = 0
    const service = WebContainerService.make()
    const boot: WebContainer.Boot = () =>
      Effect.sync(() => {
        boots += 1
        return {
          service,
          teardown: Effect.sync(() => {
            teardowns += 1
          }),
        }
      })
    const observed = yield* Effect.gen(function* () {
      const first = yield* WebContainer.WebContainer
      const second = yield* WebContainer.WebContainer
      return first === second
    }).pipe(Effect.provide(WebContainer.layerWith(boot)))
    assert.isTrue(observed)
    assert.strictEqual(boots, 1)
    assert.strictEqual(teardowns, 1)
  }),
)

it.effect('preserves boot options and reports boot failures', () =>
  Effect.gen(function* () {
    let observed: WebContainer.BootOptions | undefined
    const options: WebContainer.BootOptions = {
      coep: 'credentialless',
      workdirName: 'demo',
      forwardPreviewErrors: 'exceptions-only',
    }
    const boot: WebContainer.Boot = (received) => {
      observed = received
      return Effect.fail(
        WebContainerError.wrappedFailure(
          'WebContainer.boot',
          'boot failed',
          new Error('no browser'),
        ),
      )
    }
    const result = yield* Effect.result(
      WebContainer.WebContainer.pipe(Effect.provide(WebContainer.layerWith(boot, options))),
    )
    assert.deepStrictEqual(observed, options)
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag === 'Failure') {
      assert.strictEqual(result.failure.operation, 'WebContainer.boot')
    }
  }),
)

it.effect('tears down after typed failure and defect', () =>
  Effect.gen(function* () {
    let teardowns = 0
    const layer = WebContainer.layerWith(() =>
      Effect.succeed({
        service: WebContainerService.make(),
        teardown: Effect.sync(() => {
          teardowns += 1
        }),
      }),
    )
    yield* Effect.gen(function* () {
      yield* WebContainer.WebContainer
      return yield* Effect.fail('expected')
    }).pipe(Effect.provide(layer), Effect.exit)
    yield* Effect.gen(function* () {
      yield* WebContainer.WebContainer
      return yield* Effect.die(new Error('expected defect'))
    }).pipe(Effect.provide(layer), Effect.exit)
    assert.strictEqual(teardowns, 2)
  }),
)

it.effect('tears down after interruption', () =>
  Effect.gen(function* () {
    let teardowns = 0
    const acquired = yield* Deferred.make<void>()
    const layer = WebContainer.layerWith(() =>
      Effect.gen(function* () {
        yield* Deferred.succeed(acquired, undefined)
        return {
          service: WebContainerService.make(),
          teardown: Effect.sync(() => {
            teardowns += 1
          }),
        }
      }),
    )
    const fiber = yield* Effect.gen(function* () {
      yield* WebContainer.WebContainer
      return yield* Effect.never
    }).pipe(Effect.provide(layer), Effect.forkChild)
    yield* Deferred.await(acquired)
    yield* Fiber.interrupt(fiber)
    assert.strictEqual(teardowns, 1)
  }),
)

it.effect('forwards typed runtime operations and exposes immutable metadata', () => {
  const tree: WebContainer.FileSystemTree = {
    'demo.ts': { file: { contents: 'export {}' } },
  }
  const calls: Array<string> = []
  const service = WebContainerService.make({
    path: '/bin',
    workdir: '/workspace',
    mount: (input, options) =>
      Effect.sync(() => {
        assert.strictEqual(input, tree)
        assert.strictEqual(options?.mountPoint, 'src')
        calls.push('mount')
      }),
    exportJson: (path) =>
      Effect.sync(() => {
        calls.push(`json:${path}`)
        return tree
      }),
    exportBinary: (path) =>
      Effect.sync(() => {
        calls.push(`binary:${path}`)
        return new Uint8Array([1, 2])
      }),
    exportZip: (path) =>
      Effect.sync(() => {
        calls.push(`zip:${path}`)
        return new Uint8Array([3, 4])
      }),
    setPreviewScript: (source, options) =>
      Effect.sync(() => {
        assert.strictEqual(source, '/preview.js')
        assert.strictEqual(options?.type, 'module')
        calls.push('preview')
      }),
  })
  return Effect.gen(function* () {
    yield* WebContainer.mount(tree, { mountPoint: 'src' })
    const json = yield* WebContainer.exportJson('.')
    const binary = yield* WebContainer.exportBinary('.')
    const zip = yield* WebContainer.exportZip('.')
    yield* WebContainer.setPreviewScript('/preview.js', { type: 'module' })
    const runtime = yield* WebContainer.WebContainer
    assert.strictEqual(runtime.path, '/bin')
    assert.strictEqual(runtime.workdir, '/workspace')
    assert.strictEqual(json, tree)
    assert.deepStrictEqual([...binary], [1, 2])
    assert.deepStrictEqual([...zip], [3, 4])
    assert.deepStrictEqual(calls, ['mount', 'json:.', 'binary:.', 'zip:.', 'preview'])
  }).pipe(Effect.provide(WebContainer.layerTest(service)))
})
