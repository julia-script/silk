import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as Option from 'effect/Option'
import * as TestClock from 'effect/testing/TestClock'
import * as Document from '../src/Document.js'
import * as ProjectSession from '../src/ProjectSession.js'

const encoder = new TextEncoder()

const document = (uri: string, version: number, source = 'pub fn main() -> i32 { return 42 }') =>
  Document.make({
    uri,
    version,
    workspace: 'project:/workspace/silk.toml',
    module: uri.slice(uri.lastIndexOf('/') + 1, -'.silk'.length),
    sourceRoot: '/workspace',
    bytes: encoder.encode(source),
  })

const analyzed = Effect.fnUntraced(function* (self: Document.Document) {
  const snapshot = yield* Analysis.ofSource(self.module, self.bytes)
  return Object.freeze({
    document: self,
    snapshot,
    moduleUris: new Map([[self.module, self.uri]]),
  })
})

it.effect('coalesces a burst into the latest pending project revision', () =>
  Effect.gen(function* () {
    const analyzedDocuments: Array<string> = []
    const publishedVersions: Array<number> = []
    const project = ProjectSession.make({
      workspace: 'project:/workspace/silk.toml',
      sourceRoot: '/workspace',
      debounce: 10,
      analyze: Effect.fnUntraced(function* (self) {
        analyzedDocuments.push(`${self.module}@${self.version}`)
        return yield* analyzed(self)
      }),
      publish: Effect.fnUntraced(function* (session) {
        yield* Effect.sync(() => {
          publishedVersions.push(session.document.version)
        })
      }),
    })
    const first = yield* Effect.forkChild(
      project.open(document('file:///workspace/Main.silk', 1)),
      { startImmediately: true },
    )
    yield* Effect.yieldNow
    const second = yield* Effect.forkChild(
      project.open(document('file:///workspace/Util.silk', 1)),
      { startImmediately: true },
    )
    yield* Effect.yieldNow
    const latest = yield* Effect.forkChild(
      project.open(document('file:///workspace/Main.silk', 2)),
      { startImmediately: true },
    )
    yield* Effect.yieldNow
    yield* TestClock.adjust(10)
    yield* Fiber.join(first)
    yield* Fiber.join(second)
    yield* Fiber.join(latest)

    assert.deepEqual(analyzedDocuments, ['Main@2', 'Util@1'])
    assert.deepEqual(publishedVersions, [2, 1])
  }),
)

it.effect('runs one worker and prevents a stale revision from committing', () =>
  Effect.gen(function* () {
    const started = yield* Deferred.make<void>()
    const release = yield* Deferred.make<void>()
    const analyzedVersions: Array<number> = []
    const publishedVersions: Array<number> = []
    let active = 0
    let maximumActive = 0
    const project = ProjectSession.make({
      workspace: 'project:/workspace/silk.toml',
      sourceRoot: '/workspace',
      debounce: 10,
      analyze: Effect.fnUntraced(function* (self) {
        active += 1
        maximumActive = Math.max(maximumActive, active)
        analyzedVersions.push(self.version)
        if (self.version === 1) {
          yield* Deferred.succeed(started, undefined)
          yield* Deferred.await(release)
        }
        const result = yield* analyzed(self)
        active -= 1
        return result
      }),
      publish: Effect.fnUntraced(function* (session) {
        yield* Effect.sync(() => {
          publishedVersions.push(session.document.version)
        })
      }),
    })
    const first = yield* Effect.forkChild(
      project.open(document('file:///workspace/Main.silk', 1)),
      { startImmediately: true },
    )
    yield* TestClock.adjust(10)
    yield* Deferred.await(started)
    const second = yield* Effect.forkChild(
      project.open(document('file:///workspace/Main.silk', 2)),
      { startImmediately: true },
    )
    yield* Effect.yieldNow
    yield* Deferred.succeed(release, undefined)
    yield* TestClock.adjust(10)
    yield* Fiber.join(first)
    yield* Fiber.join(second)

    assert.strictEqual(maximumActive, 1)
    assert.deepEqual(analyzedVersions, [1, 2])
    assert.deepEqual(publishedVersions, [2])
  }),
)

it.effect('settles superseded and closed exact-version waiters without a session', () =>
  Effect.gen(function* () {
    const project = ProjectSession.make({
      workspace: 'project:/workspace/silk.toml',
      sourceRoot: '/workspace',
      debounce: 10,
      analyze: analyzed,
      publish: Effect.fnUntraced(function* () {
        yield* Effect.succeed(undefined)
      }),
    })
    const firstOpen = yield* Effect.forkChild(
      project.open(document('file:///workspace/Main.silk', 1)),
      { startImmediately: true },
    )
    yield* Effect.yieldNow
    const firstWaiter = yield* Effect.forkChild(project.acquire('file:///workspace/Main.silk', 1), {
      startImmediately: true,
    })
    const secondOpen = yield* Effect.forkChild(
      project.open(document('file:///workspace/Main.silk', 2)),
      { startImmediately: true },
    )
    yield* Effect.yieldNow
    assert.isTrue(Option.isNone(yield* Fiber.join(firstWaiter)))

    const secondWaiter = yield* Effect.forkChild(
      project.acquire('file:///workspace/Main.silk', 2),
      { startImmediately: true },
    )
    const close = yield* Effect.forkChild(project.close('file:///workspace/Main.silk'), {
      startImmediately: true,
    })
    yield* Effect.yieldNow
    assert.isTrue(Option.isNone(yield* Fiber.join(secondWaiter)))
    yield* TestClock.adjust(10)
    yield* Fiber.join(firstOpen)
    yield* Fiber.join(secondOpen)
    yield* Fiber.join(close)
  }),
)

it.effect('allows independent projects to analyze concurrently', () =>
  Effect.gen(function* () {
    const started = yield* Deferred.make<void>()
    const release = yield* Deferred.make<void>()
    let active = 0
    let maximumActive = 0
    const makeProject = (workspace: string) =>
      ProjectSession.make({
        workspace,
        sourceRoot: `/${workspace}`,
        debounce: 10,
        analyze: Effect.fnUntraced(function* (self) {
          active += 1
          maximumActive = Math.max(maximumActive, active)
          if (active === 2) yield* Deferred.succeed(started, undefined)
          yield* Deferred.await(release)
          const result = yield* analyzed(self)
          active -= 1
          return result
        }),
        publish: Effect.fnUntraced(function* () {
          yield* Effect.succeed(undefined)
        }),
      })
    const left = makeProject('left')
    const right = makeProject('right')
    const leftFiber = yield* Effect.forkChild(left.open(document('file:///left/Main.silk', 1)), {
      startImmediately: true,
    })
    const rightFiber = yield* Effect.forkChild(right.open(document('file:///right/Main.silk', 1)), {
      startImmediately: true,
    })
    yield* TestClock.adjust(10)
    yield* Deferred.await(started)
    yield* Deferred.succeed(release, undefined)
    yield* Fiber.join(leftFiber)
    yield* Fiber.join(rightFiber)
    assert.strictEqual(maximumActive, 2)
  }),
)
