import { assert, it } from '@effect/vitest'
import * as WebContainer from '@silk-lang/platform-webcontainer/WebContainer'
import * as WebContainerError from '@silk-lang/platform-webcontainer/WebContainerError'
import * as WebContainerProcess from '@silk-lang/platform-webcontainer/WebContainerProcess'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Sink from 'effect/Sink'
import * as Stream from 'effect/Stream'
import { Atom, AtomRegistry } from 'effect/unstable/reactivity'
import * as EditorEnvironment from '../app/editor/EditorEnvironment'
import * as TerminalSession from '../app/editor/TerminalSession'
import * as VirtualFileSystem from '../../../packages/platform-webcontainer/test/support/VirtualFileSystem.js'
import * as WebContainerService from '../../../packages/platform-webcontainer/test/support/WebContainerService.js'

it.effect('feeds ordered input, streams output, validates resize, and releases the process', () =>
  Effect.gen(function* () {
    const virtual = VirtualFileSystem.make()
    const received: Array<string> = []
    const resized: Array<WebContainerProcess.Dimensions> = []
    const allInputReceived = yield* Deferred.make<void>()
    let releases = 0
    let spawnTerminal: WebContainerProcess.Dimensions | undefined

    const process = WebContainerProcess.make({
      exit: Effect.succeed(0),
      output: Stream.make('first', 'second'),
      input: Sink.forEach((chunk: string) =>
        Effect.gen(function* () {
          received.push(chunk)
          if (received.length === 2) {
            yield* Deferred.succeed(allInputReceived, undefined)
          }
        }),
      ),
      kill: Effect.void,
      resize: (dimensions) =>
        Effect.sync(() => {
          resized.push(dimensions)
        }),
    })

    const service = WebContainerService.make({
      fileSystem: virtual.service.fileSystem,
      spawn: (_command, _args, options) => {
        spawnTerminal = options?.terminal
        return WebContainerProcess.scoped(
          Effect.succeed({
            process,
            running: Effect.succeed(true),
            release: Effect.sync(() => {
              releases += 1
            }),
          }),
        )
      },
    })

    const result = yield* Effect.scoped(
      Effect.gen(function* () {
        const session = yield* TerminalSession.make()
        const output = yield* TerminalSession.output(session).pipe(Stream.runCollect)

        yield* TerminalSession.write(session, 'echo ')
        yield* TerminalSession.write(session, 'hello\r')
        yield* Deferred.await(allInputReceived)
        yield* TerminalSession.resize(session, { cols: 80, rows: 24 })
        const resizeError = yield* TerminalSession.resize(session, { cols: 0, rows: 24 }).pipe(
          Effect.flip,
        )

        return { output: Array.from(output), resizeError, session }
      }).pipe(Effect.provide(WebContainer.layerTest(service))),
    )

    assert.deepStrictEqual(result.output, ['first', 'second'])
    assert.deepStrictEqual(received, ['echo ', 'hello\r'])
    assert.deepStrictEqual(resized, [{ cols: 80, rows: 24 }])
    assert.deepStrictEqual(spawnTerminal, { cols: 80, rows: 24 })
    assert.strictEqual(result.resizeError.reason._tag, 'InvalidInput')
    assert.strictEqual(releases, 1)

    const writeError = yield* TerminalSession.write(result.session, 'after-close').pipe(Effect.flip)
    assert.strictEqual(writeError.reason._tag, 'InvalidState')
  }),
)

it.effect('shares one output consumer and follows default atom lifetime in a fresh registry', () =>
  Effect.gen(function* () {
    const virtual = VirtualFileSystem.make()
    const received: Array<string> = []
    const inputReceived = yield* Deferred.make<void>()
    // The registry defers node removal to a scheduler task and then closes the session scope on a
    // forked fiber, so the release lands an unpredictable number of ticks after the last
    // unsubscribe. Await the release itself rather than a fixed number of ticks.
    const processReleased = yield* Deferred.make<void>()
    let outputAcquisitions = 0
    let spawns = 0
    let releases = 0

    const service = WebContainerService.make({
      fileSystem: virtual.service.fileSystem,
      spawn: () => {
        spawns += 1
        const process = WebContainerProcess.make({
          exit: Effect.succeed(0),
          output: Stream.unwrap(
            Effect.sync(() => {
              outputAcquisitions += 1
              return Stream.make('ready')
            }),
          ),
          input: Sink.forEach((chunk: string) =>
            Effect.gen(function* () {
              received.push(chunk)
              yield* Deferred.succeed(inputReceived, undefined)
            }),
          ),
          kill: Effect.void,
          resize: () => Effect.void,
        })
        return WebContainerProcess.scoped(
          Effect.succeed({
            process,
            running: Effect.succeed(true),
            release: Effect.gen(function* () {
              releases += 1
              yield* Deferred.succeed(processReleased, undefined)
            }),
          }),
        )
      },
    })

    const registry = AtomRegistry.make({
      initialValues: [
        Atom.initialValue(EditorEnvironment.runtime.layer, EditorEnvironment.testLayer(service)),
      ],
    })
    const releaseFirst = registry.subscribe(TerminalSession.outputAtom, () => {})
    const releaseSecond = registry.subscribe(TerminalSession.outputAtom, () => {})

    const output = yield* AtomRegistry.getResult(registry, TerminalSession.outputAtom)
    registry.set(TerminalSession.writeAtom, 'pwd\r')
    yield* AtomRegistry.getResult(registry, TerminalSession.writeAtom)
    yield* Deferred.await(inputReceived)

    assert.strictEqual(output, 'ready')
    assert.deepStrictEqual(received, ['pwd\r'])
    assert.strictEqual(outputAcquisitions, 1)
    assert.strictEqual(spawns, 1)

    releaseFirst()
    assert.strictEqual(releases, 0, 'the shared shell outlives a single unsubscribe')

    releaseSecond()
    yield* Deferred.await(processReleased)
    assert.strictEqual(releases, 1)

    registry.dispose()
  }),
)

it.effect('surfaces a typed shell acquisition failure through the session atom', () =>
  Effect.gen(function* () {
    const virtual = VirtualFileSystem.make()
    const failure = WebContainerError.invalidState(
      'WebContainer.spawn',
      'Unable to start the editor shell',
      'The test runtime rejected shell acquisition',
    )
    const service = WebContainerService.make({
      fileSystem: virtual.service.fileSystem,
      spawn: () => Effect.fail(failure),
    })
    const registry = AtomRegistry.make({
      initialValues: [
        Atom.initialValue(EditorEnvironment.runtime.layer, EditorEnvironment.testLayer(service)),
      ],
    })
    const release = registry.subscribe(TerminalSession.sessionAtom, () => {})

    const error = yield* AtomRegistry.getResult(registry, TerminalSession.sessionAtom).pipe(
      Effect.flip,
    )

    assert.strictEqual(error, failure)
    release()
    registry.dispose()
  }),
)
