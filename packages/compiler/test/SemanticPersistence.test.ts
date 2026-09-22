import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Frontend from '../src/Frontend.js'
import * as SemanticPersistence from '../src/SemanticPersistence.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Storage from '../src/Storage.js'

const source = new TextEncoder().encode(`
fn increment(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return increment(41) }
`)

const analyze = (persistence: SemanticPersistence.Persistence, bytes = source) =>
  Frontend.frontendProject({ roots: ['persistence/main'] }).pipe(
    Effect.provideService(SemanticPersistence.SemanticPersistence, persistence),
    Effect.provide(SourceResolver.memory(new Map([['persistence/main', bytes]]))),
  )

it.effect('restarts through Storage and admits complete units through the shared validator', () =>
  Effect.gen(function* () {
    const persistence = SemanticPersistence.make({
      storage: Storage.memoryService(),
      compilerIdentity: 'semantic-test-v1',
      maximumRecordBytes: 16 * 1024 * 1024,
    })
    const first = yield* analyze(persistence)
    const afterFirst = SemanticPersistence.counters(persistence)
    assert.isAbove(afterFirst.published, 0)
    assert.strictEqual(afterFirst.loaded, 0)

    const restarted = yield* analyze(persistence)
    const afterRestart = SemanticPersistence.counters(persistence)
    assert.isAbove(afterRestart.loaded, 0)
    assert.isAbove(restarted.semanticQueryCounters.validations, 0)
    assert.isAbove(restarted.semanticQueryCounters.reuses, 0)
    assert.deepEqual(restarted.diagnostics, first.diagnostics)
  }),
)

it.effect('treats corrupt optional records as rejection and safely recomputes', () =>
  Effect.gen(function* () {
    const memory = Storage.memoryService()
    let corrupt = false
    const storage: Storage.Service = {
      read: (address, maximumBytes) =>
        corrupt
          ? Effect.succeedSome(new TextEncoder().encode('{"broken":true}'))
          : memory.read(address, maximumBytes),
      publish: memory.publish,
    }
    const persistence = SemanticPersistence.make({
      storage,
      compilerIdentity: 'semantic-test-v1',
      maximumRecordBytes: 16 * 1024 * 1024,
    })
    yield* analyze(persistence)
    corrupt = true
    const restarted = yield* analyze(
      persistence,
      new TextEncoder().encode(`// moved\n${new TextDecoder().decode(source)}`),
    )
    assert.isAbove(SemanticPersistence.counters(persistence).rejected, 0)
    assert.isAbove(restarted.semanticQueryCounters.executions, 0)
  }),
)

it.effect('records external read and publication failure without changing semantics', () =>
  Effect.gen(function* () {
    const storage: Storage.Service = {
      read: (address) =>
        Effect.fail(
          new Storage.StorageError({
            operation: 'Storage.read',
            namespace: address.namespace,
            key: address.key,
            message: 'injected read failure',
            reason: { _tag: 'ReadFailure', cause: 'injected' },
          }),
        ),
      publish: (address) =>
        Effect.fail(
          new Storage.StorageError({
            operation: 'Storage.publish',
            namespace: address.namespace,
            key: address.key,
            message: 'injected publication failure',
            reason: { _tag: 'PublishFailure', cause: 'injected' },
          }),
        ),
    }
    const persistence = SemanticPersistence.make({
      storage,
      compilerIdentity: 'semantic-test-v1',
      maximumRecordBytes: 16 * 1024 * 1024,
    })
    const analysis = yield* analyze(persistence)
    const observed = SemanticPersistence.counters(persistence)
    assert.isAbove(observed.readFailures, 0)
    assert.isAbove(observed.publicationFailures, 0)
    assert.isAbove(analysis.semanticQueryCounters.executions, 0)
  }),
)
