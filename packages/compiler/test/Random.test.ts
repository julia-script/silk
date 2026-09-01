import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as BootstrapOsIntrinsics from '../src/BootstrapOsIntrinsics.js'
import * as InspectorFlowModel from '../src/InspectorFlowModel.js'
import * as InspectorProjectBackend from '../src/InspectorProjectBackend.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as RandomHost from '../src/RandomHost.js'
import * as Stdlib from '../src/Stdlib.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('random/main', encoder.encode(source), target)

const osRandomProgram = `import silk.effect as Effect
import silk.os_random as OsRandom
import silk.random as Random
import silk.u8 as u8
fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let mut provider = OsRandom.make()
  let mut output = [u8.toU8(90), u8.toU8(91), u8.toU8(92)]
  run Effect.provideMut(Random.fillBytes(&mut output), &mut provider)
  return identity(u8.toI32(output[0]) + u8.toI32(output[1]) + u8.toI32(output[2]))
}`

let realizedOsRandom: Analysis.Snapshot | undefined
const sharedOsRandom = Effect.fnUntraced(function* () {
  if (realizedOsRandom !== undefined) return realizedOsRandom
  const realized = yield* snapshot(osRandomProgram)
  realizedOsRandom = realized
  return realized
})

it('validates and advances immutable per-call scripted chunks', () => {
  const source = [[1, 2], [3]]
  const built = RandomHost.scripted(source)
  assert.strictEqual(built._tag, 'Constructed')
  if (built._tag !== 'Constructed') return
  source[0]?.splice(0, 2, 9, 9)
  assert.deepEqual(built.value.provider.fill(2), { _tag: 'Filled', bytes: [1, 2] })
  assert.strictEqual(built.value.remaining(), 1)
  assert.deepEqual(built.value.provider.fill(2), {
    _tag: 'BoundaryFailure',
    category: 'Underfill',
  })
  assert.deepEqual(built.value.provider.fill(0), {
    _tag: 'BoundaryFailure',
    category: 'Exhausted',
  })
  assert.strictEqual(RandomHost.scripted([[256]])._tag, 'ConstructionFailure')
  assert.strictEqual(RandomHost.scripted([[-1]])._tag, 'ConstructionFailure')
  assert.strictEqual(RandomHost.scripted([[0.5]])._tag, 'ConstructionFailure')
})

it('registers the secure native provider and one sealed random operation', () => {
  assert.strictEqual(Stdlib.findNamespace('OsRandom')?.module, 'silk/os_random')
  assert.deepEqual(Stdlib.find('silk/os_random')?.providerTargets, ['Evaluator', 'LLVM'])
})

it.effect('fills through only an explicitly injected evaluator host', () =>
  Effect.gen(function* () {
    const built = RandomHost.scripted([[40, 1, 1]])
    assert.strictEqual(built._tag, 'Constructed')
    if (built._tag !== 'Constructed') return
    const self = yield* sharedOsRandom()
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      Analysis.loweredMir(self)
        .functions.flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'OsCall')
        .map((operation) => operation.operation.name),
      ['osRandomFill'],
    )
    const artifact = yield* Analysis.codegen(self, { mode: 'release' })
    assert.deepEqual(artifact.nativeRuntimeSymbols, ['silk_os_random_fill_v1'])
    assert.include(artifact.ir, '@silk_os_random_fill_v1')
    const outcome = Analysis.evaluate(self, { randomHost: built.value.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.deepEqual(
      outcome.trace
        .filter((event) => event._tag === 'OsCall')
        .map((event) => [event.operation.name, event.outcome, event.byteLength]),
      [['osRandomFill', 'Completed', 3]],
    )
    const serializedTrace = JSON.stringify(outcome.trace, (_key, value) =>
      typeof value === 'bigint' ? value.toString() : value,
    )
    assert.notInclude(serializedTrace, '"bytes"')
    assert.notInclude(serializedTrace, '[40,1,1]')
  }),
)

it.effect('blocks without ambient randomness and presents the missing capability', () =>
  Effect.gen(function* () {
    const self = yield* sharedOsRandom()
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'MissingRandomHost')
    const flow = InspectorFlowModel.projectDataFlow(Analysis.rootAnalysis(self), outcome)
    assert.isTrue(
      flow.nodes.some(
        (node) => node.kind === 'Terminal' && node.detail.includes('MissingRandomHost'),
      ),
    )
    assert.isTrue(
      InspectorProjectBackend.evaluationRows(outcome).some(
        (row) => row.detail === 'missing RandomHost provider',
      ),
    )
  }),
)

it.effect('redacts returned and thrown host payloads to closed failure categories', () =>
  Effect.gen(function* () {
    const self = yield* sharedOsRandom()
    const canary = 'generated-byte-canary-40-1-1'
    const secretFailure = Object.freeze({
      _tag: 'BoundaryFailure' as const,
      category: 'ExplicitFailure' as const,
      payload: canary,
    })
    const invalidCategory: RandomHost.BoundaryFailure = {
      _tag: 'BoundaryFailure',
      category: 'ExplicitFailure',
    }
    Reflect.set(invalidCategory, 'category', canary)
    const invalidTag: RandomHost.Filled = { _tag: 'Filled', bytes: [40, 1, 1] }
    Reflect.set(invalidTag, '_tag', canary)
    const missingBytes: RandomHost.Filled = { _tag: 'Filled', bytes: [40, 1, 1] }
    Reflect.deleteProperty(missingBytes, 'bytes')
    const throwingTag: RandomHost.Filled = { _tag: 'Filled', bytes: [40, 1, 1] }
    Object.defineProperty(throwingTag, '_tag', {
      get: () => {
        throw new Error(canary)
      },
    })
    const throwingCategory: RandomHost.BoundaryFailure = {
      _tag: 'BoundaryFailure',
      category: 'ExplicitFailure',
    }
    Object.defineProperty(throwingCategory, 'category', {
      get: () => {
        throw new Error(canary)
      },
    })
    const throwingBytes: RandomHost.Filled = { _tag: 'Filled', bytes: [40, 1, 1] }
    Object.defineProperty(throwingBytes, 'bytes', {
      get: () => {
        throw new Error(canary)
      },
    })
    const sparse: Array<number> = []
    sparse.length = 3
    const throwingIterator = [40, 1, 1]
    Object.defineProperty(throwingIterator, Symbol.iterator, {
      get: () => {
        throw new Error(canary)
      },
    })
    const changingBytes = new Proxy([40, 1, 1], {
      get: (target, property, receiver) =>
        property === '1' ? canary : Reflect.get(target, property, receiver),
    })
    const exhausted = RandomHost.scripted([])
    assert.strictEqual(exhausted._tag, 'Constructed')
    if (exhausted._tag !== 'Constructed') return
    const providers: ReadonlyArray<RandomHost.Provider> = [
      RandomHost.failing(),
      exhausted.value.provider,
      { fill: () => ({ _tag: 'Filled', bytes: [40] }) },
      { fill: () => ({ _tag: 'Filled', bytes: [40, 1, 1, 9] }) },
      { fill: () => ({ _tag: 'Filled', bytes: [40, 1, 999] }) },
      { fill: () => secretFailure },
      { fill: () => invalidCategory },
      { fill: () => invalidTag },
      { fill: () => missingBytes },
      { fill: () => throwingTag },
      { fill: () => throwingCategory },
      { fill: () => throwingBytes },
      { fill: () => ({ _tag: 'Filled', bytes: sparse }) },
      { fill: () => ({ _tag: 'Filled', bytes: throwingIterator }) },
      { fill: () => ({ _tag: 'Filled', bytes: changingBytes }) },
      {
        fill: () => {
          throw new Error(canary)
        },
      },
    ]
    const categories: ReadonlyArray<RandomHost.FailureCategory> = [
      'ExplicitFailure',
      'Exhausted',
      'Underfill',
      'Overfill',
      'InvalidByte',
      'ExplicitFailure',
      'HostThrew',
      'HostThrew',
      'HostThrew',
      'HostThrew',
      'HostThrew',
      'HostThrew',
      'InvalidByte',
      'HostThrew',
      'InvalidByte',
      'HostThrew',
    ]
    for (const [index, provider] of providers.entries()) {
      const outcome = Analysis.evaluate(self, { randomHost: provider })
      assert.strictEqual(outcome._tag, 'Trap')
      const event = outcome.trace.find((candidate) => candidate._tag === 'OsCall')
      assert.strictEqual(event?.randomFailure, categories.at(index))
      const presented = InspectorProjectBackend.evaluationRows(outcome)
      const flow = InspectorFlowModel.projectDataFlow(Analysis.rootAnalysis(self), outcome)
      assert.notInclude(
        JSON.stringify({ outcome, presented, flow }, (_key, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
        canary,
      )
    }
  }),
)

it.effect('stages every evaluator failure before touching caller storage', () =>
  Effect.gen(function* () {
    const self = yield* sharedOsRandom()
    let selected:
      | {
          readonly fn: Mir.MirFunction
          readonly operation: Extract<Mir.Operation, { readonly _tag: 'OsCall' }>
        }
      | undefined
    for (const fn of Analysis.loweredMir(self).functions) {
      for (const operation of MirVerification.operations(fn)) {
        if (operation._tag === 'OsCall' && operation.operation.name === 'osRandomFill') {
          selected = { fn, operation }
        }
      }
    }
    assert.isDefined(selected)
    if (selected === undefined) return
    const exhausted = RandomHost.scripted([])
    assert.strictEqual(exhausted._tag, 'Constructed')
    if (exhausted._tag !== 'Constructed') return
    const sparseBytes: Array<number> = []
    sparseBytes.length = 3
    const providers: ReadonlyArray<RandomHost.Provider> = [
      RandomHost.failing(),
      exhausted.value.provider,
      { fill: () => ({ _tag: 'Filled', bytes: [1] }) },
      { fill: () => ({ _tag: 'Filled', bytes: [1, 2, 3, 4] }) },
      { fill: () => ({ _tag: 'Filled', bytes: [1, 2, 999] }) },
      {
        fill: () => {
          throw new Error('secret')
        },
      },
      { fill: () => ({ _tag: 'Filled', bytes: sparseBytes }) },
    ]
    for (const randomHost of providers) {
      const output = [90, 91, 92]
      const writes: Array<ReadonlyArray<number>> = []
      const context: BootstrapOsIntrinsics.ExecutionContext = {
        state: { processCaptures: [Object.freeze([]), Object.freeze([])], randomHost },
        fn: selected.fn,
        trace: [],
        read: () => {
          throw new Error('unexpected read')
        },
        write: () => undefined,
        cell: () => {
          throw new Error('unexpected cell')
        },
        readInteger: () => {
          throw new Error('unexpected integer')
        },
        replaceReferenced: () => undefined,
        byteView: () => Object.freeze(Array.from(output)),
        writeByteView: (_local, bytes) => {
          writes.push(bytes)
          for (const [index, byte] of bytes.entries()) output[index] = byte
        },
        handleValue: () => {
          throw new Error('unexpected handle')
        },
        hostHandle: () => {
          throw new Error('unexpected host handle')
        },
      }
      BootstrapOsIntrinsics.execute(context, selected.operation)
      assert.deepEqual(output, [90, 91, 92])
      assert.deepEqual(writes, [])
    }

    const emptyTrace: BootstrapOsIntrinsics.ExecutionContext['trace'] = []
    let emptyResultWrites = 0
    let emptyResultValue: bigint | undefined
    let emptyByteWrites = 0
    const emptyContext: BootstrapOsIntrinsics.ExecutionContext = {
      state: { processCaptures: [Object.freeze([]), Object.freeze([])] },
      fn: selected.fn,
      trace: emptyTrace,
      read: () => {
        throw new Error('unexpected read')
      },
      write: (_local, state) => {
        emptyResultWrites += 1
        if (state.value._tag === 'IntegerValue') emptyResultValue = state.value.value
      },
      cell: () => {
        throw new Error('unexpected cell')
      },
      readInteger: () => {
        throw new Error('unexpected integer')
      },
      replaceReferenced: () => undefined,
      byteView: () => Object.freeze([]),
      writeByteView: () => {
        emptyByteWrites += 1
      },
      handleValue: () => {
        throw new Error('unexpected handle')
      },
      hostHandle: () => {
        throw new Error('unexpected host handle')
      },
    }
    BootstrapOsIntrinsics.execute(emptyContext, selected.operation)
    assert.strictEqual(emptyResultWrites, 1)
    assert.strictEqual(emptyResultValue, 1n)
    assert.strictEqual(emptyByteWrites, 0)
    assert.deepEqual(
      emptyTrace
        .filter((event) => event._tag === 'OsCall')
        .map((event) => [event.outcome, event.byteLength]),
      [['Completed', 0]],
    )
  }),
)

it.effect('keeps direct-Wasm rejection reachable-only', () =>
  Effect.gen(function* () {
    const unused = yield* snapshot(
      `import silk.os_random as OsRandom
pub fn main() -> i32 {
  let provider = OsRandom.make()
  drop provider
  return 42
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(unused), [])
    assert.deepEqual(unused.instances.intrinsics, [])
    const wasm = yield* Analysis.codegenWasm(unused, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const reachable = yield* snapshot(osRandomProgram, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(reachable), [])
    const availability = IntrinsicAvailability.select(reachable.instances.intrinsics, 'Wasm')
    assert.strictEqual(availability._tag, 'Unavailable')
    if (availability._tag === 'Unavailable') {
      assert.deepEqual(availability.operations, ['Intrinsic.osRandomFill'])
      assert.deepEqual(
        availability.diagnostics.map((diagnostic) => diagnostic.code),
        ['SEM0093'],
      )
    }
    const emitted = yield* Effect.result(Analysis.codegenWasm(reachable, { mode: 'release' }))
    assert.strictEqual(emitted._tag, 'Failure')
    if (emitted._tag === 'Failure') assert.strictEqual(emitted.failure._tag, 'CodegenUnavailable')
  }),
)
