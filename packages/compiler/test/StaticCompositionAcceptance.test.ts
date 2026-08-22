import { NodeServices } from '@effect/platform-node'
import { assert, layer } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Analysis from '../src/Analysis.js'
import type * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Lexer from '../src/Lexer.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as StandardStreams from '../src/StandardStreams.js'
import * as SyntaxFormatter from '../src/SyntaxFormatter.js'
import * as Target from '../src/Target.js'
import { unreachable } from './support/raise.js'

const decoder = new TextDecoder()
const encoder = new TextEncoder()
const wasmHeapTableBase = 65_536

const fixture = Effect.fnUntraced(function* (name: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const file = yield* path.fromFileUrl(
    new URL(`./fixtures/static-composition/${name}.silk`, import.meta.url),
  )
  return decoder.decode(yield* fileSystem.readFile(file))
})

const parse = (id: string, source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

const canonical = Effect.fnUntraced(function* (id: string, source: string) {
  const formatted = yield* SyntaxFormatter.format(parse(id, source))
  return decoder.decode(FormattedDocument.toUint8Array(formatted))
})

const runWasm = Effect.fnUntraced(function* (bytes: Uint8Array) {
  return yield* Effect.try(() => {
    const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') return unreachable('expected Wasm silk_main')
    const result = main()
    if (typeof result !== 'number') return unreachable('expected a numeric Wasm result')
    const memory = instance.exports[StandardStreams.wasmMemoryExport]
    const class0 =
      memory instanceof WebAssembly.Memory
        ? new DataView(memory.buffer).getInt32(wasmHeapTableBase, true)
        : 0
    return Object.freeze({ result, instance, class0 })
  })
})

interface Scenario {
  readonly name: string
  readonly selection: string
  readonly result: number
  readonly selectedTarget: string
  readonly handlerCalls: number
  readonly dropCalls: number
  readonly failureEvents: number
  readonly suspensionEvents: number
}

const scenarios: ReadonlyArray<Scenario> = Object.freeze([
  {
    name: 'success',
    selection: 'RunRequest { value: 40 }',
    result: 42,
    selectedTarget: 'runCase',
    handlerCalls: 1,
    dropCalls: 0,
    failureEvents: 0,
    suspensionEvents: 0,
  },
  {
    name: 'help',
    selection: 'HelpRequest {}',
    result: 10,
    selectedTarget: 'helpCase',
    handlerCalls: 0,
    dropCalls: 0,
    failureEvents: 0,
    suspensionEvents: 0,
  },
  {
    name: 'selection-failure',
    selection: 'SelectionFailureRequest {}',
    result: 20,
    selectedTarget: 'selectionFailure',
    handlerCalls: 0,
    dropCalls: 0,
    failureEvents: 1,
    suspensionEvents: 0,
  },
  {
    name: 'decode-failure',
    selection: 'DecodeFailureRequest {}',
    result: 30,
    selectedTarget: 'decodeFailure',
    handlerCalls: 0,
    dropCalls: 0,
    failureEvents: 1,
    suspensionEvents: 0,
  },
  {
    name: 'uncalled-cleanup',
    selection: 'UncalledCleanupRequest { value: 40 }',
    result: 40,
    selectedTarget: 'uncalledCleanupCase',
    handlerCalls: 0,
    dropCalls: 1,
    failureEvents: 0,
    suspensionEvents: 0,
  },
  {
    name: 'called-cleanup',
    selection: 'CalledCleanupRequest { value: 40 }',
    result: 40,
    selectedTarget: 'guarded',
    handlerCalls: 0,
    dropCalls: 1,
    failureEvents: 0,
    suspensionEvents: 0,
  },
  {
    name: 'suspension',
    selection: 'SuspensionRequest { value: 42 }',
    result: 42,
    selectedTarget: 'delayed',
    handlerCalls: 0,
    dropCalls: 1,
    failureEvents: 0,
    suspensionEvents: 1,
  },
])

const scenarioSource = (source: string, scenario: Scenario): string =>
  source.replace('RunRequest {value: 40}', scenario.selection)

const callCount = (outcome: BootstrapEvaluation.Outcome, target: string): number =>
  outcome._tag === 'Completed'
    ? outcome.trace.filter((event) => event._tag === 'Call' && event.target.name === target).length
    : 0

const dropCallCount = (outcome: BootstrapEvaluation.Outcome): number =>
  outcome._tag === 'Completed'
    ? outcome.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      ).length
    : 0

const selectedCallCount = (outcome: BootstrapEvaluation.Outcome, target: string): number =>
  outcome._tag === 'Completed'
    ? outcome.trace.filter(
        (event) =>
          event._tag === 'Call' &&
          (event.target.name === target || event.target.name.startsWith(`${target}$effect$`)),
      ).length
    : 0

const traceCount = (
  outcome: BootstrapEvaluation.Outcome,
  tag: BootstrapEvaluation.TraceEvent['_tag'],
): number =>
  outcome._tag === 'Completed' ? outcome.trace.filter((event) => event._tag === tag).length : 0

const assertDirectTarget = (artifact: string, target: string, label: string): void => {
  assert.match(
    artifact,
    new RegExp(`\\bcall\\b[^\\n]*${target.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}`),
    `${label} direct target ${target}`,
  )
}

const analyze = Effect.fnUntraced(function* (id: string, source: string, target: string) {
  const snapshot = yield* Analysis.ofSourceRealized(id, encoder.encode(source), target)
  const diagnostics = Analysis.diagnostics(snapshot)
  assert.deepEqual(
    diagnostics,
    [],
    JSON.stringify(diagnostics.map(({ code, message }) => ({ code, message }))),
  )
  assert.strictEqual(snapshot.mir._tag, 'Available', id)
  return snapshot
})

layer(NodeServices.layer)('static composition acceptance', (it) => {
  it.effect('keeps canonical and renamed pressure fixtures formatter-idempotent', () =>
    Effect.gen(function* () {
      const fingerprints: Array<ReadonlyArray<string | number>> = []
      for (const name of [
        'static-composition-acceptance',
        'renamed-static-composition-acceptance',
      ]) {
        const source = yield* fixture(name)
        const first = yield* canonical(`static-composition/${name}/first`, source)
        const second = yield* canonical(`static-composition/${name}/second`, first)
        assert.strictEqual(source, first, `${name} checked-in canonical source`)
        assert.strictEqual(first, second, name)
        const snapshot = yield* analyze(
          `static-composition/${name}`,
          source,
          Target.wasm32UnknownUnknown.id,
        )
        const outcome = Analysis.evaluate(snapshot)
        assert.strictEqual(outcome._tag, 'Completed', name)
        if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n, name)
        assert.strictEqual(snapshot.mir._tag, 'Available', name)
        if (snapshot.mir._tag !== 'Available') continue
        const operations = snapshot.mir.value.functions.flatMap(MirVerification.operations)
        assert.strictEqual(
          operations.filter((operation) => operation._tag === 'Allocate').length,
          1,
          `${name} explicit cleanup allocations`,
        )
        const encoded = MirEncoding.encode(snapshot.mir.value)
        assert.notInclude(encoded, 'RuntimeDictionary', name)
        assert.notInclude(encoded, 'call_indirect', name)
        fingerprints.push(
          Object.freeze([
            snapshot.instances.instances.length,
            snapshot.instances.callables.length,
            snapshot.instances.effects.length,
            snapshot.layout._tag === 'Available' ? snapshot.layout.value.entries.length : -1,
            snapshot.mir.value.functions.length,
            ...operations.map((operation) => operation._tag),
          ]),
        )
      }
      assert.deepEqual(fingerprints.at(0), fingerprints.at(1), 'renamed control fingerprint')
    }),
  )

  it.effect('reports the first representation divergence and exposes complete tooling facts', () =>
    Effect.gen(function* () {
      const source = yield* fixture('static-composition-acceptance')
      const divergent = `${source}
struct FirstBranch {}
struct SecondBranch {}
fn divergent(input: FirstBranch | SecondBranch) -> i32 {
  let represented = match move input {
    FirstBranch {} => MappedSchema<NumberSchema> {
      source: NumberSchema { value: 1 },
      transform: add(1)
    }
    SecondBranch {} => MappedSchema<NumberSchema> {
      source: NumberSchema { value: 2 },
      transform: add(2)
    }
  }
  return 0
}
`
      const divergentSnapshot = yield* Analysis.ofSource(
        'static-composition/divergent',
        encoder.encode(divergent),
      )
      const diagnostic = Analysis.diagnostics(divergentSnapshot).find(
        (candidate) => candidate.code === 'SEM0105',
      )
      assert.strictEqual(diagnostic?.reason._tag, 'DivergentRepresentationJoin')
      assert.deepEqual(
        diagnostic?.relatedSpans?.map((related) => related.label),
        ['first representation originates here', 'divergent representation originates here'],
      )

      const module = 'static-composition/tooling'
      const snapshot = yield* Analysis.ofSource(module, encoder.encode(source))
      const hover = Analysis.hoverSubjectAt(snapshot, module, source.indexOf('leftSchema ='))
      assert.include(hover?.presentation.text ?? '', 'MappedSchema<NumberSchema, typeof(')
      assert.include(hover?.presentation.text ?? '', '.add@declaration:')
      const operationOffset =
        source.indexOf('return CompleteDecoder.decode') + 'return CompleteDecoder.'.length
      const navigation = Analysis.semanticOccurrenceAt(snapshot, module, operationOffset)
      assert.strictEqual(navigation?.role, 'Operation')
      assert.strictEqual(navigation?.resolution._tag, 'Available')
      assert.strictEqual(navigation?.declaration?.module, module)
    }),
  )

  it.effect(
    'agrees across evaluator and direct Wasm for every pressure exit',
    () =>
      Effect.gen(function* () {
        const source = yield* fixture('static-composition-acceptance')
        for (const scenario of scenarios) {
          const selected = scenarioSource(source, scenario)
          const module = `static-composition/${scenario.name}`
          const wasmSnapshot = yield* analyze(module, selected, Target.wasm32UnknownUnknown.id)
          const outcome = Analysis.evaluate(wasmSnapshot)
          assert.strictEqual(outcome._tag, 'Completed', scenario.name)
          if (outcome._tag !== 'Completed') continue
          assert.strictEqual(outcome.result.value, BigInt(scenario.result), scenario.name)
          assert.strictEqual(callCount(outcome, 'applicationHandler'), scenario.handlerCalls)
          assert.strictEqual(dropCallCount(outcome), scenario.dropCalls)
          assert.isAtLeast(selectedCallCount(outcome, scenario.selectedTarget), 1, scenario.name)
          assert.strictEqual(traceCount(outcome, 'EffectFailure'), scenario.failureEvents)
          assert.isAtLeast(traceCount(outcome, 'SuspensionOrigin'), scenario.suspensionEvents)

          const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'debug' })
          const wasmRun = yield* runWasm(wasm.bytes)
          assert.strictEqual(wasmRun.result, scenario.result, `${scenario.name} Wasm`)
          assert.notInclude(wasm.wat, 'call_indirect', scenario.name)
          assert.notInclude(wasm.wat, '(table ', scenario.name)
          assertDirectTarget(wasm.wat, scenario.selectedTarget, `${scenario.name} Wasm`)
          const memory = wasmRun.instance.exports[StandardStreams.wasmMemoryExport]
          assert.instanceOf(memory, WebAssembly.Memory, scenario.name)
          if (scenario.dropCalls === 1)
            assert.notStrictEqual(wasmRun.class0, 0, `${scenario.name} Wasm cleanup release`)
        }
      }),
    120_000,
  )
})
