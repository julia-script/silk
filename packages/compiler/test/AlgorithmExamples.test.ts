import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Schema from 'effect/Schema'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const Blocker = Schema.Struct({
  phase: Schema.String,
  code: Schema.String,
  message: Schema.String,
})

const AllocationExpectation = Schema.Struct({
  acquires: Schema.Number,
  releases: Schema.Number,
  peakLive: Schema.Number,
})

const Manifest = Schema.Struct({
  schemaVersion: Schema.Literal(1),
  id: Schema.String,
  title: Schema.String,
  status: Schema.Literals(['executable', 'frontier']),
  source: Schema.String,
  input: Schema.String,
  expected: Schema.Struct({
    entryResult: Schema.Number,
    algorithmResult: Schema.optionalKey(Schema.Number),
    summary: Schema.String,
    allocation: Schema.optionalKey(AllocationExpectation),
  }),
  capabilities: Schema.Array(Schema.String),
  targets: Schema.Array(Schema.Literals(['evaluation', 'native', 'wasm'])),
  blockers: Schema.Array(Blocker),
})

const examplesRoot = fileURLToPath(new URL('../../../examples/algorithms/', import.meta.url))
const exampleIds = readdirSync(examplesRoot)
  .filter((name) => statSync(join(examplesRoot, name)).isDirectory())
  .sort()

const examples = exampleIds.map((id) => {
  const root = join(examplesRoot, id)
  const manifest = Schema.decodeUnknownSync(Manifest)(
    JSON.parse(readFileSync(join(root, 'example.json'), 'utf8')),
  )
  const source = readFileSync(join(root, manifest.source), 'utf8')
  return Object.freeze({
    root,
    manifest,
    source,
    bytes: new Uint8Array(readFileSync(join(root, manifest.source))),
    readme: readFileSync(join(root, 'README.md'), 'utf8'),
  })
})

const sourceId = (id: string, target: string): string => `examples/algorithms/${id}/${target}`

interface AllocationEvent {
  readonly _tag: 'AllocationAcquire' | 'AllocationRelease'
  readonly ticket: number
}

interface ExpectedAllocation {
  readonly acquires: number
  readonly releases: number
  readonly peakLive: number
}

type AllocationVerification =
  | {
      readonly _tag: 'Valid'
      readonly observed: ExpectedAllocation
    }
  | {
      readonly _tag: 'Invalid'
      readonly reason: 'DuplicateAcquire' | 'UnknownRelease' | 'Unreleased' | 'Mismatch'
      readonly ticket?: number
      readonly observed?: ExpectedAllocation
      readonly expected?: ExpectedAllocation
    }

const verifyAllocationEvidence = (
  events: ReadonlyArray<AllocationEvent>,
  expected: ExpectedAllocation,
): AllocationVerification => {
  const seen = new Set<number>()
  const live = new Set<number>()
  let acquires = 0
  let releases = 0
  let peakLive = 0
  for (const event of events) {
    if (event._tag === 'AllocationAcquire') {
      if (seen.has(event.ticket))
        return Object.freeze({
          _tag: 'Invalid',
          reason: 'DuplicateAcquire',
          ticket: event.ticket,
        })
      seen.add(event.ticket)
      live.add(event.ticket)
      acquires += 1
      peakLive = Math.max(peakLive, live.size)
      continue
    }
    if (!live.delete(event.ticket))
      return Object.freeze({
        _tag: 'Invalid',
        reason: 'UnknownRelease',
        ticket: event.ticket,
      })
    releases += 1
  }
  const unreleased = live.values().next().value
  if (unreleased !== undefined)
    return Object.freeze({ _tag: 'Invalid', reason: 'Unreleased', ticket: unreleased })
  const observed = Object.freeze({ acquires, releases, peakLive })
  if (
    observed.acquires !== expected.acquires ||
    observed.releases !== expected.releases ||
    observed.peakLive !== expected.peakLive
  )
    return Object.freeze({ _tag: 'Invalid', reason: 'Mismatch', observed, expected })
  return Object.freeze({ _tag: 'Valid', observed })
}

const invalidReason = (verification: AllocationVerification) =>
  verification._tag === 'Invalid' ? verification.reason : undefined

const evidence = (self: Analysis.Snapshot) => {
  const diagnostics = Analysis.diagnostics(self)
  if (diagnostics.length > 0) {
    return diagnostics.map(({ phase, code, message }) => Object.freeze({ phase, code, message }))
  }
  const outcome = Analysis.evaluate(self)
  if (outcome._tag !== 'Blocked') return []
  if (outcome.reason._tag === 'EvaluationLimit') {
    return [
      Object.freeze({
        phase: 'evaluation' as const,
        code: outcome.reason._tag,
        message: `${outcome.reason.kind} ${outcome.reason.count}/${outcome.reason.limit}`,
      }),
    ]
  }
  return [
    Object.freeze({
      phase: 'evaluation',
      code: outcome.reason._tag,
      message: outcome.reason._tag,
    }),
  ]
}

it('validates allocation evidence and rejects malformed resource traces', () => {
  const expected = Object.freeze({ acquires: 2, releases: 2, peakLive: 2 })
  assert.deepEqual(
    verifyAllocationEvidence(
      [
        { _tag: 'AllocationAcquire', ticket: 0 },
        { _tag: 'AllocationAcquire', ticket: 1 },
        { _tag: 'AllocationRelease', ticket: 0 },
        { _tag: 'AllocationRelease', ticket: 1 },
      ],
      expected,
    ),
    { _tag: 'Valid', observed: expected },
  )
  assert.strictEqual(
    invalidReason(
      verifyAllocationEvidence(
        [
          { _tag: 'AllocationAcquire', ticket: 0 },
          { _tag: 'AllocationAcquire', ticket: 0 },
        ],
        expected,
      ),
    ),
    'DuplicateAcquire',
  )
  assert.strictEqual(
    invalidReason(verifyAllocationEvidence([{ _tag: 'AllocationRelease', ticket: 0 }], expected)),
    'UnknownRelease',
  )
  assert.strictEqual(
    invalidReason(verifyAllocationEvidence([{ _tag: 'AllocationAcquire', ticket: 0 }], expected)),
    'Unreleased',
  )
  assert.strictEqual(invalidReason(verifyAllocationEvidence([], expected)), 'Mismatch')
})

it('keeps seven complete, readable programs and explicit status contracts', () => {
  assert.deepEqual(exampleIds, [
    'breadth-first-search',
    'crc-32',
    'fft',
    'game-of-life',
    'matrix-multiplication',
    'quicksort',
    'sieve',
  ])
  assert.deepEqual(
    examples
      .filter(({ manifest }) => manifest.status === 'executable')
      .map(({ manifest }) => manifest.id),
    [
      'breadth-first-search',
      'crc-32',
      'fft',
      'game-of-life',
      'matrix-multiplication',
      'quicksort',
      'sieve',
    ],
  )
  for (const { root, manifest, source, bytes, readme } of examples) {
    assert.strictEqual(manifest.id, root.slice(root.lastIndexOf('/') + 1))
    assert.strictEqual(existsSync(join(root, manifest.source)), true)
    assert.isAbove(bytes.length, 100)
    assert.isAbove(readme.length, 100)
    assert.isAbove(manifest.input.length, 10)
    assert.isAbove(manifest.expected.summary.length, 20)
    assert.isAbove(manifest.capabilities.length, 2)
    assert.deepEqual(manifest.targets, ['evaluation', 'native', 'wasm'])
    assert.strictEqual(Number.isInteger(manifest.expected.entryResult), true)
    assert.strictEqual(manifest.blockers.length === 0, manifest.status === 'executable')
    if (manifest.id === 'breadth-first-search') {
      assert.include(source, 'import silk.vector')
      assert.notInclude(source, 'Report')
      assert.include(source, 'import silk.core { OutOfMemoryError }')
      assert.include(source, 'pub effect fn main() -> () ! OutOfMemoryError')
      assert.notInclude(source, 'Effect.catch')
      assert.notInclude(source, 'RawBuffer')
      assert.notInclude(source, 'Slot.')
      assert.strictEqual(manifest.expected.entryResult, 0)
      assert.strictEqual(manifest.expected.algorithmResult, 8)
    }
  }
})

it.effect('keeps frontier evidence normalized and deterministic on both targets', () =>
  Effect.gen(function* () {
    for (const { manifest, bytes } of examples) {
      if (manifest.status !== 'frontier') continue
      for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown'] as const) {
        const first = yield* Analysis.ofSourceRealized(sourceId(manifest.id, target), bytes, target)
        const second = yield* Analysis.ofSourceRealized(
          sourceId(manifest.id, target),
          bytes,
          target,
        )
        assert.deepEqual(evidence(first), manifest.blockers, `${manifest.id} ${target}`)
        assert.deepEqual(evidence(second), manifest.blockers, `${manifest.id} ${target}`)
        assert.deepEqual(evidence(first), evidence(second), `${manifest.id} changed evidence`)
      }
    }
  }),
)

it.effect(
  'executes the baseline through evaluation and direct WebAssembly with exact parity',
  () =>
    Effect.gen(function* () {
      for (const { manifest, bytes } of examples) {
        if (manifest.status !== 'executable') continue
        const native = yield* Analysis.ofSourceRealized(
          sourceId(manifest.id, 'native'),
          bytes,
          'aarch64-apple-darwin',
        )
        assert.deepEqual(Analysis.diagnostics(native), [], manifest.id)
        const evaluated = Analysis.evaluate(native)
        assert.strictEqual(
          evaluated._tag,
          'Completed',
          `${manifest.id}: ${JSON.stringify(evaluated, (_, value) =>
            typeof value === 'bigint' ? value.toString() : value,
          )}`,
        )
        if (evaluated._tag !== 'Completed') continue
        assert.strictEqual(evaluated.result._tag, 'IntegerValue', manifest.id)
        if (evaluated.result._tag !== 'IntegerValue') continue
        assert.strictEqual(
          evaluated.result.value,
          BigInt(manifest.expected.entryResult),
          manifest.id,
        )
        if (manifest.expected.allocation !== undefined) {
          const allocationEvents = evaluated.trace.flatMap(
            (event): ReadonlyArray<AllocationEvent> =>
              event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease'
                ? [Object.freeze({ _tag: event._tag, ticket: event.ticket })]
                : [],
          )
          const verification = verifyAllocationEvidence(
            allocationEvents,
            manifest.expected.allocation,
          )
          assert.strictEqual(
            verification._tag,
            'Valid',
            `${manifest.id} allocation evidence: ${JSON.stringify(verification)}`,
          )
        }
        const operations = Analysis.loweredMir(native).functions.flatMap(Mir.operations)
        assert.isFalse(
          operations.some((operation) => {
            const tag = operation._tag.toLowerCase()
            return tag.includes('vector') || tag.includes('breadth') || tag.includes('search')
          }),
          `${manifest.id} gained an algorithm-shaped MIR operation`,
        )

        const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
        assert.isAbove(nativeArtifact.bitcode.length, 0, manifest.id)

        const wasm = yield* Analysis.ofSourceRealized(
          sourceId(manifest.id, 'wasm'),
          bytes,
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(wasm), [], manifest.id)
        const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
        const instance = new WebAssembly.Instance(
          new WebAssembly.Module(wasmArtifact.bytes.slice()),
          {},
        )
        const wasmMain = instance.exports.silk_main
        assert.strictEqual(typeof wasmMain, 'function', manifest.id)
        if (typeof wasmMain !== 'function') continue
        assert.strictEqual(wasmMain(), manifest.expected.entryResult, manifest.id)
      }
    }),
  90_000,
)
