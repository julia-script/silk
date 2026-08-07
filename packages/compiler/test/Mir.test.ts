import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Mir from '../src/Mir.js'

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const raise = (message: string): never => {
  throw new Error(message)
}

const operationRegion = (region: Mir.Region | undefined): Mir.OperationRegion => {
  if (region?._tag !== 'OperationRegion') throw new Error('expected an operation region')
  return region
}

it('verifies the hand-built samples clean', () => {
  for (const sample of Mir.samples()) {
    assert.deepEqual(Mir.verify(sample), [])
  }
})

it('reports broken graphs deterministically as data', () => {
  const [straight] = Mir.samples()
  const fn = straight?.functions.at(0) ?? raise('expected the sample function')
  const first = operationRegion(fn.regions.at(0))
  const broken: Mir.Module = {
    _tag: 'MirModule',
    module: 'sample://broken.silk',
    layout: straight?.layout ?? raise('expected the sample layout'),
    functions: [
      { ...fn, entry: { _tag: 'Region', ordinal: 9 } },
      {
        ...fn,
        regions: [
          {
            ...first,
            operations: first.operations.map((operation) =>
              operation._tag === 'Literal'
                ? { ...operation, destination: { _tag: 'Local' as const, ordinal: 7 } }
                : operation,
            ),
            outcome: {
              _tag: 'Forward',
              target: { _tag: 'Region', ordinal: 9 },
              provenance: { span: first.outcome.provenance.span, generated: true },
            },
          },
        ],
      },
    ],
  }

  const violations = Mir.verify(broken)
  assert.deepEqual(
    violations.map((violation) => violation.rule),
    [
      'MissingEntryRegion',
      'InvalidInstance',
      'UnknownRegionTarget',
      'UndeclaredLocal',
      'InvalidIntegerOperation',
    ],
  )
  assert.deepEqual(Mir.verify(broken), violations)
})

it('rejects structural cycles without treating lexical repetition as an edge', () => {
  const [straight] = Mir.samples()
  const sample = straight ?? raise('expected sample')
  const fn = sample.functions.at(0) ?? raise('expected sample function')
  const first = operationRegion(fn.regions.at(0))
  const cyclic: Mir.Module = {
    ...sample,
    functions: [
      {
        ...fn,
        regions: [
          {
            ...first,
            operations: [],
            outcome: {
              _tag: 'Forward',
              target: first.id,
              provenance: first.outcome.provenance,
            },
          },
        ],
      },
    ],
  }

  assert.include(
    Mir.verify(cyclic).map((violation) => violation.rule),
    'StructuralCycle',
  )
})

it('rejects repeat and exit ports that name no lexical loop owner', () => {
  const [straight] = Mir.samples()
  const sample = straight ?? raise('expected sample')
  const fn = sample.functions.at(0) ?? raise('expected sample function')
  const first = operationRegion(fn.regions.at(0))
  const invalid: Mir.Module = {
    ...sample,
    functions: [
      {
        ...fn,
        regions: [
          {
            ...first,
            outcome: {
              _tag: 'Repeat',
              loop: { _tag: 'Loop', ordinal: 99 },
              provenance: first.outcome.provenance,
            },
          },
        ],
      },
    ],
  }

  assert.include(
    Mir.verify(invalid).map((violation) => violation.rule),
    'InvalidLoopTarget',
  )
})

it('carries and encodes exactly one compiler-owned target layout plan', () => {
  const [straight] = Mir.samples()
  const sample = straight ?? raise('expected sample')

  assert.strictEqual(sample.layout.entries.at(0)?.size, 4)
  assert.include(Mir.encode(sample), 'target aarch64-apple-darwin')
  assert.include(Mir.encode(sample), 'layout I32 size=4 align=4 repr=signed-i32')
})

it('marks generated outcomes and preserves programmer provenance', () => {
  const [, branching] = Mir.samples()
  const encoded = Mir.encode(branching ?? raise('expected branching sample'))

  assert.include(encoded, 'conditional condition=%0')
  assert.include(encoded, 'trap "otherwise" [25, 34) generated')
})

it('matches the MIR golden encodings byte-for-byte', () => {
  const [straight, branching] = Mir.samples()

  assert.strictEqual(Mir.encode(straight ?? raise('expected sample')), golden('straight.mir.txt'))
  assert.strictEqual(Mir.encode(branching ?? raise('expected sample')), golden('branching.mir.txt'))
})

it('constructs and encodes byte-identically across repeated runs', () => {
  const first = Mir.samples()
  const second = Mir.samples()

  assert.deepEqual(first, second)
  assert.deepEqual(first.map(Mir.encode), second.map(Mir.encode))
})
