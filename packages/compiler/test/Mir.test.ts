import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'
import * as MirSamples from './support/mirSamples.js'

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
  for (const sample of MirSamples.samples()) {
    assert.deepEqual(MirVerification.verify(sample), [])
  }
})

it('rejects nested unavailable values at the monomorphic MIR frontier', () => {
  const [straight] = MirSamples.samples()
  const sample = straight ?? raise('expected sample')
  const fn = sample.functions.at(0) ?? raise('expected sample function')
  const unavailable = Type.nominal('sample://unavailable.silk', 'Outer', [
    Type.nominal('sample://unavailable.silk', 'Inner', [
      Type.unavailableGenericArgument('Value', 'unresolved MIR argument'),
    ]),
  ])
  const unavailableLocal: Mir.Module = {
    ...sample,
    functions: [
      {
        ...fn,
        localTypes: [...fn.localTypes, { _tag: 'Nominal', type: unavailable }],
      },
    ],
  }
  assert.include(
    MirVerification.verify(unavailableLocal).map((violation) => violation.rule),
    'InvalidInstance',
  )

  const unavailableInstance = {
    ...fn.instance,
    typeArguments: [unavailable],
  }
  const unavailableIdentity: Mir.Module = {
    ...sample,
    entry: {
      _tag: 'OrdinaryEntry',
      target: unavailableInstance,
      machine: unavailableInstance,
    },
    functions: [{ ...fn, instance: unavailableInstance }],
  }
  assert.include(
    MirVerification.verify(unavailableIdentity).map((violation) => violation.rule),
    'InvalidInstance',
  )
})

it('retains invalid return types as a verifier invariant', () => {
  const [straight] = MirSamples.samples()
  const sample = straight ?? raise('expected sample')
  const fn = sample.functions.at(0) ?? raise('expected sample function')
  const invalid: Mir.Module = {
    ...sample,
    functions: [{ ...fn, result: { _tag: 'bool' } }],
  }

  assert.include(
    MirVerification.verify(invalid).map((violation) => violation.rule),
    'InvalidReturn',
  )
})

it('reports broken graphs deterministically as data', () => {
  const [straight] = MirSamples.samples()
  const fn = straight?.functions.at(0) ?? raise('expected the sample function')
  const first = operationRegion(fn.regions.at(0))
  const broken: Mir.Module = {
    _tag: 'MirModule',
    module: 'sample://broken.silk',
    intrinsics: straight?.intrinsics ?? raise('expected the sample intrinsic inventory'),
    entry: straight?.entry ?? raise('expected the sample entry'),
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

  const violations = MirVerification.verify(broken)
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
  assert.deepEqual(MirVerification.verify(broken), violations)
})

it('rejects structural cycles without treating lexical repetition as an edge', () => {
  const [straight] = MirSamples.samples()
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
    MirVerification.verify(cyclic).map((violation) => violation.rule),
    'StructuralCycle',
  )
})

it('rejects repeat and exit ports that name no lexical loop owner', () => {
  const [straight] = MirSamples.samples()
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
    MirVerification.verify(invalid).map((violation) => violation.rule),
    'InvalidLoopTarget',
  )
})

it('requires every yield to be one uniquely owned loop condition', () => {
  const [, branching] = MirSamples.samples()
  const sample = branching ?? raise('expected branching sample')
  const fn = sample.functions.at(0) ?? raise('expected sample function')
  const returned = operationRegion(fn.regions.at(1))
  const provenance = returned.outcome.provenance
  const region = (ordinal: number): Mir.RegionId => ({ _tag: 'Region', ordinal })
  const loop = (ordinal: number): Mir.LoopId => ({ _tag: 'Loop', ordinal })
  const loop0 = loop(0)
  const condition: Mir.OperationRegion = {
    _tag: 'OperationRegion',
    id: region(1),
    ownerLoop: loop0,
    operations: [],
    outcome: { _tag: 'Yield', provenance },
  }
  const body: Mir.OperationRegion = {
    _tag: 'OperationRegion',
    id: region(2),
    ownerLoop: loop0,
    operations: [],
    outcome: { _tag: 'Exit', loop: loop0, provenance },
  }
  const following: Mir.OperationRegion = { ...returned, id: region(3) }
  const owner: Mir.LoopRegion = {
    _tag: 'LoopRegion',
    id: region(0),
    loop: loop0,
    condition: condition.id,
    conditionValue: { _tag: 'Local', ordinal: 0 },
    body: body.id,
    following: following.id,
    provenance,
  }
  const withRegions = (regions: ReadonlyArray<Mir.Region>): Mir.Module => ({
    ...sample,
    functions: [{ ...fn, entry: owner.id, regions }],
  })
  const valid = withRegions([owner, condition, body, following])
  assert.deepEqual(MirVerification.verify(valid), [])

  const unownedYield = withRegions([
    owner,
    condition,
    { ...body, outcome: { _tag: 'Yield', provenance } },
    following,
  ])
  assert.include(
    MirVerification.verify(unownedYield).map((violation) => violation.rule),
    'InvalidLoopTarget',
  )

  const nonYieldCondition = withRegions([
    owner,
    { ...condition, outcome: { _tag: 'Exit', loop: loop0, provenance } },
    body,
    following,
  ])
  assert.include(
    MirVerification.verify(nonYieldCondition).map((violation) => violation.rule),
    'InvalidLoopTarget',
  )

  const sharedCondition: Mir.LoopRegion = {
    ...owner,
    id: region(4),
    loop: loop(1),
  }
  assert.include(
    MirVerification.verify(withRegions([owner, condition, body, following, sharedCondition])).map(
      (violation) => violation.rule,
    ),
    'InvalidLoopTarget',
  )
})

it('carries and encodes exactly one compiler-owned target layout plan', () => {
  const [straight] = MirSamples.samples()
  const sample = straight ?? raise('expected sample')

  assert.strictEqual(sample.layout.entries.at(0)?.size, 4)
  assert.include(MirEncoding.encode(sample), 'target aarch64-apple-darwin')
  assert.include(MirEncoding.encode(sample), 'layout i32 size=4 align=4 repr=signed-i32')
})

it('marks generated outcomes and preserves programmer provenance', () => {
  const [, branching] = MirSamples.samples()
  const encoded = MirEncoding.encode(branching ?? raise('expected branching sample'))

  assert.include(encoded, 'conditional condition=%0')
  assert.include(encoded, 'trap "otherwise" [25, 34) generated')
})

it('matches the MIR golden encodings byte-for-byte', () => {
  const [straight, branching] = MirSamples.samples()

  assert.strictEqual(
    MirEncoding.encode(straight ?? raise('expected sample')),
    golden('straight.mir.txt'),
  )
  assert.strictEqual(
    MirEncoding.encode(branching ?? raise('expected sample')),
    golden('branching.mir.txt'),
  )
})

it('constructs and encodes byte-identically across repeated runs', () => {
  const first = MirSamples.samples()
  const second = MirSamples.samples()

  assert.deepEqual(first, second)
  assert.deepEqual(first.map(MirEncoding.encode), second.map(MirEncoding.encode))
})
