import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CAbi from '../src/CAbi.js'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'
import * as MirSamples from './support/mirSamples.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const raise = (message: string): never => {
  throw new Error(message)
}

const operationRegion = (region: Mir.Region | undefined): Mir.OperationRegion => {
  if (region?._tag !== 'OperationRegion') throw new Error('expected an operation region')
  return region
}

it.effect(
  'lowers scalar enum constants, projection, equality, and matches with logical identity',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'mir/scalar-enum',
        ascii(`enum State { Ready = 3, Done = 7 }
enum Other { Ready = 3 }
pub fn main() -> i32 {
  let state = State.Ready
  let other = Other.Ready
  drop other
  let raw = State.value(state)
  drop raw
  let equal = state == State.Done
  drop equal
  return match state {
    State.Ready => 1
    State.Done => 2
  }
}`),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const program = Analysis.loweredMir(snapshot)
      assert.deepEqual(MirVerification.verify(program), [])
      const operations = program.functions.flatMap((fn) => MirVerification.operations(fn))
      assert.strictEqual(
        operations.filter((operation) => operation._tag === 'EnumConstant').length,
        3,
      )
      assert.strictEqual(operations.filter((operation) => operation._tag === 'EnumValue').length, 1)
      assert.strictEqual(
        operations.filter((operation) => operation._tag === 'EnumEquality').length,
        1,
      )
      const match = operations.find((operation) => operation._tag === 'Match')
      assert.strictEqual(match?.scrutineeType._tag, 'Enum')
      if (match?._tag !== 'Match') return
      assert.deepEqual(
        match.members.map((member) =>
          member._tag === 'EnumMember' ? member.member.name : member._tag,
        ),
        ['Ready', 'Done'],
      )
      assert.strictEqual(MirEncoding.encode(program), MirEncoding.encode(program))
    }),
)

it.effect('rejects malformed scalar enum MIR before execution engines', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/invalid-scalar-enum',
      ascii(`enum State { Ready = 3, Done = 7 }
enum Other { Ready = 3 }
pub fn main() -> i32 {
  let state = State.Ready
  let other = Other.Ready
  drop other
  let raw = State.value(state)
  drop raw
  let equal = state == State.Done
  drop equal
  return match state {
    State.Ready => 1
    State.Done => 2
  }
}`),
    )
    const program = Analysis.loweredMir(snapshot)
    const fn = program.functions.at(0) ?? raise('expected scalar enum function')
    const operations = MirVerification.operations(fn)
    const constants = operations.filter((operation) => operation._tag === 'EnumConstant')
    const stateReady = constants.find((operation) => operation.member.enum.name === 'State')
    const stateDone = constants.find(
      (operation) => operation.member.enum.name === 'State' && operation.member.name === 'Done',
    )
    const otherReady = constants.find((operation) => operation.member.enum.name === 'Other')
    const projection = operations.find((operation) => operation._tag === 'EnumValue')
    const equality = operations.find((operation) => operation._tag === 'EnumEquality')
    const match = operations.find((operation) => operation._tag === 'Match')
    if (
      stateReady === undefined ||
      stateDone === undefined ||
      otherReady === undefined ||
      projection === undefined ||
      equality === undefined ||
      match === undefined
    )
      return raise('expected complete scalar enum MIR')

    const replace = (target: Mir.Operation, replacement: Mir.Operation): Mir.Module => ({
      ...program,
      functions: program.functions.map((candidate) => {
        if (candidate !== fn) return candidate
        return {
          ...candidate,
          regions: candidate.regions.map((region) => {
            if (region._tag !== 'OperationRegion' || !region.operations.includes(target)) {
              return region
            }
            return {
              ...region,
              operations: region.operations.map((operation) =>
                operation === target ? replacement : operation,
              ),
            }
          }),
        }
      }),
    })
    const ruleSet = (candidate: Mir.Module) =>
      MirVerification.verify(candidate).map((violation) => violation.rule)

    assert.include(
      ruleSet(replace(stateReady, { ...stateReady, member: otherReady.member })),
      'InvalidEnumOperation',
    )
    assert.include(
      ruleSet(replace(stateReady, { ...stateReady, discriminant: 99n })),
      'InvalidEnumOperation',
    )
    assert.include(
      ruleSet(
        replace(equality, {
          ...equality,
          right: otherReady.destination,
        }),
      ),
      'InvalidEnumOperation',
    )
    assert.include(
      ruleSet(
        replace(projection, {
          ...projection,
          enum: otherReady.enum,
        }),
      ),
      'InvalidEnumOperation',
    )
    const wrongLane = 'u16' as const
    const wrongBits = 16 as const
    assert.include(
      ruleSet(
        replace(stateDone, {
          ...stateDone,
          representation: {
            ...stateDone.representation,
            scalar: wrongLane,
            bits: wrongBits,
          },
        }),
      ),
      'InvalidEnumOperation',
    )

    const firstMember = match.members.at(0) ?? raise('expected first enum match member')
    const firstDecision = match.decisions.at(0) ?? raise('expected first enum match decision')
    const incomplete = replace(match, {
      ...match,
      members: [firstMember],
      decisions: [firstDecision],
      arms: match.arms.slice(0, 1),
    })
    const duplicate = replace(match, {
      ...match,
      members: [firstMember, firstMember],
      decisions: [firstDecision, firstDecision],
    })
    const foreign = replace(match, {
      ...match,
      members: [
        firstMember,
        {
          _tag: 'EnumMember',
          enum: otherReady.enum,
          member: otherReady.member,
          type: otherReady.type.type,
        },
      ],
    })
    for (const malformed of [incomplete, duplicate, foreign]) {
      assert.include(ruleSet(malformed), 'InvalidMatchDecision')
      assert.deepEqual(MirVerification.verify(malformed), MirVerification.verify(malformed))
    }
  }),
)

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
    foreignCalls: Object.freeze([]),
    entry: straight?.entry ?? raise('expected the sample entry'),
    layout: straight?.layout ?? raise('expected the sample layout'),
    executionTransitions: straight?.executionTransitions ?? Object.freeze([]),
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

it.effect('lowers a foreign call to one ForeignCall carrying the classified C signature', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/foreign-call',
      ascii(`unsafe extern "C" fn abs(value: i32) -> i32
pub fn main() -> i32 { return unsafe abs(-42) }`),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    const main = program.functions.find((fn) => fn.id.name === 'main') ?? raise('expected main')
    const calls = main.regions.flatMap((region) =>
      region._tag === 'OperationRegion'
        ? region.operations.flatMap((operation) =>
            operation._tag === 'ForeignCall' ? [operation] : [],
          )
        : [],
    )
    assert.deepEqual(
      calls.map((call) => ({
        symbol: call.symbol,
        abi: call.abi,
        signature: CAbi.signatureKey(call.signature),
        arguments: call.arguments.map((local) => main.localTypes.at(local.ordinal)?._tag),
        destination: main.localTypes.at(call.destination.ordinal)?._tag,
      })),
      [
        {
          symbol: 'abs',
          abi: 'C',
          signature: '(i32)->i32',
          arguments: ['i32'],
          destination: 'i32',
        },
      ],
    )
    assert.deepEqual(MirVerification.verify(program), [])
    const encoded = MirEncoding.encode(program)
    assert.include(
      encoded,
      'foreign abs abi=C signature=(i32)->i32 declaration=mir/foreign-call.abs',
    )
    assert.include(encoded, '= foreign-call abs abi=C signature=(i32)->i32(')
    assert.strictEqual(MirEncoding.encode(program), encoded)
  }),
)

it('verifies foreign call arity and C classes as structural violations', () => {
  const valid = MirSamples.foreignCallSample(Target.aarch64AppleDarwin)
  assert.deepEqual(MirVerification.verify(valid), [])
  assert.strictEqual(MirEncoding.encode(valid), MirEncoding.encode(valid))
  const arityMismatch = MirSamples.foreignCallSample(Target.aarch64AppleDarwin, [])
  assert.deepEqual(
    MirVerification.verify(arityMismatch).map((violation) => violation.rule),
    ['InvalidForeignCall'],
  )
})
