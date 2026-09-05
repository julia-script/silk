import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CAbi from '../src/CAbi.js'
import * as Lifetime from '../src/Lifetime.js'
import * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'
import * as MirSamples from './support/mirSamples.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it('preserves access, safety, and outcome contracts when adapting physical operands', () => {
  const owner = { module: 'mir/operands', name: 'adapt' }
  const first = { environment: Lifetime.bound(owner, 0, 'a'), lifetimeBinders: [] }
  const second = { environment: Lifetime.bound(owner, 1, 'b'), lifetimeBinders: [] }
  const shared = Type.effect('i32', [], first)
  const taken = Type.effect('i32', [], second, 'Take')
  assert.isTrue(Mir.acceptsRuntimeOperand(shared, taken))
  assert.isFalse(Mir.acceptsRuntimeOperand(taken, shared))
  assert.isFalse(Mir.acceptsRuntimeOperand(shared, Type.effect('bool', [], second, 'Take')))
  assert.isFalse(Mir.acceptsRuntimeOperand(shared, Type.effect('i32', ['bool'], second, 'Take')))

  const callback = Type.callable(['i32'], shared, first)
  const parameter = Type.callable(['i32'], taken, second, 'Take')
  assert.isTrue(Mir.acceptsRuntimeOperand(callback, parameter))
  assert.isFalse(Mir.acceptsRuntimeOperand(parameter, callback))
  assert.isFalse(Mir.acceptsRuntimeOperand({ ...callback, unsafe: true }, parameter))
  assert.isFalse(Mir.acceptsRuntimeOperand(callback, { ...parameter, parameters: ['bool'] }))

  for (const make of [Type.reference, Type.slice]) {
    const exclusive = make('Exclusive', 'i32', first.environment)
    const sharedView = make('Shared', 'i32', second.environment)
    assert.isTrue(Mir.acceptsRuntimeOperand(exclusive, sharedView))
    assert.isFalse(Mir.acceptsRuntimeOperand(sharedView, exclusive))
    assert.isFalse(Mir.acceptsRuntimeOperand(exclusive, make('Shared', 'bool', second.environment)))
  }
})

it('erases lifetime argument positions without erasing runtime specialization arguments', () => {
  const lifetime = Lifetime.bound({ module: 'mir/arguments', name: 'select' }, 0, 'a')
  assert.isTrue(Mir.runtimeArgumentsEqual([lifetime], []))
  assert.isTrue(Mir.runtimeArgumentsEqual([lifetime, 'i32'], ['i32', Lifetime.staticLifetime]))
  assert.isFalse(Mir.runtimeArgumentsEqual([lifetime, 'i32'], []))
  assert.isFalse(Mir.runtimeArgumentsEqual([lifetime, 'i32'], ['bool']))
})

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
    foreignExports: Object.freeze([]),
    foreignStatics: Object.freeze([]),
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
  assert.include(MirEncoding.encode(sample), `target ${Target.encode(sample.layout.target)}`)
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

it.effect('verifies a foreign pointer argument against the declared pointee', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/foreign-pointer-argument',
      ascii(`import silk.pointer { Pointer }
unsafe extern "C" fn inspect(value: *const i32) -> i32
pub fn main() -> i32 {
  let mut value = 1
  return unsafe inspect(Pointer.fromMutRef(&mut value))
}`),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [])
    // The same `*mut i32` argument against a `*const u8` parameter is one violation.
    const rewritten: Mir.Module = Object.freeze({
      ...program,
      functions: Object.freeze(
        program.functions.map((fn) =>
          Object.freeze({
            ...fn,
            regions: Object.freeze(
              fn.regions.map((region) =>
                region._tag !== 'OperationRegion'
                  ? region
                  : Object.freeze({
                      ...region,
                      operations: Object.freeze(
                        region.operations.map((operation) =>
                          operation._tag !== 'ForeignCall'
                            ? operation
                            : Object.freeze({
                                ...operation,
                                signature: Object.freeze({
                                  ...operation.signature,
                                  parameters: Object.freeze([
                                    Object.freeze({
                                      _tag: 'Pointer' as const,
                                      mutable: false,
                                      pointee: 'u8' as const,
                                    }),
                                  ]),
                                }),
                              }),
                        ),
                      ),
                    }),
              ),
            ),
          }),
        ),
      ),
    })
    assert.deepEqual(
      MirVerification.verify(rewritten).map((violation) => violation.rule),
      ['InvalidForeignCall'],
    )
  }),
)

it.effect('rejects every Silk callable without one exact exported C address', () =>
  Effect.gen(function* () {
    const source = `unsafe extern "C" fn install(callback: extern "C" fn(i32) -> i32) -> ()
fn private(value: i32) -> i32 { return value }
effect fn effectful(value: i32) -> i32 { return value }
fn generic<T>(value: T) -> T { return move value }
export "C" fn wrong(value: u32) -> u32 { return value }
fn invalidPointer(callback: extern "C" fn(bool) -> i32) { drop callback }
pub fn main() -> i32 {
  let captured = 1
  unsafe {
    install(private)
    install(effectful)
    install(generic)
    install(wrong)
    install(fn(value: i32) -> i32 { return value + captured })
  }
  return 0
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/invalid-foreign-callbacks',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const diagnostics = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0207',
    )
    const invalidPointer = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0187',
    )
    assert.deepEqual(
      invalidPointer.map((diagnostic) => source.slice(diagnostic.span.start, diagnostic.span.end)),
      ['extern "C" fn(bool) -> i32'],
    )
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.reason._tag),
      [
        'InvalidForeignCallback',
        'InvalidForeignCallback',
        'InvalidForeignCallback',
        'InvalidForeignCallback',
        'InvalidForeignCallback',
      ],
    )
    assert.deepEqual(
      diagnostics.map((diagnostic) => source.slice(diagnostic.span.start, diagnostic.span.end)),
      [
        'private',
        'effectful',
        'generic',
        'wrong',
        'fn(value: i32) -> i32 { return value + captured }',
      ],
    )
  }),
)

it.effect('requires an unsafe boundary to read an imported C static', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/unsafe-foreign-static',
      ascii(`unsafe extern "C" static environment: *mut *mut u8 as "environ"
pub fn main() -> i32 { let value = environment return 0 }`),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0082'],
    )
  }),
)

it.effect('keeps a C static binding immutable independently of pointee mutability', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/immutable-foreign-static',
      ascii(`export "C" static answer: i32 = 42
pub fn main() -> i32 { answer = 1 return answer }`),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0035'],
    )
  }),
)

it.effect('rejects a pointer-sized C static initializer outside the selected target', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/foreign-static-target-range',
      ascii(`export "C" static wide: usize = 4294967296
pub fn main() -> i32 { return 0 }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0086'],
    )
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('lowers pointer formation, offset, write, and read to explicit pointer operations', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/pointer-slice',
      ascii(`import silk.pointer { Pointer }
pub fn main() -> i32 {
  let mut values = [1, 2, 3]
  let pointer = Pointer.fromMutSlice(&mut values)
  unsafe {
    let third = Pointer.offsetMut(pointer, 2)
    Pointer.write(third, 9)
    return Pointer.read(third)
  }
  return 0
}`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [])
    const described = program.functions.flatMap((fn) =>
      MirVerification.operations(fn).flatMap((operation) => {
        if (operation._tag === 'PointerFromReference')
          return [
            `${operation._tag} ${fn.localTypes.at(operation.source.ordinal)?._tag} -> ${Type.encode(operation.type.type)}`,
          ]
        if (operation._tag === 'PointerOffset')
          return [
            `${operation._tag} ${fn.localTypes.at(operation.count.ordinal)?._tag} -> ${Type.encode(operation.type.type)}`,
          ]
        if (operation._tag === 'PointerRead')
          return [`${operation._tag} -> ${Type.encode(Mir.semanticType(operation.type))}`]
        if (operation._tag === 'PointerWrite')
          return [`${operation._tag} ${fn.localTypes.at(operation.value.ordinal)?._tag}`]
        return []
      }),
    )
    assert.deepEqual(described.sort(), [
      'PointerFromReference Slice -> *mut i32',
      'PointerOffset usize -> *mut i32',
      'PointerRead -> i32',
      'PointerWrite i32',
    ])
    const encoded = MirEncoding.encode(program)
    assert.include(encoded, '= pointer-from-reference ')
    assert.include(encoded, '= pointer-offset ')
    assert.include(encoded, '= pointer-write ')
    assert.include(encoded, '= pointer-read ')
    assert.strictEqual(MirEncoding.encode(program), encoded)
  }),
)

it.effect('rejects a direct-intrinsic pointer write of a move-only pointee as data', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/pointer-move-only-write',
      ascii(`struct Owned { value: i32 }
pub fn main() -> i32 {
  let mut holder = Owned { value: 1 }
  let pointer = Intrinsic.pointerFromMutRef<Owned>(&mut holder)
  unsafe {
    Intrinsic.pointerWrite<Owned>(pointer, Owned { value: 2 })
  }
  return holder.value
}`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(
      MirVerification.verify(program).map((violation) => violation.rule),
      ['InvalidPointerOperation'],
    )
  }),
)

it.effect('rejects copying a non-Copy slot element with a conformance diagnostic', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'slot-copy/non-copy',
      ascii(`import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
struct Guard { storage: Allocation }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let allocation = run Allocator.allocate(Layout.of<[Guard; 1]>())
    |> Effect.provideMut(&mut allocator)
  let payload = run Allocator.allocate(Layout.of<[i32; 1]>())
    |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut buffer = RawBuffer.from<Guard>(move allocation, 1)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), Guard { storage: move payload })
    let copied = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    drop copied
    drop buffer
    return 42
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0083'],
    )
  }),
)

it.effect('rejects missing normal match results and invented results on transferring arms', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'mir/ordinary-arm-results',
      ascii(`enum Choice { First, Last }
fn inspect(value: Choice) -> i32 {
  return match value { Choice.First => { return 7 } Choice.Last => 9 }
}
pub fn main() -> i32 { return inspect(Choice.Last) }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [])
    const fn = program.functions.find((fn) => fn.id.name === 'inspect') ?? raise('expected inspect')
    const match =
      MirVerification.operations(fn).find((operation) => operation._tag === 'Match') ??
      raise('expected match')
    const transferring = match.arms.at(0) ?? raise('expected transferring arm')
    const completing = match.arms.at(1) ?? raise('expected completing arm')
    const result = completing.selected.execution.result ?? raise('expected normal arm result')
    const replace = (replacement: Mir.MatchOperation): Mir.Module => ({
      ...program,
      functions: program.functions.map((candidate) =>
        candidate !== fn
          ? candidate
          : {
              ...candidate,
              regions: candidate.regions.map((region) =>
                region._tag !== 'OperationRegion'
                  ? region
                  : {
                      ...region,
                      operations: region.operations.map((operation) =>
                        operation === match ? replacement : operation,
                      ),
                    },
              ),
            },
      ),
    })
    const withoutResult: Mir.Execution = {
      entry: completing.selected.execution.entry,
      regions: completing.selected.execution.regions,
    }
    const absent = replace({
      ...match,
      arms: [
        transferring,
        { ...completing, selected: { ...completing.selected, execution: withoutResult } },
      ],
    })
    assert.include(
      MirVerification.verify(absent).map((violation) => violation.rule),
      'InvalidMatchJoin',
    )
    const invented = replace({
      ...match,
      arms: [
        {
          ...transferring,
          selected: {
            ...transferring.selected,
            execution: { ...transferring.selected.execution, result },
          },
        },
        completing,
      ],
    })
    assert.include(
      MirVerification.verify(invented).map((violation) => violation.rule),
      'InvalidMatchJoin',
    )
    const uninitialized: Mir.LocalId = { _tag: 'Local', ordinal: fn.localTypes.length }
    const missingWrite = replace({
      ...match,
      arms: [
        transferring,
        {
          ...completing,
          selected: {
            ...completing.selected,
            execution: { ...completing.selected.execution, result: uninitialized },
          },
        },
      ],
    })
    assert.include(
      MirVerification.verify({
        ...missingWrite,
        functions: missingWrite.functions.map((candidate) =>
          candidate.id.name === 'inspect'
            ? { ...candidate, localTypes: [...candidate.localTypes, { _tag: 'i32' }] }
            : candidate,
        ),
      }).map((violation) => violation.rule),
      'InvalidMatchJoin',
    )
  }),
)
