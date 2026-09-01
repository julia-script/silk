import { createHash } from 'node:crypto'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as Hir from '../src/Hir.js'
import * as Instances from '../src/Instances.js'
import * as Lexer from '../src/Lexer.js'
import * as LiteralForm from '../src/LiteralForm.js'
import * as MirVerification from '../src/MirVerification.js'
import * as OwnershipEncoding from '../src/OwnershipEncoding.js'
import * as Parser from '../src/Parser.js'
import * as Residualization from '../src/Residualization.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as StaticEvaluation from '../src/StaticEvaluation.js'
import * as StaticText from '../src/StaticText.js'
import * as StaticValue from '../src/StaticValue.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'

const encoder = new TextEncoder()
const sha256 = (value: string): string => createHash('sha256').update(value).digest('hex')

const syntaxNodes = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  Object.freeze([
    node,
    ...node.children.flatMap((child) => (SyntaxTree.isNode(child) ? syntaxNodes(child) : [])),
  ])

const completedValue = (
  outcome: StaticEvaluation.Outcome<StaticValue.Value>,
): StaticValue.Value => {
  if (outcome._tag === 'Failed')
    throw new Error(`expected completed static value: ${outcome.failure._tag}`)
  return outcome.value
}

const source = `pub fn main() -> i32 {
  let text = """hé
"""
  let bytes = b"""life
"""
  let repeated = "hé\\n"
  if Intrinsic.stringByteLength(text) != 4 { return 0 }
  if bytes.length != 5 { return 1 }
  if Intrinsic.stringByteLength(repeated) != Intrinsic.stringByteLength(text) { return 2 }
  return 42
}`

const formOf = (value: string): LiteralForm.LiteralForm => {
  const form = LiteralForm.recognize(encoder.encode(value))
  if (form === undefined) throw new Error(`expected a recognized literal form: ${value}`)
  return form
}

const admitted = (value: StaticValue.Admission): StaticValue.Value => {
  if (value._tag === 'Rejected') throw new Error(`expected admitted static value: ${value.detail}`)
  return value.value
}

const staticSpan = SourceSpan.fromOffsets('static/evaluation', 4, 12)
if (staticSpan === undefined) throw new Error('expected a static evaluation span')

const staticArgument = admitted(
  StaticValue.admit({ _tag: 'IntegerValue', type: 'i32', value: 42n }, { pointerBits: 64 }),
)

const application = (
  name: string,
  value: StaticValue.Value = staticArgument,
): StaticEvaluation.Application =>
  Object.freeze({
    declaration: Object.freeze({
      _tag: 'CanonicalDeclarationId',
      module: 'example.static',
      name,
    }),
    typeArguments: Object.freeze(['i32']),
    evidence: Object.freeze(['Copy<i32>']),
    contractRow: Object.freeze(['result:i32']),
    staticArguments: Object.freeze([value]),
    span: staticSpan,
  })

it('keys and caches complete static applications by target and canonical values', () => {
  const evaluation = StaticEvaluation.make<string>(Target.x8664UnknownLinuxGnu)
  let calls = 0
  const callback: StaticEvaluation.EvaluationCallback<string> = (context) => {
    calls += 1
    assert.strictEqual(context.step(), undefined)
    assert.strictEqual(context.retain(staticArgument), undefined)
    return StaticEvaluation.complete('residual body')
  }
  const first = StaticEvaluation.evaluateApplication(evaluation, application('render'), callback)
  const second = StaticEvaluation.evaluateApplication(evaluation, application('render'), callback)

  assert.strictEqual(first._tag, 'Complete')
  assert.strictEqual(first.cached, false)
  assert.strictEqual(second._tag, 'Complete')
  assert.strictEqual(second.cached, true)
  assert.strictEqual(first.key, second.key)
  assert.strictEqual(calls, 1)
  assert.strictEqual(evaluation.environment.profile, 3)
  assert.deepEqual(StaticEvaluation.budget(evaluation), {
    steps: 1,
    callDepth: 0,
    maximumCallDepth: 1,
    retainedValueBytes: StaticValue.retainedSize(staticArgument),
    residualNodes: 0,
  })
  assert.strictEqual(StaticEvaluation.cacheEntries(evaluation).at(0)?.state._tag, 'Complete')

  const wasm = StaticEvaluation.make<string>(Target.wasm32UnknownUnknown)
  const wasmResult = StaticEvaluation.evaluateApplication(wasm, application('render'), callback)
  assert.notStrictEqual(first.key, wasmResult.key)
  assert.strictEqual(wasm.environment.profile, 2)
  assert.strictEqual(Object.isFrozen(wasm.environment), true)
  assert.strictEqual(Object.isFrozen(wasm.limits), true)
})

it('detects pending cycles with logical application and selected-arm frames', () => {
  const evaluation = StaticEvaluation.make<string>(Target.x8664UnknownLinuxGnu)
  const render = application('render')
  const result = StaticEvaluation.evaluateApplication(evaluation, render, (context) => {
    const selected = context.withTrace(StaticEvaluation.selectedArmFrame('Taken', staticSpan))
    const nested = selected.evaluate(render, () => StaticEvaluation.complete('unreachable'))
    return nested._tag === 'Failed'
      ? StaticEvaluation.failed(nested.failure)
      : StaticEvaluation.complete(nested.value)
  })

  assert.strictEqual(result._tag, 'Failed')
  if (result._tag === 'Failed') {
    assert.strictEqual(result.failure._tag, 'Cycle')
    assert.deepEqual(
      result.failure.trace.map((frame) => frame._tag),
      ['StaticApplicationFrame', 'SelectedStaticArmFrame', 'StaticApplicationFrame'],
    )
  }
  assert.strictEqual(StaticEvaluation.cacheEntries(evaluation).at(0)?.state._tag, 'Failed')
})

it('reports compile errors, phase violations, and four distinct deterministic limits', () => {
  const trace = Object.freeze([
    StaticEvaluation.selectedArmFrame('Otherwise', staticSpan),
    StaticEvaluation.staticTextFrame(staticSpan, 3),
  ])
  assert.strictEqual(
    StaticEvaluation.compileError('bad template', staticSpan, trace)._tag,
    'CompileError',
  )
  assert.strictEqual(
    StaticEvaluation.phaseViolation('call', 'ordinary function', staticSpan, trace)._tag,
    'PhaseViolation',
  )

  const limited = (
    policy: StaticEvaluation.Limits,
    callback: StaticEvaluation.EvaluationCallback<string>,
  ) => {
    const evaluation = StaticEvaluation.make<string>(Target.x8664UnknownLinuxGnu, policy)
    return StaticEvaluation.evaluateApplication(evaluation, application('limited'), callback)
  }
  const base = { steps: 10, callDepth: 10, retainedValueBytes: 10_000, residualNodes: 10 }
  const step = limited({ ...base, steps: 0 }, (context) => {
    context.step()
    return StaticEvaluation.complete('partial')
  })
  const retained = limited({ ...base, retainedValueBytes: 0 }, (context) => {
    context.retain(staticArgument)
    return StaticEvaluation.complete('partial')
  })
  const residual = limited({ ...base, residualNodes: 0 }, (context) => {
    context.growResidual()
    return StaticEvaluation.complete('partial')
  })
  const depth = limited({ ...base, callDepth: 1 }, (context) => {
    const nested = context.evaluate(application('nested'), () =>
      StaticEvaluation.complete('partial'),
    )
    return nested._tag === 'Failed'
      ? StaticEvaluation.failed(nested.failure)
      : StaticEvaluation.complete(nested.value)
  })

  const failureTag = (result: StaticEvaluation.ApplicationResult<string>): string =>
    result._tag === 'Failed' ? result.failure._tag : 'Complete'
  assert.deepEqual([step, depth, retained, residual].map(failureTag), [
    'StepLimit',
    'CallDepthLimit',
    'RetainedValueLimit',
    'ResidualGrowthLimit',
  ])
  assert.deepEqual(
    [step, depth, retained, residual].map((result) => {
      if (result._tag !== 'Failed' || !('limit' in result.failure)) return []
      return [result.failure.limit, result.failure.attempted]
    }),
    [
      [0, 1],
      [1, 2],
      [0, StaticValue.retainedSize(staticArgument)],
      [0, 1],
    ],
  )
})

it('canonicalizes finite static values without observing construction identity', () => {
  const left = admitted(
    StaticValue.admit(
      {
        _tag: 'AggregateValue',
        identity: {
          _tag: 'NominalAggregateIdentity',
          declaration: {
            _tag: 'CanonicalDeclarationId',
            module: 'example.values',
            name: 'Pair',
          },
          typeArguments: ['i32'],
        },
        fields: [
          { ordinal: 1, value: { _tag: 'TextValue', bytes: [...encoder.encode('hé')] } },
          { ordinal: 0, value: { _tag: 'IntegerValue', type: 'i32', value: 42n } },
        ],
      },
      { pointerBits: 64 },
    ),
  )
  const right = admitted(
    StaticValue.admit(
      {
        _tag: 'AggregateValue',
        identity: {
          _tag: 'NominalAggregateIdentity',
          declaration: {
            _tag: 'CanonicalDeclarationId',
            module: 'example.values',
            name: 'Pair',
          },
          typeArguments: ['i32'],
        },
        fields: [
          { ordinal: 0, value: { _tag: 'IntegerValue', type: 'i32', value: 42n } },
          { ordinal: 1, value: { _tag: 'TextValue', bytes: [...encoder.encode('hé')] } },
        ],
      },
      { pointerBits: 64 },
    ),
  )

  assert.strictEqual(StaticValue.encode(left), StaticValue.encode(right))
  assert.strictEqual(StaticValue.key(left), StaticValue.key(right))
  assert.strictEqual(StaticValue.equals(left, right), true)
  assert.strictEqual(
    StaticValue.retainedSize(left),
    encoder.encode(StaticValue.encode(left)).byteLength,
  )
  assert.strictEqual(
    StaticValue.presentation(left),
    'example.values.Pair<i32> { #0: 42i32, #1: "hé" }',
  )
  assert.strictEqual(Object.isFrozen(left), true)
  if (left._tag === 'AggregateValue') {
    assert.strictEqual(Object.isFrozen(left.identity), true)
    assert.strictEqual(Object.isFrozen(left.fields), true)
    assert.strictEqual(Object.isFrozen(left.fields.at(0)), true)
  }
})

it('normalizes float NaNs and rejects resource-bearing or malformed candidates', () => {
  const firstNaN = admitted(
    StaticValue.admit({ _tag: 'FloatValue', type: 'f32', bits: 0x7f80_0001n }, { pointerBits: 64 }),
  )
  const secondNaN = admitted(
    StaticValue.admit({ _tag: 'FloatValue', type: 'f32', bits: 0x7fff_ffffn }, { pointerBits: 64 }),
  )
  assert.strictEqual(StaticValue.encode(firstNaN), StaticValue.encode(secondNaN))
  assert.deepEqual(
    StaticValue.admit(
      { _tag: 'EffectValue', service: 'filesystem', handle: { ticket: 1 } },
      { pointerBits: 64 },
    ),
    {
      _tag: 'Rejected',
      reason: 'UnsupportedValue',
      path: [],
      detail: 'static evaluation does not admit EffectValue',
    },
  )
  const nestedResource = StaticValue.admit(
    {
      _tag: 'AggregateValue',
      identity: { _tag: 'ArrayAggregateIdentity', element: 'Resource', length: 1 },
      fields: [{ ordinal: 0, value: { _tag: 'BorrowValue', address: 42 } }],
    },
    { pointerBits: 64 },
  )
  assert.strictEqual(nestedResource._tag, 'Rejected')
  if (nestedResource._tag === 'Rejected') {
    assert.strictEqual(nestedResource.reason, 'UnsupportedValue')
    assert.deepEqual(nestedResource.path, [0])
  }
  assert.strictEqual(
    StaticValue.admit({ _tag: 'TextValue', bytes: [0xff] }, { pointerBits: 64 })._tag,
    'Rejected',
  )
  assert.strictEqual(
    StaticValue.admit(
      { _tag: 'IntegerValue', type: 'usize', value: 1n << 32n },
      { pointerBits: 32 },
    )._tag,
    'Rejected',
  )
})

it('evaluates real literal syntax with contextual scalar and target ranges', () => {
  const file = SourceFile.make(
    'static/literals',
    encoder.encode(`pub fn main() -> () {
  let unit = ()
  let boolean = true
  let character = 'é'
  let integer = -42
  let floating = -1.5
  let text = "hé"
  return ()
}`),
  )
  const parsed = Parser.parse(Lexer.lex(file))
  assert.deepEqual(parsed.lexicalDiagnostics, [])
  assert.deepEqual(parsed.parserDiagnostics, [])
  const all = syntaxNodes(parsed.root)
  const literal = (kind: SyntaxTree.Node['kind']): SyntaxTree.Node => {
    const found = all.find((node) => node.kind === kind)
    if (found === undefined) throw new Error(`expected ${kind}`)
    return found
  }
  const environment = StaticEvaluation.targetEnvironment(Target.x8664UnknownLinuxGnu)
  assert.strictEqual(
    completedValue(
      StaticEvaluation.evaluateLiteral(environment, file, literal('UnitExpression'), 'unit'),
    )._tag,
    'UnitValue',
  )
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluateLiteral(
        environment,
        file,
        literal('BooleanLiteralExpression'),
        'bool',
      ),
    ),
    { _tag: 'BooleanValue', value: true },
  )
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluateLiteral(
        environment,
        file,
        literal('CharacterLiteralExpression'),
        'char',
      ),
    ),
    { _tag: 'CharacterValue', value: 0xe9 },
  )
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluateLiteral(
        environment,
        file,
        literal('IntegerLiteralExpression'),
        'i8',
      ),
    ),
    { _tag: 'IntegerValue', type: 'i8', value: -42n },
  )
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluateLiteral(
        environment,
        file,
        literal('FloatingLiteralExpression'),
        'f32',
      ),
    ),
    { _tag: 'FloatValue', type: 'f32', bits: 0xbfc0_0000n },
  )
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluateLiteral(
        environment,
        file,
        literal('StaticTextLiteralExpression'),
        'string',
      ),
    ),
    { _tag: 'TextValue', bytes: [0x68, 0xc3, 0xa9] },
  )

  const wideFile = SourceFile.make(
    'static/target-range',
    encoder.encode('pub fn main() -> usize { return 4294967296 }'),
  )
  const wideParsed = Parser.parse(Lexer.lex(wideFile))
  const wideNode = syntaxNodes(wideParsed.root).find(
    (node) => node.kind === 'IntegerLiteralExpression',
  )
  if (wideNode === undefined) throw new Error('expected target-width integer')
  assert.strictEqual(
    StaticEvaluation.evaluateLiteral(
      StaticEvaluation.targetEnvironment(Target.wasm32UnknownUnknown),
      wideFile,
      wideNode,
      'usize',
    )._tag,
    'Failed',
  )
  assert.strictEqual(
    StaticEvaluation.evaluateLiteral(environment, wideFile, wideNode, 'usize')._tag,
    'Complete',
  )
})

it('evaluates checked primitive, enum, text, aggregate, and target-profile operations', () => {
  const environment = StaticEvaluation.targetEnvironment(Target.x8664UnknownLinuxGnu)
  const integer = (type: 'i8' | 'i32' | 'usize', value: bigint): StaticValue.Value =>
    admitted(StaticValue.admit({ _tag: 'IntegerValue', type, value }, { pointerBits: 64 }))
  const floating = (type: 'f32' | 'f64', value: number): StaticValue.Value => {
    const encoded = FloatingPoint.fromNumber(value, type === 'f32' ? 32 : 64)
    return admitted(
      StaticValue.admit({ _tag: 'FloatValue', type, bits: encoded.bits }, { pointerBits: 64 }),
    )
  }
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluatePrimitive(
        environment,
        'Multiply',
        [integer('i32', 6n), integer('i32', 7n)],
        staticSpan,
      ),
    ),
    { _tag: 'IntegerValue', type: 'i32', value: 42n },
  )
  assert.strictEqual(
    StaticEvaluation.evaluatePrimitive(
      environment,
      'Add',
      [integer('i8', 127n), integer('i8', 1n)],
      staticSpan,
    )._tag,
    'Failed',
  )
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluatePrimitive(
        environment,
        'Add',
        [floating('f32', 1.5), floating('f32', 2.25)],
        staticSpan,
      ),
    ),
    { _tag: 'FloatValue', type: 'f32', bits: 0x4070_0000n },
  )
  assert.deepEqual(
    completedValue(
      StaticEvaluation.evaluatePrimitive(
        environment,
        'Not',
        [StaticValue.boolean(true)],
        staticSpan,
      ),
    ),
    { _tag: 'BooleanValue', value: false },
  )

  const enumType = Object.freeze({
    _tag: 'CanonicalDeclarationId' as const,
    module: 'silk.target',
    name: 'Architecture',
  })
  const native = completedValue(
    StaticEvaluation.constructEnum(environment, enumType, 'X86_64', 'u8', 0n, staticSpan),
  )
  const wasm = completedValue(
    StaticEvaluation.constructEnum(environment, enumType, 'Wasm32', 'u8', 1n, staticSpan),
  )
  if (native._tag !== 'EnumValue' || wasm._tag !== 'EnumValue')
    throw new Error('expected scalar enum values')
  assert.deepEqual(
    completedValue(StaticEvaluation.evaluateEnumEquality('NotEquals', native, wasm, staticSpan)),
    { _tag: 'BooleanValue', value: true },
  )

  const text = admitted(
    StaticValue.admit(
      { _tag: 'TextValue', bytes: Array.from(encoder.encode('hé')) },
      { pointerBits: 64 },
    ),
  )
  if (text._tag !== 'TextValue') throw new Error('expected static text')
  assert.deepEqual(
    completedValue(StaticEvaluation.staticTextByteLength(environment, text, staticSpan)),
    {
      _tag: 'IntegerValue',
      type: 'usize',
      value: 3n,
    },
  )
  assert.deepEqual(
    completedValue(StaticEvaluation.staticTextByteAt(environment, text, 1n, staticSpan)),
    {
      _tag: 'IntegerValue',
      type: 'u8',
      value: 0xc3n,
    },
  )
  assert.deepEqual(
    completedValue(StaticEvaluation.staticTextSlice(environment, text, 1n, 3n, staticSpan)),
    { _tag: 'TextValue', bytes: [0xc3, 0xa9] },
  )
  const splitScalar = StaticEvaluation.staticTextSlice(environment, text, 1n, 2n, staticSpan)
  assert.strictEqual(splitScalar._tag, 'Failed')
  if (splitScalar._tag === 'Failed') {
    assert.strictEqual(splitScalar.failure._tag, 'PhaseViolation')
    assert.strictEqual(splitScalar.failure.trace.at(-1)?._tag, 'StaticTextFrame')
    const diagnostic = StaticEvaluation.diagnostic(splitScalar.failure, environment.target)
    if (diagnostic.reason._tag !== 'StaticPhaseViolation')
      throw new Error('expected a static phase diagnostic')
    assert.deepEqual(diagnostic.reason.trace.at(-1), {
      kind: 'StaticText',
      label: 'static text byte 1',
      arguments: ['byteOffset=1'],
      span: staticSpan,
    })
  }

  const aggregate = completedValue(
    StaticEvaluation.constructAggregate(
      environment,
      { _tag: 'ArrayAggregateIdentity', element: 'i32', length: 2 },
      [
        { ordinal: 0, value: integer('i32', 4n) },
        { ordinal: 1, value: integer('i32', 2n) },
      ],
      staticSpan,
    ),
  )
  assert.strictEqual(aggregate._tag, 'AggregateValue')
  assert.deepEqual(completedValue(StaticEvaluation.targetProfile(environment, staticSpan)), {
    _tag: 'IntegerValue',
    type: 'u8',
    value: 3n,
  })
  assert.deepEqual(
    completedValue(
      StaticEvaluation.targetProfile(
        StaticEvaluation.targetEnvironment(Target.aarch64AppleDarwin),
        staticSpan,
      ),
    ),
    { _tag: 'IntegerValue', type: 'u8', value: 0n },
  )
})

it.effect('selects one target arm before runtime evaluation', () =>
  Effect.gen(function* () {
    const targetConditional = `import silk.target { Arch, arch }

pub fn main() -> i32 {
  static if arch() == Arch.Wasm32 {
    return 42
  } else {
    return 7
  }
}`
    const wasm = yield* Analysis.ofSourceRealized(
      'static/target',
      encoder.encode(targetConditional),
      Target.wasm32UnknownUnknown.id,
    )
    const native = yield* Analysis.ofSourceRealized(
      'static/target',
      encoder.encode(targetConditional),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    assert.deepEqual(Analysis.diagnostics(native), [])
    const wasmOutcome = Analysis.evaluate(wasm)
    const nativeOutcome = Analysis.evaluate(native)
    assert.strictEqual(wasmOutcome._tag, 'Completed')
    assert.strictEqual(nativeOutcome._tag, 'Completed')
    if (wasmOutcome._tag === 'Completed') assert.strictEqual(wasmOutcome.result.value, 42n)
    if (nativeOutcome._tag === 'Completed') assert.strictEqual(nativeOutcome.result.value, 7n)
  }),
)

it.effect('maps every canonical target profile through the ordinary target actor', () =>
  Effect.gen(function* () {
    const program = `import silk.target { Profile, profile }

pub fn main() -> i32 {
  static if profile() == Profile.Aarch64AppleDarwin { return 10 } else {
    static if profile() == Profile.Aarch64UnknownLinuxGnu { return 20 } else {
      static if profile() == Profile.Wasm32UnknownUnknown { return 30 } else {
        static if profile() == Profile.X86_64UnknownLinuxGnu { return 40 } else {
          compileError("unknown target profile")
        }
      }
    }
  }
}`
    const cases = [
      [Target.aarch64AppleDarwin.id, 10n],
      [Target.aarch64UnknownLinuxGnu.id, 20n],
      [Target.wasm32UnknownUnknown.id, 30n],
      [Target.x8664UnknownLinuxGnu.id, 40n],
    ] as const
    for (const [target, expected] of cases) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `static/profile/${target}`,
        encoder.encode(program),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const outcome = Analysis.evaluate(snapshot)
      assert.strictEqual(outcome._tag, 'Completed')
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, expected)
    }
  }),
)

it.effect('specializes mixed functions by canonical static arguments', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/mixed',
      encoder.encode(`fn choose(static selected: bool, value: i32) -> i32 {
  static if selected { return value } else { return 0 }
}

pub fn main() -> i32 {
  let left = choose(true, 20)
  let right = choose(true, 22)
  let ignored = choose(false, 99)
  return left + right + ignored
}`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const selected = Analysis.instancesOf(snapshot).instances.filter(
      (instance) => instance.key.declaration.name === 'choose',
    )
    assert.strictEqual(selected.length, 2)
    assert.deepEqual(
      selected
        .map((instance) => instance.key.staticArguments.map(StaticValue.presentation))
        .sort((left, right) => left.join().localeCompare(right.join())),
      [['false'], ['true']],
    )
    assert.isTrue(selected.every((instance) => instance.function.statements.length === 1))
    assert.isTrue(
      selected.every(
        (instance) =>
          instance.ownership.bindings.every(
            (binding) => binding.name !== 'selected' && binding.name !== 'ignored',
          ) && instance.ownership.verdict._tag === 'Satisfied',
      ),
    )
  }),
)

it.effect('caches real residual applications and enforces their growth budget', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/residual-budget',
      encoder.encode(`fn choose(static selected: bool, value: i32) -> i32 {
  static if selected { return value } else { return 0 }
}

pub fn main() -> i32 { return choose(true, 42) }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    if (snapshot.target._tag !== 'Resolved') throw new Error('expected selected target')
    const selected = Analysis.instancesOf(snapshot).instances.find(
      (instance) => instance.key.declaration.name === 'choose',
    )
    if (selected === undefined) throw new Error('expected selected residual instance')
    const application: Residualization.ApplicationKey = Object.freeze({
      declaration: selected.key.declaration,
      typeArguments: selected.key.typeArguments.map(Type.genericArgumentKey),
      evidence: selected.key.evidence,
      contractRow: selected.key.contractRow,
      staticArguments: selected.key.staticArguments,
    })
    const coordinator = Residualization.make(
      snapshot.target.target,
      snapshot.results,
      snapshot.resolution,
      snapshot.index,
    )
    const first = Residualization.residualize(coordinator, application)
    const second = Residualization.residualize(coordinator, application)
    assert.strictEqual(first._tag, 'ResidualBody')
    assert.strictEqual(second, first)

    const limited = Residualization.make(
      snapshot.target.target,
      snapshot.results,
      snapshot.resolution,
      snapshot.index,
      { ...StaticEvaluation.defaultLimits, residualNodes: 0 },
    )
    const failed = Residualization.residualize(limited, application)
    assert.strictEqual(failed._tag, 'StaticFailure')
    if (failed._tag === 'StaticFailure')
      assert.strictEqual(failed.failure._tag, 'ResidualGrowthLimit')
  }),
)

it.effect('commits deterministic residual, specialization, ownership, and cleanup encodings', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/encoding',
      encoder.encode(`fn choose(static selected: bool, value: i32) -> i32 {
  static if selected {
    let result = value + 1
    return result
  } else {
    return 0
  }
}

pub fn main() -> i32 { return choose(true, 41) }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const selected = Analysis.instancesOf(snapshot).instances.find(
      (instance) => instance.key.declaration.name === 'choose',
    )
    if (selected === undefined) throw new Error('expected selected residual instance')
    const hir = Hir.encode(
      Object.freeze({
        _tag: 'HirModule',
        module: 'static/encoding',
        functions: Object.freeze([selected.function]),
      }),
    )
    const ownership = OwnershipEncoding.encode(
      Object.freeze({
        _tag: 'OwnershipFacts',
        module: 'static/encoding',
        functions: Object.freeze([selected.ownership]),
        diagnostics: Object.freeze([]),
      }),
    )
    assert.strictEqual(
      sha256(Instances.keyText(selected.key)),
      'eec2210666e5c049b8352f5c4c07c94c97b0492768591d37ebfd4a044fcb3926',
    )
    assert.strictEqual(
      sha256(hir),
      'bd2543084bec93081c83fffe9526d5b812bfb0db7694ec495e31091f942a44a4',
    )
    assert.strictEqual(
      sha256(ownership),
      'a51911b1ecff05afe136f413123e590bd35d22cd6210ca98f5388556c13bf398',
    )

    const alternateEvidence = Object.freeze({
      ...selected.key,
      evidence: Object.freeze([...selected.key.evidence, 'SelectedEvidence']),
    })
    assert.notStrictEqual(Instances.keyText(alternateEvidence), Instances.keyText(selected.key))
  }),
)

it.effect('embeds a directly consumed static function result without a runtime instance', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/direct-result',
      encoder.encode(`static fn answer() -> i32 { return 42 }

pub fn main() -> i32 { return answer() }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    assert.isFalse(
      Analysis.instancesOf(snapshot).instances.some(
        (instance) => instance.key.declaration.name === 'answer',
      ),
    )
  }),
)

it.effect('constructs and embeds a recursively pure static aggregate', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/aggregate-result',
      encoder.encode(`struct Pair { left: i32 right: i32 }

static fn pair() -> Pair { return Pair { left: 20, right: 22 } }

fn sum(value: Pair) -> i32 { return value.left + value.right }

pub fn main() -> i32 { return sum(pair()) }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('inspects static UTF-8 text through ordinary source wrappers', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/text-inspection',
      encoder.encode(`import silk.static_text { byteLength, byteAt, slice }

static fn inspect(value: string) -> bool {
  if byteLength(value) != 3 { return false }
  if byteAt(value, 0) != 104 { return false }
  let static suffix = slice(value, 1, 3)
  if byteLength(suffix) != 2 { return false }
  return true
}

fn choose(static valid: bool) -> i32 {
  static if valid { return 42 } else { return 0 }
}

pub fn main() -> i32 { return choose(inspect("hé")) }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect(
  'executes static mutation by complete-value replacement and erases inactive failures',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'static/accumulator',
        encoder.encode(`static fn sum(limit: i32) -> i32 {
  let mut total = 0
  let mut cursor = 0
  while cursor < limit {
    total = total + cursor
    cursor = cursor + 1
  }
  return total
}

fn guarded(static enabled: bool, value: i32) -> i32 {
  static if enabled { return value } else { compileError("guard disabled") }
}

pub fn main() -> i32 {
  let static offset = sum(7)
  return guarded(true, offset * 2)
}`),
        Target.x8664UnknownLinuxGnu.id,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const outcome = Analysis.evaluate(snapshot)
      assert.strictEqual(outcome._tag, 'Completed')
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
      assert.isFalse(
        Analysis.instancesOf(snapshot).instances.some(
          (instance) => instance.key.declaration.name === 'sum',
        ),
      )
    }),
)

it.effect(
  'reports selected static failures and runtime phase crossings at their source spans',
  () =>
    Effect.gen(function* () {
      const selectedFailure = `fn guarded(static enabled: bool) -> i32 {
  static if enabled { return 42 } else { compileError("guard disabled") }
}

pub fn main() -> i32 { return guarded(false) }`
      const failed = yield* Analysis.ofSourceRealized(
        'static/selected-failure',
        encoder.encode(selectedFailure),
        Target.x8664UnknownLinuxGnu.id,
      )
      const compileErrorStart = selectedFailure.indexOf('compileError')
      assert.deepEqual(
        Analysis.diagnostics(failed).map((diagnostic) => ({
          code: diagnostic.code,
          sourceId: diagnostic.span.sourceId,
          start: diagnostic.span.start,
          end: diagnostic.span.end,
        })),
        [
          {
            code: 'SEM0177',
            sourceId: 'static/selected-failure',
            start: compileErrorStart - 1,
            end: compileErrorStart + 'compileError("guard disabled")'.length,
          },
        ],
      )

      const phaseCrossing = `fn invalid(static enabled: bool, runtime: bool) -> i32 {
  static if runtime { return 1 } else { return 0 }
}

pub fn main() -> i32 { return invalid(true, false) }`
      const crossed = yield* Analysis.ofSourceRealized(
        'static/phase-crossing',
        encoder.encode(phaseCrossing),
        Target.x8664UnknownLinuxGnu.id,
      )
      const runtimeStart = phaseCrossing.indexOf('runtime {')
      assert.deepEqual(
        Analysis.diagnostics(crossed).map((diagnostic) => ({
          code: diagnostic.code,
          sourceId: diagnostic.span.sourceId,
          start: diagnostic.span.start,
          end: diagnostic.span.end,
        })),
        [
          {
            code: 'SEM0176',
            sourceId: 'static/phase-crossing',
            start: runtimeStart - 1,
            end: runtimeStart + 'runtime'.length,
          },
        ],
      )
    }),
)

it.effect('retains mixed specialization, selected arm, and helper call in static traces', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/nested-trace',
      encoder.encode(`static fn reject(value: i32) -> i32 { compileError("nested") }

fn choose(static enabled: bool) -> i32 {
  static if enabled { let static result = reject(1) return result }
  else { return 0 }
}

pub fn main() -> i32 { return choose(true) }`),
      Target.wasm32UnknownUnknown.id,
    )
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'SEM0177',
    )
    assert.isDefined(diagnostic)
    if (diagnostic?.reason._tag !== 'SelectedCompileError')
      throw new Error('expected selected compile error')
    assert.deepEqual(
      diagnostic.reason.trace.map((frame) => ({ kind: frame.kind, label: frame.label })),
      [
        { kind: 'Call', label: 'static/nested-trace.choose' },
        { kind: 'SelectedArm', label: 'selected static if arm' },
        { kind: 'Call', label: 'static/nested-trace.reject' },
      ],
    )
  }),
)

it.effect('preserves nested static cycle classification through static calls', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/nested-cycle',
      encoder.encode(`static fn recurse(value: i32) -> i32 { return recurse(value) }

pub fn main() -> i32 { return recurse(42) }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0178'],
    )
    const diagnostic = diagnostics.at(0)
    if (diagnostic?.reason._tag !== 'StaticEvaluationCycle')
      throw new Error('expected static evaluation cycle')
    assert.deepEqual(
      diagnostic.reason.trace.map((frame) => frame.label),
      ['static/nested-cycle.main', 'static/nested-cycle.recurse', 'static/nested-cycle.recurse'],
    )
  }),
)

it.effect('preserves static text provenance through nested source wrappers', () =>
  Effect.gen(function* () {
    const nestedTextFailure = `import silk.static_text { byteAt }

static fn inspect() -> bool { return byteAt("é", 3) == 0 }

pub fn main() -> i32 {
  static if inspect() { return 42 } else { return 0 }
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/nested-text-failure',
      encoder.encode(nestedTextFailure),
      Target.x8664UnknownLinuxGnu.id,
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0176'],
    )
    const diagnostic = diagnostics.at(0)
    if (diagnostic?.reason._tag !== 'StaticPhaseViolation')
      throw new Error('expected static phase violation')
    assert.deepEqual(
      diagnostic.reason.trace.map((frame) => frame.kind),
      ['Call', 'Call', 'Call', 'StaticText'],
    )
    const staticTextFrame = diagnostic.reason.trace.at(-1)
    assert.strictEqual(staticTextFrame?.label, 'static text byte 3')
    const literalStart = nestedTextFailure.indexOf('"é"')
    assert.strictEqual(staticTextFrame?.span.sourceId, 'static/nested-text-failure')
    assert.strictEqual(staticTextFrame?.span.start, literalStart)
    assert.strictEqual(staticTextFrame?.span.end, literalStart + encoder.encode('"é"').byteLength)
  }),
)

it.effect('preserves static text provenance through returned text and source composition', () =>
  Effect.gen(function* () {
    const cases = [
      {
        sourceId: 'static/provenance-identity',
        literal: '"é"',
        source: `import silk.static_text { byteAt }

static fn identity(value: string) -> string { return value }

static fn inspect() -> bool {
  let result = identity("é")
  return byteAt(result, 3) == 0
}

pub fn main() -> i32 { static if inspect() { return 42 } else { return 0 } }`,
      },
      {
        sourceId: 'static/provenance-slice',
        literal: '"é"',
        source: `import silk.static_text { byteAt, slice }

static fn inspect() -> bool {
  let result = slice("é", 0, 2)
  return byteAt(result, 3) == 0
}

pub fn main() -> i32 { static if inspect() { return 42 } else { return 0 } }`,
      },
      {
        sourceId: 'static/provenance-ambiguous',
        literal: '"é"',
        source: `import silk.static_text { byteAt }

static fn second(first: string, value: string) -> string { return value }

static fn inspect() -> bool {
  let result = second("éx", "é")
  return byteAt(result, 3) == 0
}

pub fn main() -> i32 { static if inspect() { return 42 } else { return 0 } }`,
      },
      {
        sourceId: 'static/provenance-cached-ascii',
        literal: '"abc"',
        source: `import silk.static_text { byteAt }

static fn second(first: string, value: string) -> string { return value }

static fn inspect() -> bool {
  let a = second("abcx", "abc")
  let ok = byteAt(a, 0) == 97
  let b = second("abcx", "abc")
  return ok && byteAt(b, 9) == 0
}

pub fn main() -> i32 { static if inspect() { return 42 } else { return 0 } }`,
      },
    ] as const
    for (const testCase of cases) {
      const snapshot = yield* Analysis.ofSourceRealized(
        testCase.sourceId,
        encoder.encode(testCase.source),
        Target.x8664UnknownLinuxGnu.id,
      )
      const diagnostic = Analysis.diagnostics(snapshot).at(0)
      assert.strictEqual(diagnostic?.code, 'SEM0176')
      if (diagnostic?.reason._tag !== 'StaticPhaseViolation')
        throw new Error('expected static phase violation')
      const frame = diagnostic.reason.trace.at(-1)
      const literalCharacterStart = testCase.source.lastIndexOf(testCase.literal)
      const literalStart = encoder.encode(
        testCase.source.slice(0, literalCharacterStart),
      ).byteLength
      const literalEnd = encoder.encode(
        testCase.source.slice(0, literalCharacterStart + testCase.literal.length),
      ).byteLength
      assert.strictEqual(frame?.kind, 'StaticText')
      assert.strictEqual(frame?.span.sourceId, testCase.sourceId)
      assert.strictEqual(frame?.span.start, literalStart)
      assert.strictEqual(frame?.span.end, literalEnd)
    }
  }),
)

it.effect('retains ownership evidence for an unavailable selected residual specialization', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/unavailable-residual',
      encoder.encode(`fn choose(static selected: bool) -> i32 {
  static if selected { return missing() } else { return 0 }
}

pub fn main() -> i32 { return choose(true) }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0004'],
    )
    const discovery = Analysis.instancesOf(snapshot)
    assert.deepEqual(
      discovery.instances.map((instance) => instance.key.declaration.name),
      ['main'],
    )
    assert.strictEqual(discovery.unavailableOwnership.length, 1)
    const unavailable = discovery.unavailableOwnership.at(0)
    assert.strictEqual(unavailable?.key.declaration.name, 'choose')
    assert.deepEqual(unavailable?.key.staticArguments, [{ _tag: 'BooleanValue', value: true }])
    assert.strictEqual(unavailable?.ownership.verdict._tag, 'Unavailable')
    if (unavailable?.ownership.verdict._tag === 'Unavailable')
      assert.isDefined(unavailable.ownership.verdict.cause)
  }),
)

it('lexes and parses literal spelling losslessly', () => {
  const file = SourceFile.make('static/lossless', encoder.encode('"hé\\n" b"life\\x0a"'))
  const lexical = Lexer.lex(file)
  assert.deepEqual(
    lexical.tokens.map((token) => token.kind),
    ['TextLiteral', 'Whitespace', 'ByteStringLiteral', 'EndOfFile'],
  )
  assert.deepEqual(lexical.diagnostics, [])

  const parsed = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'static/parse',
        encoder.encode('pub fn main() -> i32 { let value = "hé\\n" return value.length }'),
      ),
    ),
  )
  const literals = syntaxNodes(parsed.root).filter(
    (node) => node.kind === 'StaticTextLiteralExpression',
  )
  assert.strictEqual(literals.length, 1)
  assert.deepEqual(parsed.lexicalDiagnostics, [])
  assert.deepEqual(parsed.parserDiagnostics, [])
})

it('decodes UTF-8 and exact bytes atomically', () => {
  const textSpelling = '"hé\\n\\u{1f642}"'
  const text = StaticText.decode(Array.from(encoder.encode(textSpelling)), formOf(textSpelling))
  assert.strictEqual(text._tag, 'Decoded')
  if (text._tag === 'Decoded') {
    assert.deepEqual(text.data.bytes, Array.from(encoder.encode('hé\n🙂')))
    assert.strictEqual(text.data.utf8, true)
  }
  const byteSpelling = 'b"\\x00\\xff"'
  assert.deepEqual(
    StaticText.decode(Array.from(encoder.encode(byteSpelling)), formOf(byteSpelling)),
    {
      _tag: 'Decoded',
      data: {
        _tag: 'StaticData',
        id: 'bytes:00ff',
        kind: 'Bytes',
        bytes: [0, 255],
        utf8: false,
      },
    },
  )
  assert.strictEqual(
    StaticText.decode(Array.from(encoder.encode('b"\\u{100}"')), formOf('b"\\u{100}"'))._tag,
    'Invalid',
  )
  assert.strictEqual(
    StaticText.decode(Array.from(encoder.encode('"\\q"')), formOf('"\\q"'))._tag,
    'Invalid',
  )
})

it('decodes multiline content exactly without dedenting and normalizes only physical CRLF', () => {
  const spelling = '"""\r\n  first  \r\n second\n"""'
  const decoded = StaticText.decode(Array.from(encoder.encode(spelling)), formOf(spelling))
  assert.strictEqual(decoded._tag, 'Decoded')
  if (decoded._tag === 'Decoded') {
    assert.deepEqual(decoded.data.bytes, Array.from(encoder.encode('\n  first  \n second\n')))
  }

  const explicit = 'b"""\\r\\n"""'
  const explicitDecoded = StaticText.decode(Array.from(encoder.encode(explicit)), formOf(explicit))
  assert.strictEqual(explicitDecoded._tag, 'Decoded')
  if (explicitDecoded._tag === 'Decoded') assert.deepEqual(explicitDecoded.data.bytes, [13, 10])

  const continuation = '"""before\\\nafter"""'
  assert.strictEqual(
    StaticText.decode(Array.from(encoder.encode(continuation)), formOf(continuation))._tag,
    'Invalid',
  )
})

it('parses all four literal forms as pipeline operands', () => {
  const parsed = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'static/pipelines',
        encoder.encode(`import silk.string { String }
pub fn main() -> i32 {
  let a = "abc" |> String.uppercase
  let b = b"abc" |> String.uppercase
  let c = """abc""" |> String.uppercase
  let d = b"""abc""" |> String.uppercase
  return 0
}`),
      ),
    ),
  )
  const nodes = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
    Object.freeze([
      node,
      ...node.children.flatMap((child) => (SyntaxTree.isNode(child) ? nodes(child) : [])),
    ])
  assert.strictEqual(
    nodes(parsed.root).filter((node) => node.kind === 'PipelineExpression').length,
    4,
  )
  assert.deepEqual(parsed.lexicalDiagnostics, [])
  assert.deepEqual(parsed.parserDiagnostics, [])
})

it.effect('recovers malformed escapes before the following declaration', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/recovery',
      encoder.encode(`fn broken() -> i32 { let bad = "\\q" return 0 }
pub fn main() -> i32 { return 42 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0085'],
    )
    assert.strictEqual(Projections.hirOf(snapshot, 'static/recovery')?.functions.length, 2)
  }),
)

it.effect('keeps lexical literal sentinels out of parser and semantic diagnostic cascades', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/lexical-sentinel',
      encoder.encode(`pub fn main() -> i32 {
  let bad = future"value"
  return 42
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['LEX0002'],
    )
    assert.strictEqual(Projections.hirOf(snapshot, 'static/lexical-sentinel')?.functions.length, 1)
  }),
)

it.effect('keeps static bytes, reuse, lengths, and backend placement in parity', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(
      'static/parity',
      encoder.encode(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    const mir = Analysis.loweredMir(native)
    assert.deepEqual(
      mir.staticData?.map((data) => ({ id: data.id, bytes: data.bytes })),
      [
        { id: 'bytes:6c6966650a', bytes: [108, 105, 102, 101, 10] },
        { id: 'text:68c3a90a', bytes: [104, 195, 169, 10] },
      ],
    )
    assert.isFalse(
      mir.functions.some((fn) =>
        MirVerification.operations(fn).some((operation) => operation._tag === 'Allocate'),
      ),
    )
    assert.strictEqual(native.layout._tag, 'Available')
    if (native.layout._tag === 'Available') {
      assert.deepEqual(
        native.layout.value.staticData?.map((placement) => placement.lengthBits),
        [64, 64],
      )
    }
    const evaluated = Analysis.evaluate(native)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        {
          evaluated,
          staticData: native.layout._tag === 'Available' ? native.layout.value.staticData : [],
        },
        (_, value) => (typeof value === 'bigint' ? value.toString() : value),
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, 'constant [5 x i8] c"life\\0A"')

    const wasm = yield* Analysis.ofSourceRealized(
      'static/parity-wasm',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    assert.strictEqual(wasm.layout._tag, 'Available')
    if (wasm.layout._tag === 'Available') {
      assert.deepEqual(
        wasm.layout.value.staticData?.map((placement) => placement.lengthBits),
        [32, 32],
      )
    }
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.include(artifact.wat, '(data')
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new Error('static-text program lost silk_main')
    assert.strictEqual(main(), 42)
  }),
)
