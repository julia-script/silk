import { createHash } from 'node:crypto'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as Hir from '../src/Hir.js'
import * as Instances from '../src/Instances.js'
import * as Lexer from '../src/Lexer.js'
import * as LiteralForm from '../src/LiteralForm.js'
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

it('retains static text provenance without adding it to canonical identity', () => {
  const bytes = Array.from(encoder.encode('template'))
  const originOrdinal = (value: StaticValue.Value | undefined): number | undefined =>
    value?._tag === 'TextValue' && value.origin?._tag === 'ParameterTextOrigin'
      ? value.origin.ordinal
      : undefined
  const left = admitted(
    StaticValue.admit(
      {
        _tag: 'TextValue',
        bytes,
        origin: StaticEvaluation.parameterTextOrigin(0, bytes.length),
      },
      { pointerBits: 64 },
    ),
  )
  const right = admitted(
    StaticValue.admit(
      {
        _tag: 'TextValue',
        bytes,
        origin: StaticEvaluation.parameterTextOrigin(3, bytes.length),
      },
      { pointerBits: 64 },
    ),
  )
  assert.strictEqual(StaticValue.equals(left, right), true)
  assert.strictEqual(StaticValue.key(left), StaticValue.key(right))
  assert.strictEqual(originOrdinal(left), 0)
  assert.strictEqual(originOrdinal(right), 3)

  const aggregate = admitted(
    StaticValue.admit(
      {
        _tag: 'AggregateValue',
        identity: {
          _tag: 'NominalAggregateIdentity',
          declaration: {
            _tag: 'CanonicalDeclarationId',
            module: 'example.values',
            name: 'TemplatePart',
          },
          typeArguments: [],
        },
        fields: [{ ordinal: 0, value: left }],
      },
      { pointerBits: 64 },
    ),
  )
  const nested = aggregate._tag === 'AggregateValue' ? aggregate.fields.at(0)?.value : undefined
  assert.strictEqual(originOrdinal(nested), 0)
})

it('canonicalizes nominal reflection descriptors and heterogeneous field collections', () => {
  const owner = {
    _tag: 'TypeDescriptorValue',
    owner: Type.nominal('example.reflection', 'Person'),
    kind: 'Named',
  }
  const authorization = {
    _tag: 'CanonicalDeclarationId',
    module: 'example.format',
    name: 'render',
  }
  const fields = [
    {
      _tag: 'FieldDescriptorValue',
      owner,
      declarationOrdinal: 1,
      member: { _tag: 'LabeledField', label: 'age' },
      valueType: 'u32',
      authorization,
      provenance: { sourceId: 'example/reflection.silk', start: 28, end: 36 },
    },
    {
      _tag: 'FieldDescriptorValue',
      owner,
      declarationOrdinal: 0,
      member: { _tag: 'LabeledField', label: 'name' },
      valueType: 'string',
      authorization,
      provenance: { sourceId: 'example/reflection.silk', start: 12, end: 24 },
    },
  ]
  const collection = admitted(
    StaticValue.admit({ _tag: 'FieldCollectionValue', owner, fields }, { pointerBits: 64 }),
  )
  const equivalent = admitted(
    StaticValue.admit(
      { _tag: 'FieldCollectionValue', owner, fields: [...fields].reverse() },
      { pointerBits: 64 },
    ),
  )

  assert.strictEqual(collection._tag, 'FieldCollectionValue')
  if (collection._tag !== 'FieldCollectionValue') return
  assert.deepEqual(
    collection.fields.map((field) => field.declarationOrdinal),
    [0, 1],
  )
  assert.strictEqual(Object.isFrozen(collection), true)
  assert.strictEqual(Object.isFrozen(collection.owner), true)
  assert.strictEqual(Object.isFrozen(collection.fields), true)
  assert.strictEqual(Object.isFrozen(collection.fields.at(0)), true)
  assert.strictEqual(StaticValue.equals(collection, equivalent), true)
  assert.strictEqual(
    StaticValue.presentation(collection),
    'fields<example.reflection.Person>[field<example.reflection.Person, string>(name@0), field<example.reflection.Person, u32>(age@1)]',
  )
  assert.strictEqual(
    StaticValue.retainedSize(collection),
    encoder.encode(StaticValue.encode(collection)).byteLength,
  )

  const distinctOwner = admitted(
    StaticValue.admit(
      {
        _tag: 'TypeDescriptorValue',
        owner: Type.nominal('example.reflection', 'OtherPerson'),
        kind: 'Named',
      },
      { pointerBits: 64 },
    ),
  )
  assert.strictEqual(StaticValue.equals(collection.owner, distinctOwner), false)
  assert.strictEqual(
    StaticValue.admit(
      { _tag: 'FieldCollectionValue', owner: distinctOwner, fields },
      { pointerBits: 64 },
    )._tag,
    'Rejected',
  )
  assert.strictEqual(
    StaticValue.admit(
      {
        _tag: 'TypeDescriptorValue',
        owner: Object.freeze({ _tag: 'NominalType' }),
        kind: 'Named',
      },
      { pointerBits: 64 },
    )._tag,
    'Rejected',
  )
  assert.strictEqual(
    StaticValue.admit(
      {
        _tag: 'TypeDescriptorValue',
        owner: Object.freeze({
          _tag: 'NominalType',
          module: 'example.reflection',
          name: 'MutableArguments',
          arguments: [],
        }),
        kind: 'Named',
      },
      { pointerBits: 64 },
    )._tag,
    'Rejected',
  )
})

it('builds immutable homogeneous static sequences by complete replacement', () => {
  const empty = StaticValue.emptySequence('i32')
  const first = admitted(
    StaticValue.admit({ _tag: 'IntegerValue', type: 'i32', value: 1n }, { pointerBits: 64 }),
  )
  const second = admitted(
    StaticValue.admit({ _tag: 'IntegerValue', type: 'i32', value: 2n }, { pointerBits: 64 }),
  )
  const one = StaticValue.appendSequence(empty, 'i32', first)
  assert.notStrictEqual(one, undefined)
  if (one === undefined) return
  const two = StaticValue.appendSequence(one, 'i32', second)
  assert.notStrictEqual(two, undefined)
  if (two === undefined) return
  const combined = StaticValue.concatenateSequences(one, one)
  assert.notStrictEqual(combined, undefined)
  if (combined === undefined) return

  assert.strictEqual(StaticValue.sequenceLength(empty), 0)
  assert.strictEqual(StaticValue.sequenceLength(one), 1)
  assert.strictEqual(StaticValue.sequenceLength(two), 2)
  assert.strictEqual(StaticValue.sequenceElement(two, 0), first)
  assert.strictEqual(StaticValue.sequenceElement(two, 1), second)
  assert.strictEqual(StaticValue.sequenceElement(two, -1), undefined)
  assert.strictEqual(StaticValue.sequenceElement(two, 2), undefined)
  assert.deepEqual(combined.elements, [first, first])
  assert.strictEqual(Object.isFrozen(empty), true)
  assert.strictEqual(Object.isFrozen(two.elements), true)
  assert.deepEqual(empty.elements, [])
  assert.deepEqual(one.elements, [first])
  assert.strictEqual(StaticValue.appendSequence(one, 'u32', second), undefined)
  assert.strictEqual(
    StaticValue.concatenateSequences(one, StaticValue.emptySequence('u32')),
    undefined,
  )

  const pair = admitted(
    StaticValue.admit(
      {
        _tag: 'AggregateValue',
        identity: {
          _tag: 'NominalAggregateIdentity',
          declaration: {
            _tag: 'CanonicalDeclarationId',
            module: 'example.sequence',
            name: 'Pair',
          },
          typeArguments: ['builtin:i32'],
        },
        fields: [
          { ordinal: 0, value: { _tag: 'IntegerValue', type: 'i32', value: 1n } },
          { ordinal: 1, value: { _tag: 'IntegerValue', type: 'i32', value: 2n } },
        ],
      },
      { pointerBits: 64 },
    ),
  )
  const aggregateSequence = StaticValue.appendSequence(
    StaticValue.emptySequence(Type.nominal('example.sequence', 'Pair', ['i32'])),
    Type.nominal('example.sequence', 'Pair', ['i32']),
    pair,
  )
  assert.notStrictEqual(aggregateSequence, undefined)
  if (aggregateSequence === undefined) return
  assert.strictEqual(StaticValue.sequenceElement(aggregateSequence, 0), pair)
  assert.strictEqual(
    StaticValue.equals(
      aggregateSequence,
      admitted(StaticValue.admit(aggregateSequence, { pointerBits: 64 })),
    ),
    true,
  )

  const readmitted = admitted(StaticValue.admit(two, { pointerBits: 64 }))
  assert.strictEqual(StaticValue.equals(two, readmitted), true)
  assert.strictEqual(StaticValue.presentation(two), 'sequence<i32>[1i32, 2i32]')
  assert.strictEqual(
    StaticValue.retainedSize(two),
    encoder.encode(StaticValue.encode(two)).byteLength,
  )

  const cyclicElements: Array<unknown> = []
  const cyclic = {
    _tag: 'StaticSequenceValue',
    elementType: 'i32',
    elements: cyclicElements,
  }
  cyclicElements.push(cyclic)
  const rejectedCycle = StaticValue.admit(cyclic, { pointerBits: 64 })
  assert.strictEqual(rejectedCycle._tag, 'Rejected')
  if (rejectedCycle._tag === 'Rejected') {
    assert.strictEqual(rejectedCycle.reason, 'CyclicValue')
    assert.deepEqual(rejectedCycle.path, [0])
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
    completedValue(StaticEvaluation.staticTextConcat(environment, text, text, staticSpan)),
    { _tag: 'TextValue', bytes: [0x68, 0xc3, 0xa9, 0x68, 0xc3, 0xa9] },
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

it.effect('rejects a runtime static iterable before elaborating its body', () =>
  Effect.gen(function* () {
    const source = `fn invalid(values: [i32; 1]) -> i32 {
  static for value in values {
    compileError("the rejected body must stay untouched")
  }
  return 42
}

pub fn main() -> i32 { return invalid([1]) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/runtime-iteration',
      encoder.encode(source),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0176'],
    )
    assert.isFalse(
      Analysis.diagnostics(snapshot).some((diagnostic) => diagnostic.code === 'SEM0177'),
    )
    const declaration = snapshot.results
      .get('static/runtime-iteration')
      ?.functions.find(
        (candidate) =>
          candidate.declaration.name._tag === 'Present' &&
          candidate.declaration.name.spelling === 'invalid',
      )?.declaration
    assert.notStrictEqual(declaration, undefined)
    assert.strictEqual(snapshot.target._tag, 'Resolved')
    if (
      declaration === undefined ||
      declaration.canonical._tag !== 'Canonical' ||
      snapshot.target._tag !== 'Resolved'
    )
      return
    const residual = Residualization.residualize(
      Residualization.make(
        snapshot.target.target,
        snapshot.results,
        snapshot.resolution,
        snapshot.index,
      ),
      Object.freeze({
        declaration: declaration.canonical.id,
        typeArguments: Object.freeze([]),
        evidence: Object.freeze([]),
        contractRow: Object.freeze([]),
        staticArguments: Object.freeze([]),
      }),
    )
    assert.strictEqual(residual._tag, 'ResidualBody')
    if (residual._tag !== 'ResidualBody') return
    assert.deepEqual(
      residual.fact.staticIterations.map((iteration) => ({
        state: iteration.state,
        scopes: iteration.scopes.length,
      })),
      [{ state: 'Rejected', scopes: 0 }],
    )
    assert.strictEqual(residual.function.statements.length, 1)
  }),
)

it.effect('rolls back every earlier static iteration when a later element fails', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/iteration-rollback',
      encoder.encode(`import silk.static_sequence as StaticSequence

fn rejected() -> i32 {
  let mut count = 0
  let static values = StaticSequence.append<i32>(
    StaticSequence.append<i32>(StaticSequence.empty<i32>(), 1),
    2,
  )
  static for value in values {
    static if value == 2 { compileError("later iteration failed") }
    count = count + 1
  }
  return count
}

pub fn main() -> i32 { return rejected() }`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0177'],
    )
    const selectedFailure = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(selectedFailure?.reason?._tag, 'SelectedCompileError')
    if (selectedFailure?.reason?._tag === 'SelectedCompileError')
      assert.isTrue(
        selectedFailure.reason.trace.some((frame) => frame.label === 'static for element 1'),
      )
    const declaration = snapshot.results
      .get('static/iteration-rollback')
      ?.functions.find(
        (candidate) =>
          candidate.declaration.name._tag === 'Present' &&
          candidate.declaration.name.spelling === 'rejected',
      )?.declaration
    assert.notStrictEqual(declaration, undefined)
    assert.strictEqual(snapshot.target._tag, 'Resolved')
    if (
      declaration === undefined ||
      declaration.canonical._tag !== 'Canonical' ||
      snapshot.target._tag !== 'Resolved'
    )
      return
    const residual = Residualization.residualize(
      Residualization.make(
        snapshot.target.target,
        snapshot.results,
        snapshot.resolution,
        snapshot.index,
      ),
      Object.freeze({
        declaration: declaration.canonical.id,
        typeArguments: Object.freeze([]),
        evidence: Object.freeze([]),
        contractRow: Object.freeze([]),
        staticArguments: Object.freeze([]),
      }),
    )
    assert.strictEqual(residual._tag, 'ResidualBody')
    if (residual._tag !== 'ResidualBody') return
    assert.deepEqual(
      residual.fact.staticIterations.map((iteration) => ({
        state: iteration.state,
        scopes: iteration.scopes.length,
      })),
      [{ state: 'Rejected', scopes: 0 }],
    )
    assert.isFalse(residual.function.statements.some((statement) => statement._tag === 'Write'))
    assert.isFalse(
      Analysis.instancesOf(snapshot).instances.some(
        (instance) => instance.key.declaration.name === 'rejected',
      ),
    )
  }),
)

it.effect('derives ordered visible descriptors for every concrete aggregate kind', () =>
  Effect.gen(function* () {
    const sourceId = 'static/reflection-descriptors'
    const snapshot = yield* Analysis.ofSourceRealized(
      sourceId,
      encoder.encode(`import silk.reflect as Reflect

struct Box<T> { pub value: T hidden: i32 }
tuple Point(u32, u64)

fn inspect<Owner>(value: Owner) -> i32 {
  let static ownerKind = Reflect.typeKind(Reflect.typeOf<Owner>())
  static for field in Reflect.fields<Owner>() {
    static if Reflect.fieldKind(field) == Reflect.labeledFieldKind {
      let static label = Reflect.fieldLabel(field)
    } else {
      let static ordinal = Reflect.fieldOrdinal(field)
    }
  }
  return 1
}

pub fn main() -> i32 {
  let named = inspect(Box<string> { value: "Julia", hidden: 32 })
  let namedTuple = inspect(Point(20, 22))
  let anonymousTuple = inspect((true, 42))
  let anonymousRecord = inspect(.{ name: "Julia", age: 32 })
  return named + namedTuple + anonymousTuple + anonymousRecord
}`),
      Target.x8664UnknownLinuxGnu.id,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.target._tag, 'Resolved')
    if (snapshot.target._tag !== 'Resolved') return
    const declaration = snapshot.results
      .get(sourceId)
      ?.functions.find(
        (candidate) =>
          candidate.declaration.name._tag === 'Present' &&
          candidate.declaration.name.spelling === 'inspect',
      )?.declaration
    assert.notStrictEqual(declaration, undefined)
    if (declaration === undefined || declaration.canonical._tag !== 'Canonical') return
    const declarationId = declaration.canonical.id
    const coordinator = Residualization.make(
      snapshot.target.target,
      snapshot.results,
      snapshot.resolution,
      snapshot.index,
    )
    const descriptors = (owner: Type.Nominal): ReadonlyArray<StaticValue.FieldDescriptorValue> => {
      const residual = Residualization.residualize(
        coordinator,
        Object.freeze({
          declaration: declarationId,
          typeArguments: Object.freeze([owner]),
          evidence: Object.freeze([]),
          contractRow: Object.freeze([]),
          staticArguments: Object.freeze([]),
        }),
      )
      assert.strictEqual(residual._tag, 'ResidualBody')
      if (residual._tag !== 'ResidualBody') return Object.freeze([])
      const iteration = residual.fact.staticIterations.at(0)
      assert.strictEqual(iteration?.state, 'Expanded')
      return Object.freeze(
        (iteration?.scopes ?? []).flatMap((scope) =>
          scope.binding.staticValue?._tag === 'FieldDescriptorValue'
            ? [scope.binding.staticValue]
            : [],
        ),
      )
    }
    const encoded = (fields: ReadonlyArray<StaticValue.FieldDescriptorValue>) =>
      fields.map((field) => ({
        ownerKind: field.owner.kind,
        declarationOrdinal: field.declarationOrdinal,
        member:
          field.member._tag === 'LabeledField' ? field.member.label : `#${field.member.ordinal}`,
        valueType: Type.encode(field.valueType),
        authorization: `${field.authorization.module}.${field.authorization.name}`,
        provenance: field.provenance.sourceId,
      }))

    assert.deepEqual(encoded(descriptors(Type.nominal(sourceId, 'Box', [Type.string]))), [
      {
        ownerKind: 'Named',
        declarationOrdinal: 0,
        member: 'value',
        valueType: 'string',
        authorization: 'silk/reflect.fields',
        provenance: sourceId,
      },
    ])
    assert.deepEqual(encoded(descriptors(Type.nominal(sourceId, 'Point'))), [
      {
        ownerKind: 'Positional',
        declarationOrdinal: 0,
        member: '#0',
        valueType: 'u32',
        authorization: 'silk/reflect.fields',
        provenance: sourceId,
      },
      {
        ownerKind: 'Positional',
        declarationOrdinal: 1,
        member: '#1',
        valueType: 'u64',
        authorization: 'silk/reflect.fields',
        provenance: sourceId,
      },
    ])
    const generated = [...snapshot.index.generatedAggregates.values()]
    const anonymousTuple = generated.find(
      (aggregate) => aggregate.aggregateKind === 'AnonymousPositional',
    )
    const anonymousRecord = generated.find(
      (aggregate) => aggregate.aggregateKind === 'AnonymousNamed',
    )
    assert.strictEqual(anonymousTuple?.canonical._tag, 'Canonical')
    assert.strictEqual(anonymousRecord?.canonical._tag, 'Canonical')
    if (
      anonymousTuple?.canonical._tag !== 'Canonical' ||
      anonymousRecord?.canonical._tag !== 'Canonical'
    )
      return
    assert.deepEqual(
      encoded(
        descriptors(
          Type.nominal(anonymousTuple.canonical.id.module, anonymousTuple.canonical.id.name),
        ),
      ).map(({ ownerKind, declarationOrdinal, member, valueType }) => ({
        ownerKind,
        declarationOrdinal,
        member,
        valueType,
      })),
      [
        {
          ownerKind: 'AnonymousPositional',
          declarationOrdinal: 0,
          member: '#0',
          valueType: 'bool',
        },
        { ownerKind: 'AnonymousPositional', declarationOrdinal: 1, member: '#1', valueType: 'i32' },
      ],
    )
    assert.deepEqual(
      encoded(
        descriptors(
          Type.nominal(anonymousRecord.canonical.id.module, anonymousRecord.canonical.id.name),
        ),
      ).map(({ ownerKind, declarationOrdinal, member, valueType }) => ({
        ownerKind,
        declarationOrdinal,
        member,
        valueType,
      })),
      [
        { ownerKind: 'AnonymousNamed', declarationOrdinal: 0, member: 'name', valueType: 'string' },
        { ownerKind: 'AnonymousNamed', declarationOrdinal: 1, member: 'age', valueType: 'i32' },
      ],
    )
  }),
)

it.effect('rejects phase-only descriptor types from runtime signatures, bindings, and calls', () =>
  Effect.gen(function* () {
    const sourceId = 'static/phase-only-types'
    const program = `import silk.reflect as Reflect

tuple Pair(i32)

fn consume<Value>(value: Value) -> () {}
fn invalidParameter(value: Intrinsic.Type<Pair>) -> () {}
fn invalidReturn() -> Intrinsic.Fields<Pair> { return Reflect.fields<Pair>() }

fn exercise() -> () {
  let static descriptor = Reflect.typeOf<Pair>()
  let leaked = descriptor
  consume(descriptor)
}

pub fn main() -> () { exercise() }`
    const snapshot = yield* Analysis.ofSourceRealized(
      sourceId,
      encoder.encode(program),
      Target.x8664UnknownLinuxGnu.id,
    )
    const violations = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0176',
    )
    assert.isTrue(
      violations.some(
        (diagnostic) => diagnostic.span.start === program.indexOf('value: Intrinsic'),
      ),
    )
    assert.isTrue(
      violations.some((diagnostic) => {
        const position = program.indexOf('Intrinsic.Fields')
        return diagnostic.span.start <= position && diagnostic.span.end >= position
      }),
    )
    assert.isTrue(
      violations.some((diagnostic) => {
        const position = program.indexOf('leaked')
        return diagnostic.span.start <= position && diagnostic.span.end >= position
      }),
    )
    const callArgument = program.lastIndexOf('descriptor)')
    assert.isTrue(violations.some((diagnostic) => diagnostic.span.start === callArgument))
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
      typeArguments: selected.key.typeArguments,
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
      const messageStart = selectedFailure.indexOf('"guard disabled"') + 1
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
            start: messageStart,
            end: messageStart + 'guard disabled'.length,
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

it.effect('anchors compileError to a nested static-text slice of a non-literal parameter', () =>
  Effect.gen(function* () {
    const sourceId = 'static/compile-error-slice'
    const program = `import silk.static_text { slice }

static fn inner(value: string) -> string { return slice(value, 1, 4) }
static fn outer(value: string) -> string { return slice(inner(value), 0, 2) }

fn reject(static template: string) -> i32 { compileError(outer(template)) }

pub fn main() -> i32 { return reject("aéz") }`
    const snapshot = yield* Analysis.ofSourceRealized(
      sourceId,
      encoder.encode(program),
      Target.x8664UnknownLinuxGnu.id,
    )
    const diagnostic = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(diagnostic?.code, 'SEM0177')
    const literalCharacterStart = program.lastIndexOf('"aéz"')
    const prefixBytes = encoder.encode(program.slice(0, literalCharacterStart)).length
    assert.strictEqual(diagnostic?.span.sourceId, sourceId)
    assert.strictEqual(diagnostic?.span.start, prefixBytes + 2)
    assert.strictEqual(diagnostic?.span.end, prefixBytes + 4)
  }),
)

it.effect(
  'chooses caller provenance deterministically without changing specialization identity',
  () =>
    Effect.gen(function* () {
      const sourceId = 'static/compile-error-shared-specialization'
      const program = `import silk.static_text { slice }

fn reject(static template: string) -> i32 { compileError(slice(template, 1, 3)) }

pub fn main() -> i32 {
  let first = reject("aéz")
  return reject("aéz")
}`
      const snapshot = yield* Analysis.ofSourceRealized(
        sourceId,
        encoder.encode(program),
        Target.x8664UnknownLinuxGnu.id,
      )
      const diagnostics = Analysis.diagnostics(snapshot).filter(
        (diagnostic) => diagnostic.code === 'SEM0177',
      )
      assert.strictEqual(diagnostics.length, 1)
      const firstLiteralStart = program.indexOf('"aéz"')
      const prefixBytes = encoder.encode(program.slice(0, firstLiteralStart)).length
      assert.strictEqual(diagnostics.at(0)?.span.sourceId, sourceId)
      assert.strictEqual(diagnostics.at(0)?.span.start, prefixBytes + 2)
      assert.strictEqual(diagnostics.at(0)?.span.end, prefixBytes + 4)
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
    assert.deepEqual(text.data.sourceRanges, [
      { start: 1, end: 2 },
      { start: 2, end: 3 },
      { start: 3, end: 4 },
      { start: 4, end: 6 },
      { start: 6, end: 15 },
      { start: 6, end: 15 },
      { start: 6, end: 15 },
      { start: 6, end: 15 },
    ])
    assert.deepEqual(text.data.contentRange, { start: 1, end: 15 })
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
        sourceRanges: [
          { start: 2, end: 6 },
          { start: 6, end: 10 },
        ],
        contentRange: { start: 2, end: 10 },
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

it('composes decoded static-text ranges through source and parameter slices', () => {
  const spelling = '"hé\\n\\u{1f642}"'
  const decoded = StaticText.decode(Array.from(encoder.encode(spelling)), formOf(spelling))
  assert.strictEqual(decoded._tag, 'Decoded')
  if (decoded._tag !== 'Decoded') return
  const token = SourceSpan.fromOffsets('static/origin', 100, 100 + encoder.encode(spelling).length)
  if (token === undefined) throw new Error('expected source text span')
  const origin = StaticEvaluation.sourceTextOrigin(token, decoded.data)
  const newline = StaticEvaluation.sliceTextOrigin(origin, 3, 4)
  if (newline === undefined) throw new Error('expected newline origin')
  const newlineSpan = StaticEvaluation.textOriginSpan(newline)
  assert.strictEqual(newlineSpan?.sourceId, 'static/origin')
  assert.strictEqual(newlineSpan?.start, 104)
  assert.strictEqual(newlineSpan?.end, 106)
  const emoji = StaticEvaluation.sliceTextOrigin(origin, 4, 8)
  if (emoji === undefined) throw new Error('expected emoji origin')
  const emojiSpan = StaticEvaluation.textOriginSpan(emoji)
  assert.strictEqual(emojiSpan?.sourceId, 'static/origin')
  assert.strictEqual(emojiSpan?.start, 106)
  assert.strictEqual(emojiSpan?.end, 115)
  assert.deepEqual(
    StaticEvaluation.sliceTextOrigin(
      StaticEvaluation.parameterTextOrigin(0, decoded.data.bytes.length),
      3,
      8,
    ),
    { _tag: 'ParameterTextOrigin', ordinal: 0, start: 3, end: 8 },
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
