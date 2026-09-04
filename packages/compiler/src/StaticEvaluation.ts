import * as Option from 'effect/Option'
import * as Constraint from './Constraint.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as Diagnostic from './Diagnostic.js'
import type * as Elaboration from './Elaboration.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Canonical from './internal/Canonical.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as TypeInference from './internal/TypeInference.js'
import * as LiteralForm from './LiteralForm.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as StaticText from './StaticText.js'
import * as StaticValue from './StaticValue.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Target from './Target.js'
import * as Type from './Type.js'

/** The closed target facts visible to one static-evaluation session. */
export interface TargetEnvironment {
  readonly _tag: 'StaticTargetEnvironment'
  readonly target: Target.Id
  readonly profile: 0 | 1 | 2 | 3
  readonly kind: 'Native' | 'WebAssembly'
  readonly pointerBits: 32 | 64
  readonly endianness: 'little'
}

const profile = (target: Target.Id): TargetEnvironment['profile'] => {
  switch (target) {
    case 'aarch64-apple-darwin':
      return 0
    case 'aarch64-unknown-linux-gnu':
      return 1
    case 'wasm32-unknown-unknown':
      return 2
    case 'x86_64-unknown-linux-gnu':
      return 3
  }
}

/** Copies one selected compiler target into identity-free static environment facts. */
export const targetEnvironment = (target: Target.Target): TargetEnvironment =>
  Object.freeze({
    _tag: 'StaticTargetEnvironment',
    target: target.id,
    profile: profile(target.id),
    kind: target.kind,
    pointerBits: target.pointerSize === 4 ? 32 : 64,
    endianness: target.endianness,
  })

/** Compiler-owned deterministic limits for one complete static-evaluation session. */
export interface Limits {
  readonly steps: number
  readonly callDepth: number
  readonly retainedValueBytes: number
  readonly residualNodes: number
}

/** Initial policy values; callers may supply smaller explicit limits for bounded verification. */
export const defaultLimits: Limits = Object.freeze({
  steps: 100_000,
  callDepth: 128,
  retainedValueBytes: 4 * 1024 * 1024,
  residualNodes: 100_000,
})

const validLimit = (value: number): boolean => Number.isSafeInteger(value) && value >= 0

/** Validates and freezes one explicit deterministic limit policy. */
export const limits = (input: Limits): Limits => {
  if (
    !validLimit(input.steps) ||
    !validLimit(input.callDepth) ||
    !validLimit(input.retainedValueBytes) ||
    !validLimit(input.residualNodes)
  )
    throw new RangeError('Static evaluation limits must be non-negative safe integers')
  return Object.freeze({ ...input })
}

/** One concrete static application before syntax evaluation or residualization. */
export interface Application {
  readonly declaration: DeclarationFacts.CanonicalId
  /** Canonical generic-argument encodings in declared order. */
  readonly typeArguments: ReadonlyArray<string>
  /** Canonical selected-evidence encodings in declared order. */
  readonly evidence: ReadonlyArray<string>
  readonly contractRow: ReadonlyArray<string>
  readonly staticArguments: ReadonlyArray<StaticValue.Value>
  readonly span: SourceSpan.SourceSpan
}

/** One source-level frame retained without a host stack or runtime identity. */
export type TraceFrame =
  | ApplicationFrame
  | SelectedArmFrame
  | StaticIterationFrame
  | StaticTextFrame

export interface ApplicationFrame {
  readonly _tag: 'StaticApplicationFrame'
  readonly declaration: DeclarationFacts.CanonicalId
  readonly target: Target.Id
  readonly staticArguments: ReadonlyArray<string>
  readonly span: SourceSpan.SourceSpan
}

export interface SelectedArmFrame {
  readonly _tag: 'SelectedStaticArmFrame'
  readonly selected: 'Taken' | 'Otherwise'
  readonly span: SourceSpan.SourceSpan
}

export interface StaticIterationFrame {
  readonly _tag: 'StaticIterationFrame'
  readonly ordinal: number
  readonly value: string
  readonly span: SourceSpan.SourceSpan
}

export interface StaticTextFrame {
  readonly _tag: 'StaticTextFrame'
  readonly literal: SourceSpan.SourceSpan
  readonly byteOffset: number
}

export type Trace = ReadonlyArray<TraceFrame>

/** Source-independent provenance for one static text result. */
export type TextOrigin = StaticValue.TextOrigin
export type SourceTextOrigin = StaticValue.SourceTextOrigin
export type ParameterTextOrigin = StaticValue.ParameterTextOrigin

const sourcePoint = (span: SourceSpan.SourceSpan, offset: number): SourceSpan.SourceSpan =>
  SourceSpan.fromOffsets(span.sourceId, offset, offset) ?? span

/** Creates provenance for one decoded source text value. */
export const sourceTextOrigin = (
  span: SourceSpan.SourceSpan,
  data?: StaticText.Data,
): TextOrigin => {
  const byteSpans = Object.freeze(
    (data?.sourceRanges ?? []).flatMap((range) => {
      const mapped = SourceSpan.fromOffsets(
        span.sourceId,
        span.start + range.start,
        span.start + range.end,
      )
      return mapped === undefined ? [] : [mapped]
    }),
  )
  const contentStart = span.start + (data?.contentRange?.start ?? 0)
  return Object.freeze({
    _tag: 'SourceTextOrigin',
    span,
    byteSpans,
    boundary: sourcePoint(span, contentStart),
  })
}

/** Creates relative provenance for a complete static text parameter. */
export const parameterTextOrigin = (
  ordinal: number,
  byteLength: number,
  scope?: string,
): TextOrigin => {
  if (!Number.isSafeInteger(byteLength) || byteLength < 0)
    throw new RangeError('Static text parameter lengths must be non-negative safe integers')
  return Object.freeze({
    _tag: 'ParameterTextOrigin',
    ...(scope === undefined ? {} : { scope }),
    ordinal,
    start: 0,
    end: byteLength,
  })
}

const sourceBoundaryAt = (origin: SourceTextOrigin, offset: number): SourceSpan.SourceSpan => {
  const next = origin.byteSpans.at(offset)
  if (next !== undefined) return sourcePoint(origin.span, next.start)
  const previous = origin.byteSpans.at(offset - 1)
  return previous === undefined ? origin.boundary : sourcePoint(origin.span, previous.end)
}

/** Composes one half-open byte slice into existing text provenance. */
export const sliceTextOrigin = (
  origin: TextOrigin,
  start: number,
  end: number,
): TextOrigin | undefined => {
  if (!Number.isSafeInteger(start) || !Number.isSafeInteger(end) || start < 0 || start > end)
    return undefined
  if (origin._tag === 'ParameterTextOrigin') {
    if (end > origin.end - origin.start) return undefined
    return Object.freeze({
      _tag: 'ParameterTextOrigin',
      ...(origin.scope === undefined ? {} : { scope: origin.scope }),
      ordinal: origin.ordinal,
      start: origin.start + start,
      end: origin.start + end,
    })
  }
  if (end > origin.byteSpans.length) return undefined
  return Object.freeze({
    _tag: 'SourceTextOrigin',
    span: origin.span,
    byteSpans: Object.freeze(origin.byteSpans.slice(start, end)),
    boundary: sourceBoundaryAt(origin, start),
  })
}

/** Resolves the most precise authored span represented by static text provenance. */
export const textOriginSpan = (origin: TextOrigin): SourceSpan.SourceSpan | undefined => {
  if (origin._tag === 'ParameterTextOrigin') return undefined
  const first = origin.byteSpans.at(0)
  const last = origin.byteSpans.at(-1)
  if (first === undefined || last === undefined) return origin.boundary
  return SourceSpan.fromOffsets(origin.span.sourceId, first.start, last.end)
}

/** Retains one selected static arm in the logical trace. */
export const selectedArmFrame = (
  selected: SelectedArmFrame['selected'],
  span: SourceSpan.SourceSpan,
): SelectedArmFrame => Object.freeze({ _tag: 'SelectedStaticArmFrame', selected, span })

/** Retains one canonical element selected by an authored static iteration. */
export const staticIterationFrame = (
  ordinal: number,
  value: StaticValue.Value,
  span: SourceSpan.SourceSpan,
): StaticIterationFrame => {
  if (!Number.isSafeInteger(ordinal) || ordinal < 0)
    throw new RangeError('Static iteration ordinals must be non-negative safe integers')
  return Object.freeze({
    _tag: 'StaticIterationFrame',
    ordinal,
    value: StaticValue.presentation(value),
    span,
  })
}

/** Retains one validated byte position in a source static-text literal. */
export const staticTextFrame = (
  literal: SourceSpan.SourceSpan,
  byteOffset: number,
): StaticTextFrame => {
  if (!Number.isSafeInteger(byteOffset) || byteOffset < 0)
    throw new RangeError('Static text byte offsets must be non-negative safe integers')
  return Object.freeze({ _tag: 'StaticTextFrame', literal, byteOffset })
}

/** Appends logical frames without exposing or mutating evaluation storage. */
export const appendTrace = (self: Trace, ...frames: ReadonlyArray<TraceFrame>): Trace =>
  Object.freeze([...self, ...frames])

interface FailureBase {
  readonly span: SourceSpan.SourceSpan
  readonly trace: Trace
}

export interface CompileError extends FailureBase {
  readonly _tag: 'CompileError'
  readonly message: string
  /** Text provenance retained relative to static parameters until the call boundary. */
  readonly origin?: TextOrigin
}

export interface PhaseViolation extends FailureBase {
  readonly _tag: 'PhaseViolation'
  readonly operation: string
  readonly detail: string
}

export interface Cycle extends FailureBase {
  readonly _tag: 'Cycle'
  readonly declaration: DeclarationFacts.CanonicalId
}

interface LimitFailureBase extends FailureBase {
  readonly limit: number
  readonly attempted: number
}

export interface StepLimit extends LimitFailureBase {
  readonly _tag: 'StepLimit'
}

export interface CallDepthLimit extends LimitFailureBase {
  readonly _tag: 'CallDepthLimit'
}

export interface RetainedValueLimit extends LimitFailureBase {
  readonly _tag: 'RetainedValueLimit'
}

export interface ResidualGrowthLimit extends LimitFailureBase {
  readonly _tag: 'ResidualGrowthLimit'
}

/** Every expected deterministic failure of the static-evaluation coordinator. */
export type StaticFailure =
  | CompileError
  | PhaseViolation
  | Cycle
  | StepLimit
  | CallDepthLimit
  | RetainedValueLimit
  | ResidualGrowthLimit

const diagnosticTrace = (trace: Trace): ReadonlyArray<Diagnostic.StaticTraceFrame> =>
  Object.freeze(
    trace.flatMap((frame): ReadonlyArray<Diagnostic.StaticTraceFrame> => {
      if (frame._tag === 'StaticTextFrame')
        return [
          Object.freeze({
            kind: 'StaticText',
            label: `static text byte ${frame.byteOffset}`,
            arguments: Object.freeze([`byteOffset=${frame.byteOffset}`]),
            span: frame.literal,
          }),
        ]
      if (frame._tag === 'StaticApplicationFrame')
        return [
          Object.freeze({
            kind: 'Call',
            label: `${frame.declaration.module}.${frame.declaration.name}`,
            arguments: frame.staticArguments,
            span: frame.span,
          }),
        ]
      if (frame._tag === 'StaticIterationFrame')
        return [
          Object.freeze({
            kind: 'SelectedArm',
            label: `static for element ${frame.ordinal}`,
            arguments: Object.freeze([frame.value]),
            span: frame.span,
          }),
        ]
      return [
        Object.freeze({
          kind: 'SelectedArm',
          label: frame.selected === 'Taken' ? 'selected static if arm' : 'selected static else arm',
          arguments: Object.freeze([]),
          span: frame.span,
        }),
      ]
    }),
  )

/** Converts one static-evaluation failure into its stable public semantic diagnostic. */
export const diagnostic = (failure: StaticFailure, target: string): Diagnostic.Diagnostic => {
  const trace = diagnosticTrace(failure.trace)
  if (failure._tag === 'CompileError')
    return Diagnostic.selectedCompileError(failure.message, target, trace, failure.span)
  if (failure._tag === 'PhaseViolation')
    return Diagnostic.staticPhaseViolation(failure.operation, target, trace, failure.span)
  if (failure._tag === 'Cycle')
    return Diagnostic.staticEvaluationCycle(
      `${failure.declaration.module}.${failure.declaration.name}`,
      target,
      trace,
      failure.span,
    )
  let factory = Diagnostic.staticResidualGrowthLimit
  if (failure._tag === 'StepLimit') factory = Diagnostic.staticStepLimit
  else if (failure._tag === 'CallDepthLimit') factory = Diagnostic.staticCallDepthLimit
  else if (failure._tag === 'RetainedValueLimit') factory = Diagnostic.staticRetainedValueLimit
  return factory(failure.limit, target, trace, failure.span)
}

const frozenTrace = (trace: Trace): Trace => Object.freeze([...trace])

/** Creates one source-requested compile failure for the selected specialization. */
export const compileError = (
  message: string,
  span: SourceSpan.SourceSpan,
  trace: Trace,
  origin?: TextOrigin,
): CompileError =>
  Object.freeze({
    _tag: 'CompileError',
    message,
    span,
    trace: frozenTrace(trace),
    ...(origin === undefined ? {} : { origin }),
  })

/** Creates one rejected crossing from static work into an unavailable phase. */
export const phaseViolation = (
  operation: string,
  detail: string,
  span: SourceSpan.SourceSpan,
  trace: Trace,
): PhaseViolation =>
  Object.freeze({
    _tag: 'PhaseViolation',
    operation,
    detail,
    span,
    trace: frozenTrace(trace),
  })

/** The contextual type supplied while evaluating one ordinary literal syntax node. */
export type LiteralExpectation =
  | 'unit'
  | 'bool'
  | 'char'
  | 'string'
  | Scalar.IntegerSpelling
  | Scalar.FloatSpelling

/** The primitive operations admitted before static calls and control flow are implemented. */
export type PrimitiveOperation =
  | 'Add'
  | 'Subtract'
  | 'Multiply'
  | 'Divide'
  | 'Remainder'
  | 'Negate'
  | 'Equals'
  | 'NotEquals'
  | 'LessThan'
  | 'LessOrEqual'
  | 'GreaterThan'
  | 'GreaterOrEqual'
  | 'Not'

const primitiveFailure = (
  operation: string,
  detail: string,
  span: SourceSpan.SourceSpan,
  trace: Trace,
): Outcome<StaticValue.Value> => failed(phaseViolation(operation, detail, span, trace))

const admittedValue = (
  environment: TargetEnvironment,
  candidate: unknown,
  operation: string,
  span: SourceSpan.SourceSpan,
  trace: Trace,
): Outcome<StaticValue.Value> => {
  const admission = StaticValue.admit(candidate, { pointerBits: environment.pointerBits })
  return admission._tag === 'Admitted'
    ? complete(admission.value)
    : primitiveFailure(operation, `${admission.reason}: ${admission.detail}`, span, trace)
}

const expectedLiteral = (
  expected: LiteralExpectation | undefined,
  actual: LiteralExpectation,
  span: SourceSpan.SourceSpan,
  trace: Trace,
): Outcome<StaticValue.Value> | undefined =>
  expected === undefined || expected === actual
    ? undefined
    : primitiveFailure(
        'StaticEvaluation.evaluateLiteral',
        `expected ${expected}, received ${actual} literal`,
        span,
        trace,
      )

const tokenBytes = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  kind: Parameters<typeof SyntaxTree.directToken>[1],
): LiteralForm.ByteSequence | undefined => {
  const token = SyntaxTree.directToken(node, kind)
  if (token === undefined) return undefined
  return Option.getOrUndefined(SourceFile.slice(source, token.span))
}

/**
 * Evaluates one real parser literal node under an optional contextual primitive type.
 *
 * This operation deliberately stops at a canonical value or a phase failure, so a later syntax
 * walker can own diagnostics, step charging, calls, and control flow without duplicating decoding.
 */
export const evaluateLiteral = (
  environment: TargetEnvironment,
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  expected?: LiteralExpectation,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> => {
  const mismatch = (actual: LiteralExpectation): Outcome<StaticValue.Value> | undefined =>
    expectedLiteral(expected, actual, node.span, trace)
  if (node.kind === 'UnitExpression') return mismatch('unit') ?? complete(StaticValue.unit())
  if (node.kind === 'BooleanLiteralExpression') {
    const wrong = mismatch('bool')
    if (wrong !== undefined) return wrong
    const token =
      SyntaxTree.directToken(node, 'TrueKeyword') ?? SyntaxTree.directToken(node, 'FalseKeyword')
    return token === undefined
      ? primitiveFailure(
          'StaticEvaluation.evaluateLiteral',
          'boolean syntax has no value token',
          node.span,
          trace,
        )
      : complete(StaticValue.boolean(token.kind === 'TrueKeyword'))
  }
  if (node.kind === 'IntegerLiteralExpression') {
    const selected = expected === undefined ? Scalar.defaultInteger.spelling : expected
    if (!Scalar.isIntegerSpelling(selected))
      return primitiveFailure(
        'StaticEvaluation.evaluateLiteral',
        `expected ${selected}, received integer literal`,
        node.span,
        trace,
      )
    const bytes = tokenBytes(source, node, 'DecimalInteger')
    if (bytes === undefined)
      return primitiveFailure(
        'StaticEvaluation.evaluateLiteral',
        'integer syntax has no source token',
        node.span,
        trace,
      )
    const magnitude = IntegerLiteral.magnitude(bytes)
    const value = SyntaxTree.directToken(node, 'Minus') === undefined ? magnitude : -magnitude
    return admittedValue(
      environment,
      { _tag: 'IntegerValue', type: selected, value },
      'StaticEvaluation.evaluateLiteral',
      node.span,
      trace,
    )
  }
  if (node.kind === 'FloatingLiteralExpression') {
    const selected = expected === undefined ? Scalar.defaultFloat.spelling : expected
    if (!Scalar.isFloatSpelling(selected))
      return primitiveFailure(
        'StaticEvaluation.evaluateLiteral',
        `expected ${selected}, received floating literal`,
        node.span,
        trace,
      )
    const bytes = tokenBytes(source, node, 'DecimalFloat')
    if (bytes === undefined)
      return primitiveFailure(
        'StaticEvaluation.evaluateLiteral',
        'floating syntax has no source token',
        node.span,
        trace,
      )
    const unsigned = DigitSeparator.strip(bytes)
    const spelling = SyntaxTree.directToken(node, 'Minus') === undefined ? unsigned : `-${unsigned}`
    const encoded = FloatingPoint.fromDecimal(spelling, selected === 'f32' ? 32 : 64)
    return encoded === undefined
      ? primitiveFailure(
          'StaticEvaluation.evaluateLiteral',
          'floating syntax cannot be encoded',
          node.span,
          trace,
        )
      : admittedValue(
          environment,
          { _tag: 'FloatValue', type: selected, bits: encoded.bits },
          'StaticEvaluation.evaluateLiteral',
          node.span,
          trace,
        )
  }
  if (node.kind === 'CharacterLiteralExpression') {
    const wrong = mismatch('char')
    if (wrong !== undefined) return wrong
    const bytes = tokenBytes(source, node, 'CharLiteral')
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const decoded =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decodeScalar(Array.from(bytes), form)
    return decoded?._tag === 'Scalar'
      ? admittedValue(
          environment,
          { _tag: 'CharacterValue', value: decoded.value },
          'StaticEvaluation.evaluateLiteral',
          node.span,
          trace,
        )
      : primitiveFailure(
          'StaticEvaluation.evaluateLiteral',
          decoded?._tag === 'Invalid' ? decoded.detail : 'character syntax has no source token',
          node.span,
          trace,
        )
  }
  if (node.kind === 'StaticTextLiteralExpression') {
    const wrong = mismatch('string')
    if (wrong !== undefined) return wrong
    const bytes = tokenBytes(source, node, 'TextLiteral')
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const decoded =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decode(Array.from(bytes), form)
    if (decoded?._tag !== 'Decoded' || decoded.data.kind !== 'Text')
      return primitiveFailure(
        'StaticEvaluation.evaluateLiteral',
        decoded?._tag === 'Invalid' ? decoded.detail : 'text syntax has no text token',
        node.span,
        trace,
      )
    return admittedValue(
      environment,
      { _tag: 'TextValue', bytes: decoded.data.bytes },
      'StaticEvaluation.evaluateLiteral',
      node.span,
      trace,
    )
  }
  return primitiveFailure(
    'StaticEvaluation.evaluateLiteral',
    `${node.kind} is not a static literal`,
    node.span,
    trace,
  )
}

const samePrimitiveType = (left: StaticValue.Value, right: StaticValue.Value): boolean => {
  if (left._tag !== right._tag) return false
  if (left._tag === 'IntegerValue' && right._tag === 'IntegerValue') return left.type === right.type
  if (left._tag === 'FloatValue' && right._tag === 'FloatValue') return left.type === right.type
  return true
}

/** Evaluates checked scalar equality and arithmetic without consulting a runtime engine. */
export const evaluatePrimitive = (
  environment: TargetEnvironment,
  operation: PrimitiveOperation,
  operands: ReadonlyArray<StaticValue.Value>,
  span: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> => {
  const left = operands.at(0)
  const right = operands.at(1)
  const arity = operation === 'Negate' || operation === 'Not' ? 1 : 2
  if (operands.length !== arity || left === undefined)
    return primitiveFailure(
      'StaticEvaluation.evaluatePrimitive',
      `${operation} expects ${arity} operand${arity === 1 ? '' : 's'}`,
      span,
      trace,
    )
  if ((operation === 'Equals' || operation === 'NotEquals') && right !== undefined) {
    if (!samePrimitiveType(left, right))
      return primitiveFailure(
        'StaticEvaluation.evaluatePrimitive',
        'equality operands must have the same primitive type',
        span,
        trace,
      )
    if (
      (left._tag === 'BooleanValue' && right._tag === 'BooleanValue') ||
      (left._tag === 'CharacterValue' && right._tag === 'CharacterValue') ||
      (left._tag === 'IntegerValue' && right._tag === 'IntegerValue')
    ) {
      const equal = left.value === right.value
      return complete(StaticValue.boolean(operation === 'Equals' ? equal : !equal))
    }
    if (
      (left._tag === 'UnitValue' && right._tag === 'UnitValue') ||
      (left._tag === 'TextValue' && right._tag === 'TextValue')
    ) {
      const equal = StaticValue.equals(left, right)
      return complete(StaticValue.boolean(operation === 'Equals' ? equal : !equal))
    }
    if (left._tag === 'FloatValue' && right._tag === 'FloatValue') {
      const width = left.type === 'f32' ? 32 : 64
      const equal =
        FloatingPoint.toNumber({ width, bits: left.bits }) ===
        FloatingPoint.toNumber({ width, bits: right.bits })
      return complete(StaticValue.boolean(operation === 'Equals' ? equal : !equal))
    }
    return primitiveFailure(
      'StaticEvaluation.evaluatePrimitive',
      `${left._tag} does not expose primitive equality`,
      span,
      trace,
    )
  }
  const comparison =
    operation === 'LessThan' ||
    operation === 'LessOrEqual' ||
    operation === 'GreaterThan' ||
    operation === 'GreaterOrEqual'
  if (comparison && right !== undefined) {
    if (!samePrimitiveType(left, right))
      return primitiveFailure(
        'StaticEvaluation.evaluatePrimitive',
        'comparison operands must have the same primitive type',
        span,
        trace,
      )
    let order: number | undefined
    if (left._tag === 'IntegerValue' && right._tag === 'IntegerValue') {
      if (left.value < right.value) order = -1
      else if (left.value > right.value) order = 1
      else order = 0
    } else if (left._tag === 'FloatValue' && right._tag === 'FloatValue') {
      const width = left.type === 'f32' ? 32 : 64
      const leftNumber = FloatingPoint.toNumber({ width, bits: left.bits })
      const rightNumber = FloatingPoint.toNumber({ width, bits: right.bits })
      if (!Number.isNaN(leftNumber) && !Number.isNaN(rightNumber)) {
        if (leftNumber < rightNumber) order = -1
        else if (leftNumber > rightNumber) order = 1
        else order = 0
      }
    }
    if (order === undefined) return complete(StaticValue.boolean(false))
    let result = order >= 0
    if (operation === 'LessThan') result = order < 0
    else if (operation === 'LessOrEqual') result = order <= 0
    else if (operation === 'GreaterThan') result = order > 0
    return complete(StaticValue.boolean(result))
  }
  if (operation === 'Not' && left._tag === 'BooleanValue')
    return complete(StaticValue.boolean(!left.value))
  if (left._tag === 'IntegerValue') {
    if (operation === 'Negate') {
      const scalar = Scalar.find(left.type)
      if (scalar?.category !== 'Integer' || scalar.signedness !== 'Signed')
        return primitiveFailure(
          'StaticEvaluation.evaluatePrimitive',
          `Negate is unavailable for ${left.type}`,
          span,
          trace,
        )
      return admittedValue(
        environment,
        { _tag: 'IntegerValue', type: left.type, value: -left.value },
        'StaticEvaluation.evaluatePrimitive',
        span,
        trace,
      )
    }
    if (right?._tag !== 'IntegerValue' || right.type !== left.type)
      return primitiveFailure(
        'StaticEvaluation.evaluatePrimitive',
        'integer arithmetic operands must have the same type',
        span,
        trace,
      )
    if ((operation === 'Divide' || operation === 'Remainder') && right.value === 0n)
      return primitiveFailure('StaticEvaluation.evaluatePrimitive', 'division by zero', span, trace)
    const scalar = Scalar.find(left.type)
    if (scalar?.category !== 'Integer')
      return primitiveFailure(
        'StaticEvaluation.evaluatePrimitive',
        `unknown integer scalar ${left.type}`,
        span,
        trace,
      )
    const range = Scalar.range(scalar, environment.pointerBits)
    if (
      operation === 'Remainder' &&
      scalar.signedness === 'Signed' &&
      left.value === range.minimum &&
      right.value === -1n
    )
      return primitiveFailure(
        'StaticEvaluation.evaluatePrimitive',
        'arithmetic overflow',
        span,
        trace,
      )
    let value: bigint
    switch (operation) {
      case 'Add':
        value = left.value + right.value
        break
      case 'Subtract':
        value = left.value - right.value
        break
      case 'Multiply':
        value = left.value * right.value
        break
      case 'Divide':
        value = left.value / right.value
        break
      case 'Remainder':
        value = left.value % right.value
        break
      default:
        return primitiveFailure(
          'StaticEvaluation.evaluatePrimitive',
          `${operation} is unavailable for ${left.type}`,
          span,
          trace,
        )
    }
    return admittedValue(
      environment,
      { _tag: 'IntegerValue', type: left.type, value },
      'StaticEvaluation.evaluatePrimitive',
      span,
      trace,
    )
  }
  if (left._tag === 'FloatValue') {
    const width = left.type === 'f32' ? 32 : 64
    const leftNumber = FloatingPoint.toNumber({ width, bits: left.bits })
    let value: number
    if (operation === 'Negate') value = -leftNumber
    else {
      if (right?._tag !== 'FloatValue' || right.type !== left.type)
        return primitiveFailure(
          'StaticEvaluation.evaluatePrimitive',
          'floating arithmetic operands must have the same type',
          span,
          trace,
        )
      const rightNumber = FloatingPoint.toNumber({ width, bits: right.bits })
      switch (operation) {
        case 'Add':
          value = leftNumber + rightNumber
          break
        case 'Subtract':
          value = leftNumber - rightNumber
          break
        case 'Multiply':
          value = leftNumber * rightNumber
          break
        case 'Divide':
          value = leftNumber / rightNumber
          break
        case 'Remainder':
          value = leftNumber % rightNumber
          break
        default:
          return primitiveFailure(
            'StaticEvaluation.evaluatePrimitive',
            `${operation} is unavailable for ${left.type}`,
            span,
            trace,
          )
      }
    }
    const encoded = FloatingPoint.fromNumber(value, width)
    return admittedValue(
      environment,
      { _tag: 'FloatValue', type: left.type, bits: encoded.bits },
      'StaticEvaluation.evaluatePrimitive',
      span,
      trace,
    )
  }
  return primitiveFailure(
    'StaticEvaluation.evaluatePrimitive',
    `${operation} is unavailable for ${left._tag}`,
    span,
    trace,
  )
}

/** Constructs one scalar enum member after validating its fixed-width representation. */
export const constructEnum = (
  environment: TargetEnvironment,
  type: DeclarationFacts.CanonicalId,
  member: string,
  representation: Scalar.EnumRepresentationSpelling,
  discriminant: bigint,
  span: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> =>
  admittedValue(
    environment,
    { _tag: 'EnumValue', type, member, representation, discriminant },
    'StaticEvaluation.constructEnum',
    span,
    trace,
  )

/** Evaluates nominal scalar-enum equality after semantic typing has selected one enum. */
export const evaluateEnumEquality = (
  operation: 'Equals' | 'NotEquals',
  left: StaticValue.EnumValue,
  right: StaticValue.EnumValue,
  span: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> => {
  if (left.type.module !== right.type.module || left.type.name !== right.type.name)
    return primitiveFailure(
      'StaticEvaluation.evaluateEnumEquality',
      'enum equality operands must have the same nominal type',
      span,
      trace,
    )
  const equal = left.member === right.member && left.discriminant === right.discriminant
  return complete(StaticValue.boolean(operation === 'Equals' ? equal : !equal))
}

const staticTextFailure = (
  detail: string,
  literal: SourceSpan.SourceSpan,
  byteOffset: bigint,
  trace: Trace,
): Outcome<StaticValue.Value> => {
  const offset =
    byteOffset >= 0n && byteOffset <= BigInt(Number.MAX_SAFE_INTEGER) ? Number(byteOffset) : 0
  return primitiveFailure(
    'StaticEvaluation.inspectStaticText',
    detail,
    literal,
    appendTrace(trace, staticTextFrame(literal, offset)),
  )
}

/** Returns the UTF-8 byte length of one admitted static text as target-sized `usize`. */
export const staticTextByteLength = (
  environment: TargetEnvironment,
  text: StaticValue.TextValue,
  literal: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> =>
  admittedValue(
    environment,
    { _tag: 'IntegerValue', type: 'usize', value: BigInt(text.bytes.length) },
    'StaticEvaluation.inspectStaticText',
    literal,
    trace,
  )

/** Returns one UTF-8 byte from admitted static text as `u8`. */
export const staticTextByteAt = (
  environment: TargetEnvironment,
  text: StaticValue.TextValue,
  byteOffset: bigint,
  literal: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> => {
  if (byteOffset < 0n || byteOffset >= BigInt(text.bytes.length))
    return staticTextFailure(
      `byte offset ${byteOffset.toString()} is outside length ${text.bytes.length}`,
      literal,
      byteOffset,
      trace,
    )
  const byte = text.bytes.at(Number(byteOffset))
  return byte === undefined
    ? staticTextFailure('static text byte is unavailable', literal, byteOffset, trace)
    : admittedValue(
        environment,
        { _tag: 'IntegerValue', type: 'u8', value: BigInt(byte) },
        'StaticEvaluation.inspectStaticText',
        literal,
        trace,
      )
}

/** Concatenates two admitted static texts while retaining the left operand as the source anchor. */
export const staticTextConcat = (
  environment: TargetEnvironment,
  left: StaticValue.TextValue,
  right: StaticValue.TextValue,
  literal: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> =>
  admittedValue(
    environment,
    {
      _tag: 'TextValue',
      bytes: Object.freeze([...left.bytes, ...right.bytes]),
      ...(left.origin === undefined ? {} : { origin: left.origin }),
    },
    'StaticEvaluation.staticTextConcat',
    literal,
    trace,
  )

/** Returns the half-open UTF-8 byte slice when both offsets lie on scalar boundaries. */
export const staticTextSlice = (
  environment: TargetEnvironment,
  text: StaticValue.TextValue,
  start: bigint,
  end: bigint,
  literal: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> => {
  if (start < 0n || end < start || end > BigInt(text.bytes.length))
    return staticTextFailure(
      `byte range ${start.toString()}..${end.toString()} is outside length ${text.bytes.length}`,
      literal,
      start,
      trace,
    )
  return admittedValue(
    environment,
    {
      _tag: 'TextValue',
      bytes: text.bytes.slice(Number(start), Number(end)),
      ...(text.origin === undefined
        ? {}
        : { origin: sliceTextOrigin(text.origin, Number(start), Number(end)) }),
    },
    'StaticEvaluation.inspectStaticText',
    literal,
    appendTrace(trace, staticTextFrame(literal, Number(start))),
  )
}

/** Constructs a recursively pure aggregate through the sole static-value admission boundary. */
export const constructAggregate = (
  environment: TargetEnvironment,
  identity: StaticValue.AggregateIdentity,
  fields: ReadonlyArray<StaticValue.AggregateField>,
  span: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
  runtimeFields?: ReadonlyArray<{
    readonly id: DeclarationFacts.FieldId
    readonly type: Type.Type
  }>,
): Outcome<StaticValue.Value> => {
  const outcome = admittedValue(
    environment,
    { _tag: 'AggregateValue', identity, fields },
    'StaticEvaluation.constructAggregate',
    span,
    trace,
  )
  return outcome._tag === 'Complete' && outcome.value._tag === 'AggregateValue'
    ? complete(
        Object.freeze({
          ...outcome.value,
          ...(runtimeFields === undefined
            ? {}
            : {
                runtimeFields: Object.freeze(
                  runtimeFields.map((field) => Object.freeze({ ...field })),
                ),
              }),
        }),
      )
    : outcome
}

/** Implements the StaticOnly `Intrinsic.targetProfile() -> u8` primitive. */
export const targetProfile = (
  environment: TargetEnvironment,
  span: SourceSpan.SourceSpan,
  trace: Trace = Object.freeze([]),
): Outcome<StaticValue.Value> =>
  admittedValue(
    environment,
    { _tag: 'IntegerValue', type: 'u8', value: BigInt(environment.profile) },
    'StaticEvaluation.targetProfile',
    span,
    trace,
  )

/** Stable environment key for one source parameter or local binding. */
export const localValueKey = (
  value: DeclarationFacts.ParameterFact | Elaboration.BindingDeclarationFact,
): string =>
  value._tag === 'ParameterDeclaration'
    ? `parameter:${value.id.function.sourceId}:${value.id.function.ordinal}:${value.id.ordinal}`
    : `binding:${value.id.function.sourceId}:${value.id.function.ordinal}:${value.id.ordinal}`

export interface FactEvaluationContext {
  readonly environment: TargetEnvironment
  /** Concrete declaration arguments retained while interpreting a generic static body. */
  readonly typeSubstitution?: Type.Substitution
  readonly values: ReadonlyMap<string, StaticValue.Value>
  /** Source provenance retained separately from canonical value identity. */
  readonly valueSpans: ReadonlyMap<string, SourceSpan.SourceSpan>
  readonly valueOrigins: ReadonlyMap<string, TextOrigin>
  /** Per-expression provenance retained outside canonical value identity. */
  readonly expressionSpans: Map<Elaboration.ExpressionFact, SourceSpan.SourceSpan>
  readonly expressionOrigins: Map<Elaboration.ExpressionFact, TextOrigin>
  /** Return provenance written by one static-function statement evaluation. */
  readonly returnedTextSpan?: { value: SourceSpan.SourceSpan | undefined }
  readonly returnedTextOrigin?: { value: TextOrigin | undefined }
  readonly trace: Trace
  readonly reflect: (
    owner: Type.Type,
    kind: 'Type' | 'Fields',
    span: SourceSpan.SourceSpan,
    trace: Trace,
  ) => Outcome<StaticValue.Value>
  readonly call: (
    declaration: DeclarationFacts.DeclarationFact,
    arguments_: ReadonlyArray<StaticValue.Value>,
    argumentSpans: ReadonlyArray<SourceSpan.SourceSpan | undefined>,
    argumentOrigins: ReadonlyArray<TextOrigin | undefined>,
    span: SourceSpan.SourceSpan,
    trace: Trace,
    identity: {
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly evidence: ReadonlyArray<string>
      readonly contractRow: ReadonlyArray<string>
    },
  ) => FactCallResult
  readonly constant?: (
    declaration: DeclarationFacts.ConstantFact,
    span: SourceSpan.SourceSpan,
    trace: Trace,
  ) => Outcome<StaticValue.Value>
  readonly step?: (span: SourceSpan.SourceSpan, trace: Trace) => StaticFailure | undefined
}

export interface FactCallResult {
  readonly outcome: Outcome<StaticValue.Value>
  readonly textSpan?: SourceSpan.SourceSpan
  readonly textOrigin?: TextOrigin
}

const unavailableFact = (
  fact: Elaboration.ExpressionFact,
  context: FactEvaluationContext,
  detail: string,
): Outcome<StaticValue.Value> =>
  primitiveFailure('StaticEvaluation.evaluateFact', detail, fact.syntax.span, context.trace)

const valueOfConstant = (
  fact: Elaboration.ConstantExpressionFact,
  context: FactEvaluationContext,
): Outcome<StaticValue.Value> => {
  const value = fact.value
  if (value?._tag === 'Boolean') return complete(StaticValue.boolean(value.value))
  if (value?._tag === 'Character')
    return admittedValue(
      context.environment,
      { _tag: 'CharacterValue', value: value.value },
      'StaticEvaluation.evaluateFact',
      fact.syntax.span,
      context.trace,
    )
  if (value?._tag === 'Integer')
    return admittedValue(
      context.environment,
      { _tag: 'IntegerValue', type: value.type, value: value.value },
      'StaticEvaluation.evaluateFact',
      fact.syntax.span,
      context.trace,
    )
  if (value?._tag === 'Floating')
    return admittedValue(
      context.environment,
      { _tag: 'FloatValue', type: value.type, bits: value.bits },
      'StaticEvaluation.evaluateFact',
      fact.syntax.span,
      context.trace,
    )
  if (value?._tag === 'String')
    return admittedValue(
      context.environment,
      { _tag: 'TextValue', bytes: value.data.bytes },
      'StaticEvaluation.evaluateFact',
      fact.syntax.span,
      context.trace,
    )
  if (context.constant !== undefined)
    return context.constant(fact.declaration, fact.syntax.span, context.trace)
  return unavailableFact(fact, context, 'constant has no selected static value')
}

const evaluateArguments = (
  arguments_: ReadonlyArray<Elaboration.ArgumentFact>,
  context: FactEvaluationContext,
): Outcome<ReadonlyArray<StaticValue.Value>> => {
  const values: Array<StaticValue.Value> = []
  for (const argument of arguments_) {
    const evaluated = evaluateFact(argument.expression, context)
    if (evaluated._tag === 'Failed') return evaluated
    values.push(evaluated.value)
  }
  return complete(Object.freeze(values))
}

const intrinsicTypeArgument = (
  fact: Extract<Elaboration.ExpressionFact, { readonly _tag: 'Call' }>,
  ordinal: number,
  substitution: Type.Substitution = new Map(),
): Type.Type | undefined => {
  if (fact.contract._tag !== 'Compatible') return undefined
  const argument = fact.contract.typeArguments.at(ordinal)
  if (argument === undefined) return undefined
  const specialized = Type.substituteGenericArgument(argument, substitution)
  return Type.isTypeArgument(specialized) ? specialized : undefined
}

const reflectedAggregateKindCode = (kind: StaticValue.AggregateKind): bigint => {
  switch (kind) {
    case 'Named':
      return 0n
    case 'Positional':
      return 1n
    case 'AnonymousNamed':
      return 2n
    case 'AnonymousPositional':
      return 3n
  }
}

const staticTextSpan = (
  fact: Elaboration.ExpressionFact,
  context: FactEvaluationContext,
): SourceSpan.SourceSpan | undefined => {
  const evaluated = context.expressionSpans.get(fact)
  if (evaluated !== undefined) return evaluated
  if (fact._tag === 'Call' && fact.staticTextSpan !== undefined) return fact.staticTextSpan
  if (fact._tag === 'StaticText') return fact.token?.span ?? fact.syntax.span
  if (fact._tag === 'Grouped') return staticTextSpan(fact.expression, context)
  if (fact._tag === 'Move') return staticTextSpan(fact.subject, context)
  if (fact._tag === 'Identifier') {
    if (fact.reference._tag === 'Resolved')
      return context.valueSpans.get(localValueKey(fact.reference.parameter))
    if (fact.reference._tag === 'ResolvedBinding')
      return context.valueSpans.get(localValueKey(fact.reference.binding))
  }
  if (
    fact._tag === 'Call' &&
    fact.reference._tag === 'ResolvedIntrinsicContract' &&
    fact.reference.intrinsic.id.actor === 'Intrinsic' &&
    (fact.reference.intrinsic.id.name === 'staticTextSlice' ||
      fact.reference.intrinsic.id.name === 'staticTextConcat')
  ) {
    const subject = fact.arguments.at(0)
    return subject === undefined ? undefined : staticTextSpan(subject.expression, context)
  }
  return undefined
}

/** Resolves static-text provenance for one analyzed expression without changing value identity. */
export interface TextOriginContext {
  readonly valueOrigins: ReadonlyMap<string, TextOrigin>
  readonly expressionOrigins: ReadonlyMap<Elaboration.ExpressionFact, TextOrigin>
}

export const staticTextOrigin = (
  fact: Elaboration.ExpressionFact,
  context: TextOriginContext,
): TextOrigin | undefined => {
  const evaluated = context.expressionOrigins.get(fact)
  if (evaluated !== undefined) return evaluated
  if (fact._tag === 'Call' && fact.staticTextOrigin !== undefined) return fact.staticTextOrigin
  if (fact._tag === 'StaticText')
    return sourceTextOrigin(fact.token?.span ?? fact.syntax.span, fact.data)
  if (fact._tag === 'Grouped') return staticTextOrigin(fact.expression, context)
  if (fact._tag === 'Move') return staticTextOrigin(fact.subject, context)
  if (fact._tag === 'Identifier') {
    if (fact.reference._tag === 'Resolved')
      return context.valueOrigins.get(localValueKey(fact.reference.parameter))
    if (fact.reference._tag === 'ResolvedBinding')
      return context.valueOrigins.get(localValueKey(fact.reference.binding))
  }
  if (
    fact._tag === 'Call' &&
    fact.reference._tag === 'ResolvedIntrinsicContract' &&
    fact.reference.intrinsic.id.actor === 'Intrinsic' &&
    (fact.reference.intrinsic.id.name === 'staticTextSlice' ||
      fact.reference.intrinsic.id.name === 'staticTextConcat')
  ) {
    const subject = fact.arguments.at(0)
    return subject === undefined ? undefined : staticTextOrigin(subject.expression, context)
  }
  return undefined
}

/** Evaluates one already-resolved expression fact without consulting a runtime engine. */
export const evaluateFact = (
  fact: Elaboration.ExpressionFact,
  context: FactEvaluationContext,
): Outcome<StaticValue.Value> => {
  switch (fact._tag) {
    case 'Unit':
      return complete(StaticValue.unit())
    case 'Boolean':
      return complete(StaticValue.boolean(fact.value))
    case 'Character':
      return fact.value === undefined
        ? unavailableFact(fact, context, 'character value is unavailable')
        : admittedValue(
            context.environment,
            { _tag: 'CharacterValue', value: fact.value },
            'StaticEvaluation.evaluateFact',
            fact.syntax.span,
            context.trace,
          )
    case 'Integer':
      return fact.integer._tag !== 'Available' || !Scalar.isIntegerSpelling(fact.integer.type)
        ? unavailableFact(fact, context, 'integer value is unavailable')
        : admittedValue(
            context.environment,
            { _tag: 'IntegerValue', type: fact.integer.type, value: fact.integer.value },
            'StaticEvaluation.evaluateFact',
            fact.syntax.span,
            context.trace,
          )
    case 'Duration':
      return fact.value === undefined
        ? unavailableFact(fact, context, 'duration value is unavailable')
        : admittedValue(
            context.environment,
            { _tag: 'IntegerValue', type: 'u64', value: fact.value },
            'StaticEvaluation.evaluateFact',
            fact.syntax.span,
            context.trace,
          )
    case 'Floating':
      return fact.floating._tag !== 'Available'
        ? unavailableFact(fact, context, 'floating value is unavailable')
        : admittedValue(
            context.environment,
            {
              _tag: 'FloatValue',
              type: fact.floating.type,
              bits: fact.floating.bits,
            },
            'StaticEvaluation.evaluateFact',
            fact.syntax.span,
            context.trace,
          )
    case 'StaticText':
      return fact.data === undefined || fact.data.kind !== 'Text'
        ? unavailableFact(fact, context, 'static text value is unavailable')
        : admittedValue(
            context.environment,
            {
              _tag: 'TextValue',
              bytes: fact.data.bytes,
              origin: sourceTextOrigin(fact.token?.span ?? fact.syntax.span, fact.data),
            },
            'StaticEvaluation.evaluateFact',
            fact.syntax.span,
            context.trace,
          )
    case 'Constant':
      return valueOfConstant(fact, context)
    case 'Grouped':
      return evaluateFact(fact.expression, context)
    case 'Move':
      return evaluateFact(fact.subject, context)
    case 'StructLiteral': {
      if (
        fact.target._tag !== 'Resolved' ||
        fact.target.struct.canonical._tag !== 'Canonical' ||
        fact.type._tag !== 'Available'
      )
        return unavailableFact(fact, context, 'struct value is unavailable')
      const fields: Array<StaticValue.AggregateField> = []
      for (const initializer of fact.initializers) {
        if (initializer.state._tag !== 'Resolved')
          return unavailableFact(fact, context, 'struct initializer is unavailable')
        const value = evaluateFact(initializer.expression, context)
        if (value._tag === 'Failed') return value
        fields.push(
          Object.freeze({ ordinal: initializer.state.field.id.ordinal, value: value.value }),
        )
      }
      const substitution =
        TypeInference.substitution(
          fact.target.struct.typeParameters.map((parameter) => parameter.type),
          fact.target.type.arguments,
        ) ?? new Map<string, Type.GenericArgument>()
      return constructAggregate(
        context.environment,
        Object.freeze({
          _tag: 'NominalAggregateIdentity',
          declaration: fact.target.struct.canonical.id,
          typeArguments: Object.freeze(fact.target.type.arguments.map(Type.genericArgumentKey)),
        }),
        fields,
        fact.syntax.span,
        context.trace,
        fact.target.struct.fields.flatMap((field) =>
          field.declaredType._tag === 'Resolved'
            ? [
                Object.freeze({
                  id: field.id,
                  type: Type.substitute(field.declaredType.type, substitution),
                }),
              ]
            : [],
        ),
      )
    }
    case 'UnionVariant': {
      if (
        fact.target._tag !== 'Resolved' ||
        fact.target.union.canonical._tag !== 'Canonical' ||
        fact.target.variant.canonical._tag !== 'Canonical' ||
        fact.type._tag !== 'Available'
      )
        return unavailableFact(fact, context, 'union value is unavailable')
      const fields: Array<StaticValue.AggregateField> = []
      for (const initializer of fact.initializers) {
        if (initializer.state._tag !== 'Resolved')
          return unavailableFact(fact, context, 'union initializer is unavailable')
        const value = evaluateFact(initializer.expression, context)
        if (value._tag === 'Failed') return value
        fields.push(
          Object.freeze({ ordinal: initializer.state.field.id.ordinal, value: value.value }),
        )
      }
      const substitution =
        TypeInference.substitution(
          fact.target.union.typeParameters.map((parameter) => parameter.type),
          fact.target.type.arguments,
        ) ?? new Map<string, Type.GenericArgument>()
      return constructAggregate(
        context.environment,
        Object.freeze({
          _tag: 'NominalAggregateIdentity',
          declaration: fact.target.union.canonical.id,
          typeArguments: Object.freeze(fact.target.type.arguments.map(Type.genericArgumentKey)),
          variant: Object.freeze({
            ordinal: fact.target.variant.id.ordinal,
            name: fact.target.variant.canonical.id.name,
          }),
        }),
        fields,
        fact.syntax.span,
        context.trace,
        fact.target.variant.fields.flatMap((field) =>
          field.declaredType._tag === 'Resolved'
            ? [
                Object.freeze({
                  id: field.id,
                  type: Type.substitute(field.declaredType.type, substitution),
                }),
              ]
            : [],
        ),
      )
    }
    case 'ArrayLiteral': {
      if (fact.state._tag !== 'Complete')
        return unavailableFact(fact, context, 'array value is unavailable')
      const fields: Array<StaticValue.AggregateField> = []
      for (const element of fact.elements) {
        const value = evaluateFact(element.expression, context)
        if (value._tag === 'Failed') return value
        fields.push(Object.freeze({ ordinal: element.ordinal, value: value.value }))
      }
      return constructAggregate(
        context.environment,
        Object.freeze({
          _tag: 'ArrayAggregateIdentity',
          element: Type.key(fact.state.type.element),
          length: fact.state.type.length,
        }),
        fields,
        fact.syntax.span,
        context.trace,
      )
    }
    case 'FieldProjection': {
      if (fact.state._tag !== 'Resolved')
        return unavailableFact(fact, context, 'projected static field is unavailable')
      const subject = evaluateFact(fact.subject, context)
      if (subject._tag === 'Failed') return subject
      if (subject.value._tag !== 'AggregateValue')
        return unavailableFact(fact, context, 'field projection depends on runtime storage')
      const ordinal = fact.state.field.id.ordinal
      const field = subject.value.fields.find((candidate) => candidate.ordinal === ordinal)
      return field === undefined
        ? unavailableFact(fact, context, 'projected static field has no admitted value')
        : complete(field.value)
    }
    case 'Identifier': {
      const reference = fact.reference
      let value: StaticValue.Value | undefined
      if (reference._tag === 'Resolved')
        value = context.values.get(localValueKey(reference.parameter))
      else if (reference._tag === 'ResolvedBinding')
        value = context.values.get(localValueKey(reference.binding))
      if (value === undefined)
        return unavailableFact(fact, context, 'identifier depends on runtime storage')
      const origin = staticTextOrigin(fact, context)
      return value._tag === 'TextValue' && origin !== undefined
        ? complete(Object.freeze({ ...value, origin }))
        : complete(value)
    }
    case 'EnumMember': {
      const member = fact.member
      const type = fact.enum.canonical
      const representation = fact.enum.representation
      if (
        member?.name._tag !== 'Present' ||
        member.discriminant._tag !== 'Available' ||
        type._tag !== 'Canonical' ||
        representation._tag !== 'Available'
      )
        return unavailableFact(fact, context, 'enum member is unavailable')
      return constructEnum(
        context.environment,
        type.id,
        member.name.spelling,
        representation.scalar.spelling,
        member.discriminant.value,
        fact.syntax.span,
        context.trace,
      )
    }
    case 'ShortCircuit': {
      const left = fact.arguments.at(0)
      const right = fact.arguments.at(1)
      if (left === undefined || right === undefined)
        return unavailableFact(fact, context, 'short-circuit operands are unavailable')
      const selected = evaluateFact(left.expression, context)
      if (selected._tag === 'Failed') return selected
      if (selected.value._tag !== 'BooleanValue')
        return unavailableFact(fact, context, 'short-circuit condition is not bool')
      if (fact.operator === 'And' && !selected.value.value) return selected
      if (fact.operator === 'Or' && selected.value.value) return selected
      return evaluateFact(right.expression, context)
    }
    case 'Operator': {
      const operands = evaluateArguments(fact.arguments, context)
      if (operands._tag === 'Failed') return operands
      const left = operands.value.at(0)
      const right = operands.value.at(1)
      if (
        (fact.operator === 'Equals' || fact.operator === 'NotEquals') &&
        left?._tag === 'EnumValue' &&
        right?._tag === 'EnumValue'
      )
        return evaluateEnumEquality(fact.operator, left, right, fact.syntax.span, context.trace)
      if (
        fact.operator === 'Add' ||
        fact.operator === 'Subtract' ||
        fact.operator === 'Multiply' ||
        fact.operator === 'Divide' ||
        fact.operator === 'Remainder' ||
        fact.operator === 'Negate' ||
        fact.operator === 'Equals' ||
        fact.operator === 'NotEquals' ||
        fact.operator === 'LessThan' ||
        fact.operator === 'LessOrEqual' ||
        fact.operator === 'GreaterThan' ||
        fact.operator === 'GreaterOrEqual' ||
        fact.operator === 'Not'
      )
        return evaluatePrimitive(
          context.environment,
          fact.operator,
          operands.value,
          fact.syntax.span,
          context.trace,
        )
      return unavailableFact(fact, context, `${fact.operator} is not admitted statically`)
    }
    case 'Call': {
      if (fact.staticFailure !== undefined) return failed(fact.staticFailure)
      const arguments_ = evaluateArguments(fact.arguments, context)
      if (arguments_._tag === 'Failed') return arguments_
      if (
        fact.reference._tag === 'ResolvedIntrinsicContract' &&
        fact.reference.intrinsic.id.actor === 'Intrinsic'
      ) {
        const operation = fact.reference.intrinsic.id.name
        if (operation === 'targetProfile')
          return targetProfile(context.environment, fact.syntax.span, context.trace)
        const typeArgument = intrinsicTypeArgument(fact, 0, context.typeSubstitution)
        if (operation === 'reflectType' || operation === 'reflectFields') {
          if (typeArgument === undefined)
            return unavailableFact(fact, context, `${operation} requires one concrete owner type`)
          return context.reflect(
            typeArgument,
            operation === 'reflectType' ? 'Type' : 'Fields',
            fact.syntax.span,
            context.trace,
          )
        }
        if (operation === 'reflectTypeKind') {
          const descriptor = arguments_.value.at(0)
          if (descriptor?._tag !== 'TypeDescriptorValue')
            return unavailableFact(fact, context, `${operation} requires one type descriptor`)
          return admittedValue(
            context.environment,
            {
              _tag: 'IntegerValue',
              type: 'u8',
              value: reflectedAggregateKindCode(descriptor.kind),
            },
            'StaticEvaluation.reflectTypeKind',
            fact.syntax.span,
            context.trace,
          )
        }
        if (operation.startsWith('reflectField')) {
          const descriptor = arguments_.value.at(0)
          if (descriptor?._tag !== 'FieldDescriptorValue')
            return unavailableFact(fact, context, `${operation} requires one field descriptor`)
          if (operation === 'reflectFieldKind')
            return admittedValue(
              context.environment,
              {
                _tag: 'IntegerValue',
                type: 'u8',
                value: descriptor.member._tag === 'LabeledField' ? 0n : 1n,
              },
              'StaticEvaluation.reflectFieldKind',
              fact.syntax.span,
              context.trace,
            )
          if (operation === 'reflectFieldLabel')
            return descriptor.member._tag === 'LabeledField'
              ? admittedValue(
                  context.environment,
                  {
                    _tag: 'TextValue',
                    bytes: Array.from(new TextEncoder().encode(descriptor.member.label)),
                  },
                  'StaticEvaluation.reflectFieldLabel',
                  fact.syntax.span,
                  context.trace,
                )
              : unavailableFact(fact, context, `${operation} cannot read a positional field`)
          if (operation === 'reflectFieldOrdinal')
            return descriptor.member._tag === 'PositionalField'
              ? admittedValue(
                  context.environment,
                  {
                    _tag: 'IntegerValue',
                    type: 'usize',
                    value: BigInt(descriptor.member.ordinal),
                  },
                  'StaticEvaluation.reflectFieldOrdinal',
                  fact.syntax.span,
                  context.trace,
                )
              : unavailableFact(fact, context, `${operation} cannot read a labeled field`)
          return unavailableFact(fact, context, `${operation} is not admitted reflection metadata`)
        }
        if (operation === 'staticSequenceEmpty') {
          if (typeArgument === undefined)
            return unavailableFact(fact, context, `${operation} requires one concrete element type`)
          return admittedValue(
            context.environment,
            StaticValue.emptySequence(typeArgument),
            'StaticEvaluation.staticSequenceEmpty',
            fact.syntax.span,
            context.trace,
          )
        }
        if (operation.startsWith('staticSequence')) {
          if (typeArgument === undefined)
            return unavailableFact(fact, context, `${operation} requires one concrete element type`)
          const sequence = arguments_.value.at(0)
          if (sequence?._tag !== 'StaticSequenceValue')
            return unavailableFact(fact, context, `${operation} requires one static sequence`)
          if (operation === 'staticSequenceLength')
            return admittedValue(
              context.environment,
              {
                _tag: 'IntegerValue',
                type: 'usize',
                value: BigInt(StaticValue.sequenceLength(sequence)),
              },
              'StaticEvaluation.staticSequenceLength',
              fact.syntax.span,
              context.trace,
            )
          if (operation === 'staticSequenceAppend') {
            const value = arguments_.value.at(1)
            if (value === undefined)
              return unavailableFact(fact, context, `${operation} requires one static value`)
            const appended = StaticValue.appendSequence(sequence, typeArgument, value)
            return appended === undefined
              ? unavailableFact(fact, context, `${operation} element type does not match`)
              : admittedValue(
                  context.environment,
                  appended,
                  'StaticEvaluation.staticSequenceAppend',
                  fact.syntax.span,
                  context.trace,
                )
          }
          if (operation === 'staticSequenceConcat') {
            const right = arguments_.value.at(1)
            if (right?._tag !== 'StaticSequenceValue')
              return unavailableFact(fact, context, `${operation} requires two static sequences`)
            const concatenated = StaticValue.concatenateSequences(sequence, right)
            return concatenated === undefined
              ? unavailableFact(fact, context, `${operation} element types do not match`)
              : admittedValue(
                  context.environment,
                  concatenated,
                  'StaticEvaluation.staticSequenceConcat',
                  fact.syntax.span,
                  context.trace,
                )
          }
          if (operation === 'staticSequenceAt') {
            const index = arguments_.value.at(1)
            if (
              index?._tag !== 'IntegerValue' ||
              index.value < 0n ||
              index.value > BigInt(Number.MAX_SAFE_INTEGER)
            )
              return unavailableFact(fact, context, `${operation} requires one static index`)
            const element = StaticValue.sequenceElement(sequence, Number(index.value))
            return element === undefined
              ? unavailableFact(fact, context, `${operation} index is out of bounds`)
              : complete(element)
          }
          return unavailableFact(
            fact,
            context,
            `${operation} is not an admitted sequence operation`,
          )
        }
        const text = arguments_.value.at(0)
        const argument = fact.arguments.at(0)
        const literal =
          (argument === undefined ? undefined : staticTextSpan(argument.expression, context)) ??
          fact.syntax.span
        if (text?._tag !== 'TextValue')
          return unavailableFact(fact, context, `${operation} requires static text`)
        if (operation === 'staticTextByteLength')
          return staticTextByteLength(context.environment, text, literal, context.trace)
        if (operation === 'staticTextConcat') {
          const right = arguments_.value.at(1)
          if (right?._tag !== 'TextValue')
            return unavailableFact(fact, context, `${operation} requires two static texts`)
          const concatenated = staticTextConcat(
            context.environment,
            text,
            right,
            literal,
            context.trace,
          )
          if (concatenated._tag === 'Complete') {
            const origin =
              argument === undefined ? undefined : staticTextOrigin(argument.expression, context)
            if (origin !== undefined) context.expressionOrigins.set(fact, origin)
            context.expressionSpans.set(fact, literal)
            if (origin !== undefined && concatenated.value._tag === 'TextValue')
              return complete(Object.freeze({ ...concatenated.value, origin }))
          }
          return concatenated
        }
        const first = arguments_.value.at(1)
        if (first?._tag !== 'IntegerValue')
          return unavailableFact(fact, context, `${operation} requires a static index`)
        if (operation === 'staticTextByteAt')
          return staticTextByteAt(context.environment, text, first.value, literal, context.trace)
        const second = arguments_.value.at(2)
        if (operation === 'staticTextSlice' && second?._tag === 'IntegerValue') {
          const sliced = staticTextSlice(
            context.environment,
            text,
            first.value,
            second.value,
            literal,
            context.trace,
          )
          if (sliced._tag === 'Complete') {
            const origin =
              argument === undefined ? undefined : staticTextOrigin(argument.expression, context)
            const slicedOrigin =
              origin === undefined
                ? undefined
                : sliceTextOrigin(origin, Number(first.value), Number(second.value))
            if (slicedOrigin !== undefined) {
              context.expressionOrigins.set(fact, slicedOrigin)
            }
            context.expressionSpans.set(fact, literal)
            if (slicedOrigin !== undefined && sliced.value._tag === 'TextValue')
              return complete(Object.freeze({ ...sliced.value, origin: slicedOrigin }))
          }
          return sliced
        }
      }
      if (fact.reference._tag === 'Resolved' && fact.reference.declaration.phase === 'Static') {
        const called = context.call(
          fact.reference.declaration,
          arguments_.value,
          Object.freeze(
            fact.arguments.map((argument) => staticTextSpan(argument.expression, context)),
          ),
          Object.freeze(
            fact.arguments.map((argument, ordinal) => {
              const value = arguments_.value.at(ordinal)
              return (
                staticTextOrigin(argument.expression, context) ??
                (value?._tag === 'TextValue' ? value.origin : undefined)
              )
            }),
          ),
          fact.syntax.span,
          context.trace,
          Object.freeze({
            typeArguments: Object.freeze(
              fact.contract._tag === 'Compatible'
                ? fact.contract.typeArguments.map((argument) =>
                    Type.substituteGenericArgument(argument, context.typeSubstitution ?? new Map()),
                  )
                : [],
            ),
            evidence: Object.freeze(
              fact.contract._tag === 'Compatible'
                ? fact.contract.evidence.map(Constraint.evidenceKey)
                : [],
            ),
            contractRow: Object.freeze([]),
          }),
        )
        if (called.textSpan !== undefined) context.expressionSpans.set(fact, called.textSpan)
        if (called.textOrigin !== undefined) context.expressionOrigins.set(fact, called.textOrigin)
        return called.outcome
      }
      return unavailableFact(fact, context, 'ordinary calls are runtime operations')
    }
    case 'CompileError': {
      const message = evaluateFact(fact.message, context)
      if (message._tag === 'Failed') return message
      if (message.value._tag !== 'TextValue')
        return unavailableFact(fact, context, 'compileError message must be static text')
      const origin = staticTextOrigin(fact.message, context) ?? message.value.origin
      return failed(
        compileError(
          new TextDecoder().decode(Uint8Array.from(message.value.bytes)),
          (origin === undefined ? undefined : textOriginSpan(origin)) ?? fact.syntax.span,
          context.trace,
          origin,
        ),
      )
    }
    default:
      return unavailableFact(fact, context, `${fact._tag} is not admitted statically`)
  }
}

type StatementControl =
  | { readonly _tag: 'Fallthrough' }
  | { readonly _tag: 'Return'; readonly value: StaticValue.Value }
  | { readonly _tag: 'Break' }
  | { readonly _tag: 'Continue' }

const fallthrough: StatementControl = Object.freeze({ _tag: 'Fallthrough' })

const evaluateStatementSequence = (
  statements: ReadonlyArray<Elaboration.StatementFact>,
  context: FactEvaluationContext,
): Outcome<StatementControl> => {
  const values = context.values instanceof Map ? context.values : new Map(context.values)
  const valueSpans =
    context.valueSpans instanceof Map ? context.valueSpans : new Map(context.valueSpans)
  const nestedContext: FactEvaluationContext = Object.freeze({ ...context, values, valueSpans })
  const valueOrigins =
    context.valueOrigins instanceof Map ? context.valueOrigins : new Map(context.valueOrigins)
  const contextual: FactEvaluationContext = Object.freeze({
    ...nestedContext,
    valueOrigins,
  })
  for (const statement of statements) {
    const statementSpan =
      statement._tag === 'BindStatement' ? statement.binding.syntax.span : statement.syntax.span
    const exhausted = context.step?.(statementSpan, context.trace)
    if (exhausted !== undefined) return failed(exhausted)
    if (statement._tag === 'BindStatement') {
      const value = evaluateFact(statement.binding.initializer, contextual)
      if (value._tag === 'Failed') return value
      const key = localValueKey(statement.binding)
      values.set(key, value.value)
      const span = staticTextSpan(statement.binding.initializer, contextual)
      if (span === undefined) valueSpans.delete(key)
      else valueSpans.set(key, span)
      const origin = staticTextOrigin(statement.binding.initializer, contextual)
      if (origin === undefined) valueOrigins.delete(key)
      else valueOrigins.set(key, origin)
      continue
    }
    if (statement._tag === 'ExpressionStatement') {
      const value = evaluateFact(statement.expression, contextual)
      if (value._tag === 'Failed') return value
      continue
    }
    if (statement._tag === 'ReturnStatement') {
      const value = evaluateFact(statement.expression, contextual)
      if (value._tag === 'Complete' && context.returnedTextSpan !== undefined)
        context.returnedTextSpan.value = staticTextSpan(statement.expression, contextual)
      if (value._tag === 'Complete' && context.returnedTextOrigin !== undefined)
        context.returnedTextOrigin.value = staticTextOrigin(statement.expression, contextual)
      return value._tag === 'Failed'
        ? value
        : complete(Object.freeze({ _tag: 'Return', value: value.value }))
    }
    if (statement._tag === 'IfStatement') {
      const condition = evaluateFact(statement.condition, contextual)
      if (condition._tag === 'Failed') return condition
      if (condition.value._tag !== 'BooleanValue')
        return failed(
          phaseViolation(
            'StaticEvaluation.evaluateStatements',
            'if condition is not bool',
            statement.condition.syntax.span,
            context.trace,
          ),
        )
      const selected = evaluateStatementSequence(
        condition.value.value ? statement.taken : statement.otherwise,
        contextual,
      )
      if (selected._tag === 'Failed' || selected.value._tag !== 'Fallthrough') return selected
      continue
    }
    if (statement._tag === 'WhileStatement') {
      while (true) {
        const condition = evaluateFact(statement.condition, contextual)
        if (condition._tag === 'Failed') return condition
        if (condition.value._tag !== 'BooleanValue')
          return failed(
            phaseViolation(
              'StaticEvaluation.evaluateStatements',
              'while condition is not bool',
              statement.condition.syntax.span,
              context.trace,
            ),
          )
        if (!condition.value.value) break
        const body = evaluateStatementSequence(statement.body, contextual)
        if (body._tag === 'Failed') return body
        if (body.value._tag === 'Return') return body
        if (body.value._tag === 'Break') break
      }
      continue
    }
    if (statement._tag === 'WriteStatement') {
      if (statement.root?._tag !== 'BindingFact' || statement.root.phase !== 'Static')
        return failed(
          phaseViolation(
            'StaticEvaluation.evaluateStatements',
            'assignment does not replace one static local',
            statement.syntax.span,
            context.trace,
          ),
        )
      const value = evaluateFact(statement.value, contextual)
      if (value._tag === 'Failed') return value
      const key = localValueKey(statement.root)
      values.set(key, value.value)
      const span = staticTextSpan(statement.value, contextual)
      if (span === undefined) valueSpans.delete(key)
      else valueSpans.set(key, span)
      const origin = staticTextOrigin(statement.value, contextual)
      if (origin === undefined) valueOrigins.delete(key)
      else valueOrigins.set(key, origin)
      continue
    }
    if (statement._tag === 'BreakStatement') return complete(Object.freeze({ _tag: 'Break' }))
    if (statement._tag === 'ContinueStatement') return complete(Object.freeze({ _tag: 'Continue' }))
    return failed(
      phaseViolation(
        'StaticEvaluation.evaluateStatements',
        `${statement._tag} is not admitted in a static function`,
        statement.syntax.span,
        context.trace,
      ),
    )
  }
  return complete(fallthrough)
}

/** Executes one fully analyzed static function body to a complete immutable value. */
export const evaluateStatements = (
  statements: ReadonlyArray<Elaboration.StatementFact>,
  context: FactEvaluationContext,
): Outcome<StaticValue.Value> => {
  const result = evaluateStatementSequence(statements, context)
  if (result._tag === 'Failed') return result
  return result.value._tag === 'Return'
    ? complete(result.value.value)
    : complete(StaticValue.unit())
}

/** Immutable deterministic resource counters for one static-evaluation session. */
export interface Budget {
  readonly steps: number
  readonly callDepth: number
  readonly maximumCallDepth: number
  readonly retainedValueBytes: number
  readonly residualNodes: number
}

interface MutableBudget {
  steps: number
  callDepth: number
  maximumCallDepth: number
  retainedValueBytes: number
  residualNodes: number
  failure?: StaticFailure
}

const budgetSnapshot = (budget: MutableBudget): Budget =>
  Object.freeze({
    steps: budget.steps,
    callDepth: budget.callDepth,
    maximumCallDepth: budget.maximumCallDepth,
    retainedValueBytes: budget.retainedValueBytes,
    residualNodes: budget.residualNodes,
  })

export type Outcome<A> =
  | { readonly _tag: 'Complete'; readonly value: A }
  | { readonly _tag: 'Failed'; readonly failure: StaticFailure }

/** Completes one callback evaluation with immutable deterministic output supplied by the caller. */
export const complete = <A>(value: A): Outcome<A> => Object.freeze({ _tag: 'Complete', value })

/** Stops one callback evaluation without a partial value or residual body. */
export const failed = <A = never>(failure: StaticFailure): Outcome<A> =>
  Object.freeze({ _tag: 'Failed', failure })

/** The explicit state of one target-and-application cache entry. */
export type CacheState<A> = Pending | Complete<A> | Failed

export interface Pending {
  readonly _tag: 'Pending'
  readonly trace: Trace
}

export interface Complete<A> {
  readonly _tag: 'Complete'
  readonly value: A
}

export interface Failed {
  readonly _tag: 'Failed'
  readonly failure: StaticFailure
}

export interface CacheEntry<A> {
  readonly key: string
  readonly state: CacheState<A>
}

interface MutableState<A> {
  readonly cache: Map<string, CacheState<A>>
  readonly budget: MutableBudget
}

const stateSymbol: unique symbol = Symbol('StaticEvaluation.state')

/** One target-scoped static-evaluation session with hidden cache and accounting state. */
export interface Evaluation<A> {
  readonly _tag: 'StaticEvaluation'
  readonly environment: TargetEnvironment
  readonly limits: Limits
  readonly [stateSymbol]: MutableState<A>
}

/** Starts one target-scoped static-evaluation session. */
export const make = <A>(target: Target.Target, policy: Limits = defaultLimits): Evaluation<A> =>
  Object.freeze({
    _tag: 'StaticEvaluation',
    environment: targetEnvironment(target),
    limits: limits(policy),
    [stateSymbol]: {
      cache: new Map(),
      budget: {
        steps: 0,
        callDepth: 0,
        maximumCallDepth: 0,
        retainedValueBytes: 0,
        residualNodes: 0,
      },
    },
  })

/** Returns a frozen observation of deterministic session accounting. */
export const budget = <A>(self: Evaluation<A>): Budget => budgetSnapshot(self[stateSymbol].budget)

/** Returns cache states in canonical key order without exposing the mutable cache map. */
export const cacheEntries = <A>(self: Evaluation<A>): ReadonlyArray<CacheEntry<A>> =>
  Object.freeze(
    [...self[stateSymbol].cache]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([key, state]) => Object.freeze({ key, state })),
  )

/** Canonical target-and-application identity used only inside static-evaluation coordination. */
export const applicationKey = (environment: TargetEnvironment, application: Application): string =>
  Canonical.record('StaticApplication', [
    environment.target,
    application.declaration.module,
    application.declaration.name,
    Canonical.array(application.typeArguments),
    Canonical.array(application.evidence),
    Canonical.array(application.contractRow),
    Canonical.array(application.staticArguments.map(StaticValue.key)),
  ])

const applicationFrame = (
  environment: TargetEnvironment,
  application: Application,
): ApplicationFrame =>
  Object.freeze({
    _tag: 'StaticApplicationFrame',
    declaration: Object.freeze({ ...application.declaration }),
    target: environment.target,
    staticArguments: Object.freeze(application.staticArguments.map(StaticValue.presentation)),
    span: application.span,
  })

type LimitTag = 'StepLimit' | 'CallDepthLimit' | 'RetainedValueLimit' | 'ResidualGrowthLimit'

const frameSpan = (frame: TraceFrame): SourceSpan.SourceSpan =>
  frame._tag === 'StaticTextFrame' ? frame.literal : frame.span

const limitFailure = (
  tag: LimitTag,
  limit: number,
  attempted: number,
  trace: Trace,
): StaticFailure => {
  const lastFrame = trace.at(-1)
  const span = lastFrame === undefined ? undefined : frameSpan(lastFrame)
  if (span === undefined) throw new RangeError('Static limit failure lost its source trace')
  return Object.freeze({ _tag: tag, limit, attempted, span, trace: frozenTrace(trace) })
}

const charge = <A>(
  self: Evaluation<A>,
  field: 'steps' | 'retainedValueBytes' | 'residualNodes',
  amount: number,
  tag: Exclude<LimitTag, 'CallDepthLimit'>,
  trace: Trace,
): StaticFailure | undefined => {
  if (!Number.isSafeInteger(amount) || amount < 0)
    throw new RangeError('Static budget charges must be non-negative safe integers')
  const state = self[stateSymbol]
  if (state.budget.failure !== undefined) return state.budget.failure
  const attempted = state.budget[field] + amount
  const limit = self.limits[field]
  if (attempted > limit) {
    const failure = limitFailure(tag, limit, attempted, trace)
    state.budget.failure = failure
    return failure
  }
  state.budget[field] = attempted
  return undefined
}

const enterCall = <A>(self: Evaluation<A>, trace: Trace): StaticFailure | undefined => {
  const state = self[stateSymbol]
  if (state.budget.failure !== undefined) return state.budget.failure
  const attempted = state.budget.callDepth + 1
  state.budget.maximumCallDepth = Math.max(state.budget.maximumCallDepth, attempted)
  if (attempted > self.limits.callDepth) {
    const failure = limitFailure('CallDepthLimit', self.limits.callDepth, attempted, trace)
    state.budget.failure = failure
    return failure
  }
  state.budget.callDepth = attempted
  return undefined
}

const leaveCall = <A>(self: Evaluation<A>): void => {
  const state = self[stateSymbol]
  state.budget.callDepth = Math.max(0, state.budget.callDepth - 1)
}

export interface EvaluationContext<A> {
  readonly application: Application
  readonly environment: TargetEnvironment
  readonly limits: Limits
  readonly trace: Trace
  readonly budget: () => Budget
  readonly step: (amount?: number) => StaticFailure | undefined
  readonly stepAt: (trace: Trace, amount?: number) => StaticFailure | undefined
  readonly retain: (value: StaticValue.Value) => StaticFailure | undefined
  readonly growResidual: (nodes?: number) => StaticFailure | undefined
  readonly growResidualAt: (trace: Trace, nodes?: number) => StaticFailure | undefined
  readonly withTrace: (...frames: ReadonlyArray<TraceFrame>) => EvaluationContext<A>
  readonly evaluate: (
    application: Application,
    callback: EvaluationCallback<A>,
  ) => ApplicationResult<A>
}

/** Future syntax residualization plugs into this deterministic callback boundary. */
export type EvaluationCallback<A> = (context: EvaluationContext<A>) => Outcome<A>

export type ApplicationResult<A> =
  | {
      readonly _tag: 'Complete'
      readonly key: string
      readonly cached: boolean
      readonly value: A
      readonly budget: Budget
    }
  | {
      readonly _tag: 'Failed'
      readonly key: string
      readonly cached: boolean
      readonly failure: StaticFailure
      readonly budget: Budget
    }

const resultOf = <A>(
  self: Evaluation<A>,
  key: string,
  state: Complete<A> | Failed,
  cached: boolean,
): ApplicationResult<A> =>
  state._tag === 'Complete'
    ? Object.freeze({
        _tag: 'Complete',
        key,
        cached,
        value: state.value,
        budget: budget(self),
      })
    : Object.freeze({
        _tag: 'Failed',
        key,
        cached,
        failure: state.failure,
        budget: budget(self),
      })

const contextOf = <A>(
  self: Evaluation<A>,
  application: Application,
  trace: Trace,
): EvaluationContext<A> =>
  Object.freeze({
    application,
    environment: self.environment,
    limits: self.limits,
    trace,
    budget: () => budget(self),
    step: (amount = 1) => charge(self, 'steps', amount, 'StepLimit', trace),
    stepAt: (at: Trace, amount = 1) => charge(self, 'steps', amount, 'StepLimit', at),
    retain: (value: StaticValue.Value) =>
      charge(
        self,
        'retainedValueBytes',
        StaticValue.retainedSize(value),
        'RetainedValueLimit',
        trace,
      ),
    growResidual: (nodes = 1) => charge(self, 'residualNodes', nodes, 'ResidualGrowthLimit', trace),
    growResidualAt: (at: Trace, nodes = 1) =>
      charge(self, 'residualNodes', nodes, 'ResidualGrowthLimit', at),
    withTrace: (...frames: ReadonlyArray<TraceFrame>) =>
      contextOf(self, application, appendTrace(trace, ...frames)),
    evaluate: (nested: Application, callback: EvaluationCallback<A>) =>
      evaluateAt(self, nested, callback, trace),
  })

const evaluateAt = <A>(
  self: Evaluation<A>,
  application: Application,
  callback: EvaluationCallback<A>,
  parentTrace: Trace,
): ApplicationResult<A> => {
  const key = applicationKey(self.environment, application)
  const state = self[stateSymbol]
  const cached = state.cache.get(key)
  if (cached?._tag === 'Complete' || cached?._tag === 'Failed')
    return resultOf(self, key, cached, true)
  const trace = appendTrace(parentTrace, applicationFrame(self.environment, application))
  if (cached?._tag === 'Pending') {
    const failure: Cycle = Object.freeze({
      _tag: 'Cycle',
      declaration: Object.freeze({ ...application.declaration }),
      span: application.span,
      trace: frozenTrace(trace),
    })
    return resultOf(self, key, Object.freeze({ _tag: 'Failed', failure }), false)
  }

  state.cache.set(key, Object.freeze({ _tag: 'Pending', trace }))
  const depthFailure = enterCall(self, trace)
  if (depthFailure !== undefined) {
    const failedState: Failed = Object.freeze({ _tag: 'Failed', failure: depthFailure })
    state.cache.set(key, failedState)
    return resultOf(self, key, failedState, false)
  }

  let outcome: Outcome<A>
  try {
    outcome = callback(contextOf(self, application, trace))
  } catch (defect) {
    state.cache.delete(key)
    throw defect
  } finally {
    leaveCall(self)
  }
  const finalOutcome =
    state.budget.failure === undefined ? outcome : failed<A>(state.budget.failure)
  const completedState: Complete<A> | Failed =
    finalOutcome._tag === 'Complete'
      ? Object.freeze({ _tag: 'Complete', value: finalOutcome.value })
      : Object.freeze({ _tag: 'Failed', failure: finalOutcome.failure })
  state.cache.set(key, completedState)
  return resultOf(self, key, completedState, false)
}

/** Evaluates or reuses one canonical target application through the supplied deterministic policy. */
export const evaluateApplication = <A>(
  self: Evaluation<A>,
  application: Application,
  callback: EvaluationCallback<A>,
): ApplicationResult<A> => evaluateAt(self, application, callback, Object.freeze([]))

/** Evaluates a nested canonical application while retaining its source-level parent trace. */
export const evaluateApplicationFrom = <A>(
  self: Evaluation<A>,
  application: Application,
  parentTrace: Trace,
  callback: EvaluationCallback<A>,
): ApplicationResult<A> => evaluateAt(self, application, callback, parentTrace)
