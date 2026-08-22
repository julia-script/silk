import type { TraceEvent } from './BootstrapTrace.js'
import type { EffectOutcomeValue, EffectValue, Value } from './BootstrapValue.js'
import { repackFailurePayload } from './BootstrapValue.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Mir from './Mir.js'
import type * as Termination from './Termination.js'
import * as Type from './Type.js'

interface LocalState {
  readonly value: Value
  readonly fromCall: boolean
}

export interface CaptureContext {
  readonly frame: number
  readonly cells: Map<string, LocalState>
  readonly read: (local: Mir.LocalId) => LocalState
  readonly cellKey: (frame: number, local: number) => string
}

/** Captures one Effect closure, materializing borrowed fields in evaluator cells. */
export const makeValue = (
  context: CaptureContext,
  operation: Extract<Mir.Operation, { readonly _tag: 'MakeEffect' }>,
): EffectValue => {
  const captures = operation.captures.map((capture, ordinal): Value => {
    const field = operation.type.environment.fields.at(ordinal)
    if (
      capture.access === 'Copy' ||
      capture.access === 'Take' ||
      field?.representation === 'Callable' ||
      field?.effectIdentity !== undefined
    )
      return context.read(capture.source).value
    const key = context.cellKey(context.frame, capture.source.ordinal)
    if (!context.cells.has(key)) context.cells.set(key, context.read(capture.source))
    return Object.freeze({
      _tag: 'EffectBorrowValue',
      frame: context.frame,
      cell: capture.source.ordinal,
      access: capture.access,
    })
  })
  return Object.freeze({
    _tag: 'EffectValue',
    type: operation.type.type,
    site: operation.type.site,
    runner: operation.runner,
    runnerTypeArguments: operation.runnerTypeArguments,
    captures: Object.freeze(captures),
  })
}

type RunnableOperation = Extract<
  Mir.Operation,
  { readonly _tag: 'RunEffectComposite' | 'RunEffectValue' | 'RunStaticEffect' }
>

export interface Execution {
  readonly runner: DeclarationIndex.CanonicalId
  readonly runnerTypeArguments: ReadonlyArray<Type.GenericArgument>
  readonly arguments: ReadonlyArray<Value>
  readonly composite?: {
    readonly alternative: Extract<
      RunnableOperation,
      { readonly _tag: 'RunEffectComposite' }
    >['alternatives'][number]
  }
}

/** Resolves a stored, composite, or static Effect to its runner and ordered runtime arguments. */
export const prepareExecution = (
  operation: RunnableOperation,
  read: (local: Mir.LocalId) => LocalState,
): Execution => {
  if (operation._tag === 'RunEffectComposite') {
    const value = read(operation.effect).value
    if (value._tag !== 'EffectCompositeValue')
      throw new RangeError('MIR attempted to run a non-composite Effect value')
    const alternative = operation.alternatives.at(value.alternative)
    if (alternative === undefined)
      throw new RangeError('MIR Effect composite selected an absent alternative')
    return Object.freeze({
      runner: alternative.runner,
      runnerTypeArguments: alternative.runnerTypeArguments,
      arguments: Object.freeze([
        ...value.effect.captures,
        ...alternative.arguments.map((argument) => read(argument).value),
      ]),
      composite: Object.freeze({ alternative }),
    })
  }
  if (operation._tag === 'RunEffectValue') {
    const effect = read(operation.effect).value
    if (effect._tag !== 'EffectValue')
      throw new RangeError('MIR attempted to run a non-Effect value')
    return Object.freeze({
      runner: operation.runner,
      runnerTypeArguments: operation.runnerTypeArguments,
      arguments: Object.freeze([
        ...effect.captures,
        ...operation.arguments.map((argument) => read(argument).value),
      ]),
    })
  }
  return Object.freeze({
    runner: operation.runner,
    runnerTypeArguments: operation.runnerTypeArguments,
    arguments: Object.freeze([
      ...operation.captures.map((capture) => read(capture.source).value),
      ...operation.arguments.map((argument) => read(argument).value),
    ]),
  })
}

/** Repackages a runner failure through an optional composite-alternative mapping. */
export const normalizeOutcome = (
  operation: RunnableOperation,
  raw: EffectOutcomeValue,
  execution: Execution,
): EffectOutcomeValue => {
  const alternative = execution.composite?.alternative
  if (alternative === undefined || raw.tag === 0) return raw
  const mapping = alternative.tagMappings.find((candidate) => candidate.source === raw.tag)
  if (mapping === undefined)
    throw new RangeError('MIR Effect composite has no alternative failure mapping')
  return Object.freeze({
    _tag: 'EffectOutcomeValue',
    type: operation.outcomeType.type,
    tag: mapping.target,
    payload: repackFailurePayload(
      raw.payload,
      alternative.type.type,
      raw.tag,
      operation.outcomeType.type,
      mapping.target,
    ),
  })
}

export const isPhysicalEntryAdapter = (name: string): boolean =>
  name === '$effect-entry' || name === '$unit-entry'

export const failureIdentity = (type: Type.Effect | Type.FailureRow, tag: number): string => {
  const failure = Type.failureMembers(type).at(tag - 1)
  if (failure === undefined) throw new RangeError(`Effect failure tag ${tag} has no type identity`)
  return Type.encode(failure)
}

export const logicalPathAt = (
  trace: ReadonlyArray<TraceEvent>,
  through: number,
): ReadonlyArray<Termination.LogicalFrame> => {
  const active = new Map<
    number,
    { readonly depth: number; readonly value: Termination.LogicalFrame }
  >()
  for (let index = 0; index <= through; index += 1) {
    const event = trace.at(index)
    if (event?._tag === 'Entry' && !isPhysicalEntryAdapter(event.function.name))
      active.set(
        event.frame,
        Object.freeze({
          depth: event.depth,
          value: Object.freeze({ function: event.function, provenance: event.span }),
        }),
      )
    else if (event?._tag === 'Return') active.delete(event.frame)
  }
  return Object.freeze(
    [...active.values()].sort((left, right) => left.depth - right.depth).map(({ value }) => value),
  )
}

export const causalHistory = (
  trace: ReadonlyArray<TraceEvent>,
  terminal: 'Success' | 'TypedFailure' | 'Trap',
  terminalIdentity?: string,
): ReadonlyArray<Termination.CausalFailure> => {
  const failures = trace.flatMap((event, index) =>
    event._tag === 'EffectFailure' && event.phase === 'Produced'
      ? [
          Object.freeze({
            event,
            index,
            key: `${event.function.module}\u0000${event.function.name}\u0000${event.tag}\u0000${event.span.start}\u0000${event.span.end}`,
          }),
        ]
      : [],
  )
  const distinct = failures.filter(
    (failure, index) => failures.findIndex((candidate) => candidate.key === failure.key) === index,
  )
  return Object.freeze(
    distinct.map(({ event, index }, ordinal) => {
      const identity =
        event.identity ?? (ordinal === distinct.length - 1 ? terminalIdentity : undefined)
      return Object.freeze({
        tag: event.tag,
        ...(identity === undefined ? {} : { identity }),
        provenance: event.span,
        logicalPath: logicalPathAt(trace, index),
        recovered: terminal !== 'TypedFailure' || ordinal !== distinct.length - 1,
      })
    }),
  )
}

export const longestCausalPath = (
  history: ReadonlyArray<Termination.CausalFailure>,
  fallback: ReadonlyArray<Termination.LogicalFrame>,
): ReadonlyArray<Termination.LogicalFrame> =>
  history.reduce(
    (selected, candidate) =>
      candidate.logicalPath.length > selected.length ? candidate.logicalPath : selected,
    fallback,
  )
