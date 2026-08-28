import type { LocalState, MachineRequest, Step } from './BootstrapMachine.js'
import type { BlockedReason, TraceEvent } from './BootstrapTrace.js'
import type { EffectOutcomeValue, EffectValue, Value } from './BootstrapValue.js'
import { repackFailurePayload } from './BootstrapValue.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as Mir from './Mir.js'
import type * as Termination from './Termination.js'
import * as Type from './Type.js'

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
  readonly runner: DeclarationFacts.CanonicalId
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

export const functionFor = (
  program: Mir.Module,
  id: DeclarationFacts.CanonicalId,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): Mir.MirFunction | undefined =>
  program.functions.find((fn) => Mir.matchesInstance(fn, id, typeArguments))

interface ExecutionState {
  nextFrame: number
  readonly cells: Map<string, LocalState>
}

export interface OperationContext {
  readonly program: Mir.Module
  readonly fn: Mir.MirFunction
  readonly activation: { readonly frame: number; readonly depth: number }
  readonly frame: number
  readonly state: ExecutionState
  readonly trace: Array<TraceEvent>
  readonly read: (local: Mir.LocalId) => LocalState
  readonly write: (local: Mir.LocalId, state: LocalState) => void
  readonly callEffectRunner: (
    target: Mir.MirFunction,
    arguments_: ReadonlyArray<Value>,
    operation: Extract<
      Mir.Operation,
      { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
    >,
  ) => Generator<MachineRequest, Step, Step>
  readonly callFunction: (
    target: Mir.MirFunction,
    arguments_: ReadonlyArray<Value>,
    span: Mir.Provenance['span'],
  ) => Generator<MachineRequest, Step, Step>
  readonly executeOperations: (
    operations: ReadonlyArray<Mir.Operation>,
  ) => Generator<MachineRequest, Step | undefined, Step>
}

const cellKey = (frame: number, cell: number): string => `${frame}:${cell}`

const blockedStep = (reason: BlockedReason): Step =>
  Object.freeze({ _tag: 'Blocked', reason: Object.freeze(reason) })

type EffectOperation = Extract<
  Mir.Operation,
  {
    readonly _tag:
      | 'MakeEffect'
      | 'PackEffectComposite'
      | 'PackEffectOutcome'
      | 'PackEffectFailureUnion'
      | 'UnpackEffectSuccess'
      | 'PropagateEffectFailure'
      | 'RunEffect'
      | 'RunEffectComposite'
      | 'RunEffectValue'
      | 'RunStaticEffect'
      | 'ReifyEffect'
  }
>

/** Executes Effect construction, execution, propagation, and reification. */
export function* execute(
  context: OperationContext,
  operation: EffectOperation,
): Generator<MachineRequest, Step | undefined, Step> {
  const {
    program,
    fn,
    activation,
    frame,
    state,
    trace,
    read,
    write,
    callEffectRunner,
    callFunction,
    executeOperations,
  } = context
  switch (operation._tag) {
    case 'MakeEffect': {
      write(operation.destination, {
        value: makeValue(Object.freeze({ frame, cells: state.cells, read, cellKey }), operation),
        fromCall: false,
      })
      break
    }
    case 'PackEffectComposite': {
      const effect = read(operation.source).value
      if (effect._tag !== 'EffectValue')
        throw new RangeError('MIR attempted to pack a non-Effect alternative')
      write(operation.destination, {
        value: Object.freeze({
          _tag: 'EffectCompositeValue',
          alternative: operation.alternative,
          effect,
        }),
        fromCall: false,
      })
      break
    }
    case 'PackEffectOutcome': {
      const payload = read(operation.source).value
      write(operation.destination, {
        value: Object.freeze({
          _tag: 'EffectOutcomeValue',
          type: operation.type.type,
          tag: operation.tag,
          payload,
        }),
        fromCall: false,
      })
      trace.push(
        Object.freeze({
          _tag: operation.tag === 0 ? 'EffectSuccess' : 'EffectFailure',
          phase: 'Produced',
          frame,
          depth: activation.depth,
          function: fn.id,
          tag: operation.tag,
          ...(operation.tag === 0
            ? {}
            : {
                identity: failureIdentity(operation.type.type, operation.tag),
              }),
          span: operation.provenance.span,
        }),
      )
      break
    }
    case 'PackEffectFailureUnion': {
      const source = read(operation.source).value
      if (source._tag !== 'UnionValue')
        throw new RangeError('MIR attempted to fail with a non-union value')
      const mapping = operation.mappings.find((candidate) =>
        Type.equals(
          Type.failureCarrierMember(operation.sourceType.type, candidate.source, 'ZeroBased') ??
            Type.unit,
          source.member,
        ),
      )
      if (mapping === undefined)
        throw new RangeError('MIR failure union has no canonical E-channel mapping')
      write(operation.destination, {
        value: Object.freeze({
          _tag: 'EffectOutcomeValue',
          type: operation.type.type,
          tag: mapping.target,
          payload: repackFailurePayload(
            source.payload,
            operation.sourceType.type,
            mapping.source,
            operation.type.type,
            mapping.target,
          ),
        }),
        fromCall: false,
      })
      trace.push(
        Object.freeze({
          _tag: 'EffectFailure',
          phase: 'Produced',
          frame,
          depth: activation.depth,
          function: fn.id,
          tag: mapping.target,
          identity: failureIdentity(operation.type.type, mapping.target),
          span: operation.provenance.span,
        }),
      )
      break
    }
    case 'UnpackEffectSuccess': {
      const outcome = read(operation.source)
      if (outcome.value._tag !== 'EffectOutcomeValue' || outcome.value.tag !== 0) {
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: 'attempted to unpack a failed effect outcome as success',
          span: operation.provenance.span,
        })
      }
      write(operation.destination, { value: outcome.value.payload, fromCall: true })
      break
    }
    case 'PropagateEffectFailure': {
      const source = read(operation.source).value
      let sourceTag: number
      if (source._tag === 'UnionValue') {
        if (operation.sourceType._tag === 'Union') {
          sourceTag = operation.sourceType.type.members.findIndex((member) =>
            Type.equals(member, source.member),
          )
        } else {
          sourceTag = -1
        }
      } else if (source._tag === 'AggregateValue' && operation.sourceType._tag === 'Nominal') {
        sourceTag = 0
      } else {
        sourceTag = -1
      }
      let payload: Value | undefined
      if (source._tag === 'UnionValue') {
        payload = source.payload
      } else if (source._tag === 'AggregateValue') {
        payload = source
      } else {
        payload = undefined
      }
      const mapping = operation.tagMappings.find((candidate) => candidate.source === sourceTag)
      if (payload === undefined || mapping === undefined)
        throw new RangeError('MIR propagated failure has no canonical tag mapping')
      const released = yield* executeOperations(operation.releases ?? [])
      if (released !== undefined) return released
      return Object.freeze({
        _tag: 'Value',
        value: Object.freeze({
          _tag: 'EffectOutcomeValue',
          type: operation.propagationType.type,
          tag: mapping.target,
          payload: repackFailurePayload(
            payload,
            Mir.semanticType(operation.sourceType),
            mapping.source,
            operation.propagationType.type,
            mapping.target,
          ),
        }),
      })
    }
    case 'RunEffect': {
      const target = functionFor(program, operation.target, operation.typeArguments)
      if (target === undefined)
        return blockedStep({
          _tag: 'MissingFunction',
          target: operation.target,
          span: operation.provenance.span,
        })
      trace.push(
        Object.freeze({
          _tag: 'Call',
          frame: activation.frame,
          depth: activation.depth,
          caller: fn.id,
          target: operation.target,
          callerInstance: fn.instance,
          targetInstance: target.instance,
          span: operation.provenance.span,
        }),
      )
      const arguments_ = operation.arguments.map((argument) => read(argument))
      arguments_.forEach((argument, ordinal) => {
        trace.push(
          Object.freeze({
            _tag: 'Binding',
            frame: state.nextFrame,
            depth: activation.depth + 1,
            target: operation.target,
            targetInstance: target.instance,
            callSpan: operation.provenance.span,
            argumentOrdinal: ordinal,
            parameterOrdinal: ordinal,
            value: argument.value,
            fromCall: argument.fromCall,
            span: operation.provenance.span,
          }),
        )
      })
      const result = yield* callEffectRunner(
        target,
        arguments_.map((argument) => argument.value),
        operation,
      )
      if (result._tag === 'Blocked') return result
      if (result._tag === 'Transfer') return result
      if (result.value._tag !== 'EffectOutcomeValue')
        throw new RangeError('MIR propagated effect returned a non-outcome value')
      const effectOutcome = result.value
      write(operation.outcome, { value: effectOutcome, fromCall: true })
      if (effectOutcome.tag === 0) {
        write(operation.destination, { value: effectOutcome.payload, fromCall: true })
        break
      }
      const mapping = operation.tagMappings.find(
        (candidate) => candidate.source === effectOutcome.tag,
      )
      if (mapping === undefined)
        throw new RangeError('MIR propagated effect has no canonical failure-tag mapping')
      const ended = yield* executeOperations(operation.failureLoanEnds ?? [])
      if (ended !== undefined) return ended
      const released = yield* executeOperations(operation.releases ?? [])
      if (released !== undefined) return released
      const propagated: EffectOutcomeValue = Object.freeze({
        _tag: 'EffectOutcomeValue',
        type: operation.propagationType.type,
        tag: mapping.target,
        payload: repackFailurePayload(
          effectOutcome.payload,
          operation.outcomeType.type,
          effectOutcome.tag,
          operation.propagationType.type,
          mapping.target,
        ),
      })
      trace.push(
        Object.freeze({
          _tag: 'EffectFailure',
          phase: 'Propagated',
          frame,
          depth: activation.depth,
          function: fn.id,
          tag: mapping.target,
          span: operation.provenance.span,
        }),
        Object.freeze({
          _tag: 'Return',
          frame: activation.frame,
          depth: activation.depth,
          function: fn.id,
          instance: fn.instance,
          value: propagated,
          span: operation.provenance.span,
        }),
      )
      return Object.freeze({ _tag: 'Value', value: propagated })
    }
    case 'RunEffectComposite':
    case 'RunEffectValue':
    case 'RunStaticEffect': {
      const execution = prepareExecution(operation, read)
      const target = functionFor(program, execution.runner, execution.runnerTypeArguments)
      if (target === undefined)
        return blockedStep({
          _tag: 'MissingFunction',
          target: execution.runner,
          span: operation.provenance.span,
        })
      trace.push(
        Object.freeze({
          _tag: 'Call',
          frame: activation.frame,
          depth: activation.depth,
          caller: fn.id,
          target: execution.runner,
          callerInstance: fn.instance,
          targetInstance: target.instance,
          span: operation.provenance.span,
        }),
      )
      const result =
        operation._tag === 'RunEffectValue'
          ? yield* callEffectRunner(target, execution.arguments, operation)
          : yield* callFunction(target, execution.arguments, operation.provenance.span)
      if (result._tag === 'Blocked') return result
      if (result._tag === 'Transfer') return result
      if (result.value._tag !== 'EffectOutcomeValue')
        throw new RangeError('MIR Effect runner returned a non-outcome value')
      const effectOutcome = normalizeOutcome(operation, result.value, execution)
      write(operation.outcome, { value: effectOutcome, fromCall: true })
      if (effectOutcome.tag === 0) {
        write(operation.destination, { value: effectOutcome.payload, fromCall: true })
        break
      }
      if (operation.propagationType === undefined) {
        const ended = yield* executeOperations(operation.failureLoanEnds ?? [])
        if (ended !== undefined) return ended
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: 'unhandled Effect failure escaped an infallible context',
          span: operation.provenance.span,
        })
      }
      const mapping = operation.tagMappings.find(
        (candidate) => candidate.source === effectOutcome.tag,
      )
      if (mapping === undefined)
        throw new RangeError('MIR Effect runner has no failure-tag mapping')
      const ended = yield* executeOperations(operation.failureLoanEnds ?? [])
      if (ended !== undefined) return ended
      const released = yield* executeOperations(operation.releases ?? [])
      if (released !== undefined) return released
      const propagated: EffectOutcomeValue = Object.freeze({
        _tag: 'EffectOutcomeValue',
        type: operation.propagationType.type,
        tag: mapping.target,
        payload: repackFailurePayload(
          effectOutcome.payload,
          operation.outcomeType.type,
          effectOutcome.tag,
          operation.propagationType.type,
          mapping.target,
        ),
      })
      return Object.freeze({ _tag: 'Value', value: propagated })
    }
    case 'ReifyEffect': {
      const effect = read(operation.effect).value
      if (effect._tag !== 'EffectValue')
        throw new RangeError('MIR attempted to reify a non-Effect value')
      const target = functionFor(program, operation.runner, operation.runnerTypeArguments)
      if (target === undefined)
        return blockedStep({
          _tag: 'MissingFunction',
          target: operation.runner,
          span: operation.provenance.span,
        })
      trace.push(
        Object.freeze({
          _tag: 'Call',
          frame: activation.frame,
          depth: activation.depth,
          caller: fn.id,
          target: operation.runner,
          callerInstance: fn.instance,
          targetInstance: target.instance,
          span: operation.provenance.span,
        }),
      )
      const result = yield* callEffectRunner(
        target,
        Object.freeze([
          ...effect.captures,
          ...operation.arguments.map((argument) => read(argument).value),
        ]),
        operation,
      )
      if (result._tag === 'Blocked') return result
      if (result._tag === 'Transfer') return result
      if (result.value._tag !== 'EffectOutcomeValue')
        throw new RangeError('MIR Effect result runner returned a non-outcome value')
      const outcome = result.value
      write(operation.outcome, { value: outcome, fromCall: true })
      write(operation.destination, {
        value: Object.freeze({
          _tag: 'IntegerValue',
          type: 'i32',
          value: outcome.tag === 0 ? 1n : 0n,
        }),
        fromCall: true,
      })
      if (outcome.tag === 0) {
        write(operation.successValue, { value: outcome.payload, fromCall: true })
        break
      }
      const failure = Type.failureCarrierMember(operation.outcomeType.type, outcome.tag, 'OneBased')
      if (failure === undefined)
        throw new RangeError('MIR Effect result has an invalid failure tag')
      const failureValue: Value = Type.isUnion(operation.failureValueType)
        ? Object.freeze({
            _tag: 'UnionValue',
            type: operation.failureValueType,
            member: failure,
            payload: outcome.payload,
          })
        : outcome.payload
      write(operation.failureValue, { value: failureValue, fromCall: true })
      break
    }
  }
}
