import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Instances from './Instances.js'
import type * as Mir from './Mir.js'
import type * as SourceSpan from './SourceSpan.js'

/**
 * The closed bootstrap interpreter, executing the lowered MIR program from the entry instance
 * discovery resolved. It is the semantics oracle for the coming native differential checks and
 * the second consumer keeping MIR's meaning in MIR. A severable leaf: nothing in the pipeline
 * depends on it.
 */

/** The exact value produced by the closed bootstrap interpreter. */
export interface I32Value {
  readonly _tag: 'I32Value'
  readonly value: number
}

/** Entered the resolved entry instance. */
export interface EntryTraceEvent {
  readonly _tag: 'Entry'
  readonly function: DeclarationIndex.CanonicalId
  readonly span: SourceSpan.SourceSpan
}

/** Executed one call operation after its argument locals were computed. */
export interface CallTraceEvent {
  readonly _tag: 'Call'
  readonly caller: DeclarationIndex.CanonicalId
  readonly target: DeclarationIndex.CanonicalId
  readonly span: SourceSpan.SourceSpan
}

/** Bound one computed argument value to its positional parameter local. */
export interface BindingTraceEvent {
  readonly _tag: 'Binding'
  readonly target: DeclarationIndex.CanonicalId
  readonly callSpan: SourceSpan.SourceSpan
  readonly argumentOrdinal: number
  readonly parameterOrdinal: number
  readonly value: I32Value
  readonly fromCall: boolean
  readonly span: SourceSpan.SourceSpan
}

/** Returned one evaluated value from a lowered function. */
export interface ReturnTraceEvent {
  readonly _tag: 'Return'
  readonly function: DeclarationIndex.CanonicalId
  readonly value: I32Value
  readonly span: SourceSpan.SourceSpan
}

/** One deterministic event emitted while replaying lowered operations. */
export type TraceEvent = EntryTraceEvent | CallTraceEvent | BindingTraceEvent | ReturnTraceEvent

/** Every expected reason the closed bootstrap interpreter can stop. */
export type BlockedReason =
  | {
      readonly _tag: 'UnavailableEntry'
      readonly reason: 'MissingEntry' | 'AmbiguousEntry' | 'ParameterizedEntry' | 'UntypedEntry'
    }
  | {
      readonly _tag: 'Trap'
      readonly function: DeclarationIndex.CanonicalId
      readonly reason: string
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'MissingFunction'
      readonly target: DeclarationIndex.CanonicalId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'RecursiveCycle'
      readonly cycle: ReadonlyArray<DeclarationIndex.CanonicalId>
      readonly closingCallSpan: SourceSpan.SourceSpan
    }

/** A completed exact bootstrap result. */
export interface Completed {
  readonly _tag: 'Completed'
  readonly entry: DeclarationIndex.CanonicalId
  readonly result: I32Value
  readonly trace: ReadonlyArray<TraceEvent>
}

/** A normal, inspectable reason bootstrap evaluation could not complete. */
export interface Blocked {
  readonly _tag: 'Blocked'
  readonly entry: DeclarationIndex.CanonicalId | undefined
  readonly reason: BlockedReason
  readonly trace: ReadonlyArray<TraceEvent>
}

/** The closed outcome of executing one lowered program. */
export type Outcome = Completed | Blocked

type Step =
  | { readonly _tag: 'Value'; readonly value: I32Value }
  | { readonly _tag: 'Blocked'; readonly reason: BlockedReason }

const value = (input: number): I32Value => Object.freeze({ _tag: 'I32Value', value: input })

const blockedStep = (reason: BlockedReason): Step =>
  Object.freeze({ _tag: 'Blocked', reason: Object.freeze(reason) })

const sameId = (left: DeclarationIndex.CanonicalId, right: DeclarationIndex.CanonicalId): boolean =>
  left.module === right.module && left.name === right.name

const functionFor = (
  program: Mir.Module,
  id: DeclarationIndex.CanonicalId,
): Mir.MirFunction | undefined => program.functions.find((fn) => sameId(fn.id, id))

interface LocalState {
  readonly value: I32Value
  readonly fromCall: boolean
}

function executeFunction(
  program: Mir.Module,
  fn: Mir.MirFunction,
  argumentValues: ReadonlyArray<I32Value>,
  active: ReadonlyArray<DeclarationIndex.CanonicalId>,
  trace: Array<TraceEvent>,
): Step {
  const locals = new Map<number, LocalState>()
  argumentValues.forEach((argument, ordinal) => {
    locals.set(ordinal, { value: argument, fromCall: false })
  })

  const read = (local: Mir.LocalId): LocalState =>
    locals.get(local.ordinal) ?? { value: value(0), fromCall: false }

  let blockOrdinal = 0
  for (;;) {
    const block = fn.blocks.at(blockOrdinal)
    if (block === undefined) {
      return blockedStep({
        _tag: 'Trap',
        function: fn.id,
        reason: `missing block bb${blockOrdinal}`,
        span: fn.blocks.at(0)?.terminator.provenance.span ?? argumentSpanFallback(fn),
      })
    }

    for (const operation of block.operations) {
      switch (operation._tag) {
        case 'Literal':
          locals.set(operation.destination.ordinal, {
            value: value(operation.value),
            fromCall: false,
          })
          break
        case 'Move':
          locals.set(operation.destination.ordinal, read(operation.source))
          break
        case 'Drop':
          break
        case 'Call': {
          const target = functionFor(program, operation.target)
          if (target === undefined) {
            return blockedStep({
              _tag: 'MissingFunction',
              target: operation.target,
              span: operation.provenance.span,
            })
          }
          trace.push(
            Object.freeze({
              _tag: 'Call',
              caller: fn.id,
              target: operation.target,
              span: operation.provenance.span,
            }),
          )
          const cycleStart = active.findIndex((candidate) => sameId(candidate, operation.target))
          if (cycleStart >= 0) {
            return blockedStep({
              _tag: 'RecursiveCycle',
              cycle: Object.freeze([...active.slice(cycleStart), operation.target]),
              closingCallSpan: operation.provenance.span,
            })
          }
          const argumentStates = operation.arguments.map((argument) => read(argument))
          argumentStates.forEach((state, ordinal) => {
            trace.push(
              Object.freeze({
                _tag: 'Binding',
                target: operation.target,
                callSpan: operation.provenance.span,
                argumentOrdinal: ordinal,
                parameterOrdinal: ordinal,
                value: state.value,
                fromCall: state.fromCall,
                span: operation.provenance.span,
              }),
            )
          })
          const result = executeFunction(
            program,
            target,
            argumentStates.map((state) => state.value),
            Object.freeze([...active, operation.target]),
            trace,
          )
          if (result._tag === 'Blocked') return result
          locals.set(operation.destination.ordinal, { value: result.value, fromCall: true })
          break
        }
      }
    }

    const terminator = block.terminator
    switch (terminator._tag) {
      case 'Return': {
        const result = read(terminator.value)
        trace.push(
          Object.freeze({
            _tag: 'Return',
            function: fn.id,
            value: result.value,
            span: terminator.provenance.span,
          }),
        )
        return Object.freeze({ _tag: 'Value', value: result.value })
      }
      case 'Jump':
        blockOrdinal = terminator.target.ordinal
        break
      case 'Branch':
        blockOrdinal =
          read(terminator.condition).value.value !== 0
            ? terminator.taken.ordinal
            : terminator.otherwise.ordinal
        break
      case 'Trap':
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: terminator.reason,
          span: terminator.provenance.span,
        })
    }
  }
}

const argumentSpanFallback = (fn: Mir.MirFunction): SourceSpan.SourceSpan => {
  const span = fn.blocks.at(0)?.terminator.provenance.span
  if (span === undefined) {
    throw new RangeError(`Lowered function ${fn.id.name} has no blocks`)
  }
  return span
}

/** Executes the lowered program from the discovered entry, replaying MIR operations as a trace. */
export const evaluate = (discovery: Instances.Discovery, program: Mir.Module): Outcome => {
  if (discovery.entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'Blocked',
      entry: undefined,
      reason: Object.freeze({ _tag: 'UnavailableEntry', reason: discovery.entry.reason }),
      trace: Object.freeze([]),
    })
  }

  const entry = discovery.entry.key.declaration
  const fn = functionFor(program, entry)
  if (fn === undefined) {
    return Object.freeze({
      _tag: 'Blocked',
      entry,
      reason: Object.freeze({
        _tag: 'MissingFunction',
        target: entry,
        span: program.functions.at(0)?.blocks.at(0)?.terminator.provenance.span ?? raiseNoSpan(),
      }),
      trace: Object.freeze([]),
    })
  }

  const trace: Array<TraceEvent> = []
  trace.push(
    Object.freeze({
      _tag: 'Entry',
      function: entry,
      span: argumentSpanFallback(fn),
    }),
  )
  const result = executeFunction(program, fn, [], Object.freeze([entry]), trace)
  return result._tag === 'Blocked'
    ? Object.freeze({
        _tag: 'Blocked',
        entry,
        reason: result.reason,
        trace: Object.freeze([...trace]),
      })
    : Object.freeze({
        _tag: 'Completed',
        entry,
        result: result.value,
        trace: Object.freeze([...trace]),
      })
}

const raiseNoSpan = (): never => {
  throw new RangeError('Lowered program has no functions to attach a span to')
}
