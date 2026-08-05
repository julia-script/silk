import * as SemanticAnalysis from './SemanticAnalysis.js'
import type * as SourceSpan from './SourceSpan.js'

/** The exact value produced by the closed bootstrap evaluator. */
export interface I32Value {
  readonly _tag: 'I32Value'
  readonly value: number
}

/** Entered the selected bootstrap entry point. */
export interface EntryTraceEvent {
  readonly _tag: 'Entry'
  readonly declaration: SemanticAnalysis.DeclarationFact
  readonly span: SourceSpan.SourceSpan
}

/** Reached one checked caller-to-target relationship. */
export interface CallTraceEvent {
  readonly _tag: 'Call'
  readonly caller: SemanticAnalysis.DeclarationFact
  readonly target: SemanticAnalysis.DeclarationFact
  readonly call: Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }>
  readonly span: SourceSpan.SourceSpan
}

/** Bound one evaluated argument to its checked positional parameter. */
export interface BindingTraceEvent {
  readonly _tag: 'Binding'
  readonly target: SemanticAnalysis.DeclarationFact
  readonly argument: SemanticAnalysis.ArgumentFact
  readonly parameter: SemanticAnalysis.ParameterFact
  readonly value: I32Value
  readonly span: SourceSpan.SourceSpan
}

/** Read one resolved parameter from the current immutable call frame. */
export interface ParameterReadTraceEvent {
  readonly _tag: 'ParameterRead'
  readonly declaration: SemanticAnalysis.DeclarationFact
  readonly reference: SemanticAnalysis.IdentifierExpressionFact
  readonly parameter: SemanticAnalysis.ParameterFact
  readonly value: I32Value
  readonly span: SourceSpan.SourceSpan
}

/** Returned one evaluated value from a reachable function. */
export interface ReturnTraceEvent {
  readonly _tag: 'Return'
  readonly declaration: SemanticAnalysis.DeclarationFact
  readonly expression: SemanticAnalysis.ReturnedExpressionFact
  readonly value: I32Value
  readonly span: SourceSpan.SourceSpan
}

/** One deterministic event emitted by bootstrap evaluation. */
export type TraceEvent =
  | EntryTraceEvent
  | CallTraceEvent
  | BindingTraceEvent
  | ParameterReadTraceEvent
  | ReturnTraceEvent

/** Every expected reason the closed bootstrap evaluator can stop. */
export type BlockedReason =
  | {
      readonly _tag: 'MissingEntry'
      readonly lookup: Extract<SemanticAnalysis.DeclarationLookup, { readonly _tag: 'Missing' }>
    }
  | {
      readonly _tag: 'AmbiguousEntry'
      readonly lookup: Extract<SemanticAnalysis.DeclarationLookup, { readonly _tag: 'Ambiguous' }>
    }
  | {
      readonly _tag: 'ParameterizedEntry'
      readonly declaration: SemanticAnalysis.DeclarationFact
      readonly actualCount: number
    }
  | {
      readonly _tag: 'UnavailableEntryType'
      readonly declaration: SemanticAnalysis.DeclarationFact
      readonly returnType: SemanticAnalysis.ReturnTypeFact
    }
  | {
      readonly _tag: 'UnavailableInteger'
      readonly declaration: SemanticAnalysis.DeclarationFact
      readonly integer: SemanticAnalysis.IntegerExpressionFact
    }
  | {
      readonly _tag: 'MissingParameterReference'
      readonly declaration: SemanticAnalysis.DeclarationFact
      readonly reference: Extract<
        SemanticAnalysis.ParameterReferenceFact,
        { readonly _tag: 'Missing' }
      >
    }
  | {
      readonly _tag: 'AmbiguousParameterReference'
      readonly declaration: SemanticAnalysis.DeclarationFact
      readonly reference: Extract<
        SemanticAnalysis.ParameterReferenceFact,
        { readonly _tag: 'Ambiguous' }
      >
    }
  | {
      readonly _tag: 'UnavailableParameterReference'
      readonly declaration: SemanticAnalysis.DeclarationFact
      readonly reference: Extract<
        SemanticAnalysis.ParameterReferenceFact,
        { readonly _tag: 'Unavailable' }
      >
    }
  | {
      readonly _tag: 'UnboundParameter'
      readonly declaration: SemanticAnalysis.DeclarationFact
      readonly parameter: SemanticAnalysis.ParameterFact
      readonly reference: SemanticAnalysis.IdentifierExpressionFact
    }
  | {
      readonly _tag: 'MissingCallTarget'
      readonly caller: SemanticAnalysis.DeclarationFact
      readonly call: Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }>
    }
  | {
      readonly _tag: 'AmbiguousCallTarget'
      readonly caller: SemanticAnalysis.DeclarationFact
      readonly call: Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }>
    }
  | {
      readonly _tag: 'UnavailableCallTarget'
      readonly caller: SemanticAnalysis.DeclarationFact
      readonly call: Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }>
    }
  | {
      readonly _tag: 'ArityMismatch'
      readonly caller: SemanticAnalysis.DeclarationFact
      readonly target: SemanticAnalysis.DeclarationFact
      readonly call: Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }>
      readonly expectedCount: number
      readonly actualCount: number
    }
  | {
      readonly _tag: 'UnavailableCallContract'
      readonly caller: SemanticAnalysis.DeclarationFact
      readonly target: SemanticAnalysis.DeclarationFact | undefined
      readonly call: Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }>
      readonly reason: SemanticAnalysis.UnavailableCallContractReason
    }
  | {
      readonly _tag: 'UnavailableFunction'
      readonly declaration: SemanticAnalysis.DeclarationFact
    }
  | {
      readonly _tag: 'RecursiveCycle'
      readonly cycle: ReadonlyArray<SemanticAnalysis.DeclarationFact>
      readonly closingCall: Extract<
        SemanticAnalysis.ReturnedExpressionFact,
        { readonly _tag: 'Call' }
      >
      readonly closingCallSpan: SourceSpan.SourceSpan
    }

/** A completed exact bootstrap result. */
export interface Completed {
  readonly _tag: 'Completed'
  readonly entry: SemanticAnalysis.DeclarationFact
  readonly result: I32Value
  readonly trace: ReadonlyArray<TraceEvent>
}

/** A normal, inspectable reason bootstrap evaluation could not complete. */
export interface Blocked {
  readonly _tag: 'Blocked'
  readonly entry: SemanticAnalysis.DeclarationFact | undefined
  readonly reason: BlockedReason
  readonly trace: ReadonlyArray<TraceEvent>
}

/** The closed outcome of evaluating one analyzed bootstrap source snapshot. */
export type Outcome = Completed | Blocked

interface Binding {
  readonly parameter: SemanticAnalysis.ParameterFact
  readonly value: I32Value
}

type Frame = ReadonlyArray<Binding>

type Step =
  | { readonly _tag: 'Value'; readonly value: I32Value }
  | { readonly _tag: 'Blocked'; readonly reason: BlockedReason }

const value = (input: number): I32Value => Object.freeze({ _tag: 'I32Value', value: input })

const blockedStep = (reason: BlockedReason): Step =>
  Object.freeze({ _tag: 'Blocked', reason: Object.freeze(reason) })

const sameDeclaration = (
  left: SemanticAnalysis.DeclarationFact,
  right: SemanticAnalysis.DeclarationFact,
): boolean => left.id.sourceId === right.id.sourceId && left.id.ordinal === right.id.ordinal

const sameParameter = (
  left: SemanticAnalysis.ParameterFact,
  right: SemanticAnalysis.ParameterFact,
): boolean =>
  sameDeclarationId(left.id.function, right.id.function) && left.id.ordinal === right.id.ordinal

const sameDeclarationId = (
  left: SemanticAnalysis.DeclarationId,
  right: SemanticAnalysis.DeclarationId,
): boolean => left.sourceId === right.sourceId && left.ordinal === right.ordinal

const append = (trace: Array<TraceEvent>, event: TraceEvent): void => {
  trace.push(Object.freeze(event))
}

const functionFor = (
  analysis: SemanticAnalysis.Result,
  declaration: SemanticAnalysis.DeclarationFact,
): SemanticAnalysis.FunctionFact | undefined =>
  analysis.functions.find((fact) => sameDeclaration(fact.declaration, declaration))

const readParameter = (
  declaration: SemanticAnalysis.DeclarationFact,
  expression: SemanticAnalysis.IdentifierExpressionFact,
  frame: Frame,
  trace: Array<TraceEvent>,
): Step => {
  const reference = expression.reference
  if (reference._tag === 'Missing') {
    return blockedStep({ _tag: 'MissingParameterReference', declaration, reference })
  }
  if (reference._tag === 'Ambiguous') {
    return blockedStep({ _tag: 'AmbiguousParameterReference', declaration, reference })
  }
  if (reference._tag === 'Unavailable') {
    return blockedStep({ _tag: 'UnavailableParameterReference', declaration, reference })
  }

  const binding = frame.find((entry) => sameParameter(entry.parameter, reference.parameter))
  if (binding === undefined) {
    return blockedStep({
      _tag: 'UnboundParameter',
      declaration,
      parameter: reference.parameter,
      reference: expression,
    })
  }

  append(trace, {
    _tag: 'ParameterRead',
    declaration,
    reference: expression,
    parameter: reference.parameter,
    value: binding.value,
    span: expression.syntax.span,
  })
  return Object.freeze({ _tag: 'Value', value: binding.value })
}

const evaluateArgument = (
  declaration: SemanticAnalysis.DeclarationFact,
  target: SemanticAnalysis.DeclarationFact,
  call: Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }>,
  argument: SemanticAnalysis.ArgumentFact,
  frame: Frame,
  trace: Array<TraceEvent>,
): Step => {
  const expression = argument.expression
  if (expression._tag === 'Identifier') return readParameter(declaration, expression, frame, trace)
  if (expression._tag === 'UnavailableNestedCall') {
    return blockedStep({
      _tag: 'UnavailableCallContract',
      caller: declaration,
      target,
      call,
      reason: Object.freeze({ _tag: 'UnavailableNestedArgument', argument }),
    })
  }
  if (expression.integer._tag !== 'Available') {
    return blockedStep({
      _tag: 'UnavailableInteger',
      declaration,
      integer: expression.integer,
    })
  }
  return Object.freeze({ _tag: 'Value', value: value(expression.integer.value) })
}

const evaluateFunction = (
  analysis: SemanticAnalysis.Result,
  fact: SemanticAnalysis.FunctionFact,
  frame: Frame,
  active: ReadonlyArray<SemanticAnalysis.DeclarationFact>,
  trace: Array<TraceEvent>,
): Step => {
  const declaration = fact.declaration
  const expression = fact.returnedExpression
  let evaluated: Step

  if (expression._tag === 'Integer') {
    evaluated =
      expression.integer._tag === 'Available'
        ? Object.freeze({ _tag: 'Value', value: value(expression.integer.value) })
        : blockedStep({ _tag: 'UnavailableInteger', declaration, integer: expression.integer })
  } else if (expression._tag === 'Identifier') {
    evaluated = readParameter(declaration, expression, frame, trace)
  } else {
    const call = expression
    const reference = call.reference
    if (reference._tag === 'Missing') {
      return blockedStep({ _tag: 'MissingCallTarget', caller: declaration, call })
    }
    if (reference._tag === 'Ambiguous') {
      return blockedStep({ _tag: 'AmbiguousCallTarget', caller: declaration, call })
    }
    if (reference._tag === 'Unavailable') {
      return blockedStep({ _tag: 'UnavailableCallTarget', caller: declaration, call })
    }

    const target = reference.declaration
    if (call.contract._tag === 'ArityMismatch') {
      return blockedStep({
        _tag: 'ArityMismatch',
        caller: declaration,
        target,
        call,
        expectedCount: call.contract.expectedCount,
        actualCount: call.contract.actualCount,
      })
    }
    if (call.contract._tag === 'Unavailable') {
      return blockedStep({
        _tag: 'UnavailableCallContract',
        caller: declaration,
        target,
        call,
        reason: call.contract.reason,
      })
    }

    append(trace, {
      _tag: 'Call',
      caller: declaration,
      target,
      call,
      span: call.syntax.span,
    })

    const bindings: Array<Binding> = []
    for (const argument of call.arguments) {
      const mapping = call.mappings.find(
        (candidate) => candidate.argument.id.ordinal === argument.id.ordinal,
      )
      if (mapping === undefined) {
        return blockedStep({
          _tag: 'UnavailableCallContract',
          caller: declaration,
          target,
          call,
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call.syntax }),
        })
      }
      const argumentResult = evaluateArgument(declaration, target, call, argument, frame, trace)
      if (argumentResult._tag === 'Blocked') return argumentResult
      const binding = Object.freeze({ parameter: mapping.parameter, value: argumentResult.value })
      bindings.push(binding)
      append(trace, {
        _tag: 'Binding',
        target,
        argument,
        parameter: mapping.parameter,
        value: argumentResult.value,
        span: argument.syntax.span,
      })
    }

    const cycleStart = active.findIndex((candidate) => sameDeclaration(candidate, target))
    if (cycleStart >= 0) {
      return blockedStep({
        _tag: 'RecursiveCycle',
        cycle: Object.freeze([...active.slice(cycleStart), target]),
        closingCall: call,
        closingCallSpan: call.syntax.span,
      })
    }

    const targetFact = functionFor(analysis, target)
    if (targetFact === undefined) {
      return blockedStep({ _tag: 'UnavailableFunction', declaration: target })
    }
    evaluated = evaluateFunction(
      analysis,
      targetFact,
      Object.freeze(bindings),
      Object.freeze([...active, target]),
      trace,
    )
  }

  if (evaluated._tag === 'Blocked') return evaluated
  append(trace, {
    _tag: 'Return',
    declaration,
    expression,
    value: evaluated.value,
    span: expression.syntax.span,
  })
  return evaluated
}

const blockedOutcome = (
  entry: SemanticAnalysis.DeclarationFact | undefined,
  reason: BlockedReason,
  trace: ReadonlyArray<TraceEvent>,
): Blocked =>
  Object.freeze({
    _tag: 'Blocked',
    entry,
    reason: Object.freeze(reason),
    trace: Object.freeze([...trace]),
  })

/** Evaluates the reachable closed bootstrap slice from one analyzed `main`. */
export const evaluate = (analysis: SemanticAnalysis.Result): Outcome => {
  const lookup = SemanticAnalysis.declarationByName(analysis, 'main')
  if (lookup._tag === 'Missing') {
    return blockedOutcome(undefined, { _tag: 'MissingEntry', lookup }, [])
  }
  if (lookup._tag === 'Ambiguous') {
    return blockedOutcome(undefined, { _tag: 'AmbiguousEntry', lookup }, [])
  }

  const entry = lookup.declaration
  if (entry.parameterCount !== 0) {
    return blockedOutcome(
      entry,
      { _tag: 'ParameterizedEntry', declaration: entry, actualCount: entry.parameterCount },
      [],
    )
  }
  if (entry.returnType._tag !== 'Resolved' || entry.returnType.type !== 'I32') {
    return blockedOutcome(
      entry,
      { _tag: 'UnavailableEntryType', declaration: entry, returnType: entry.returnType },
      [],
    )
  }

  const fact = functionFor(analysis, entry)
  if (fact === undefined) {
    return blockedOutcome(entry, { _tag: 'UnavailableFunction', declaration: entry }, [])
  }

  const trace: Array<TraceEvent> = []
  append(trace, { _tag: 'Entry', declaration: entry, span: entry.syntax.span })
  const result = evaluateFunction(analysis, fact, Object.freeze([]), Object.freeze([entry]), trace)
  return result._tag === 'Blocked'
    ? blockedOutcome(entry, result.reason, trace)
    : Object.freeze({
        _tag: 'Completed',
        entry,
        result: result.value,
        trace: Object.freeze([...trace]),
      })
}
