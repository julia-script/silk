import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import type * as Mir from './Mir.js'
import type * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'

/**
 * Lowering: reachable instances become one MIR program module. Statement sequences linearize
 * into basic blocks in evaluation order; each `let` binding occupies one typed local; drops are
 * inserted exactly as the ownership plan's releases direct; unavailable bodies and ownership
 * violations lower to explicit generated traps. Provenance stays attached throughout.
 */

const i32: Mir.Type = Object.freeze({ _tag: 'I32' })

const local = (ordinal: number): Mir.LocalId => Object.freeze({ _tag: 'Local', ordinal })

interface LoweredExpression {
  readonly operations: ReadonlyArray<Mir.Operation>
  readonly result: Mir.LocalId
  readonly nextLocal: number
}

const lowerExpression = (
  expression: Hir.Expression,
  nextLocal: number,
  bindingLocals: ReadonlyMap<number, Mir.LocalId>,
): LoweredExpression | undefined => {
  switch (expression._tag) {
    case 'IntegerLiteral': {
      const destination = local(nextLocal)
      return {
        operations: [
          Object.freeze({
            _tag: 'Literal',
            destination,
            type: i32,
            value: expression.value,
            provenance: Object.freeze({ span: expression.span, generated: false }),
          }),
        ],
        result: destination,
        nextLocal: nextLocal + 1,
      }
    }
    case 'ParameterReference':
      return {
        operations: [],
        result: local(expression.parameter.ordinal),
        nextLocal,
      }
    case 'BindingReference': {
      const bound = bindingLocals.get(expression.binding.ordinal)
      if (bound === undefined) return undefined
      return { operations: [], result: bound, nextLocal }
    }
    case 'Move':
      return lowerExpression(expression.subject, nextLocal, bindingLocals)
    case 'Call': {
      const operations: Array<Mir.Operation> = []
      const argumentLocals: Array<Mir.LocalId> = []
      let cursor = nextLocal
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(argument, cursor, bindingLocals)
        if (lowered === undefined) return undefined
        operations.push(...lowered.operations)
        argumentLocals.push(lowered.result)
        cursor = lowered.nextLocal
      }
      const destination = local(cursor)
      operations.push(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          arguments: Object.freeze(argumentLocals),
          type: i32,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { operations, result: destination, nextLocal: cursor + 1 }
    }
    case 'Unavailable':
      return undefined
  }
}

interface LoweredBody {
  readonly operations: ReadonlyArray<Mir.Operation>
  readonly result: Mir.LocalId
  readonly resultSpan: SourceSpan.SourceSpan
  readonly localCount: number
  readonly bindingLocals: ReadonlyMap<number, Mir.LocalId>
}

const lowerBody = (fn: Hir.HirFunction): LoweredBody | undefined => {
  const operations: Array<Mir.Operation> = []
  const bindingLocals = new Map<number, Mir.LocalId>()
  let nextLocal = fn.declaration.parameterCount

  for (const statement of fn.statements) {
    if (statement._tag === 'Bind') {
      const initializer = lowerExpression(statement.initializer, nextLocal, bindingLocals)
      if (initializer === undefined) return undefined
      operations.push(...initializer.operations)
      const destination = local(initializer.nextLocal)
      operations.push(
        Object.freeze({
          _tag: 'Move',
          destination,
          source: initializer.result,
          type: i32,
          provenance: Object.freeze({ span: statement.span, generated: false }),
        }),
      )
      bindingLocals.set(statement.binding.ordinal, destination)
      nextLocal = initializer.nextLocal + 1
      continue
    }
    const returned = lowerExpression(statement.expression, nextLocal, bindingLocals)
    if (returned === undefined) return undefined
    operations.push(...returned.operations)
    return {
      operations: Object.freeze([...operations]),
      result: returned.result,
      resultSpan: statement.span,
      localCount: returned.nextLocal,
      bindingLocals,
    }
  }
  return undefined
}

const trapFunction = (
  instance: Instances.Instance,
  reason: string,
  span: SourceSpan.SourceSpan,
): Mir.MirFunction => {
  const parameterCount = instance.function.declaration.parameterCount
  return Object.freeze({
    _tag: 'MirFunction',
    id: instance.key.declaration,
    parameterCount,
    localTypes: Object.freeze(Array.from({ length: parameterCount }, () => i32)),
    result: i32,
    blocks: Object.freeze([
      Object.freeze({
        _tag: 'MirBlock' as const,
        id: Object.freeze({ _tag: 'Block' as const, ordinal: 0 }),
        kind: 'Normal' as const,
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Trap' as const,
          reason,
          provenance: Object.freeze({ span, generated: true }),
        }),
      }),
    ]),
  })
}

const planFor = (
  ownership: Ownership.ModuleOwnership | undefined,
  fn: Hir.HirFunction,
): Ownership.FunctionOwnership | undefined =>
  ownership?.functions.find(
    (candidate) => candidate.declaration.id.ordinal === fn.declaration.id.ordinal,
  )

const bodySpan = (fn: Hir.HirFunction): SourceSpan.SourceSpan =>
  fn.statements.at(-1)?.span ?? fn.declaration.syntax.span

const lowerInstance = (
  instance: Instances.Instance,
  ownership: Ownership.ModuleOwnership | undefined,
): Mir.MirFunction => {
  const fn = instance.function
  const plan = planFor(ownership, fn)

  if (plan !== undefined && plan.verdict._tag === 'Violation') {
    return trapFunction(instance, 'ownership violation', plan.verdict.cause.span)
  }

  const lowered = lowerBody(fn)
  if (lowered === undefined) {
    const unavailable = Hir.firstUnavailable(fn)
    return trapFunction(instance, 'unavailable body', unavailable?.span ?? bodySpan(fn))
  }

  // ponytail: drops precede the return read; sound while every type is copyable — move the
  // returned value out before releasing once cleanup-bearing types exist.
  const releases = plan?.exits.at(0)?.releases ?? []
  const drops: ReadonlyArray<Mir.Operation> = Object.freeze(
    releases.flatMap((release): ReadonlyArray<Mir.Operation> => {
      const site = release.binding.site
      const dropped =
        site._tag === 'Parameter'
          ? local(site.parameter.ordinal)
          : lowered.bindingLocals.get(site.binding.ordinal)
      if (dropped === undefined) return []
      return [
        Object.freeze({
          _tag: 'Drop' as const,
          local: dropped,
          provenance: Object.freeze({ span: release.binding.liveFrom, generated: true }),
        }),
      ]
    }),
  )

  return Object.freeze({
    _tag: 'MirFunction',
    id: instance.key.declaration,
    parameterCount: fn.declaration.parameterCount,
    localTypes: Object.freeze(Array.from({ length: lowered.localCount }, () => i32)),
    result: i32,
    blocks: Object.freeze([
      Object.freeze({
        _tag: 'MirBlock' as const,
        id: Object.freeze({ _tag: 'Block' as const, ordinal: 0 }),
        kind: 'Normal' as const,
        operations: Object.freeze([...lowered.operations, ...drops]),
        terminator: Object.freeze({
          _tag: 'Return' as const,
          value: lowered.result,
          provenance: Object.freeze({ span: lowered.resultSpan, generated: false }),
        }),
      }),
    ]),
  })
}

/** Lowers the discovered instances into one MIR program module in discovery order. */
export const lowerProgram = (
  discovery: Instances.Discovery,
  ownership: ReadonlyMap<string, Ownership.ModuleOwnership>,
): Mir.Module =>
  Object.freeze({
    _tag: 'MirModule',
    module: discovery.rootModule,
    functions: Object.freeze(
      discovery.instances.map((instance) =>
        lowerInstance(instance, ownership.get(instance.key.declaration.module)),
      ),
    ),
  })
