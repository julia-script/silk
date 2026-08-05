import type * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import type * as Mir from './Mir.js'
import type * as Ownership from './Ownership.js'

/**
 * Lowering: reachable instances become one MIR program module. Structured HIR bodies linearize
 * into basic blocks in evaluation order; drops and cleanup edges are inserted exactly as the
 * ownership plan directs (nothing in the frozen slice, where every exit releases nothing);
 * unavailable bodies lower to explicit generated traps. Provenance stays attached throughout.
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
    case 'Call': {
      const operations: Array<Mir.Operation> = []
      const argumentLocals: Array<Mir.LocalId> = []
      let cursor = nextLocal
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(argument, cursor)
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

const planFor = (
  ownership: Ownership.ModuleOwnership | undefined,
  fn: Hir.HirFunction,
): Ownership.FunctionOwnership | undefined =>
  ownership?.functions.find(
    (candidate) => candidate.declaration.id.ordinal === fn.declaration.id.ordinal,
  )

const lowerInstance = (
  instance: Instances.Instance,
  ownership: Ownership.ModuleOwnership | undefined,
): Mir.MirFunction => {
  const fn = instance.function
  const parameterCount = fn.declaration.parameterCount
  const lowered = lowerExpression(fn.body, parameterCount)
  const plan = planFor(ownership, fn)

  if (lowered === undefined) {
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
            reason: 'unavailable body',
            provenance: Object.freeze({ span: fn.body.span, generated: true }),
          }),
        }),
      ]),
    })
  }

  const releases = plan?.exits.at(0)?.releases ?? []
  const drops: ReadonlyArray<Mir.Operation> = Object.freeze(
    releases.map((release) =>
      Object.freeze({
        _tag: 'Drop' as const,
        local: local(release.binding.parameter.ordinal),
        provenance: Object.freeze({ span: fn.body.span, generated: true }),
      }),
    ),
  )

  return Object.freeze({
    _tag: 'MirFunction',
    id: instance.key.declaration,
    parameterCount,
    localTypes: Object.freeze(Array.from({ length: lowered.nextLocal }, () => i32)),
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
          provenance: Object.freeze({ span: fn.body.span, generated: false }),
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
