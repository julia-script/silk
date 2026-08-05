import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'
import type * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'

/**
 * Lowering: reachable instances become one MIR program module. Statement sequences linearize
 * into basic blocks; conditionals become user-authored branch diamonds with join blocks; each
 * `let` binding occupies one typed local; drops follow the ownership plan's exits; unavailable
 * bodies and ownership violations lower to explicit generated traps.
 */

const i32: Mir.Type = Object.freeze({ _tag: 'I32' })
const bool: Mir.Type = Object.freeze({ _tag: 'Bool' })

const mirType = (type: 'I32' | 'Bool'): Mir.Type => (type === 'Bool' ? bool : i32)

const local = (ordinal: number): Mir.LocalId => Object.freeze({ _tag: 'Local', ordinal })

const spanKey = (span: SourceSpan.SourceSpan): string => `${span.start}:${span.end}`

interface OpenBlock {
  readonly id: Mir.BlockId
  readonly kind: 'Normal'
  operations: Array<Mir.Operation>
  terminator: Mir.Terminator | undefined
}

class FunctionLowering {
  readonly blocks: Array<OpenBlock> = []
  readonly localTypes: Array<Mir.Type> = []
  readonly bindingLocals = new Map<number, Mir.LocalId>()
  current: OpenBlock

  constructor(parameterTypes: ReadonlyArray<Mir.Type>) {
    this.localTypes.push(...parameterTypes)
    this.current = this.open()
  }

  open(): OpenBlock {
    const block: OpenBlock = {
      id: Object.freeze({ _tag: 'Block', ordinal: this.blocks.length }),
      kind: 'Normal',
      operations: [],
      terminator: undefined,
    }
    this.blocks.push(block)
    return block
  }

  alloc(type: Mir.Type): Mir.LocalId {
    const id = local(this.localTypes.length)
    this.localTypes.push(type)
    return id
  }

  emit(operation: Mir.Operation): void {
    this.current.operations.push(operation)
  }

  terminate(terminator: Mir.Terminator): void {
    if (this.current.terminator === undefined) this.current.terminator = terminator
  }
}

interface LoweredExpression {
  readonly result: Mir.LocalId
}

const lowerExpression = (
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredExpression | undefined => {
  switch (expression._tag) {
    case 'IntegerLiteral': {
      const destination = fn.alloc(i32)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type: i32,
          value: expression.value,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'BooleanLiteral': {
      const destination = fn.alloc(bool)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type: bool,
          value: expression.value ? 1 : 0,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'ParameterReference':
      return { result: local(expression.parameter.ordinal) }
    case 'BindingReference': {
      const bound = fn.bindingLocals.get(expression.binding.ordinal)
      if (bound === undefined) return undefined
      return { result: bound }
    }
    case 'Move':
      return lowerExpression(fn, expression.subject)
    case 'Call': {
      const argumentLocals: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      const destination = fn.alloc(mirType(expression.type))
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          arguments: Object.freeze(argumentLocals),
          type: mirType(expression.type),
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'BuiltinCall': {
      const argumentLocals: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      if (expression.operation === 'Not') {
        const [subject] = argumentLocals
        if (subject === undefined) return undefined
        const zero = fn.alloc(bool)
        fn.emit(
          Object.freeze({
            _tag: 'Literal',
            destination: zero,
            type: bool,
            value: 0,
            provenance: Object.freeze({ span: expression.span, generated: true }),
          }),
        )
        const destination = fn.alloc(bool)
        fn.emit(
          Object.freeze({
            _tag: 'Binary',
            operator: 'Equals',
            destination,
            left: subject,
            right: zero,
            type: bool,
            provenance: Object.freeze({ span: expression.span, generated: false }),
          }),
        )
        return { result: destination }
      }
      const [left, right] = argumentLocals
      if (left === undefined || right === undefined) return undefined
      const destination = fn.alloc(mirType(expression.type))
      fn.emit(
        Object.freeze({
          _tag: 'Binary',
          operator: expression.operation,
          destination,
          left,
          right,
          type: mirType(expression.type),
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'Unavailable':
      return undefined
  }
}

interface ExitIndex {
  readonly returns: ReadonlyMap<string, Ownership.ExitPlan>
  readonly armEnds: ReadonlyMap<string, Ownership.ExitPlan>
}

const indexExits = (plan: Ownership.FunctionOwnership | undefined): ExitIndex => {
  const returns = new Map<string, Ownership.ExitPlan>()
  const armEnds = new Map<string, Ownership.ExitPlan>()
  for (const exit of plan?.exits ?? []) {
    if (exit.kind === 'Return') returns.set(spanKey(exit.span), exit)
    else armEnds.set(`${spanKey(exit.span)}:${exit.arm ?? 'Taken'}`, exit)
  }
  return { returns, armEnds }
}

const emitReleases = (fn: FunctionLowering, exit: Ownership.ExitPlan | undefined): void => {
  for (const release of exit?.releases ?? []) {
    const site = release.binding.site
    const dropped =
      site._tag === 'Parameter'
        ? local(site.parameter.ordinal)
        : fn.bindingLocals.get(site.binding.ordinal)
    if (dropped === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'Drop',
        local: dropped,
        provenance: Object.freeze({ span: release.binding.liveFrom, generated: true }),
      }),
    )
  }
}

// ponytail: drops precede the return read; sound while every type is copyable — move the
// returned value out before releasing once cleanup-bearing types exist.
const lowerStatements = (
  fn: FunctionLowering,
  statements: ReadonlyArray<Hir.Statement>,
  exits: ExitIndex,
  arm: 'Taken' | 'Otherwise' | undefined,
): boolean | undefined => {
  for (const statement of statements) {
    if (statement._tag === 'Bind') {
      const initializer = lowerExpression(fn, statement.initializer)
      if (initializer === undefined) return undefined
      const destination = fn.alloc(fn.localTypes[initializer.result.ordinal] ?? i32)
      fn.emit(
        Object.freeze({
          _tag: 'Move',
          destination,
          source: initializer.result,
          provenance: Object.freeze({ span: statement.span, generated: false }),
        }),
      )
      fn.bindingLocals.set(statement.binding.ordinal, destination)
      continue
    }
    if (statement._tag === 'If') {
      const condition = lowerExpression(fn, statement.condition)
      if (condition === undefined) return undefined
      const entry = fn.current

      const takenBlock = fn.open()
      fn.current = takenBlock
      const takenReturned = lowerStatements(fn, statement.taken, exits, 'Taken')
      if (takenReturned === undefined) return undefined
      const takenEnd = fn.current

      const hasOtherwise = statement.otherwise.length > 0
      let otherwiseBlock: OpenBlock | undefined
      let otherwiseEnd: OpenBlock | undefined
      let otherwiseReturned = false
      if (hasOtherwise) {
        otherwiseBlock = fn.open()
        fn.current = otherwiseBlock
        const lowered = lowerStatements(fn, statement.otherwise, exits, 'Otherwise')
        if (lowered === undefined) return undefined
        otherwiseReturned = lowered
        otherwiseEnd = fn.current
      }

      const bothReturn = takenReturned && hasOtherwise && otherwiseReturned
      let join: OpenBlock | undefined
      if (!bothReturn) join = fn.open()

      entry.terminator = Object.freeze({
        _tag: 'Branch',
        condition: condition.result,
        taken: takenBlock.id,
        otherwise: (otherwiseBlock ?? join)?.id ?? takenBlock.id,
        provenance: Object.freeze({ span: statement.span, generated: false }),
      })

      const joinArm = (end: OpenBlock, armName: 'Taken' | 'Otherwise'): void => {
        if (end.terminator !== undefined || join === undefined) return
        fn.current = end
        emitReleases(fn, exits.armEnds.get(`${spanKey(statement.span)}:${armName}`))
        end.terminator = Object.freeze({
          _tag: 'Jump',
          target: join.id,
          provenance: Object.freeze({ span: statement.span, generated: true }),
        })
      }
      if (!takenReturned) joinArm(takenEnd, 'Taken')
      if (hasOtherwise && !otherwiseReturned && otherwiseEnd !== undefined) {
        joinArm(otherwiseEnd, 'Otherwise')
      }

      if (join === undefined) return true
      fn.current = join
      continue
    }
    const returned = lowerExpression(fn, statement.expression)
    if (returned === undefined) return undefined
    emitReleases(fn, exits.returns.get(spanKey(statement.span)))
    fn.terminate(
      Object.freeze({
        _tag: 'Return',
        value: returned.result,
        provenance: Object.freeze({ span: statement.span, generated: false }),
      }),
    )
    return true
  }
  return arm === undefined ? undefined : false
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

  const contract = fn.contract
  const parameterTypes =
    contract._tag === 'Contract'
      ? contract.parameters.map(mirType)
      : Array.from({ length: fn.declaration.parameterCount }, () => i32)
  const resultType = contract._tag === 'Contract' ? mirType(contract.result) : i32

  const lowering = new FunctionLowering(parameterTypes)
  const outcome = lowerStatements(lowering, fn.statements, indexExits(plan), undefined)

  if (outcome !== true || lowering.blocks.some((block) => block.terminator === undefined)) {
    const unavailable = Hir.firstUnavailable(fn)
    return trapFunction(instance, 'unavailable body', unavailable?.span ?? bodySpan(fn))
  }

  return Object.freeze({
    _tag: 'MirFunction',
    id: instance.key.declaration,
    parameterCount: fn.declaration.parameterCount,
    localTypes: Object.freeze([...lowering.localTypes]),
    result: resultType,
    blocks: Object.freeze(
      lowering.blocks.map((block) => {
        const terminator = block.terminator
        if (terminator === undefined) {
          throw new RangeError('Lowering left an unterminated block')
        }
        return Object.freeze({
          _tag: 'MirBlock' as const,
          id: block.id,
          kind: block.kind,
          operations: Object.freeze([...block.operations]),
          terminator,
        })
      }),
    ),
  })
}

/** Lowers the discovered instances into one MIR program module in discovery order. */
export const lowerProgram = (
  discovery: Instances.Discovery,
  ownership: ReadonlyMap<string, Ownership.ModuleOwnership>,
  layout: Layout.Plan,
): Mir.Module =>
  Object.freeze({
    _tag: 'MirModule',
    module: discovery.rootModule,
    layout,
    functions: Object.freeze(
      discovery.instances.map((instance) =>
        lowerInstance(instance, ownership.get(instance.key.declaration.module)),
      ),
    ),
  })
