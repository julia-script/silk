import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'
import type * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * Lowering preserves source control as canonical acyclic regions. Repetition is represented by a
 * loop region plus lexical repeat/exit outcomes; backend-private CFGs are derived later.
 */

const i32: Mir.Type = Object.freeze({ _tag: 'I32' })
const bool: Mir.Type = Object.freeze({ _tag: 'Bool' })

const mirType = (type: Type.Type): Mir.Type | undefined =>
  typeof type === 'string'
    ? type === 'Bool'
      ? bool
      : i32
    : Type.isNominal(type)
      ? Object.freeze({ _tag: 'Nominal', type })
      : Object.freeze({ _tag: 'FixedArray', type })

const local = (ordinal: number): Mir.LocalId => Object.freeze({ _tag: 'Local', ordinal })

const spanKey = (span: SourceSpan.SourceSpan): string => `${span.start}:${span.end}`

class FunctionLowering {
  readonly regions: Array<Mir.Region | undefined> = []
  readonly localTypes: Array<Mir.Type> = []
  readonly bindingLocals = new Map<number, Mir.LocalId>()
  private operations: Array<Mir.Operation> = []

  constructor(parameterTypes: ReadonlyArray<Mir.Type>) {
    this.localTypes.push(...parameterTypes)
  }

  reserve(): Mir.RegionId {
    const id = Object.freeze({ _tag: 'Region' as const, ordinal: this.regions.length })
    this.regions.push(undefined)
    return id
  }

  publish(region: Mir.Region): void {
    this.regions[region.id.ordinal] = region
  }

  capture<A>(body: () => A): readonly [A, ReadonlyArray<Mir.Operation>] {
    const previous = this.operations
    this.operations = []
    const result = body()
    const operations = Object.freeze([...this.operations])
    this.operations = previous
    return [result, operations]
  }

  alloc(type: Mir.Type): Mir.LocalId {
    const id = local(this.localTypes.length)
    this.localTypes.push(type)
    return id
  }

  emit(operation: Mir.Operation): void {
    this.operations.push(operation)
  }
}

interface LoweredExpression {
  readonly result: Mir.LocalId
}

interface LoweredPlace {
  readonly root: Mir.LocalId
  readonly selectors: ReadonlyArray<Mir.PlaceSelector>
}

const lowerPlacePath = (
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredPlace | undefined => {
  if (expression._tag === 'Project') {
    const subject = lowerPlacePath(fn, expression.subject)
    if (subject === undefined) return undefined
    return Object.freeze({
      root: subject.root,
      selectors: Object.freeze([
        ...subject.selectors,
        Object.freeze({
          _tag: 'FieldSelector' as const,
          field: expression.field,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      ]),
    })
  }
  if (expression._tag === 'IndexPlace') {
    const subject = lowerPlacePath(fn, expression.subject)
    if (subject === undefined) return undefined
    const index:
      | Extract<Mir.PlaceSelector, { readonly _tag: 'ElementSelector' }>['index']
      | undefined =
      expression.bounds._tag === 'Proven'
        ? Object.freeze({ _tag: 'Proven', value: expression.bounds.index })
        : (() => {
            const lowered = lowerExpression(fn, expression.index)
            return lowered === undefined
              ? undefined
              : Object.freeze({ _tag: 'Runtime' as const, local: lowered.result })
          })()
    if (index === undefined) return undefined
    return Object.freeze({
      root: subject.root,
      selectors: Object.freeze([
        ...subject.selectors,
        Object.freeze({
          _tag: 'ElementSelector' as const,
          length: expression.array.length,
          index,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      ]),
    })
  }
  const root = lowerExpression(fn, expression)
  return root === undefined
    ? undefined
    : Object.freeze({ root: root.result, selectors: Object.freeze([]) })
}

const lowerPlace = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'Project' | 'IndexPlace' }>,
): LoweredExpression | undefined => {
  const place = lowerPlacePath(fn, expression)
  const type = mirType(expression.type)
  if (place === undefined || type === undefined) return undefined
  const destination = fn.alloc(type)
  fn.emit(
    Object.freeze({
      _tag: 'ReadPlace',
      destination,
      root: place.root,
      selectors: place.selectors,
      type,
      provenance: Object.freeze({ span: expression.span, generated: false }),
    }),
  )
  return Object.freeze({ result: destination })
}

function lowerExpression(
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredExpression | undefined {
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
    case 'Construct': {
      const type = mirType(expression.type)
      if (type?._tag !== 'Nominal') return undefined
      const canonicalFields = new Map(
        expression.fields.map((field) => [field.field.ordinal, field] as const),
      )
      const loweredFields = new Map<number, Mir.LocalId>()
      for (const fieldId of expression.evaluationOrder) {
        const field = canonicalFields.get(fieldId.ordinal)
        if (field === undefined) return undefined
        const lowered = lowerExpression(fn, field.value)
        if (lowered === undefined) return undefined
        loweredFields.set(field.field.ordinal, lowered.result)
      }
      const fields = expression.fields.flatMap((field) => {
        const value = loweredFields.get(field.field.ordinal)
        return value === undefined ? [] : [Object.freeze({ field: field.field, value })]
      })
      if (fields.length !== expression.fields.length) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Construct',
          destination,
          type,
          fields: Object.freeze(fields),
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'ArrayConstruct': {
      const type = mirType(expression.type)
      if (type?._tag !== 'FixedArray') return undefined
      const elements: Array<Mir.LocalId> = []
      for (const element of expression.elements) {
        const lowered = lowerExpression(fn, element)
        if (lowered === undefined) return undefined
        elements.push(lowered.result)
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ConstructArray',
          destination,
          type,
          elements: Object.freeze(elements),
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'Project': {
      return lowerPlace(fn, expression)
    }
    case 'IndexPlace': {
      return lowerPlace(fn, expression)
    }
    case 'Call': {
      const argumentLocals: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      const type = mirType(expression.type)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          arguments: Object.freeze(argumentLocals),
          type,
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
      if (expression.operation === 'Not' || expression.operation === 'Negate') {
        const [subject] = argumentLocals
        if (subject === undefined) return undefined
        const operandType = expression.operation === 'Not' ? bool : i32
        const zero = fn.alloc(operandType)
        fn.emit(
          Object.freeze({
            _tag: 'Literal',
            destination: zero,
            type: operandType,
            value: 0,
            provenance: Object.freeze({ span: expression.span, generated: true }),
          }),
        )
        const destination = fn.alloc(operandType)
        fn.emit(
          Object.freeze({
            _tag: 'Binary',
            operator: expression.operation === 'Not' ? 'Equals' : 'Subtract',
            destination,
            left: expression.operation === 'Not' ? subject : zero,
            right: expression.operation === 'Not' ? zero : subject,
            type: operandType,
            provenance: Object.freeze({ span: expression.span, generated: false }),
          }),
        )
        return { result: destination }
      }
      const [left, right] = argumentLocals
      if (left === undefined || right === undefined) return undefined
      const type = mirType(expression.type)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Binary',
          operator: expression.operation,
          destination,
          left,
          right,
          type,
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
  readonly loopFallthroughs: ReadonlyMap<number, Ownership.ExitPlan>
  readonly transfers: ReadonlyMap<string, Ownership.ExitPlan>
}

const indexExits = (plan: Ownership.FunctionOwnership | undefined): ExitIndex => {
  const returns = new Map<string, Ownership.ExitPlan>()
  const armEnds = new Map<string, Ownership.ExitPlan>()
  const loopFallthroughs = new Map<number, Ownership.ExitPlan>()
  const transfers = new Map<string, Ownership.ExitPlan>()
  for (const exit of plan?.exits ?? []) {
    switch (exit.kind) {
      case 'Return':
        returns.set(spanKey(exit.span), exit)
        break
      case 'ArmEnd':
        armEnds.set(`${spanKey(exit.span)}:${exit.arm ?? 'Taken'}`, exit)
        break
      case 'LoopFallthrough':
        if (exit.target !== undefined) loopFallthroughs.set(exit.target.ordinal, exit)
        break
      case 'Break':
      case 'Continue':
        transfers.set(spanKey(exit.span), exit)
        break
    }
  }
  return { returns, armEnds, loopFallthroughs, transfers }
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

const copyType = (type: Type.Type): boolean =>
  Type.isBuiltin(type) || (Type.isFixedArray(type) && copyType(type.element))

const ownerFields = (ownerLoop: Mir.LoopId | undefined): { readonly ownerLoop?: Mir.LoopId } =>
  ownerLoop === undefined ? {} : { ownerLoop }

const generated = (span: SourceSpan.SourceSpan): Mir.Provenance =>
  Object.freeze({ span, generated: true })

const authored = (span: SourceSpan.SourceSpan): Mir.Provenance =>
  Object.freeze({ span, generated: false })

const lowerWriteSelectors = (
  fn: FunctionLowering,
  selectors: ReadonlyArray<Hir.WriteSelector>,
): ReadonlyArray<Mir.PlaceSelector> | undefined => {
  const lowered: Array<Mir.PlaceSelector> = []
  for (const selector of selectors) {
    if (selector._tag === 'Field') {
      lowered.push(
        Object.freeze({
          _tag: 'FieldSelector',
          field: selector.field,
          provenance: authored(selector.span),
        }),
      )
      continue
    }
    const index =
      selector.bounds._tag === 'Proven'
        ? Object.freeze({ _tag: 'Proven' as const, value: selector.bounds.index })
        : (() => {
            const expression = lowerExpression(fn, selector.index)
            return expression === undefined
              ? undefined
              : Object.freeze({ _tag: 'Runtime' as const, local: expression.result })
          })()
    if (index === undefined) return undefined
    lowered.push(
      Object.freeze({
        _tag: 'ElementSelector',
        length: selector.array.length,
        index,
        provenance: authored(selector.span),
      }),
    )
  }
  return Object.freeze(lowered)
}

const lowerSequence = (
  fn: FunctionLowering,
  statements: ReadonlyArray<Hir.Statement>,
  exits: ExitIndex,
  ownerLoop: Mir.LoopId | undefined,
  terminal: Mir.Outcome,
  reserved?: Mir.RegionId,
  armExit?: Ownership.ExitPlan,
): Mir.RegionId | undefined => {
  const id = reserved ?? fn.reserve()
  const [statement, ...rest] = statements
  if (statement === undefined) {
    const [, releases] = fn.capture(() => emitReleases(fn, armExit))
    if (releases.length > 0) {
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) => (operation._tag === 'Drop' ? [operation] : [])),
          ),
          outcome: terminal,
        }),
      )
    } else {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: terminal,
        }),
      )
    }
    return id
  }

  if (statement._tag === 'UnavailableStatement') {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations: Object.freeze([]),
        outcome: Object.freeze({
          _tag: 'Trap',
          reason: 'unavailable statement',
          provenance: generated(statement.span),
        }),
      }),
    )
    return id
  }

  if (statement._tag === 'Bind') {
    const [initializer, operations] = fn.capture(() => {
      const lowered = lowerExpression(fn, statement.initializer)
      if (lowered === undefined) return undefined
      const destination = fn.alloc(fn.localTypes.at(lowered.result.ordinal) ?? i32)
      fn.emit(
        Object.freeze({
          _tag: 'Move',
          destination,
          source: lowered.result,
          provenance: authored(statement.span),
        }),
      )
      fn.bindingLocals.set(statement.binding.ordinal, destination)
      return destination
    })
    if (initializer === undefined) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Write') {
    const root = fn.bindingLocals.get(statement.place.root.ordinal)
    const rootType = root === undefined ? undefined : fn.localTypes.at(root.ordinal)
    const type = mirType(statement.place.type)
    const [written, operations] = fn.capture(() => {
      if (root === undefined || rootType === undefined || type === undefined) return false
      const selectors = lowerWriteSelectors(fn, statement.place.selectors)
      if (selectors === undefined) return false
      fn.emit(
        Object.freeze({
          _tag: 'CheckPlace',
          root,
          selectors,
          type,
          provenance: authored(statement.place.span),
        }),
      )
      const value = lowerExpression(fn, statement.value)
      if (value === undefined) return false
      fn.emit(
        Object.freeze({
          _tag: 'WritePlace',
          root,
          selectors,
          source: value.result,
          rootType,
          type,
          mutable: true,
          replacement: copyType(statement.place.type) ? 'Copy' : 'Owned',
          commit: 'AfterCleanup',
          provenance: authored(statement.span),
        }),
      )
      return true
    })
    if (!written) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'If') {
    const conditional = fn.reserve()
    const taken = fn.reserve()
    const otherwise = fn.reserve()
    const following = fn.reserve()
    const [condition, operations] = fn.capture(() => lowerExpression(fn, statement.condition))
    if (condition === undefined) return undefined
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: conditional,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'ConditionalRegion',
        id: conditional,
        ...ownerFields(ownerLoop),
        condition: condition.result,
        taken,
        otherwise,
        following,
        provenance: authored(statement.span),
      }),
    )
    const forward = Object.freeze({
      _tag: 'Forward' as const,
      target: following,
      provenance: generated(statement.span),
    })
    if (
      lowerSequence(
        fn,
        statement.taken,
        exits,
        ownerLoop,
        forward,
        taken,
        exits.armEnds.get(`${spanKey(statement.span)}:Taken`),
      ) === undefined ||
      lowerSequence(
        fn,
        statement.otherwise,
        exits,
        ownerLoop,
        forward,
        otherwise,
        exits.armEnds.get(`${spanKey(statement.span)}:Otherwise`),
      ) === undefined ||
      lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
    ) {
      return undefined
    }
    return id
  }

  if (statement._tag === 'While') {
    const loop: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.loop.ordinal })
    const conditionId = fn.reserve()
    const bodyId = fn.reserve()
    const following = fn.reserve()
    const [condition, conditionOperations] = fn.capture(() =>
      lowerExpression(fn, statement.condition),
    )
    if (condition === undefined) return undefined
    fn.publish(
      Object.freeze({
        _tag: 'LoopRegion',
        id,
        ...ownerFields(ownerLoop),
        loop,
        ...(ownerLoop === undefined ? {} : { parent: ownerLoop }),
        condition: conditionId,
        conditionValue: condition.result,
        body: bodyId,
        following,
        provenance: authored(statement.span),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id: conditionId,
        ownerLoop: loop,
        operations: conditionOperations,
        outcome: Object.freeze({ _tag: 'Yield', provenance: generated(statement.span) }),
      }),
    )
    const repeat = Object.freeze({
      _tag: 'Repeat' as const,
      loop,
      provenance: generated(statement.span),
    })
    if (
      lowerSequence(
        fn,
        statement.body,
        exits,
        loop,
        repeat,
        bodyId,
        exits.loopFallthroughs.get(statement.loop.ordinal),
      ) === undefined ||
      lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
    ) {
      return undefined
    }
    return id
  }

  if (statement._tag === 'Break' || statement._tag === 'Continue') {
    const target: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.target.ordinal })
    const outcome: Mir.Outcome = Object.freeze({
      _tag: statement._tag === 'Break' ? ('Exit' as const) : ('Repeat' as const),
      loop: target,
      provenance: authored(statement.span),
    })
    const [, releases] = fn.capture(() =>
      emitReleases(fn, exits.transfers.get(spanKey(statement.span))),
    )
    if (releases.length === 0) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome,
        }),
      )
    } else {
      const cleanup = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: cleanup,
            provenance: generated(statement.span),
          }),
        }),
      )
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id: cleanup,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) => (operation._tag === 'Drop' ? [operation] : [])),
          ),
          outcome,
        }),
      )
    }
    return id
  }

  const [returned, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
  if (returned === undefined) return undefined
  const returnOutcome: Mir.Outcome = Object.freeze({
    _tag: 'Return',
    value: returned.result,
    provenance: authored(statement.span),
  })
  const [, releases] = fn.capture(() =>
    emitReleases(fn, exits.returns.get(spanKey(statement.span))),
  )
  if (releases.length === 0) {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: returnOutcome,
      }),
    )
  } else {
    const cleanup = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: cleanup,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'CleanupRegion',
        id: cleanup,
        ...ownerFields(ownerLoop),
        releases: Object.freeze(
          releases.flatMap((operation) => (operation._tag === 'Drop' ? [operation] : [])),
        ),
        outcome: returnOutcome,
      }),
    )
  }
  return id
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
    entry: Object.freeze({ _tag: 'Region', ordinal: 0 }),
    regions: Object.freeze([
      Object.freeze({
        _tag: 'OperationRegion' as const,
        id: Object.freeze({ _tag: 'Region' as const, ordinal: 0 }),
        operations: Object.freeze([]),
        outcome: Object.freeze({
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
      ? contract.parameters.flatMap((type) => {
          const lowered = mirType(type)
          return lowered === undefined ? [] : [lowered]
        })
      : Array.from({ length: fn.declaration.parameterCount }, () => i32)
  const resultType = contract._tag === 'Contract' ? mirType(contract.result) : i32
  if (resultType === undefined) {
    return trapFunction(instance, 'unavailable contract type', bodySpan(fn))
  }

  const lowering = new FunctionLowering(parameterTypes)
  const terminal: Mir.Outcome = Object.freeze({
    _tag: 'Trap',
    reason: 'body fell through without return',
    provenance: generated(bodySpan(fn)),
  })
  const entry = lowerSequence(lowering, fn.statements, indexExits(plan), undefined, terminal)

  if (entry === undefined || lowering.regions.some((region) => region === undefined)) {
    const unavailable = Hir.firstUnavailable(fn)
    return trapFunction(instance, 'unavailable body', unavailable?.span ?? bodySpan(fn))
  }

  return Object.freeze({
    _tag: 'MirFunction',
    id: instance.key.declaration,
    parameterCount: fn.declaration.parameterCount,
    localTypes: Object.freeze([...lowering.localTypes]),
    result: resultType,
    entry,
    regions: Object.freeze(
      lowering.regions.flatMap((region) => (region === undefined ? [] : [region])),
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
