import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * Lowering preserves source control as canonical acyclic regions. Repetition is represented by a
 * loop region plus lexical repeat/exit outcomes; backend-private CFGs are derived later.
 */

const i32: Extract<Mir.Type, { readonly _tag: 'I32' }> = Object.freeze({ _tag: 'I32' })
const usize: Extract<Mir.Type, { readonly _tag: 'Usize' }> = Object.freeze({ _tag: 'Usize' })
const bool: Extract<Mir.Type, { readonly _tag: 'Bool' }> = Object.freeze({ _tag: 'Bool' })

const mirType = (
  type: Type.Type,
  substitution: ReadonlyMap<string, Type.Type> = new Map(),
): Mir.Type | undefined => {
  const specialized = Type.substitute(type, substitution)
  if (!Type.isConcrete(specialized)) return undefined
  return typeof specialized === 'string'
    ? specialized === 'Never'
      ? undefined
      : specialized === 'Bool'
        ? bool
        : specialized === 'Usize'
          ? usize
          : i32
    : Type.isNominal(specialized)
      ? Object.freeze({ _tag: 'Nominal', type: specialized })
      : Type.isFixedArray(specialized)
        ? Object.freeze({ _tag: 'FixedArray', type: specialized })
        : Type.isSlice(specialized)
          ? Object.freeze({ _tag: 'Slice', type: specialized })
          : Type.isReference(specialized)
            ? Object.freeze({ _tag: 'Reference', type: specialized })
            : Type.isUnion(specialized)
              ? Object.freeze({ _tag: 'Union', type: specialized })
              : Type.isEffect(specialized)
                ? Object.freeze({ _tag: 'EffectOutcome', type: specialized })
                : undefined
}

const local = (ordinal: number): Mir.LocalId => Object.freeze({ _tag: 'Local', ordinal })

const spanKey = (span: SourceSpan.SourceSpan): string => `${span.start}:${span.end}`
const patternKey = (binding: Match.BindingId): string =>
  `${spanKey(binding.arm.match.span)}:${binding.arm.ordinal}:${binding.ordinal}`
const borrowKey = (borrow: Hir.BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

class FunctionLowering {
  readonly regions: Array<Mir.Region | undefined> = []
  readonly localTypes: Array<Mir.Type> = []
  readonly bindingLocals = new Map<number, Mir.LocalId>()
  readonly parameterLocals = new Map<number, Mir.LocalId>()
  readonly effectRecipes = new Map<number, Hir.Expression>()
  readonly effectTransforms = new Map<
    number,
    {
      readonly expression: Extract<Hir.Expression, { readonly _tag: 'EffectTransform' }>
      readonly protected: Mir.LocalId
      readonly callback: Mir.LocalId
      readonly loanEnds: ReadonlyArray<Hir.BorrowId>
    }
  >()
  readonly patternLocals = new Map<string, Mir.LocalId>()
  readonly loanLocals = new Map<string, Mir.LocalId>()
  readonly slotLoans = new Map<number, ReadonlyArray<Hir.BorrowId>>()
  private operations: Array<Mir.Operation> = []

  constructor(
    readonly layout: Layout.Plan,
    parameterTypes: ReadonlyArray<Mir.Type>,
    readonly ownership: Ownership.FunctionOwnership | undefined,
    readonly substitution: ReadonlyMap<string, Type.Type>,
    readonly effectOutcome: Type.Effect | undefined,
    readonly owner: Instances.Instance,
    readonly effectResults: ReadonlyMap<
      string,
      Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
    >,
    readonly generatedRunners: Array<GeneratedEffectRunner>,
  ) {
    this.localTypes.push(...parameterTypes)
    parameterTypes.forEach((_, ordinal) => {
      this.parameterLocals.set(ordinal, local(ordinal))
    })
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

  type(type: Type.Type): Mir.Type | undefined {
    return mirType(type, this.substitution)
  }

  semantic(type: Type.Type): Type.Type {
    return Type.substitute(type, this.substitution)
  }
}

interface GeneratedEffectRunner {
  readonly owner: Instances.Instance
  readonly block: Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }>
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
}

const instanceText = (
  declaration: { readonly module: string; readonly name: string },
  typeArguments: ReadonlyArray<Type.Type>,
): string =>
  `${declaration.module}\u0000${declaration.name}\u0000${typeArguments.map(Type.key).join('\u0000')}`

const runnerId = (owner: Instances.InstanceKey, site: Hir.EffectSiteId): typeof owner.declaration =>
  Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: owner.declaration.module,
    name: `${owner.declaration.name}$effect$${site.function.ordinal}$${site.span.start}`,
  })

const effectValueType = (
  layout: Layout.Plan,
  instance: Instances.InstanceKey,
  block: Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }>,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  const environment = layout.effectEnvironments.find(
    (candidate) =>
      candidate._tag === 'EffectEnvironment' &&
      instanceText(candidate.instance.declaration, candidate.instance.typeArguments) ===
        instanceText(instance.declaration, instance.typeArguments) &&
      candidate.site.function.sourceId === block.site.function.sourceId &&
      candidate.site.function.ordinal === block.site.function.ordinal &&
      candidate.site.span.start === block.site.span.start,
  )
  if (environment?._tag !== 'EffectEnvironment') return undefined
  return Object.freeze({
    _tag: 'EffectValue',
    type: environment.effect,
    site: block.site,
    environment,
  })
}

const sameSite = (left: Hir.CallableSiteId, right: Hir.CallableSiteId): boolean =>
  left.function.sourceId === right.function.sourceId &&
  left.function.ordinal === right.function.ordinal &&
  left.span.start === right.span.start &&
  left.span.end === right.span.end

const callableValueType = (
  fn: FunctionLowering,
  section: Extract<Hir.Expression, { readonly _tag: 'CallableSection' }>,
  applicationSubstitution: ReadonlyMap<string, Type.Type> = new Map(),
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const expected = Type.substitute(
    Type.substitute(section.type, fn.substitution),
    new Map([...section.substitution, ...applicationSubstitution]),
  )
  const candidates = fn.layout.callableEnvironments.filter(
    (
      candidate,
    ): candidate is Extract<Layout.CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> =>
      candidate._tag === 'CallableEnvironment' &&
      instanceText(candidate.callable.owner.declaration, candidate.callable.owner.typeArguments) ===
        instanceText(fn.owner.key.declaration, fn.owner.key.typeArguments) &&
      sameSite(candidate.callable.site, section.site) &&
      (!Type.isConcrete(expected) || Type.equals(candidate.callable.type, expected)),
  )
  const environment = candidates.length === 1 ? candidates.at(0) : undefined
  if (environment === undefined) return undefined
  return Object.freeze({
    _tag: 'CallableValue',
    type: environment.callable.type,
    target: environment.callable.target,
    site: section.site,
    environment,
  })
}

const functionItemValueType = (
  fn: FunctionLowering,
  item: Extract<Hir.Expression, { readonly _tag: 'FunctionItem' }>,
  applicationSubstitution: ReadonlyMap<string, Type.Type> = new Map(),
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const type = Type.substitute(Type.substitute(item.type, fn.substitution), applicationSubstitution)
  return Type.isCallable(type) && Type.isConcrete(type)
    ? Object.freeze({ _tag: 'CallableValue', type, target: item.target })
    : undefined
}

interface LoweredExpression {
  readonly result: Mir.LocalId
}

interface LoweredPlace {
  readonly root: Mir.LocalId
  readonly selectors: ReadonlyArray<Mir.PlaceSelector>
}

const lowerRunEffectValue = (
  fn: FunctionLowering,
  effect: Mir.LocalId,
  effectType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  success: Type.Type,
  span: SourceSpan.SourceSpan,
): LoweredExpression | undefined => {
  const successType = fn.type(success)
  if (
    successType === undefined ||
    successType._tag === 'EffectOutcome' ||
    successType._tag === 'EffectValue'
  )
    return undefined
  const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: effectType.type,
  })
  const outcome = fn.alloc(outcomeType)
  const destination = fn.alloc(successType)
  const propagationType =
    effectType.type.failures.length === 0 || fn.effectOutcome === undefined
      ? undefined
      : fn.type(fn.effectOutcome)
  if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome') return undefined
  const tagMappings = effectType.type.failures.flatMap((failure, sourceOrdinal) => {
    const target = propagationType?.type.failures.findIndex((candidate) =>
      Type.equals(candidate, failure),
    )
    return target === undefined || target < 0
      ? []
      : [Object.freeze({ source: sourceOrdinal + 1, target: target + 1 })]
  })
  if (tagMappings.length !== effectType.type.failures.length) return undefined
  const propagationShape =
    propagationType === undefined ? undefined : Layout.callingShape(fn.layout, propagationType.type)
  fn.emit(
    Object.freeze({
      _tag: 'RunEffectValue',
      destination,
      outcome,
      effect,
      runner: runnerId(effectType.environment.instance, effectType.site),
      runnerTypeArguments: effectType.environment.instance.typeArguments,
      outcomeType,
      ...(propagationType === undefined ? {} : { propagationType }),
      tagMappings: Object.freeze(tagMappings),
      propagationLaneCount: propagationShape?.laneCount ?? 0,
      type: successType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({ result: destination })
}

const lowerCallableLocalApplication = (
  fn: FunctionLowering,
  callable: Mir.LocalId,
  argument: Mir.LocalId,
  callableType: Type.Callable,
  span: SourceSpan.SourceSpan,
): LoweredExpression | undefined => {
  const stored = fn.localTypes.at(callable.ordinal)
  if (stored?._tag !== 'CallableValue') return undefined
  const typeArguments = stored.environment?.callable.typeArguments ?? Object.freeze([])
  const semanticResult = fn.semantic(callableType.result)
  const resultType =
    Type.isEffect(semanticResult) && stored.target._tag === 'DeclarationCallableTarget'
      ? fn.effectResults.get(instanceText(stored.target.declaration, typeArguments))
      : fn.type(semanticResult)
  if (resultType === undefined) return undefined
  const destination = fn.alloc(resultType)
  fn.emit(
    Object.freeze({
      _tag: 'ApplyCallable',
      destination,
      callable,
      typeArguments,
      captures: Object.freeze([]),
      arguments: Object.freeze([argument]),
      callableType,
      access: callableType.mode,
      evaluation: 'CalleeThenArguments',
      realization: 'Environment',
      type: resultType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({ result: destination })
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
  if (expression._tag === 'SliceIndexPlace') {
    const subject = lowerPlacePath(fn, expression.slice)
    const index = lowerExpression(fn, expression.index)
    if (subject === undefined || index === undefined) return undefined
    return Object.freeze({
      root: subject.root,
      selectors: Object.freeze([
        ...subject.selectors,
        Object.freeze({
          _tag: 'SliceElementSelector',
          index: index.result,
          access: expression.access,
          provenance: authored(expression.span),
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
  expression: Extract<
    Hir.Expression,
    { readonly _tag: 'Project' | 'IndexPlace' | 'SliceIndexPlace' }
  >,
): LoweredExpression | undefined => {
  const place = lowerPlacePath(fn, expression)
  const type = fn.type(expression.type)
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
      const type = fn.type(expression.type)
      if (type === undefined || (type._tag !== 'I32' && type._tag !== 'Usize')) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type,
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
    case 'ParameterReference': {
      const parameter = fn.parameterLocals.get(expression.parameter.ordinal)
      return parameter === undefined ? undefined : { result: parameter }
    }
    case 'BindingReference': {
      const bound = fn.bindingLocals.get(expression.binding.ordinal)
      if (bound === undefined) return undefined
      return { result: bound }
    }
    case 'PatternBindingReference': {
      const bound = fn.patternLocals.get(patternKey(expression.binding))
      if (bound === undefined) return undefined
      return { result: bound }
    }
    case 'Move':
      return lowerExpression(fn, expression.subject)
    case 'FunctionItem': {
      const type = functionItemValueType(fn, expression)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'MakeCallable',
          destination,
          target: expression.target,
          typeArguments: Object.freeze([]),
          captures: Object.freeze([]),
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'CallableSection': {
      const type = callableValueType(fn, expression)
      if (type === undefined || type.environment === undefined) return undefined
      const captures: Array<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly source: Mir.LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }> = []
      for (const capture of expression.captures) {
        const lowered = lowerExpression(fn, capture.value)
        if (lowered === undefined) return undefined
        captures.push(
          Object.freeze({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            source: lowered.result,
            access: capture.access,
          }),
        )
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'MakeCallable',
          destination,
          target: expression.target,
          typeArguments: type.environment.callable.typeArguments,
          captures: Object.freeze(captures),
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'CallableApply': {
      const directSection =
        expression.realization === 'DirectErasedSection' &&
        expression.callee._tag === 'CallableSection'
          ? expression.callee
          : undefined
      const directItem = expression.callee._tag === 'FunctionItem' ? expression.callee : undefined
      const directType =
        directSection !== undefined
          ? callableValueType(fn, directSection, expression.substitution)
          : directItem !== undefined
            ? functionItemValueType(fn, directItem, expression.substitution)
            : undefined
      const arguments_: Array<Mir.LocalId> = []
      const captures: Array<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly source: Mir.LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }> = []
      let callable: Mir.LocalId | undefined
      let callableType: Type.Callable | undefined
      let target: Hir.CallableTarget | undefined
      let typeArguments: ReadonlyArray<Type.Type> = Object.freeze([])
      const lowerArguments = (): boolean => {
        for (const argument of expression.arguments) {
          const lowered = lowerExpression(fn, argument)
          if (lowered === undefined) return false
          arguments_.push(lowered.result)
        }
        return true
      }
      const lowerCallee = (): boolean => {
        if (directSection !== undefined || directItem !== undefined) {
          if (directType === undefined) return false
          callableType = directType.type
          target = directType.target
          typeArguments =
            directType.environment?.callable.typeArguments ??
            Object.freeze([...expression.substitution.values()].map((type) => fn.semantic(type)))
          if (directSection !== undefined) {
            for (const capture of directSection.captures) {
              const lowered = lowerExpression(fn, capture.value)
              if (lowered === undefined) return false
              captures.push(
                Object.freeze({
                  ordinal: capture.ordinal,
                  parameterOrdinal: capture.parameterOrdinal,
                  source: lowered.result,
                  access: capture.access,
                }),
              )
            }
          }
          return true
        }
        const lowered = lowerExpression(fn, expression.callee)
        const loweredType =
          lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
        if (lowered === undefined || loweredType?._tag !== 'CallableValue') return false
        callable = lowered.result
        callableType = loweredType.type
        typeArguments = loweredType.environment?.callable.typeArguments ?? Object.freeze([])
        return true
      }
      const lowered =
        expression.evaluation === 'LeftThenCallable'
          ? lowerArguments() && lowerCallee()
          : lowerCallee() && lowerArguments()
      const type = fn.type(expression.type)
      if (!lowered || type === undefined || callableType === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ApplyCallable',
          destination,
          ...(callable === undefined ? {} : { callable }),
          ...(target === undefined ? {} : { target }),
          typeArguments,
          captures: Object.freeze(captures),
          arguments: Object.freeze(arguments_),
          callableType,
          access: expression.access,
          evaluation: expression.evaluation,
          realization: callable === undefined ? 'DirectErasedSection' : expression.realization,
          type,
          provenance: authored(expression.span),
        }),
      )
      for (const capture of directSection?.captures ?? []) {
        if (capture.value._tag !== 'SliceBorrow') continue
        const slice = fn.loanLocals.get(borrowKey(capture.value.borrow))
        if (slice === undefined) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'EndLoan',
            borrow: capture.value.borrow,
            slice,
            provenance: generated(expression.span),
          }),
        )
        fn.loanLocals.delete(borrowKey(capture.value.borrow))
      }
      return Object.freeze({ result: destination })
    }
    case 'EffectConstruct': {
      const resultType = fn.effectResults.get(
        instanceText(
          expression.target,
          expression.typeArguments.map((argument) => fn.semantic(argument)),
        ),
      )
      if (resultType === undefined) return undefined
      const arguments_: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        arguments_.push(lowered.result)
      }
      const destination = fn.alloc(resultType)
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          typeArguments: Object.freeze(
            expression.typeArguments.map((argument) => fn.semantic(argument)),
          ),
          arguments: Object.freeze(arguments_),
          type: resultType,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'EffectBlock': {
      const type = effectValueType(fn.layout, fn.owner.key, expression)
      if (type === undefined) return undefined
      const captures: Array<{
        readonly source: Mir.LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }> = []
      for (const capture of expression.captures) {
        const source =
          capture.binding === undefined
            ? capture.parameter === undefined
              ? undefined
              : fn.parameterLocals.get(capture.parameter.ordinal)
            : fn.bindingLocals.get(capture.binding.ordinal)
        if (source === undefined) return undefined
        captures.push(Object.freeze({ source, access: capture.access }))
      }
      const destination = fn.alloc(type)
      const runner = runnerId(fn.owner.key, expression.site)
      fn.emit(
        Object.freeze({
          _tag: 'MakeEffect',
          destination,
          runner,
          runnerTypeArguments: fn.owner.key.typeArguments,
          captures: Object.freeze(captures),
          type,
          provenance: authored(expression.span),
        }),
      )
      if (
        !fn.generatedRunners.some(
          (candidate) =>
            instanceText(candidate.owner.key.declaration, candidate.owner.key.typeArguments) ===
              instanceText(fn.owner.key.declaration, fn.owner.key.typeArguments) &&
            candidate.block.site.span.start === expression.site.span.start,
        )
      ) {
        fn.generatedRunners.push(
          Object.freeze({
            owner: fn.owner,
            block: expression,
            type,
          }),
        )
      }
      return Object.freeze({ result: destination })
    }
    case 'EffectTransform':
      return undefined
    case 'Run': {
      const loweredSubject = lowerExpression(fn, expression.subject)
      const effectValueType =
        loweredSubject === undefined ? undefined : fn.localTypes.at(loweredSubject.result.ordinal)
      if (loweredSubject !== undefined && effectValueType?._tag === 'EffectValue') {
        const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
          _tag: 'EffectOutcome',
          type: effectValueType.type,
        })
        const successType = fn.type(expression.type)
        if (
          successType === undefined ||
          successType._tag === 'EffectOutcome' ||
          successType._tag === 'EffectValue'
        )
          return undefined
        const outcome = fn.alloc(outcomeType)
        const destination = fn.alloc(successType)
        const propagationType =
          effectValueType.type.failures.length === 0 || fn.effectOutcome === undefined
            ? undefined
            : fn.type(fn.effectOutcome)
        if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome')
          return undefined
        const tagMappings = effectValueType.type.failures.flatMap((failure, source) => {
          const target = propagationType?.type.failures.findIndex((candidate) =>
            Type.equals(candidate, failure),
          )
          return target === undefined || target < 0
            ? []
            : [Object.freeze({ source: source + 1, target: target + 1 })]
        })
        if (tagMappings.length !== effectValueType.type.failures.length) return undefined
        const propagationShape =
          propagationType === undefined
            ? undefined
            : Layout.callingShape(fn.layout, propagationType.type)
        fn.emit(
          Object.freeze({
            _tag: 'RunEffectValue',
            destination,
            outcome,
            effect: loweredSubject.result,
            runner: runnerId(effectValueType.environment.instance, effectValueType.site),
            runnerTypeArguments: effectValueType.environment.instance.typeArguments,
            outcomeType,
            ...(propagationType === undefined ? {} : { propagationType }),
            tagMappings: Object.freeze(tagMappings),
            propagationLaneCount: propagationShape?.laneCount ?? 0,
            type: successType,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      const recipe =
        expression.subject._tag === 'BindingReference'
          ? fn.effectRecipes.get(expression.subject.binding.ordinal)
          : expression.subject
      const storedTransform =
        expression.subject._tag === 'BindingReference'
          ? fn.effectTransforms.get(expression.subject.binding.ordinal)
          : undefined
      const transform =
        storedTransform?.expression ??
        (expression.subject._tag === 'EffectTransform' ? expression.subject : undefined)
      if (transform !== undefined) {
        const protected_ =
          storedTransform === undefined
            ? lowerExpression(fn, transform.protected)?.result
            : storedTransform.protected
        const callback =
          storedTransform === undefined
            ? lowerExpression(fn, transform.callback)?.result
            : storedTransform.callback
        const protectedType =
          protected_ === undefined ? undefined : fn.localTypes.at(protected_.ordinal)
        const callbackValueType =
          callback === undefined ? undefined : fn.localTypes.at(callback.ordinal)
        if (
          protected_ === undefined ||
          callback === undefined ||
          protectedType?._tag !== 'EffectValue' ||
          callbackValueType?._tag !== 'CallableValue'
        )
          return undefined
        const protectedSuccess = lowerRunEffectValue(
          fn,
          protected_,
          protectedType,
          protectedType.type.success,
          transform.span,
        )
        if (protectedSuccess === undefined) return undefined
        const transformed = lowerCallableLocalApplication(
          fn,
          callback,
          protectedSuccess.result,
          callbackValueType.type,
          transform.span,
        )
        if (transformed === undefined) return undefined
        if (transform.operation === 'Map') return transformed
        const transformedType = fn.localTypes.at(transformed.result.ordinal)
        if (transformedType?._tag !== 'EffectValue') return undefined
        const callbackSuccess = lowerRunEffectValue(
          fn,
          transformed.result,
          transformedType,
          transformedType.type.success,
          transform.span,
        )
        return transform.operation === 'Tap' ? protectedSuccess : callbackSuccess
      }
      if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'AllocatorAllocate') {
        const [layoutExpression] = recipe.arguments
        if (layoutExpression === undefined || fn.effectOutcome === undefined) return undefined
        const loweredLayout = lowerExpression(fn, layoutExpression)
        const type = fn.type(expression.type)
        const propagationType = fn.type(fn.effectOutcome)
        const failureTag = fn.effectOutcome.failures.findIndex((failure) =>
          Type.equals(failure, Type.outOfMemory),
        )
        if (
          loweredLayout === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.allocation) ||
          propagationType?._tag !== 'EffectOutcome' ||
          failureTag < 0
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'Allocate' as const,
            destination,
            layout: loweredLayout.result,
            type,
            failure: Type.outOfMemory,
            propagationType,
            failureTag: failureTag + 1,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      if (recipe?._tag === 'EffectProvide') {
        const provider =
          recipe.provider.binding !== undefined
            ? fn.bindingLocals.get(recipe.provider.binding.ordinal)
            : recipe.provider.parameter !== undefined
              ? fn.parameterLocals.get(recipe.provider.parameter.ordinal)
              : undefined
        if (provider === undefined) return undefined
        const witness = recipe.provider.witness
        if (
          witness._tag === 'SourceConformanceWitness' &&
          witness.operation !== undefined &&
          recipe.protected._tag === 'BuiltinCall' &&
          recipe.protected.operation === 'AllocatorAllocate'
        ) {
          // A source-declared witness dispatches through the ordinary effect-call machinery,
          // exactly as if the source had called the qualified operation and run the result:
          // an exclusive provider loan strictly around the call and run, so provider access
          // ends when the allocation outcome returns.
          const [layoutExpression] = recipe.protected.arguments
          if (layoutExpression === undefined || fn.effectOutcome === undefined) return undefined
          const loweredLayout = lowerExpression(fn, layoutExpression)
          const effectResult = fn.effectResults.get(instanceText(witness.operation, []))
          const successType = fn.type(expression.type)
          const propagationType = fn.type(fn.effectOutcome)
          const propagationShape = Layout.callingShape(fn.layout, fn.effectOutcome)
          const providerType = fn.type(recipe.provider.providerType)
          const referenceType = fn.type(
            Object.freeze({
              _tag: 'ReferenceType' as const,
              access: 'Exclusive' as const,
              target: recipe.provider.providerType,
            }),
          )
          const loan = fn.ownership?.loans.find(
            (candidate) =>
              candidate.origin === 'EffectCapture' &&
              candidate.access === 'Exclusive' &&
              candidate.startSpan.start === recipe.provider.span.start &&
              candidate.startSpan.end === recipe.provider.span.end,
          )
          if (
            loweredLayout === undefined ||
            effectResult === undefined ||
            successType?._tag !== 'Nominal' ||
            propagationType?._tag !== 'EffectOutcome' ||
            propagationShape === undefined ||
            providerType?._tag !== 'Nominal' ||
            referenceType?._tag !== 'Reference' ||
            loan === undefined
          )
            return undefined
          const tagMappings = effectResult.type.failures.flatMap((failure, source) => {
            const target = propagationType.type.failures.findIndex((candidate) =>
              Type.equals(candidate, failure),
            )
            return target < 0 ? [] : [Object.freeze({ source: source + 1, target: target + 1 })]
          })
          if (tagMappings.length !== effectResult.type.failures.length) return undefined
          const reference = fn.alloc(referenceType)
          fn.emit(
            Object.freeze({
              _tag: 'BeginLoan',
              borrow: loan.id,
              destination: reference,
              root: provider,
              sourceType: providerType,
              type: referenceType,
              access: 'Exclusive',
              reborrow: false,
              suspendsParent: false,
              provenance: authored(recipe.provider.span),
            }),
          )
          const effectLocal = fn.alloc(effectResult)
          fn.emit(
            Object.freeze({
              _tag: 'Call',
              destination: effectLocal,
              target: witness.operation,
              typeArguments: Object.freeze([]),
              arguments: Object.freeze([reference, loweredLayout.result]),
              type: effectResult,
              provenance: authored(expression.span),
            }),
          )
          const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
            _tag: 'EffectOutcome',
            type: effectResult.type,
          })
          const outcome = fn.alloc(outcomeType)
          const destination = fn.alloc(successType)
          fn.emit(
            Object.freeze({
              _tag: 'RunEffectValue',
              destination,
              outcome,
              effect: effectLocal,
              runner: runnerId(effectResult.environment.instance, effectResult.site),
              runnerTypeArguments: effectResult.environment.instance.typeArguments,
              outcomeType,
              propagationType,
              tagMappings: Object.freeze(tagMappings),
              propagationLaneCount: propagationShape.laneCount,
              type: successType,
              provenance: authored(expression.span),
            }),
          )
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan',
              borrow: loan.id,
              slice: reference,
              provenance: generated(recipe.provider.span),
            }),
          )
          return Object.freeze({ result: destination })
        }
        return lowerExpression(
          fn,
          Object.freeze({
            ...expression,
            subject: recipe.protected,
          }),
        )
      }
      if (recipe?._tag === 'EffectRetry') {
        const arguments_: Array<Mir.LocalId> = []
        let protectedValue: Mir.LocalId
        let protectedValueType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined
        let protectedTarget: DeclarationIndex.CanonicalId | undefined
        let protectedTypeArguments: ReadonlyArray<Type.Type> = Object.freeze([])
        if (recipe.protected._tag === 'EffectConstruct') {
          for (const argument of recipe.protected.arguments) {
            const lowered = lowerExpression(fn, argument)
            if (lowered === undefined) return undefined
            arguments_.push(lowered.result)
          }
          protectedTarget = recipe.protected.target
          protectedTypeArguments = Object.freeze(
            recipe.protected.typeArguments.map((argument) => fn.semantic(argument)),
          )
          protectedValueType = fn.effectResults.get(
            instanceText(recipe.protected.target, protectedTypeArguments),
          )
          if (protectedValueType === undefined) return undefined
          protectedValue = fn.alloc(protectedValueType)
        } else {
          const loweredProtected = lowerExpression(fn, recipe.protected)
          if (loweredProtected === undefined) return undefined
          const loweredType = fn.localTypes.at(loweredProtected.result.ordinal)
          if (loweredType?._tag !== 'EffectValue') return undefined
          protectedValue = loweredProtected.result
          protectedValueType = loweredType
          protectedTypeArguments = loweredType.environment.instance.typeArguments
        }
        const loweredRetries = lowerExpression(fn, recipe.retries)
        if (loweredRetries === undefined) return undefined
        const protectedType =
          protectedValueType === undefined
            ? undefined
            : Object.freeze({ _tag: 'EffectOutcome' as const, type: protectedValueType.type })
        const successType = fn.type(expression.type)
        if (
          protectedValueType === undefined ||
          protectedType === undefined ||
          successType === undefined ||
          successType._tag === 'EffectOutcome' ||
          successType._tag === 'EffectValue' ||
          successType._tag === 'EffectBorrow'
        )
          return undefined
        const propagationType =
          protectedValueType.type.failures.length === 0 || fn.effectOutcome === undefined
            ? undefined
            : fn.type(fn.effectOutcome)
        if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome')
          return undefined
        const propagationShape =
          propagationType === undefined
            ? undefined
            : Layout.callingShape(fn.layout, propagationType.type)
        const tagMappings = protectedValueType.type.failures.flatMap((failure, source) => {
          const target = propagationType?.type.failures.findIndex((candidate) =>
            Type.equals(candidate, failure),
          )
          return target === undefined || target < 0
            ? []
            : [Object.freeze({ source: source + 1, target: target + 1 })]
        })
        if (
          protectedValueType.type.failures.length > 0 &&
          (propagationType?._tag !== 'EffectOutcome' ||
            propagationShape === undefined ||
            tagMappings.length !== protectedValueType.type.failures.length)
        )
          return undefined
        const protectedOutcome = fn.alloc(protectedType)
        const destination = fn.alloc(successType)
        fn.emit(
          Object.freeze({
            _tag: 'RetryEffect',
            destination,
            protectedValue,
            protectedOutcome,
            ...(protectedTarget === undefined ? {} : { protectedTarget }),
            protectedTypeArguments,
            protectedArguments: Object.freeze(arguments_),
            protectedValueType,
            protectedRunner: runnerId(
              protectedValueType.environment.instance,
              protectedValueType.site,
            ),
            protectedType,
            retries: loweredRetries.result,
            ...(propagationType === undefined ? {} : { propagationType }),
            tagMappings: Object.freeze(tagMappings),
            propagationLaneCount: propagationShape?.laneCount ?? 0,
            type: successType,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      if (recipe?._tag === 'EffectCatch') {
        const loweredHandler = lowerExpression(fn, recipe.handler)
        const handlerCallableType =
          loweredHandler === undefined ? undefined : fn.localTypes.at(loweredHandler.result.ordinal)
        const handlerTarget =
          handlerCallableType?._tag === 'CallableValue' &&
          handlerCallableType.target._tag === 'DeclarationCallableTarget'
            ? handlerCallableType.target.declaration
            : undefined
        const handlerTypeArguments =
          handlerCallableType?._tag === 'CallableValue'
            ? (handlerCallableType.environment?.callable.typeArguments ?? Object.freeze([]))
            : Object.freeze([])
        if (
          recipe.type.failures.length !== 0 ||
          recipe.handlerEffect.failures.length !== 0 ||
          loweredHandler === undefined ||
          handlerCallableType?._tag !== 'CallableValue' ||
          handlerTarget === undefined ||
          recipe.protected._tag !== 'EffectConstruct'
        )
          return undefined
        const protectedRecipe = recipe.protected
        const arguments_: Array<Mir.LocalId> = []
        for (const argument of protectedRecipe.arguments) {
          const lowered = lowerExpression(fn, argument)
          if (lowered === undefined) return undefined
          arguments_.push(lowered.result)
        }
        const protectedTypeArguments = Object.freeze(
          protectedRecipe.typeArguments.map((argument) => fn.semantic(argument)),
        )
        const protectedValueType = fn.effectResults.get(
          instanceText(protectedRecipe.target, protectedTypeArguments),
        )
        const handlerValueType = fn.effectResults.get(
          instanceText(handlerTarget, handlerTypeArguments),
        )
        const protectedType =
          protectedValueType === undefined
            ? undefined
            : Object.freeze({ _tag: 'EffectOutcome' as const, type: protectedValueType.type })
        const handlerType =
          handlerValueType === undefined
            ? undefined
            : Object.freeze({ _tag: 'EffectOutcome' as const, type: handlerValueType.type })
        const successType = fn.type(expression.type)
        const handledTag = protectedRecipe.type.failures.findIndex((failure) =>
          Type.equals(failure, recipe.handled),
        )
        const handledShape = Layout.callingShape(fn.layout, fn.semantic(recipe.handled))
        if (
          protectedType?._tag !== 'EffectOutcome' ||
          handlerType?._tag !== 'EffectOutcome' ||
          protectedValueType === undefined ||
          handlerValueType === undefined ||
          successType === undefined ||
          successType._tag === 'EffectOutcome' ||
          successType._tag === 'EffectValue' ||
          handledTag < 0 ||
          handledShape === undefined
        )
          return undefined
        const protectedOutcome = fn.alloc(protectedType)
        const protectedValue = fn.alloc(protectedValueType)
        const handlerOutcome = fn.alloc(handlerType)
        const handlerValue = fn.alloc(handlerValueType)
        const destination = fn.alloc(successType)
        fn.emit(
          Object.freeze({
            _tag: 'CatchEffect',
            destination,
            protectedValue,
            protectedOutcome,
            handlerValue,
            handlerOutcome,
            protectedTarget: protectedRecipe.target,
            protectedTypeArguments,
            protectedArguments: Object.freeze(arguments_),
            protectedValueType,
            protectedRunner: runnerId(
              protectedValueType.environment.instance,
              protectedValueType.site,
            ),
            protectedType,
            handledTag: handledTag + 1,
            handledLaneCount: handledShape.laneCount,
            handlerCallable: loweredHandler.result,
            handlerCallableType,
            handlerTarget,
            handlerTypeArguments,
            handlerValueType,
            handlerRunner: runnerId(handlerValueType.environment.instance, handlerValueType.site),
            handlerType,
            type: successType,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      if (recipe?._tag !== 'EffectConstruct') return undefined
      const arguments_: Array<Mir.LocalId> = []
      for (const argument of recipe.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        arguments_.push(lowered.result)
      }
      const outcomeType = fn.type(recipe.type)
      const successType = fn.type(expression.type)
      if (
        outcomeType?._tag !== 'EffectOutcome' ||
        successType === undefined ||
        successType._tag === 'EffectOutcome'
      )
        return undefined
      const outcome = fn.alloc(outcomeType)
      const destination = fn.alloc(successType)
      if (recipe.type.failures.length > 0) {
        const propagationType =
          fn.effectOutcome === undefined ? undefined : fn.type(fn.effectOutcome)
        const propagationShape =
          fn.effectOutcome === undefined
            ? undefined
            : Layout.callingShape(fn.layout, fn.effectOutcome)
        if (propagationType?._tag !== 'EffectOutcome' || propagationShape === undefined)
          return undefined
        const tagMappings = recipe.type.failures.flatMap((failure, source) => {
          const target = propagationType.type.failures.findIndex((candidate) =>
            Type.equals(candidate, failure),
          )
          return target < 0 ? [] : [Object.freeze({ source: source + 1, target: target + 1 })]
        })
        if (tagMappings.length !== recipe.type.failures.length) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'RunEffect',
            destination,
            outcome,
            target: recipe.target,
            typeArguments: Object.freeze(
              recipe.typeArguments.map((argument) => fn.semantic(argument)),
            ),
            arguments: Object.freeze(arguments_),
            outcomeType,
            propagationType,
            tagMappings: Object.freeze(tagMappings),
            propagationLaneCount: propagationShape.laneCount,
            type: successType,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination: outcome,
          target: recipe.target,
          typeArguments: Object.freeze(
            recipe.typeArguments.map((argument) => fn.semantic(argument)),
          ),
          arguments: Object.freeze(arguments_),
          type: outcomeType,
          provenance: authored(expression.span),
        }),
      )
      fn.emit(
        Object.freeze({
          _tag: 'UnpackEffectSuccess',
          destination,
          source: outcome,
          type: successType,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'UnionConvert': {
      const source = lowerExpression(fn, expression.source)
      const sourceType = fn.type(expression.sourceType)
      const targetType = fn.type(expression.target)
      const sourceShape = Layout.callingShape(fn.layout, fn.semantic(expression.sourceType))
      const targetShape = Layout.callingShape(fn.layout, fn.semantic(expression.target))
      if (
        source === undefined ||
        sourceShape === undefined ||
        targetShape === undefined ||
        (sourceType?._tag !== 'Nominal' && sourceType?._tag !== 'Union') ||
        targetType?._tag !== 'Union'
      ) {
        return undefined
      }
      const destination = fn.alloc(targetType)
      fn.emit(
        Object.freeze({
          _tag: 'ConvertUnion',
          destination,
          source: source.result,
          sourceType,
          targetType,
          conversion: expression.conversion,
          mappings: expression.mappings,
          sourceShape,
          targetShape,
          access: expression.access,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'Match': {
      if (expression.scrutinee._tag === 'Unavailable') return undefined
      const scrutinee = lowerExpression(fn, expression.scrutinee)
      const scrutineeType = fn.type(expression.scrutinee.type)
      const resultType = fn.type(expression.type)
      const scrutineeShape = Layout.callingShape(fn.layout, fn.semantic(expression.scrutinee.type))
      const resultShape = Layout.callingShape(fn.layout, fn.semantic(expression.type))
      if (
        scrutinee === undefined ||
        (scrutineeType?._tag !== 'Nominal' && scrutineeType?._tag !== 'Union') ||
        resultType === undefined ||
        scrutineeShape === undefined ||
        resultShape === undefined
      ) {
        return undefined
      }
      const ownership = fn.ownership?.matches.find(
        (candidate) =>
          candidate.id.span.start === expression.id.span.start &&
          candidate.id.span.end === expression.id.span.end,
      )
      const specializeMember = (member: Type.Nominal): Type.Nominal | undefined => {
        const specialized = fn.semantic(member)
        return Type.isNominal(specialized) ? specialized : undefined
      }
      const members = expression.members.flatMap((member) => {
        const specialized = specializeMember(member)
        return specialized === undefined ? [] : [specialized]
      })
      if (members.length !== expression.members.length) return undefined
      const arms: Array<Mir.MatchArm> = []
      for (const arm of expression.arms) {
        if (!arm.reachable) continue
        const member = arm.member === undefined ? undefined : specializeMember(arm.member)
        if (arm.member !== undefined && member === undefined) return undefined
        const before = arm.before.flatMap((candidate) => {
          const specialized = specializeMember(candidate)
          return specialized === undefined ? [] : [specialized]
        })
        const after = arm.after.flatMap((candidate) => {
          const specialized = specializeMember(candidate)
          return specialized === undefined ? [] : [specialized]
        })
        if (before.length !== arm.before.length || after.length !== arm.after.length)
          return undefined
        const bindings: Array<Mir.MatchBinding> = []
        for (const binding of arm.bindings) {
          const type = fn.type(binding.type)
          if (type === undefined) return undefined
          const destination = fn.alloc(type)
          fn.patternLocals.set(patternKey(binding.id), destination)
          bindings.push(
            Object.freeze({
              id: binding.id,
              destination,
              path: binding.path,
              type,
              access: binding.access,
              provenance: authored(binding.span),
            }),
          )
        }
        const guardExpression = arm.guard
        const guard =
          guardExpression === undefined
            ? undefined
            : (() => {
                const [lowered, operations] = fn.capture(() => lowerExpression(fn, guardExpression))
                return lowered === undefined
                  ? undefined
                  : Object.freeze({ operations, result: lowered.result })
              })()
        if (guardExpression !== undefined && guard === undefined) return undefined
        const [selectedResult, selectedOperations] = fn.capture(() =>
          lowerExpression(fn, arm.result),
        )
        if (selectedResult === undefined) return undefined
        const ownedArm = ownership?.arms.find(
          (candidate) => candidate.id.ordinal === arm.id.ordinal,
        )
        arms.push(
          Object.freeze({
            id: arm.id,
            ...(member === undefined ? {} : { member }),
            universal: arm.universal,
            before: Object.freeze(before),
            after: Object.freeze(after),
            bindings: Object.freeze(bindings),
            ...(guard === undefined ? {} : { guard }),
            selected: Object.freeze({
              access: expression.access,
              operations: selectedOperations,
              result: selectedResult.result,
              cleanup: Object.freeze(
                (ownedArm?.cleanup ?? []).map((release) =>
                  Object.freeze({
                    path: release.path,
                    cleanup: specializedCleanup(fn, release.cleanup),
                  }),
                ),
              ),
              endBorrow: expression.access === 'Shared' || expression.access === 'Exclusive',
            }),
            provenance: authored(arm.span),
          }),
        )
        for (const binding of arm.bindings) fn.patternLocals.delete(patternKey(binding.id))
      }
      const destination = fn.alloc(resultType)
      const decisions = members.map((member) =>
        Object.freeze({
          member,
          candidates: Object.freeze(
            arms
              .filter(
                (arm) =>
                  arm.universal || (arm.member !== undefined && Type.equals(arm.member, member)),
              )
              .map((arm) => arm.id),
          ),
        }),
      )
      fn.emit(
        Object.freeze({
          _tag: 'Match',
          id: expression.id,
          destination,
          scrutinee: scrutinee.result,
          scrutineeType,
          scrutineeShape,
          access: expression.access,
          members: Object.freeze(members),
          decisions: Object.freeze(decisions),
          arms: Object.freeze(arms),
          type: resultType,
          resultShape,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'Construct': {
      const type = fn.type(expression.type)
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
      const type = fn.type(expression.type)
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
    case 'SliceBorrow': {
      const root =
        expression.root._tag === 'BindingSliceRoot'
          ? fn.bindingLocals.get(expression.root.binding.ordinal)
          : local(expression.root.parameter.ordinal)
      const sourceType = fn.type(expression.source)
      const type = fn.type(expression.type)
      if (
        root === undefined ||
        (sourceType?._tag !== 'FixedArray' && sourceType?._tag !== 'Slice') ||
        type?._tag !== 'Slice'
      ) {
        return undefined
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow: expression.borrow,
          destination,
          root,
          sourceType,
          type,
          access: expression.access,
          reborrow: expression.reborrow,
          suspendsParent: expression.suspendsParent,
          provenance: authored(expression.span),
        }),
      )
      fn.loanLocals.set(borrowKey(expression.borrow), destination)
      return Object.freeze({ result: destination })
    }
    case 'ValueBorrow': {
      const root =
        expression.root._tag === 'BindingSliceRoot'
          ? fn.bindingLocals.get(expression.root.binding.ordinal)
          : local(expression.root.parameter.ordinal)
      const sourceType = fn.type(expression.source)
      const type = fn.type(expression.type)
      if (root === undefined || sourceType?._tag !== 'Nominal' || type?._tag !== 'Reference') {
        return undefined
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow: expression.borrow,
          destination,
          root,
          sourceType,
          type,
          access: expression.access,
          reborrow: false,
          suspendsParent: false,
          provenance: authored(expression.span),
        }),
      )
      fn.loanLocals.set(borrowKey(expression.borrow), destination)
      return Object.freeze({ result: destination })
    }
    case 'SliceLength': {
      const slice = lowerExpression(fn, expression.slice)
      if (slice === undefined || fn.localTypes.at(slice.result.ordinal)?._tag !== 'Slice') {
        return undefined
      }
      const destination = fn.alloc(i32)
      fn.emit(
        Object.freeze({
          _tag: 'SliceLength',
          destination,
          slice: slice.result,
          type: i32,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'SliceIndexPlace': {
      return lowerPlace(fn, expression)
    }
    case 'Call': {
      const argumentLocals: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      const type = fn.type(expression.type)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          typeArguments: Object.freeze(
            expression.typeArguments.map((argument) => fn.semantic(argument)),
          ),
          arguments: Object.freeze(argumentLocals),
          type,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      for (const borrow of expression.loanEnds) {
        const slice = fn.loanLocals.get(borrowKey(borrow))
        if (slice === undefined) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'EndLoan',
            borrow,
            slice,
            provenance: generated(expression.span),
          }),
        )
        fn.loanLocals.delete(borrowKey(borrow))
      }
      return { result: destination }
    }
    case 'BuiltinCall': {
      const argumentLocals: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      const finishBuiltin = (result: Mir.LocalId): { readonly result: Mir.LocalId } => {
        const slot = argumentLocals.at(0)
        const inherited =
          expression.operation === 'SlotWrite' ||
          expression.operation === 'SlotTake' ||
          expression.operation === 'SlotDrop'
            ? slot === undefined
              ? []
              : (fn.slotLoans.get(slot.ordinal) ?? [])
            : []
        const endings = new Map(
          [...expression.loanEnds, ...inherited].map((borrow) => [borrowKey(borrow), borrow]),
        )
        for (const borrow of endings.values()) {
          const loan = fn.loanLocals.get(borrowKey(borrow))
          if (loan === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan' as const,
              borrow,
              slice: loan,
              provenance: generated(expression.span),
            }),
          )
          fn.loanLocals.delete(borrowKey(borrow))
        }
        if (slot !== undefined && inherited.length > 0) fn.slotLoans.delete(slot.ordinal)
        return Object.freeze({ result })
      }
      if (expression.operation === 'LayoutOf') {
        const [element] = expression.typeArguments
        const elementLayout = element === undefined ? undefined : Layout.entry(fn.layout, element)
        const layoutEntry = Layout.entry(fn.layout, Type.layout)
        const type = fn.type(Type.layout)
        if (
          elementLayout === undefined ||
          layoutEntry?.representation._tag !== 'Aggregate' ||
          type?._tag !== 'Nominal'
        )
          return undefined
        const fields: Array<{
          readonly field: DeclarationIndex.FieldId
          readonly value: Mir.LocalId
        }> = []
        for (const field of layoutEntry.representation.fields) {
          const value = fn.alloc(usize)
          fn.emit(
            Object.freeze({
              _tag: 'Literal' as const,
              destination: value,
              type: usize,
              value: BigInt(field.name === 'bytes' ? elementLayout.size : elementLayout.alignment),
              provenance: generated(expression.span),
            }),
          )
          fields.push(Object.freeze({ field: field.id, value }))
        }
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'Construct' as const,
            destination,
            type,
            fields: Object.freeze(fields),
            provenance: generated(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'LayoutMake' || expression.operation === 'LayoutRepeat') {
        const [left, right] = argumentLocals
        const type = fn.type(expression.type)
        if (left === undefined || right === undefined || type?._tag !== 'Union') return undefined
        const destination = fn.alloc(type)
        fn.emit(
          expression.operation === 'LayoutMake'
            ? Object.freeze({
                _tag: 'ValidateLayout' as const,
                destination,
                bytes: left,
                alignment: right,
                type,
                provenance: authored(expression.span),
              })
            : Object.freeze({
                _tag: 'RepeatLayout' as const,
                destination,
                layout: left,
                count: right,
                type,
                provenance: authored(expression.span),
              }),
        )
        return { result: destination }
      }
      if (expression.operation === 'SystemAllocatorMake') {
        const type = fn.type(expression.type)
        if (type?._tag !== 'Nominal' || !Type.equals(type.type, Type.systemAllocator))
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'Construct' as const,
            destination,
            type,
            fields: Object.freeze([]),
            provenance: authored(expression.span),
          }),
        )
        return { result: destination }
      }
      if (expression.operation === 'AllocatorAllocate') return undefined
      if (expression.operation === 'RawBufferFrom') {
        const [allocation, count] = argumentLocals
        const type = fn.type(expression.type)
        const element = Type.isRawBuffer(expression.type)
          ? expression.type.arguments.at(0)
          : undefined
        const elementLayout = element === undefined ? undefined : Layout.entry(fn.layout, element)
        if (
          allocation === undefined ||
          count === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.isRawBuffer(type.type) ||
          element === undefined ||
          elementLayout === undefined
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferFrom' as const,
            destination,
            allocation,
            count,
            element,
            stride:
              Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
            elementAlignment: elementLayout.alignment,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferCount') {
        const [buffer] = argumentLocals
        if (buffer === undefined) return undefined
        const destination = fn.alloc(usize)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferCount' as const,
            destination,
            buffer,
            type: usize,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferSlot') {
        const [buffer, index] = argumentLocals
        const type = fn.type(expression.type)
        const element = Type.isSlot(expression.type) ? expression.type.arguments.at(0) : undefined
        if (
          buffer === undefined ||
          index === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.isSlot(type.type) ||
          element === undefined
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferSlot' as const,
            destination,
            buffer,
            index,
            element,
            type,
            provenance: authored(expression.span),
          }),
        )
        fn.slotLoans.set(destination.ordinal, expression.heldLoans)
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotWrite') {
        const [slot, value] = argumentLocals
        const slotArgument = expression.arguments.at(0)
        const slotType = slotArgument?._tag === 'Unavailable' ? undefined : slotArgument?.type
        const slotElement =
          slotType !== undefined && Type.isSlot(slotType) ? slotType.arguments.at(0) : undefined
        const type = fn.type(expression.type)
        if (
          slot === undefined ||
          value === undefined ||
          slotElement === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotWrite' as const,
            destination,
            slot,
            value,
            element: slotElement,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotTake') {
        const [slot] = argumentLocals
        const type = fn.type(expression.type)
        if (slot === undefined || type === undefined) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotTake' as const,
            destination,
            slot,
            element: expression.type,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotDrop') {
        const [slot] = argumentLocals
        const slotArgument = expression.arguments.at(0)
        const slotType = slotArgument?._tag === 'Unavailable' ? undefined : slotArgument?.type
        const element =
          slotType !== undefined && Type.isSlot(slotType) ? slotType.arguments.at(0) : undefined
        const type = fn.type(expression.type)
        if (
          slot === undefined ||
          element === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotDrop' as const,
            destination,
            slot,
            element,
            cleanup: concreteCleanup(fn, element),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'UnitMake') {
        const type = fn.type(expression.type)
        if (type?._tag !== 'Nominal' || !Type.equals(type.type, Type.unit)) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'Construct' as const,
            destination,
            type,
            fields: Object.freeze([]),
            provenance: authored(expression.span),
          }),
        )
        return { result: destination }
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
      if (!Mir.isBinaryOperator(expression.operation)) return undefined
      const [left, right] = argumentLocals
      if (left === undefined || right === undefined) return undefined
      const type = fn.type(expression.type)
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
  readonly scopeEnds: ReadonlyMap<string, Ownership.ExitPlan>
  readonly armEnds: ReadonlyMap<string, Ownership.ExitPlan>
  readonly loopFallthroughs: ReadonlyMap<number, Ownership.ExitPlan>
  readonly transfers: ReadonlyMap<string, Ownership.ExitPlan>
}

const indexExits = (plan: Ownership.FunctionOwnership | undefined): ExitIndex => {
  const returns = new Map<string, Ownership.ExitPlan>()
  const scopeEnds = new Map<string, Ownership.ExitPlan>()
  const armEnds = new Map<string, Ownership.ExitPlan>()
  const loopFallthroughs = new Map<number, Ownership.ExitPlan>()
  const transfers = new Map<string, Ownership.ExitPlan>()
  for (const exit of plan?.exits ?? []) {
    switch (exit.kind) {
      case 'Return':
        returns.set(spanKey(exit.span), exit)
        break
      case 'ScopeEnd':
        scopeEnds.set(spanKey(exit.span), exit)
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
  return { returns, scopeEnds, armEnds, loopFallthroughs, transfers }
}

const concreteCleanup = (
  fn: FunctionLowering,
  type: Type.Type,
  seen = new Set<string>(),
): Ownership.CleanupPlan => {
  if (Type.isBuiltin(type) || Type.isNever(type) || Type.isParameter(type)) {
    return Object.freeze({ _tag: 'NoCleanup', type })
  }
  if (Type.isSlice(type) || Type.isReference(type))
    return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isEffect(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.equals(type, Type.allocation))
    return Object.freeze({
      _tag: 'AllocationCleanup',
      type: Type.allocation,
      ticket: 'ActiveReclaimTicket',
    })
  if (Type.isRawBuffer(type))
    return Object.freeze({
      _tag: 'RawBufferCleanup',
      type,
      allocation: Object.freeze({
        _tag: 'AllocationCleanup',
        type: Type.allocation,
        ticket: 'ActiveReclaimTicket',
      }),
    })
  if (Type.isFixedArray(type)) {
    return Object.freeze({
      _tag: 'ArrayCleanup',
      type,
      length: type.length,
      element: concreteCleanup(fn, type.element, seen),
    })
  }
  if (Type.isUnion(type)) {
    return Object.freeze({
      _tag: 'UnionCleanup',
      type,
      cases: Object.freeze(
        type.members.map((member, ordinal) =>
          Object.freeze({ member, ordinal, cleanup: concreteCleanup(fn, member, seen) }),
        ),
      ),
    })
  }
  if (Type.isCallable(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  const key = Type.key(type)
  if (seen.has(key)) return Object.freeze({ _tag: 'NoCleanup', type })
  const entry = Layout.entry(fn.layout, type)
  if (entry?.representation._tag !== 'Aggregate') {
    return Object.freeze({ _tag: 'NoCleanup', type })
  }
  const next = new Set(seen).add(key)
  return Object.freeze({
    _tag: 'StructCleanup',
    type,
    fields: Object.freeze(
      entry.representation.fields.map((field) =>
        Object.freeze({ field: field.id, cleanup: concreteCleanup(fn, field.type, next) }),
      ),
    ),
  })
}

const specializedCleanup = (
  fn: FunctionLowering,
  cleanup: Ownership.CleanupPlan,
): Ownership.CleanupPlan =>
  Ownership.specializeCleanup(cleanup, fn.substitution, (type) => concreteCleanup(fn, type))

const cleanupForLocal = (
  fn: FunctionLowering,
  cleanup: Ownership.CleanupPlan,
  localType: Mir.Type,
): Ownership.CleanupPlan => {
  const specialized = specializedCleanup(fn, cleanup)
  if (specialized._tag !== 'CallableCleanup' || localType._tag !== 'CallableValue') {
    return specialized
  }
  const fields = localType.environment?.fields ?? []
  return Object.freeze({
    _tag: 'CallableCleanup',
    type: localType.type,
    site: specialized.site,
    slots: Object.freeze(
      specialized.slots.flatMap((slot) => {
        const field = fields.find((candidate) => candidate.ordinal === slot.ordinal)
        return field === undefined
          ? []
          : [Object.freeze({ ordinal: slot.ordinal, cleanup: concreteCleanup(fn, field.type) })]
      }),
    ),
  })
}

const callableLocalCleanup = (
  fn: FunctionLowering,
  localType: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>,
): Ownership.CleanupPlan => {
  const environment = localType.environment
  if (environment === undefined || localType.site === undefined)
    return Object.freeze({ _tag: 'NoCleanup', type: localType.type })
  return Object.freeze({
    _tag: 'CallableCleanup',
    type: localType.type,
    site: localType.site,
    slots: Object.freeze(
      [...environment.fields]
        .reverse()
        .flatMap((field) =>
          field.access === 'Take' && !copyType(field.type)
            ? [Object.freeze({ ordinal: field.ordinal, cleanup: concreteCleanup(fn, field.type) })]
            : [],
        ),
    ),
  })
}

const emitReleases = (fn: FunctionLowering, exit: Ownership.ExitPlan | undefined): void => {
  for (const borrow of exit?.loanEnds ?? []) {
    const slice = fn.loanLocals.get(borrowKey(borrow))
    if (slice === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'EndLoan',
        borrow,
        slice,
        provenance: generated(exit?.span ?? borrow.callSpan),
      }),
    )
    fn.loanLocals.delete(borrowKey(borrow))
  }
  for (const release of exit?.releases ?? []) {
    if (release.binding.site._tag !== 'Let') continue
    const transform = fn.effectTransforms.get(release.binding.site.binding.ordinal)
    if (transform === undefined) continue
    for (const borrow of transform.loanEnds) {
      const slice = fn.loanLocals.get(borrowKey(borrow))
      if (slice === undefined) continue
      fn.emit(
        Object.freeze({
          _tag: 'EndLoan',
          borrow,
          slice,
          provenance: generated(exit?.span ?? borrow.callSpan),
        }),
      )
      fn.loanLocals.delete(borrowKey(borrow))
    }
  }
  for (const release of exit?.releases ?? []) {
    const site = release.binding.site
    const dropped =
      site._tag === 'Parameter'
        ? local(site.parameter.ordinal)
        : fn.bindingLocals.get(site.binding.ordinal)
    if (dropped === undefined) continue
    const localType = fn.localTypes.at(dropped.ordinal)
    if (localType === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'Drop',
        local: dropped,
        cleanup: cleanupForLocal(fn, release.cleanup, localType),
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

const lowerBorrowedWriteSelectors = (
  fn: FunctionLowering,
  selectors: ReadonlyArray<Hir.BorrowedWriteSelector>,
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
    const index = lowerExpression(fn, selector.index)
    if (index === undefined) return undefined
    lowered.push(
      Object.freeze({
        _tag: 'SliceElementSelector',
        index: index.result,
        access: selector.slice.access,
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
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
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
    if (statement.initializer._tag === 'EffectTransform') {
      const transform = statement.initializer
      const [[protected_, callback], operations] = fn.capture(
        () =>
          [
            lowerExpression(fn, transform.protected),
            lowerExpression(fn, transform.callback),
          ] as const,
      )
      if (protected_ === undefined || callback === undefined) {
        fn.publish(
          Object.freeze({
            _tag: 'OperationRegion',
            id,
            ...ownerFields(ownerLoop),
            operations,
            outcome: Object.freeze({
              _tag: 'Trap',
              reason: 'unavailable Effect transform',
              provenance: generated(statement.span),
            }),
          }),
        )
        return id
      }
      fn.effectTransforms.set(
        statement.binding.ordinal,
        Object.freeze({
          expression: transform,
          protected: protected_.result,
          callback: callback.result,
          loanEnds: Object.freeze(
            transform.callback._tag === 'CallableSection'
              ? transform.callback.captures.flatMap((capture) =>
                  capture.value._tag === 'SliceBorrow' ? [capture.value.borrow] : [],
                )
              : [],
          ),
        }),
      )
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
    if (
      (statement.initializer._tag === 'EffectConstruct' &&
        fn.effectResults.get(
          instanceText(
            statement.initializer.target,
            statement.initializer.typeArguments.map((argument) => fn.semantic(argument)),
          ),
        ) === undefined) ||
      statement.initializer._tag === 'EffectCatch' ||
      statement.initializer._tag === 'EffectRetry' ||
      statement.initializer._tag === 'EffectProvide' ||
      statement.initializer._tag === 'EffectProvideWith' ||
      (statement.initializer._tag === 'BuiltinCall' && Type.isEffect(statement.initializer.type))
    ) {
      fn.effectRecipes.set(statement.binding.ordinal, statement.initializer)
      const following = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
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
      const heldLoans = fn.slotLoans.get(lowered.result.ordinal)
      if (heldLoans !== undefined) {
        fn.slotLoans.delete(lowered.result.ordinal)
        fn.slotLoans.set(destination.ordinal, heldLoans)
      }
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
    const place = statement.place
    const root =
      place._tag === 'BorrowedWritePlace'
        ? local(place.root.ordinal)
        : fn.bindingLocals.get(place.root.ordinal)
    const rootType = root === undefined ? undefined : fn.localTypes.at(root.ordinal)
    const type = fn.type(place.type)
    const [written, operations] = fn.capture(() => {
      if (root === undefined || rootType === undefined || type === undefined) return false
      const selectors =
        place._tag === 'BorrowedWritePlace'
          ? lowerBorrowedWriteSelectors(fn, place.selectors)
          : lowerWriteSelectors(fn, place.selectors)
      if (selectors === undefined) return false
      fn.emit(
        Object.freeze({
          _tag: 'CheckPlace',
          root,
          selectors,
          type,
          provenance: authored(place.span),
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
          replacement: copyType(fn.semantic(statement.place.type)) ? 'Copy' : 'Owned',
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

  if (statement._tag === 'Drop') {
    const droppedExpression =
      statement.expression._tag === 'Move' ? statement.expression.subject : statement.expression
    const transformedBindingOrdinal =
      droppedExpression._tag === 'BindingReference' ? droppedExpression.binding.ordinal : undefined
    const transformedBinding =
      transformedBindingOrdinal === undefined
        ? undefined
        : fn.effectTransforms.get(transformedBindingOrdinal)
    if (transformedBinding !== undefined && transformedBindingOrdinal !== undefined) {
      const callbackType = fn.localTypes.at(transformedBinding.callback.ordinal)
      if (callbackType?._tag !== 'CallableValue') return undefined
      const releases: Array<Extract<Mir.Operation, { readonly _tag: 'Drop' | 'EndLoan' }>> = []
      for (const borrow of transformedBinding.loanEnds) {
        const slice = fn.loanLocals.get(borrowKey(borrow))
        if (slice === undefined) continue
        releases.push(
          Object.freeze({
            _tag: 'EndLoan',
            borrow,
            slice,
            provenance: generated(statement.span),
          }),
        )
        fn.loanLocals.delete(borrowKey(borrow))
      }
      releases.push(
        Object.freeze({
          _tag: 'Drop',
          local: transformedBinding.callback,
          cleanup: callableLocalCleanup(fn, callbackType),
          provenance: authored(statement.span),
        }),
      )
      fn.effectTransforms.delete(transformedBindingOrdinal)
      const cleanup = fn.reserve()
      const following = fn.reserve()
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
          releases: Object.freeze(releases),
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
    const [lowered, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (lowered === undefined) return undefined
    const localType = fn.localTypes.at(lowered.result.ordinal)
    if (localType === undefined) return undefined
    const droppedBinding =
      statement.expression._tag === 'BindingReference'
        ? statement.expression.binding.ordinal
        : undefined
    const bindingFact =
      droppedBinding !== undefined
        ? fn.ownership?.bindings.find(
            (binding) =>
              binding.site._tag === 'Let' && binding.site.binding.ordinal === droppedBinding,
          )
        : undefined
    const loanReleases = (fn.ownership?.loans ?? []).flatMap((loan) => {
      if (loan.endSpan.start !== statement.span.start || loan.endSpan.end !== statement.span.end) {
        return []
      }
      const slice = fn.loanLocals.get(borrowKey(loan.id))
      if (slice === undefined) return []
      fn.loanLocals.delete(borrowKey(loan.id))
      return [
        Object.freeze({
          _tag: 'EndLoan' as const,
          borrow: loan.id,
          slice,
          provenance: generated(statement.span),
        }),
      ]
    })
    const cleanup = fn.reserve()
    const following = fn.reserve()
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
        releases: Object.freeze([
          ...loanReleases,
          Object.freeze({
            _tag: 'Drop',
            local: lowered.result,
            cleanup:
              bindingFact === undefined
                ? concreteCleanup(fn, Mir.semanticType(localType))
                : cleanupForLocal(fn, bindingFact.cleanup, localType),
            provenance: authored(statement.span),
          }),
        ]),
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

  if (statement._tag === 'Unsafe') {
    const body = fn.reserve()
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations: Object.freeze([]),
        outcome: Object.freeze({
          _tag: 'Forward',
          target: body,
          provenance: authored(statement.span),
        }),
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
        statement.statements,
        exits,
        ownerLoop,
        forward,
        body,
        exits.scopeEnds.get(spanKey(statement.span)),
      ) === undefined ||
      lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
    )
      return undefined
    return id
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
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome,
        }),
      )
    }
    return id
  }

  if (statement._tag === 'Fail') {
    const [failedValue, operations] = fn.capture(() => {
      const failed = lowerExpression(fn, statement.expression)
      const outcomeType = fn.effectOutcome === undefined ? undefined : fn.type(fn.effectOutcome)
      const specializedFailure = fn.semantic(statement.failure)
      if (
        failed === undefined ||
        outcomeType?._tag !== 'EffectOutcome' ||
        !Type.isNominal(specializedFailure)
      )
        return undefined
      const tag = outcomeType.type.failures.findIndex((failure) =>
        Type.equals(failure, specializedFailure),
      )
      if (tag < 0) return undefined
      const destination = fn.alloc(outcomeType)
      fn.emit(
        Object.freeze({
          _tag: 'PackEffectOutcome',
          destination,
          source: failed.result,
          tag: tag + 1,
          type: outcomeType,
          provenance: authored(statement.span),
        }),
      )
      return destination
    })
    if (failedValue === undefined) return undefined
    const failureOutcome: Mir.Outcome = Object.freeze({
      _tag: 'Return',
      value: failedValue,
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
          outcome: failureOutcome,
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
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome: failureOutcome,
        }),
      )
    }
    return id
  }

  const [returnedValue, operations] = fn.capture(() => {
    const returned = lowerExpression(fn, statement.expression)
    if (returned === undefined) return undefined
    if (fn.effectOutcome === undefined) return returned.result
    const outcomeType = fn.type(fn.effectOutcome)
    if (outcomeType?._tag !== 'EffectOutcome') return undefined
    const destination = fn.alloc(outcomeType)
    fn.emit(
      Object.freeze({
        _tag: 'PackEffectOutcome',
        destination,
        source: returned.result,
        tag: 0,
        type: outcomeType,
        provenance: authored(statement.span),
      }),
    )
    return destination
  })
  if (returnedValue === undefined) return undefined
  const returnOutcome: Mir.Outcome = Object.freeze({
    _tag: 'Return',
    value: returnedValue,
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
          releases.flatMap((operation) =>
            operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
          ),
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
    instance: instance.key,
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

const returnedEffectBlock = (
  fn: Hir.HirFunction,
): Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }> | undefined => {
  const terminal = fn.statements.at(-1)
  if (terminal?._tag !== 'Return') return undefined
  const returned = terminal.expression
  if (returned._tag === 'EffectBlock') return returned
  if (returned._tag !== 'BindingReference') return undefined
  const binding = fn.statements.find(
    (statement): statement is Extract<Hir.Statement, { readonly _tag: 'Bind' }> =>
      statement._tag === 'Bind' && statement.binding.ordinal === returned.binding.ordinal,
  )
  return binding?.initializer._tag === 'EffectBlock' ? binding.initializer : undefined
}

const lowerInstance = (
  instance: Instances.Instance,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
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
          const lowered = mirType(type, instance.substitution)
          return lowered === undefined ? [] : [lowered]
        })
      : Array.from({ length: fn.declaration.parameterCount }, () => i32)
  const effectOutcome =
    contract._tag === 'Contract' && contract.functionKind === 'Effect'
      ? Type.effect(
          Type.substitute(contract.result, instance.substitution),
          (contract.failures ?? []).flatMap((failure) => {
            const specialized = Type.substitute(failure, instance.substitution)
            return Type.isNominal(specialized) ? [specialized] : []
          }),
          'Shared',
          (contract.requirements ?? []).flatMap((requirement) => {
            const capability = Type.substitute(requirement.capability, instance.substitution)
            return Type.isNominal(capability) ? [Object.freeze({ ...requirement, capability })] : []
          }),
        )
      : undefined
  const returnedBlock = contract._tag === 'Contract' ? returnedEffectBlock(fn) : undefined
  const hiddenEffectResult =
    returnedBlock === undefined ? undefined : effectValueType(layout, instance.key, returnedBlock)
  const resultType =
    hiddenEffectResult ??
    (contract._tag === 'Contract'
      ? mirType(effectOutcome ?? contract.result, instance.substitution)
      : i32)
  if (resultType === undefined) {
    return trapFunction(instance, 'unavailable contract type', bodySpan(fn))
  }

  const lowering = new FunctionLowering(
    layout,
    parameterTypes,
    plan,
    instance.substitution,
    effectOutcome,
    instance,
    effectResults,
    generatedRunners,
  )
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
    instance: instance.key,
    parameterCount: fn.declaration.parameterCount,
    localTypes: Object.freeze([...lowering.localTypes]),
    result: resultType,
    entry,
    regions: Object.freeze(
      lowering.regions.flatMap((region) => (region === undefined ? [] : [region])),
    ),
  })
}

const lowerEffectRunner = (
  spec: GeneratedEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
): Mir.MirFunction | undefined => {
  const { owner, block, type } = spec
  const id = runnerId(owner.key, block.site)
  const instance: Instances.InstanceKey = Object.freeze({
    _tag: 'InstanceKey',
    declaration: id,
    typeArguments: owner.key.typeArguments,
    contractRow: Object.freeze([
      ...owner.key.contractRow,
      `effect-site:${block.site.function.sourceId}:${block.site.function.ordinal}:${block.site.span.start}`,
    ]),
  })
  const parameterTypes = type.environment.fields.flatMap((field) => {
    const lowered = mirType(field.type)
    if (lowered === undefined) return []
    return [
      field.access === 'Shared' || field.access === 'Exclusive'
        ? Object.freeze({ _tag: 'EffectBorrow' as const, type: field.type, access: field.access })
        : lowered,
    ]
  })
  if (parameterTypes.length !== block.captures.length) return undefined
  const plan = planFor(ownership, owner.function)
  const lowering = new FunctionLowering(
    layout,
    parameterTypes,
    plan,
    owner.substitution,
    type.type,
    owner,
    effectResults,
    generatedRunners,
  )
  lowering.parameterLocals.clear()
  block.captures.forEach((capture, ordinal) => {
    const captureLocal = local(ordinal)
    if (capture.binding !== undefined)
      lowering.bindingLocals.set(capture.binding.ordinal, captureLocal)
    if (capture.parameter !== undefined)
      lowering.parameterLocals.set(capture.parameter.ordinal, captureLocal)
  })
  const terminal: Mir.Outcome = Object.freeze({
    _tag: 'Trap',
    reason: 'effect body fell through without return',
    provenance: generated(block.span),
  })
  const entry = lowerSequence(lowering, block.statements, indexExits(plan), undefined, terminal)
  if (entry === undefined || lowering.regions.some((region) => region === undefined))
    return undefined
  const result: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: type.type,
  })
  return Object.freeze({
    _tag: 'MirFunction',
    id,
    instance,
    parameterCount: parameterTypes.length,
    localTypes: Object.freeze([...lowering.localTypes]),
    result,
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
): Mir.Module => {
  const effectResults = new Map<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>()
  for (const instance of discovery.instances) {
    const block = returnedEffectBlock(instance.function)
    if (block === undefined) continue
    const type = effectValueType(layout, instance.key, block)
    if (type !== undefined)
      effectResults.set(instanceText(instance.key.declaration, instance.key.typeArguments), type)
  }
  const generatedRunners: Array<GeneratedEffectRunner> = []
  const functions = discovery.instances.map((instance) =>
    lowerInstance(
      instance,
      ownership.get(instance.key.declaration.module),
      layout,
      effectResults,
      generatedRunners,
    ),
  )
  for (let ordinal = 0; ordinal < generatedRunners.length; ordinal += 1) {
    const generated = generatedRunners.at(ordinal)
    if (generated === undefined) continue
    const runner = lowerEffectRunner(
      generated,
      ownership.get(generated.owner.key.declaration.module),
      layout,
      effectResults,
      generatedRunners,
    )
    if (runner !== undefined) functions.push(runner)
  }
  return Object.freeze({
    _tag: 'MirModule',
    module: discovery.rootModule,
    layout,
    functions: Object.freeze(functions),
  })
}
