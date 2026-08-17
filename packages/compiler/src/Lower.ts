import * as CallableFieldRealization from './CallableFieldRealization.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import type * as Intrinsic from './Intrinsic.js'
import * as Layout from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as Ownership from './Ownership.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceSpan from './SourceSpan.js'
import * as Specialization from './Specialization.js'
import * as TargetConstant from './TargetConstant.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/**
 * Lowering preserves source control as canonical acyclic regions. Repetition is represented by a
 * loop region plus lexical repeat/exit outcomes; backend-private CFGs are derived later.
 */

const i32: Extract<Mir.Type, { readonly _tag: 'i32' }> = Object.freeze({ _tag: 'i32' })
const usize: Extract<Mir.Type, { readonly _tag: 'usize' }> = Object.freeze({ _tag: 'usize' })
const bool: Extract<Mir.Type, { readonly _tag: 'bool' }> = Object.freeze({ _tag: 'bool' })
const character: Extract<Mir.Type, { readonly _tag: 'char' }> = Object.freeze({ _tag: 'char' })

const isOsOperation = (
  operation: Hir.BuiltinOperation,
): operation is Extract<Hir.BuiltinOperation, `Os${string}`> => operation.startsWith('Os')

const mirType = (
  type: Type.Type,
  substitution: Type.Substitution = new Map(),
): Mir.Type | undefined => {
  const specialized = Type.substitute(type, substitution)
  if (!Type.isRuntimeConcrete(specialized)) return undefined
  return typeof specialized === 'string'
    ? Type.isBuiltin(specialized)
      ? Object.freeze({ _tag: specialized })
      : Type.isString(specialized)
        ? Object.freeze({ _tag: 'String', type: specialized })
        : Type.isNever(specialized)
          ? Object.freeze({ _tag: 'Bottom', type: specialized })
          : undefined
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

interface ProvidedRequirement {
  readonly capability: Type.Nominal
  readonly providerType: Type.Nominal
  readonly witness: DeclarationIndex.ConformanceWitness
  readonly role: string
  readonly requirementAccess: Type.Requirement['access']
  readonly access: 'Shared' | 'Exclusive' | 'Take'
  readonly local?: Mir.LocalId
}

const specializeProvider = (
  fn: FunctionLowering,
  provider: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
): ProvidedRequirement | undefined => {
  const proof = Instances.requirementSelection(fn.owner, provider)
  if (proof === undefined) return undefined
  const selected = proof.selected
  const capability = selected.capability
  const providerType = proof.provider
  if (capability === undefined || !Type.isNominal(capability) || !Type.isNominal(providerType))
    return undefined
  const witness = provider.witness ?? DeclarationIndex.witness(fn.index, providerType, capability)
  if (witness === undefined) return undefined
  return Object.freeze({
    capability,
    providerType,
    witness,
    role: selected.role,
    requirementAccess: selected.access,
    access: provider.selectionAccess,
  })
}

class FunctionLowering {
  readonly regions: Array<Mir.Region | undefined> = []
  readonly localTypes: Array<Mir.Type> = []
  readonly bindingLocals = new Map<number, Mir.LocalId>()
  readonly parameterLocals = new Map<number, Mir.LocalId>()
  readonly effectRecipes = new Map<number, Hir.Expression>()
  readonly callableRecipes = new Map<number, Hir.Expression>()
  readonly effectLoanEnds = new Map<number, ReadonlyArray<Hir.BorrowId>>()
  readonly realizedRecipeBorrows = new Set<string>()
  readonly issuedBorrowKeys: Set<string>
  readonly patternLocals = new Map<string, Mir.LocalId>()
  readonly loanLocals = new Map<string, Mir.LocalId>()
  readonly loanIds = new Map<string, Hir.BorrowId>()
  readonly loanParents = new Map<string, string>()
  readonly slotLoans = new Map<number, ReadonlyArray<Hir.BorrowId>>()
  readonly callableDefinitions = new Map<
    number,
    Extract<Mir.Operation, { readonly _tag: 'MakeCallable' }>
  >()
  private operations: Array<Mir.Operation> = []
  private syntheticBorrowOrdinal = 0
  private replayBorrowSubstitution: Map<string, Hir.BorrowId> | undefined

  constructor(
    readonly layout: Layout.Plan,
    readonly index: DeclarationIndex.Index,
    parameterTypes: ReadonlyArray<Mir.Type>,
    readonly ownership: Ownership.FunctionOwnership | undefined,
    readonly substitution: Type.Substitution,
    readonly effectOutcome: Type.Effect | undefined,
    readonly owner: Instances.Instance,
    readonly instances: ReadonlyArray<Instances.Instance>,
    readonly calls: ReadonlyArray<Instances.CallInstance>,
    readonly effectResults: ReadonlyMap<
      string,
      Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
    >,
    readonly generatedRunners: Array<GeneratedEffectRunner>,
    readonly opaqueRealizations: OpaqueRealization.Catalog,
    readonly providedRequirements: ReadonlyArray<ProvidedRequirement> = Object.freeze([]),
  ) {
    this.issuedBorrowKeys = new Set((ownership?.loans ?? []).map((loan) => borrowKey(loan.id)))
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

  freshSyntheticBorrow(span: SourceSpan.SourceSpan): Hir.BorrowId {
    while (true) {
      const borrow: Hir.BorrowId = Object.freeze({
        _tag: 'BorrowId',
        function: this.owner.function.declaration.id,
        callSpan: span,
        ordinal: this.syntheticBorrowOrdinal,
      })
      this.syntheticBorrowOrdinal += 1
      const key = borrowKey(borrow)
      if (this.issuedBorrowKeys.has(key)) continue
      this.issuedBorrowKeys.add(key)
      return borrow
    }
  }

  withRecipeReplay<A>(body: () => A): A {
    if (this.replayBorrowSubstitution !== undefined) return body()
    this.replayBorrowSubstitution = new Map()
    try {
      return body()
    } finally {
      this.replayBorrowSubstitution = undefined
    }
  }

  beginRecipeBorrow(authored: Hir.BorrowId): Hir.BorrowId {
    if (this.replayBorrowSubstitution === undefined) return authored
    const key = borrowKey(authored)
    const existing = this.replayBorrowSubstitution.get(key)
    if (existing !== undefined) return existing
    const realized = this.realizedRecipeBorrows.has(key)
      ? this.freshSyntheticBorrow(authored.callSpan)
      : authored
    this.issuedBorrowKeys.add(borrowKey(realized))
    this.realizedRecipeBorrows.add(key)
    this.replayBorrowSubstitution.set(key, realized)
    return realized
  }

  recipeBorrow(authored: Hir.BorrowId): Hir.BorrowId {
    return this.replayBorrowSubstitution?.get(borrowKey(authored)) ?? authored
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
    if (operation._tag === 'BeginLoan') {
      const key = borrowKey(operation.borrow)
      const parent = [...this.loanLocals.entries()].find(
        ([, slice]) => slice.ordinal === operation.root.ordinal,
      )
      if (parent !== undefined) this.loanParents.set(key, parent[0])
      this.loanIds.set(key, operation.borrow)
    } else if (operation._tag === 'EndLoan')
      this.loanIds.set(borrowKey(operation.borrow), operation.borrow)
    if (operation._tag === 'MakeCallable')
      this.callableDefinitions.set(operation.destination.ordinal, operation)
    if (operation._tag === 'Move') {
      const definition = this.callableDefinitions.get(operation.source.ordinal)
      if (definition !== undefined)
        this.callableDefinitions.set(operation.destination.ordinal, definition)
    }
  }

  type(type: Type.Type): Mir.Type | undefined {
    const specialized = Type.substitute(type, this.substitution)
    return (
      storedCallableValueType(this.layout, specialized) ??
      storedEffectValueType(this.layout, specialized) ??
      representedValueType(this.layout, this.opaqueRealizations, type, this.substitution) ??
      mirType(specialized)
    )
  }

  semantic(type: Type.Type): Type.Type {
    return Type.substitute(type, this.substitution)
  }

  semanticArgument(argument: Type.GenericArgument): Type.GenericArgument {
    return Type.substituteGenericArgument(argument, this.substitution)
  }

  call(span: SourceSpan.SourceSpan): Instances.CallInstance | undefined {
    const exact = this.calls.find(
      (call) =>
        Instances.keyText(call.owner) === Instances.keyText(this.owner.key) &&
        call.span.sourceId === span.sourceId &&
        call.span.start === span.start &&
        call.span.end === span.end,
    )
    if (exact !== undefined || this.providedRequirements.length === 0) return exact
    // A provided generated runner reuses the base Effect body's HIR and call sites but has a
    // private synthesized InstanceKey that discovery never owns. Resolve its calls through the
    // source owner whose body is being specialized; provider dispatch is represented separately.
    const sameSite = this.calls.filter(
      (call) =>
        call.span.sourceId === span.sourceId &&
        call.span.start === span.start &&
        call.span.end === span.end,
    )
    return sameSite.length === 1 ? sameSite.at(0) : undefined
  }
}

interface DelayedEffectState {
  readonly recipes: ReadonlyMap<number, Hir.Expression>
  readonly loanEnds: ReadonlyMap<number, ReadonlyArray<Hir.BorrowId>>
  readonly loanLocals: ReadonlyMap<string, Mir.LocalId>
}

const delayedEffectState = (fn: FunctionLowering): DelayedEffectState =>
  Object.freeze({
    recipes: new Map(fn.effectRecipes),
    loanEnds: new Map(fn.effectLoanEnds),
    loanLocals: new Map(fn.loanLocals),
  })

const restoreDelayedEffectState = (fn: FunctionLowering, state: DelayedEffectState): void => {
  fn.effectRecipes.clear()
  for (const [binding, recipe] of state.recipes) fn.effectRecipes.set(binding, recipe)
  fn.effectLoanEnds.clear()
  for (const [binding, loans] of state.loanEnds) fn.effectLoanEnds.set(binding, loans)
  fn.loanLocals.clear()
  for (const [borrow, local_] of state.loanLocals) fn.loanLocals.set(borrow, local_)
}

const directForwardedRequirementBinding = (
  instance: Instances.Instance,
): Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }> | undefined => {
  const returned = instance.function.statements.at(-1)
  if (instance.function.statements.length !== 1 || returned?._tag !== 'Return') return undefined
  const block = returned.expression
  if (block._tag !== 'EffectBlock' || block.statements.length !== 2) return undefined
  const binding = block.statements.at(0)
  const completed = block.statements.at(1)
  if (
    binding?._tag !== 'Bind' ||
    binding.initializer._tag !== 'EffectBindRequirement' ||
    binding.initializer.protected._tag !== 'Move' ||
    binding.initializer.protected.subject._tag !== 'ParameterReference' ||
    binding.initializer.protected.subject.parameter.ordinal !== 0 ||
    binding.initializer.provider.parameter?.ordinal !== 1 ||
    completed?._tag !== 'Return' ||
    completed.expression._tag !== 'Run' ||
    completed.expression.subject._tag !== 'BindingReference' ||
    completed.expression.subject.binding.ordinal !== binding.binding.ordinal
  )
    return undefined
  return binding.initializer
}

const forwardedRequirementBinding = (
  instance: Instances.Instance,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  resolving: ReadonlySet<string> = new Set(),
):
  | {
      readonly instance: Instances.Instance
      readonly binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>
    }
  | undefined => {
  const direct = directForwardedRequirementBinding(instance)
  if (direct !== undefined) return Object.freeze({ instance, binding: direct })
  const key_ = Instances.keyText(instance.key)
  if (resolving.has(key_)) return undefined
  const returned = instance.function.statements.at(-1)
  const block = returned?._tag === 'Return' ? returned.expression : undefined
  const completed = block?._tag === 'EffectBlock' ? block.statements.at(-1) : undefined
  const run = completed?._tag === 'Return' ? completed.expression : undefined
  const nested = run?._tag === 'Run' ? run.subject : undefined
  if (nested?._tag !== 'EffectConstruct' && nested?._tag !== 'Call') return undefined
  const call = calls.find(
    (candidate) =>
      Instances.keyText(candidate.owner) === key_ &&
      candidate.span.sourceId === nested.span.sourceId &&
      candidate.span.start === nested.span.start &&
      candidate.span.end === nested.span.end,
  )
  const candidates = instances
    .filter(
      (candidate) =>
        candidate.key.declaration.module === nested.target.module &&
        candidate.key.declaration.name === nested.target.name &&
        (call === undefined || Instances.keyText(candidate.key) === Instances.keyText(call.target)),
    )
    .sort(
      (left, right) =>
        right.key.typeArguments.filter(Type.isHiddenIdentityArgument).length -
        left.key.typeArguments.filter(Type.isHiddenIdentityArgument).length,
    )
  const searchable =
    candidates.length === 0 && call !== undefined
      ? instances.filter(
          (candidate) =>
            candidate.key.declaration.module === nested.target.module &&
            candidate.key.declaration.name === nested.target.name,
        )
      : candidates
  return searchable
    .map((candidate) =>
      forwardedRequirementBinding(candidate, instances, calls, new Set(resolving).add(key_)),
    )
    .find((binding) => binding !== undefined)
}

const forwardedCallableParameter = (
  fn: FunctionLowering,
  instance: Instances.Instance,
  resolving: ReadonlySet<string> = new Set(),
): number | undefined => {
  const key_ = Instances.keyText(instance.key)
  if (resolving.has(key_)) return undefined
  const leading = instance.function.statements.slice(0, -1)
  const returned = instance.function.statements.at(-1)
  if (
    instance.function.contract._tag !== 'Contract' ||
    instance.function.contract.parameters.length !== 1 ||
    returned?._tag !== 'Return' ||
    leading.some((statement) => statement._tag !== 'Bind')
  )
    return undefined
  const next = new Set(resolving).add(key_)
  const forwardedBindings = new Set<number>()
  const expression = (
    current: Hir.Expression,
    resolvingBindings: ReadonlySet<number> = new Set(),
  ): number | undefined => {
    if (current._tag === 'Move') return expression(current.subject, resolvingBindings)
    if (current._tag === 'ParameterReference') return current.parameter.ordinal
    if (current._tag === 'BindingReference') {
      const ordinal = current.binding.ordinal
      if (resolvingBindings.has(ordinal)) return undefined
      const binding = leading.find(
        (statement) => statement._tag === 'Bind' && statement.binding.ordinal === ordinal,
      )
      if (binding?._tag !== 'Bind') return undefined
      forwardedBindings.add(ordinal)
      return expression(binding.initializer, new Set(resolvingBindings).add(ordinal))
    }
    if (current._tag !== 'Call') return undefined
    const target = fn.instances.find(
      (candidate) =>
        candidate.key.declaration.module === current.target.module &&
        candidate.key.declaration.name === current.target.name,
    )
    const forwarded =
      target === undefined ? undefined : forwardedCallableParameter(fn, target, next)
    const argument = forwarded === undefined ? undefined : current.arguments.at(forwarded)
    return argument === undefined ? undefined : expression(argument, resolvingBindings)
  }
  const forwarded = expression(returned.expression)
  return forwarded === undefined ||
    leading.some(
      (statement) => statement._tag !== 'Bind' || !forwardedBindings.has(statement.binding.ordinal),
    )
    ? undefined
    : forwarded
}

const staticallyForwardedCallableRecipe = (
  fn: FunctionLowering,
  current: Hir.Expression,
  owner: Hir.HirFunction,
  arguments_: ReadonlyArray<Hir.Expression> = Object.freeze([]),
  resolving: ReadonlySet<string> = new Set(),
): Extract<Hir.Expression, { readonly _tag: 'CallableSection' }> | undefined => {
  if (current._tag === 'CallableSection') return current
  if (current._tag === 'Move')
    return staticallyForwardedCallableRecipe(fn, current.subject, owner, arguments_, resolving)
  if (current._tag === 'ParameterReference') {
    const argument = arguments_.at(current.parameter.ordinal)
    return argument === undefined
      ? undefined
      : staticallyForwardedCallableRecipe(fn, argument, owner, arguments_, resolving)
  }
  if (current._tag === 'BindingReference') {
    const localBinding = owner.statements.find(
      (statement) =>
        statement._tag === 'Bind' && statement.binding.ordinal === current.binding.ordinal,
    )
    const stored =
      owner === fn.owner.function
        ? fn.callableRecipes.get(current.binding.ordinal)
        : localBinding?._tag === 'Bind'
          ? localBinding.initializer
          : undefined
    return stored === undefined
      ? undefined
      : staticallyForwardedCallableRecipe(fn, stored, owner, arguments_, resolving)
  }
  if (current._tag !== 'Call') return undefined
  const identity = `${current.target.module}\u0000${current.target.name}`
  if (resolving.has(identity)) return undefined
  const target = fn.instances.find(
    (instance) =>
      instance.key.declaration.module === current.target.module &&
      instance.key.declaration.name === current.target.name,
  )
  const forwarded =
    target === undefined ? undefined : forwardedCallableParameter(fn, target, resolving)
  const argument = forwarded === undefined ? undefined : current.arguments.at(forwarded)
  return argument === undefined
    ? undefined
    : staticallyForwardedCallableRecipe(
        fn,
        argument,
        owner,
        arguments_,
        new Set(resolving).add(identity),
      )
}

const callableRecipe = (
  fn: FunctionLowering,
  expression: Hir.Expression,
  resolving: ReadonlySet<number> = new Set(),
): Extract<Hir.Expression, { readonly _tag: 'CallableSection' }> | undefined => {
  if (expression._tag === 'CallableSection') return expression
  if (expression._tag === 'Move') return callableRecipe(fn, expression.subject, resolving)
  if (expression._tag === 'BindingReference') {
    if (resolving.has(expression.binding.ordinal)) return undefined
    const stored = fn.callableRecipes.get(expression.binding.ordinal)
    return stored === undefined
      ? undefined
      : callableRecipe(fn, stored, new Set(resolving).add(expression.binding.ordinal))
  }
  if (expression._tag === 'Call') {
    const forwarded = staticallyForwardedCallableRecipe(fn, expression, fn.owner.function)
    if (forwarded !== undefined) return forwarded
  }
  if (expression._tag !== 'CallableApply' && expression._tag !== 'Call') return undefined
  const call = fn.call(expression.span)
  const target =
    (call === undefined
      ? undefined
      : fn.instances.find(
          (instance) => Instances.keyText(instance.key) === Instances.keyText(call.target),
        )) ??
    (expression._tag === 'Call'
      ? fn.instances.find(
          (instance) =>
            instance.key.declaration.module === expression.target.module &&
            instance.key.declaration.name === expression.target.name,
        )
      : undefined)
  const forwarded = target === undefined ? undefined : forwardedCallableParameter(fn, target)
  const argument = forwarded === undefined ? undefined : expression.arguments.at(forwarded)
  return argument === undefined ? undefined : callableRecipe(fn, argument, resolving)
}

const inlineForwardedEffectResult = (
  fn: FunctionLowering,
  expression: Hir.Expression,
): Extract<Hir.Expression, { readonly _tag: 'EffectResult' }> | undefined => {
  if (expression._tag !== 'EffectConstruct') return undefined
  const call = fn.call(expression.span)
  const parameter = call?.effectResultParameter
  const protected_ = parameter === undefined ? undefined : expression.arguments.at(parameter)
  const recipeBinding =
    protected_?._tag === 'BindingReference'
      ? protected_.binding.ordinal
      : protected_?._tag === 'Move' && protected_.subject._tag === 'BindingReference'
        ? protected_.subject.binding.ordinal
        : undefined
  const type = fn.semantic(expression.type)
  return protected_ === undefined ||
    recipeBinding === undefined ||
    !fn.effectRecipes.has(recipeBinding) ||
    !Type.isEffect(type)
    ? undefined
    : Object.freeze({
        _tag: 'EffectResult',
        protected: protected_,
        type,
        span: expression.span,
      })
}

const effectRecipe = (
  fn: FunctionLowering,
  expression: Hir.Expression,
  resolving: ReadonlySet<number> = new Set(),
): Hir.Expression => {
  if (expression._tag === 'BindingReference') {
    const ordinal = expression.binding.ordinal
    if (resolving.has(ordinal)) return expression
    const stored = fn.effectRecipes.get(ordinal)
    return stored === undefined
      ? expression
      : effectRecipe(fn, stored, new Set(resolving).add(ordinal))
  }
  if (expression._tag === 'Move') {
    const subject = effectRecipe(fn, expression.subject, resolving)
    return subject === expression.subject ? expression : subject
  }
  const forwarded = inlineForwardedEffectResult(fn, expression)
  return forwarded === undefined ? expression : effectRecipe(fn, forwarded, resolving)
}

const movedEffectRecipe = (
  fn: FunctionLowering,
  expression: Hir.Expression,
):
  | {
      readonly source: number
      readonly recipe: Hir.Expression
      readonly loanEnds: ReadonlyArray<Hir.BorrowId>
    }
  | undefined => {
  if (expression._tag !== 'Move' || expression.subject._tag !== 'BindingReference') return undefined
  const source = expression.subject.binding.ordinal
  const recipe = fn.effectRecipes.get(source)
  return recipe === undefined
    ? undefined
    : Object.freeze({
        source,
        recipe,
        loanEnds: fn.effectLoanEnds.get(source) ?? Object.freeze([]),
      })
}

const callableApplicationArgument = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
  ordinal: number,
): Hir.Expression | undefined => {
  const section = callableRecipe(fn, expression.callee)
  if (section === undefined) return expression.arguments.at(ordinal)
  const captured = section.captures.find((capture) => capture.parameterOrdinal === ordinal)
  if (captured !== undefined) return captured.value
  return ordinal === section.omittedParameter ? expression.arguments.at(0) : undefined
}

const inlineForwardedRequirement = (
  fn: FunctionLowering,
  expression: Hir.Expression,
):
  | {
      readonly binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>
      readonly provider: Hir.Expression
      readonly selection: Omit<ProvidedRequirement, 'local'>
    }
  | undefined => {
  if (expression._tag !== 'CallableApply' && expression._tag !== 'EffectConstruct') return undefined
  const call = fn.call(expression.span)
  const section =
    expression._tag === 'CallableApply' ? callableRecipe(fn, expression.callee) : undefined
  const declaration =
    expression._tag === 'EffectConstruct'
      ? expression.target
      : section?.target._tag === 'DeclarationCallableTarget'
        ? section.target.declaration
        : undefined
  const candidates =
    declaration === undefined
      ? []
      : fn.instances.filter(
          (instance) =>
            instance.key.declaration.module === declaration.module &&
            instance.key.declaration.name === declaration.name,
        )
  const inferredArguments = (() => {
    if (expression._tag === 'EffectConstruct')
      return expression.typeArguments.map((argument) => fn.semanticArgument(argument))
    if (section === undefined) return undefined
    const candidate = candidates.at(0)
    if (candidate === undefined) return undefined
    const inferred = new Map([...section.substitution, ...expression.substitution])
    const arguments_ = candidate.function.declaration.typeParameters.flatMap((parameter) => {
      const argument = inferred.get(Type.key(parameter.type))
      return argument === undefined ? [] : [fn.semanticArgument(argument)]
    })
    return arguments_.length === candidate.function.declaration.typeParameters.length
      ? arguments_
      : undefined
  })()
  let target =
    call !== undefined
      ? fn.instances.find(
          (instance) => Instances.keyText(instance.key) === Instances.keyText(call.target),
        )
      : inferredArguments === undefined
        ? undefined
        : (candidates
            .filter((candidate) => {
              const explicit = candidate.key.typeArguments.filter(
                (argument) => !Type.isHiddenIdentityArgument(argument),
              )
              return (
                explicit.length === inferredArguments.length &&
                explicit.every(
                  (argument, ordinal) =>
                    Type.genericArgumentKey(argument) ===
                    Type.genericArgumentKey(inferredArguments.at(ordinal) ?? argument),
                )
              )
            })
            .sort(
              (left, right) =>
                right.key.typeArguments.filter(Type.isHiddenIdentityArgument).length -
                left.key.typeArguments.filter(Type.isHiddenIdentityArgument).length,
            )
            .at(0) ?? (candidates.length === 1 ? candidates.at(0) : undefined))
  let resolved =
    target === undefined ? undefined : forwardedRequirementBinding(target, fn.instances, fn.calls)
  if (resolved !== undefined) target = resolved.instance
  let forwarded = resolved?.binding
  if (target === undefined || forwarded === undefined) {
    const candidate = candidates
      .filter(
        (candidate) => forwardedRequirementBinding(candidate, fn.instances, fn.calls) !== undefined,
      )
      .sort(
        (left, right) =>
          right.key.typeArguments.filter(Type.isHiddenIdentityArgument).length -
          left.key.typeArguments.filter(Type.isHiddenIdentityArgument).length,
      )
      .at(0)
    if (candidate !== undefined) {
      resolved = forwardedRequirementBinding(candidate, fn.instances, fn.calls)
      target = resolved?.instance
      forwarded = resolved?.binding
    }
  }
  if (target === undefined || forwarded === undefined) return undefined
  const protected_ =
    expression._tag === 'CallableApply'
      ? callableApplicationArgument(fn, expression, 0)
      : expression.arguments.at(0)
  const provider =
    expression._tag === 'CallableApply'
      ? callableApplicationArgument(fn, expression, 1)
      : expression.arguments.at(1)
  if (protected_ === undefined || provider === undefined) return undefined
  const borrowedProvider = provider._tag === 'ValueBorrow' ? provider : undefined
  let proof =
    target === undefined || forwarded === undefined
      ? undefined
      : Instances.requirementSelection(target, forwarded.provider)
  if (proof === undefined) {
    for (const candidate of candidates
      .filter((candidate) => candidate !== target)
      .sort(
        (left, right) =>
          right.key.typeArguments.filter(Type.isHiddenIdentityArgument).length -
          left.key.typeArguments.filter(Type.isHiddenIdentityArgument).length,
      )) {
      const candidateResolved = forwardedRequirementBinding(candidate, fn.instances, fn.calls)
      const candidateForwarded = candidateResolved?.binding
      const candidateProof =
        candidateResolved === undefined || candidateForwarded === undefined
          ? undefined
          : Instances.requirementSelection(candidateResolved.instance, candidateForwarded.provider)
      if (
        candidateResolved === undefined ||
        candidateForwarded === undefined ||
        candidateProof === undefined
      )
        continue
      target = candidateResolved.instance
      forwarded = candidateForwarded
      proof = candidateProof
      break
    }
  }
  if (target === undefined || forwarded === undefined || proof === undefined) return undefined
  if (
    (borrowedProvider === undefined && forwarded.provider.selectionAccess !== 'Take') ||
    borrowedProvider?.root._tag === 'PatternSliceRoot'
  )
    return undefined
  const selected = proof.selected
  const capability = selected.capability
  const providerType = proof.provider
  const type = fn.semantic(expression.type)
  if (
    capability === undefined ||
    !Type.isNominal(capability) ||
    !Type.isNominal(providerType) ||
    !Type.isEffect(type)
  )
    return undefined
  const witness = DeclarationIndex.witness(fn.index, providerType, capability)
  if (witness === undefined) return undefined
  return Object.freeze({
    binding: Object.freeze({
      _tag: 'EffectBindRequirement',
      protected: protected_,
      provider: Object.freeze({
        ...(borrowedProvider?.root._tag === 'BindingSliceRoot'
          ? { binding: borrowedProvider.root.binding }
          : borrowedProvider?.root._tag === 'ParameterSliceRoot'
            ? { parameter: borrowedProvider.root.parameter }
            : {}),
        capability,
        selected: Type.requirementRowArgument([selected]).row,
        evidence: forwarded.provider.evidence,
        providerType,
        witness,
        role: selected.role,
        selectionAccess: forwarded.provider.selectionAccess,
        captureAccess: borrowedProvider?.access ?? ('Take' as const),
        span: provider.span,
      }),
      type,
      span: expression.span,
    }),
    provider,
    selection: Object.freeze({
      capability,
      providerType,
      witness,
      role: selected.role,
      requirementAccess: selected.access,
      access: forwarded.provider.selectionAccess,
    }),
  })
}

interface GeneratedBlockEffectRunner {
  readonly _tag: 'BlockEffectRunner'
  readonly id: DeclarationIndex.CanonicalId
  readonly owner: Instances.Instance
  readonly block: Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }>
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly specializationKey: string
  readonly providedRequirements: ReadonlyArray<Omit<ProvidedRequirement, 'local'>>
}

interface GeneratedWitnessEffectRunner {
  readonly _tag: 'WitnessEffectRunner'
  readonly id: DeclarationIndex.CanonicalId
  readonly owner: Instances.Instance
  readonly expression: Extract<
    Hir.Expression,
    { readonly _tag: 'BuiltinCall' | 'BoundOperationCall' }
  >
  readonly target?: DeclarationIndex.InterfaceWitnessTarget
  readonly intrinsic?: Intrinsic.Operation
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly specializationKey: string
  readonly providedRequirements: ReadonlyArray<Omit<ProvidedRequirement, 'local'>>
}

interface GeneratedCatchEffectRunner {
  readonly _tag: 'CatchEffectRunner'
  readonly id: DeclarationIndex.CanonicalId
  readonly owner: Instances.Instance
  readonly expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly protectedType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly handlerType: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>
  readonly specializationKey: string
  readonly providedRequirements: ReadonlyArray<Omit<ProvidedRequirement, 'local'>>
}

type GeneratedEffectRunner =
  | GeneratedBlockEffectRunner
  | GeneratedWitnessEffectRunner
  | GeneratedCatchEffectRunner

const instanceText = (
  declaration: { readonly module: string; readonly name: string },
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): string => Specialization.key({ declaration, typeArguments })

const effectEntryAdapterId = (module: string): DeclarationIndex.CanonicalId =>
  Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module,
    name: '$effect-entry',
  })

const baseRunnerKey = (owner: Instances.InstanceKey, site: Hir.EffectSiteId): string =>
  `${instanceText(owner.declaration, owner.typeArguments)}\u0000${Hir.executableSiteKey(site)}`

const witnessKey = (witness: DeclarationIndex.ConformanceWitness): string =>
  witness._tag === 'SourceConformanceWitness'
    ? `${witness._tag}:${witness.operations
        .map(
          (operation) =>
            `${operation.name}=${instanceText(operation.implementation, witness.typeArguments)}`,
        )
        .join(',')}`
    : `${witness._tag}:${Type.key(witness.provider)}`

const providedContractEntry = (requirement: Omit<ProvidedRequirement, 'local'>): string =>
  `provided:${Type.key(requirement.capability)}@${requirement.role}:${requirement.requirementAccess}:${requirement.access}:${Type.key(requirement.providerType)}:${requirement.witness._tag}`

const providedRunnerKey = (
  type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  requirements: ReadonlyArray<ProvidedRequirement>,
): string =>
  `${baseRunnerKey(type.environment.instance, type.site)}\u0000${requirements
    .map(
      (requirement) =>
        `${Type.key(requirement.capability)}@${requirement.role}:${requirement.access}:${Type.key(requirement.providerType)}:${witnessKey(requirement.witness)}`,
    )
    .join('\u0000')}`

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
      Hir.sameExecutableSite(candidate.site, block.site),
  )
  if (environment?._tag !== 'EffectEnvironment') return undefined
  return Object.freeze({
    _tag: 'EffectValue',
    type: environment.effect,
    site: block.site,
    environment,
  })
}

const witnessEffectValueType = (
  layout: Layout.Plan,
  instance: Instances.InstanceKey,
  site: Hir.EffectSiteId,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  const environment = layout.effectEnvironments.find(
    (candidate) =>
      candidate._tag === 'EffectEnvironment' &&
      instanceText(candidate.instance.declaration, candidate.instance.typeArguments) ===
        instanceText(instance.declaration, instance.typeArguments) &&
      Hir.sameExecutableSite(candidate.site, site),
  )
  return environment?._tag !== 'EffectEnvironment'
    ? undefined
    : Object.freeze({ _tag: 'EffectValue', type: environment.effect, site, environment })
}

const effectValueByIdentity = (
  layout: Layout.Plan,
  identity: string,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  const available = layout.effectEnvironments.filter(
    (
      candidate,
    ): candidate is Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment',
  )
  const environment =
    available.find(
      (candidate) => Instances.effectIdentity(candidate.instance, candidate.site) === identity,
    ) ?? available.find((candidate) => candidate.successEffectIdentity === identity)
  return environment === undefined
    ? undefined
    : Object.freeze({
        _tag: 'EffectValue',
        type: environment.effect,
        site: environment.site,
        environment,
      })
}

const callableValueByIdentity = (
  layout: Layout.Plan,
  identity: Type.CallableIdentityArgument,
  type: Type.Callable,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const target = Hir.callableTargetFromIdentity(identity.target)
  const environment =
    identity.environment === undefined
      ? undefined
      : layout.callableEnvironments.find(
          (
            candidate,
          ): candidate is Extract<
            Layout.CallableEnvironment,
            { readonly _tag: 'CallableEnvironment' }
          > =>
            candidate._tag === 'CallableEnvironment' &&
            CallableFieldRealization.matchesIdentity(identity, candidate.callable),
        )
  if (identity.environment !== undefined && environment === undefined) return undefined
  const specializedType =
    environment === undefined
      ? Object.freeze({ ...type, mode: 'Shared' as const })
      : Object.freeze({ ...environment.callable.type, mode: environment.callable.mode })
  return Object.freeze({
    _tag: 'CallableValue',
    type: specializedType,
    target,
    ...(environment === undefined ? {} : { site: environment.callable.site, environment }),
  })
}

const sameArguments = (
  left: ReadonlyArray<Type.GenericArgument>,
  right: ReadonlyArray<Type.GenericArgument>,
): boolean =>
  left.length === right.length &&
  left.every((argument, ordinal) => {
    const candidate = right.at(ordinal)
    return candidate !== undefined && Type.equalsGenericArgument(argument, candidate)
  })

const representedValueType = (
  layout: Layout.Plan,
  catalog: OpaqueRealization.Catalog,
  type: Type.Type,
  substitution: Type.Substitution,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' | 'EffectValue' }> | undefined => {
  const specialized = Type.substitute(type, substitution)
  if (!Type.isRepresented(specialized)) return undefined
  const representation = specialized.representation.argument
  if (Type.isExactRepresentationArgument(representation)) {
    if (
      Type.isCallable(specialized.contract) &&
      Type.isCallableIdentityArgument(representation.identity)
    )
      return callableValueByIdentity(layout, representation.identity, specialized.contract)
    if (
      Type.isEffect(specialized.contract) &&
      Type.isEffectIdentityArgument(representation.identity)
    )
      return effectValueByIdentity(layout, representation.identity.identity)
    return undefined
  }
  const opaque = representation
  if (!Type.isOpaqueRepresentationArgument(opaque)) return undefined
  const definition = OpaqueRealization.definitionOf(catalog, opaque)
  if (definition === undefined) return undefined
  const realization = definition.realization
  if (realization?._tag !== 'ExactRepresentationArgument') return undefined
  if (
    Type.isCallable(specialized.contract) &&
    Type.isCallableIdentityArgument(realization.identity)
  ) {
    const identity = realization.identity
    if (identity.environment === undefined)
      return callableValueByIdentity(layout, identity, specialized.contract)
    const environment = layout.callableEnvironments.find(
      (
        candidate,
      ): candidate is Extract<
        Layout.CallableEnvironment,
        { readonly _tag: 'CallableEnvironment' }
      > =>
        candidate._tag === 'CallableEnvironment' &&
        candidate.callable.owner.declaration.module === definition.construction.producer.module &&
        candidate.callable.owner.declaration.name === definition.construction.producer.name &&
        sameArguments(
          candidate.callable.owner.typeArguments.filter(
            (argument) => !Type.isHiddenIdentityArgument(argument),
          ),
          definition.construction.arguments,
        ) &&
        definition.construction.site ===
          Type.callableEnvironmentKey(Instances.callableEnvironmentIdentity(candidate.callable)),
    )
    return environment === undefined
      ? undefined
      : callableValueByIdentity(
          layout,
          Type.callableIdentityArgument(
            identity.identity,
            identity.target,
            identity.typeArguments,
            Instances.callableEnvironmentIdentity(environment.callable),
          ),
          specialized.contract,
        )
  }
  if (Type.isEffect(specialized.contract) && Type.isEffectIdentityArgument(realization.identity)) {
    const environment = layout.effectEnvironments.find(
      (
        candidate,
      ): candidate is Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
        candidate._tag === 'EffectEnvironment' &&
        candidate.instance.declaration.module === definition.construction.producer.module &&
        candidate.instance.declaration.name === definition.construction.producer.name &&
        sameArguments(
          candidate.instance.typeArguments.filter(
            (argument) => !Type.isHiddenIdentityArgument(argument),
          ),
          definition.construction.arguments,
        ) &&
        definition.construction.site === Hir.effectRepresentationIdentity(candidate.site),
    )
    return environment === undefined
      ? undefined
      : Object.freeze({
          _tag: 'EffectValue',
          type: environment.effect,
          site: environment.site,
          environment,
        })
  }
  return undefined
}

const storedCallableValueType = (
  layout: Layout.Plan,
  type: Type.Type,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  if (!Type.isRepresented(type) || !Type.isCallable(type.contract)) return undefined
  const representation = Layout.entry(layout, type)?.representation
  if (representation?._tag !== 'CallableEnvironment') return undefined
  const realization = representation.realization
  const environment =
    realization.site === undefined
      ? undefined
      : layout.callableEnvironments.find(
          (
            candidate,
          ): candidate is Extract<
            Layout.CallableEnvironment,
            { readonly _tag: 'CallableEnvironment' }
          > =>
            candidate._tag === 'CallableEnvironment' &&
            CallableFieldRealization.matchesCallable(realization, candidate.callable),
        )
  if (realization.site !== undefined && environment === undefined) return undefined
  return Object.freeze({
    _tag: 'CallableValue',
    type: realization.contract,
    target: Hir.callableTargetFromIdentity(realization.target),
    ...(realization.site === undefined ? {} : { site: realization.site }),
    ...(environment === undefined ? {} : { environment }),
    storage: Object.freeze({
      _tag: 'StoredCallableField',
      type,
      realization,
    }),
  })
}

const storedEffectValueType = (
  layout: Layout.Plan,
  type: Type.Type,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  if (!Type.isRepresented(type) || !Type.isEffect(type.contract)) return undefined
  if (Type.isOpaqueRepresentationArgument(type.representation.argument)) return undefined
  const entry = Layout.entry(layout, type)
  const representation = entry?.representation
  if (entry === undefined || representation?._tag !== 'StoredEffectEnvironment') return undefined
  const realization = representation.realization
  const environment: Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =
    Object.freeze({
      _tag: 'EffectEnvironment',
      instance: realization.runnerInstance,
      site: realization.site,
      effect: realization.contract,
      fields: representation.fields,
      size: entry.size,
      alignment: entry.alignment,
      tailPadding: representation.tailPadding,
    })
  return Object.freeze({
    _tag: 'EffectValue',
    type: realization.contract,
    site: realization.site,
    environment,
    storage: Object.freeze({
      _tag: 'StoredEffectField',
      type,
      realization,
    }),
  })
}

const requirementsFor = (
  available: ReadonlyArray<ProvidedRequirement>,
  effect: Type.Effect,
): ReadonlyArray<ProvidedRequirement> | undefined => {
  const selected = Type.requirementMembers(effect).map((requirement) =>
    available.find(
      (candidate) =>
        candidate.role === requirement.role &&
        Type.equals(candidate.capability, requirement.capability) &&
        (requirement.access === 'Shared' ||
          candidate.access === 'Exclusive' ||
          candidate.access === 'Take'),
    ),
  )
  return selected.every((candidate) => candidate !== undefined)
    ? Object.freeze(
        selected.flatMap((candidate, ordinal) => {
          const requirement = Type.requirementMembers(effect).at(ordinal)
          return candidate === undefined || requirement === undefined
            ? []
            : [Object.freeze({ ...candidate, requirementAccess: requirement.access })]
        }),
      )
    : undefined
}

const ensureProvidedRunner = (
  fn: FunctionLowering,
  type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  requirements: ReadonlyArray<ProvidedRequirement>,
): DeclarationIndex.CanonicalId | undefined => {
  const key = providedRunnerKey(type, requirements)
  const existing = fn.generatedRunners.find((candidate) => candidate.specializationKey === key)
  if (existing !== undefined) return existing.id
  const baseKey = baseRunnerKey(type.environment.instance, type.site)
  const base = fn.generatedRunners.find((candidate) => candidate.specializationKey === baseKey)
  if (base === undefined) return undefined
  const id: DeclarationIndex.CanonicalId = Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: base.id.module,
    name: `${base.id.name}$provided$${fn.generatedRunners.length}`,
  })
  fn.generatedRunners.push(
    Object.freeze({
      ...base,
      id,
      specializationKey: key,
      providedRequirements: Object.freeze(
        requirements.map(({ local: _local, ...requirement }) => Object.freeze(requirement)),
      ),
    }),
  )
  return id
}

const runtimeRequirementArguments = (
  requirements: ReadonlyArray<ProvidedRequirement> | undefined,
): ReadonlyArray<Mir.LocalId> =>
  Object.freeze(
    requirements?.flatMap((requirement) =>
      requirement.witness._tag !== 'SourceConformanceWitness' || requirement.local === undefined
        ? []
        : [requirement.local],
    ) ?? [],
  )

const providerBindings = (
  requirements: ReadonlyArray<ProvidedRequirement> | undefined,
): Extract<Mir.Operation, { readonly _tag: 'RunEffectValue' }>['providers'] =>
  Object.freeze(
    requirements?.map((requirement) =>
      Object.freeze({
        capability: requirement.capability,
        providerType: requirement.providerType,
        witness: requirement.witness,
        role: requirement.role,
        requirementAccess: requirement.requirementAccess,
        access: requirement.access,
        ...(requirement.witness._tag === 'SourceConformanceWitness' &&
        requirement.local !== undefined
          ? { argument: requirement.local }
          : {}),
      }),
    ) ?? [],
  )

const sameSite = (left: Hir.CallableSiteId, right: Hir.CallableSiteId): boolean =>
  Hir.sameExecutableSite(left, right)

const callableValueType = (
  fn: FunctionLowering,
  section: Extract<Hir.Expression, { readonly _tag: 'CallableSection' }>,
  applicationSubstitution: Type.Substitution = new Map(),
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
      (!Type.isRuntimeConcrete(expected) || Type.equals(candidate.callable.type, expected)),
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

const directCallableSectionValueType = (
  fn: FunctionLowering,
  section: Extract<Hir.Expression, { readonly _tag: 'CallableSection' }>,
  applicationSubstitution: Type.Substitution,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const type = Type.substitute(
    Type.substitute(section.type, fn.substitution),
    new Map([...section.substitution, ...applicationSubstitution]),
  )
  return Type.isCallable(type) && Type.isRuntimeConcrete(type)
    ? Object.freeze({ _tag: 'CallableValue', type, target: section.target })
    : undefined
}

const functionItemValueType = (
  fn: FunctionLowering,
  item: Extract<Hir.Expression, { readonly _tag: 'FunctionItem' }>,
  applicationSubstitution: Type.Substitution = new Map(),
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const type = Type.substitute(Type.substitute(item.type, fn.substitution), applicationSubstitution)
  return Type.isCallable(type) && Type.isRuntimeConcrete(type)
    ? Object.freeze({ _tag: 'CallableValue', type, target: item.target })
    : undefined
}

const lowerCatchEffectValue = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
): LoweredExpression | undefined => {
  const protected_ = lowerExpression(fn, expression.protected)
  const protectedType =
    protected_ === undefined ? undefined : fn.localTypes.at(protected_.result.ordinal)
  const handler = lowerExpression(fn, expression.handler)
  const handlerType = handler === undefined ? undefined : fn.localTypes.at(handler.result.ordinal)
  if (
    protected_ === undefined ||
    protectedType?._tag !== 'EffectValue' ||
    handler === undefined ||
    handlerType?._tag !== 'CallableValue'
  )
    return undefined

  const site = Hir.effectCatchSite(
    fn.owner.function.declaration.id,
    fn.owner.key.declaration,
    expression.span,
  )
  const environment = fn.layout.effectEnvironments.find(
    (
      candidate,
    ): candidate is Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment' &&
      Instances.keyText(candidate.instance) === Instances.keyText(fn.owner.key) &&
      Hir.sameExecutableSite(candidate.site, site),
  )
  if (environment === undefined || environment.fields.length !== 2) return undefined
  const type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }> = Object.freeze({
    _tag: 'EffectValue',
    type: environment.effect,
    site,
    environment,
  })
  const runner = Hir.effectRunnerId(fn.owner.key.declaration, site)
  const specializationKey = baseRunnerKey(fn.owner.key, site)
  const destination = fn.alloc(type)
  fn.emit(
    Object.freeze({
      _tag: 'MakeEffect',
      destination,
      runner,
      runnerTypeArguments: fn.owner.key.typeArguments,
      captures: Object.freeze([
        Object.freeze({
          source: protected_.result,
          access: environment.fields[0]?.access ?? 'Take',
        }),
        Object.freeze({ source: handler.result, access: environment.fields[1]?.access ?? 'Take' }),
      ]),
      type,
      provenance: authored(expression.span),
    }),
  )
  if (!fn.generatedRunners.some((candidate) => candidate.specializationKey === specializationKey))
    fn.generatedRunners.push(
      Object.freeze({
        _tag: 'CatchEffectRunner',
        id: runner,
        owner: fn.owner,
        expression,
        type,
        protectedType,
        handlerType,
        specializationKey,
        providedRequirements: Object.freeze([]),
      }),
    )
  return Object.freeze({ result: destination })
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
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
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
    Type.failureMembers(effectType.type).length === 0 || fn.effectOutcome === undefined
      ? undefined
      : fn.type(fn.effectOutcome)
  if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome') return undefined
  const tagMappings = Type.failureMembers(effectType.type).flatMap((failure, sourceOrdinal) => {
    const target =
      propagationType === undefined
        ? undefined
        : Type.failureMembers(propagationType.type).findIndex((candidate) =>
            Type.equals(candidate, failure),
          )
    return target === undefined || target < 0
      ? []
      : [Object.freeze({ source: sourceOrdinal + 1, target: target + 1 })]
  })
  if (tagMappings.length !== Type.failureMembers(effectType.type).length) return undefined
  const propagationShape =
    propagationType === undefined ? undefined : Layout.callingShape(fn.layout, propagationType.type)
  const provided = requirementsFor(availableRequirements, effectType.type)
  const providedRunner =
    provided === undefined || provided.length === 0
      ? undefined
      : ensureProvidedRunner(fn, effectType, provided)
  if (provided !== undefined && provided.length > 0 && providedRunner === undefined)
    return undefined
  const baseRunner =
    effectType.storage?.realization.runner ??
    Hir.effectRunnerId(effectType.environment.instance.declaration, effectType.site)
  const baseRunnerTypeArguments =
    effectType.storage?.realization.runnerArguments ?? effectType.environment.instance.typeArguments
  const failureEnds = propagationLoanEnds(fn, span)
  const releases = propagationReleases(fn, span)
  fn.emit(
    Object.freeze({
      _tag: 'RunEffectValue',
      destination,
      outcome,
      effect,
      runner: providedRunner ?? baseRunner,
      runnerTypeArguments: baseRunnerTypeArguments,
      ...(providedRunner === undefined
        ? {}
        : {
            runnerBase: Object.freeze({
              declaration: baseRunner,
              typeArguments: baseRunnerTypeArguments,
            }),
          }),
      providers: providerBindings(provided),
      arguments: runtimeRequirementArguments(provided),
      outcomeType,
      ...(propagationType === undefined ? {} : { propagationType }),
      tagMappings: Object.freeze(tagMappings),
      propagationLaneCount: propagationShape?.laneCount ?? 0,
      ...(Type.failureMembers(effectType.type).length === 0 || failureEnds.length === 0
        ? {}
        : { failureLoanEnds: failureEnds }),
      ...(propagationType === undefined || releases.length === 0 ? {} : { releases }),
      type: successType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({ result: destination })
}

interface ReifiedEffect {
  readonly result: Mir.LocalId
  readonly resultType: Extract<Mir.Type, { readonly _tag: 'Nominal' }>
  readonly resultField: DeclarationIndex.FieldId
  readonly resultUnion: Type.StructuralUnion
  readonly successType: Type.Nominal
  readonly successField: DeclarationIndex.FieldId
  readonly failureType: Type.Nominal
  readonly failureField: DeclarationIndex.FieldId
  readonly failureValueType: Type.Type
}

const reifyEffectValue = (
  fn: FunctionLowering,
  effect: Mir.LocalId,
  effectType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  result: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): ReifiedEffect | undefined => {
  const provided = requirementsFor(availableRequirements, effectType.type)
  if (provided === undefined) return undefined
  const runner =
    provided.length === 0
      ? (effectType.storage?.realization.runner ??
        Hir.effectRunnerId(effectType.environment.instance.declaration, effectType.site))
      : ensureProvidedRunner(fn, effectType, provided)
  if (runner === undefined) return undefined
  const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: effectType.type,
  })
  const resultType = fn.type(result)
  const failureValueType = Type.failureValue(Type.failureMembers(effectType.type))
  const successType = Type.resultSuccess(effectType.type.success)
  const failureType = Type.resultFailure(failureValueType)
  const resultUnionNormalization = Type.union([successType, failureType])
  const resultUnion =
    resultUnionNormalization._tag === 'Normalized' && Type.isUnion(resultUnionNormalization.type)
      ? resultUnionNormalization.type
      : undefined
  const resultEntry =
    resultType?._tag === 'Nominal' ? Layout.entry(fn.layout, resultType.type) : undefined
  const successEntry = Layout.entry(fn.layout, successType)
  const failureEntry = Layout.entry(fn.layout, failureType)
  const resultField =
    resultEntry?.representation._tag === 'Aggregate'
      ? resultEntry.representation.fields.at(0)?.id
      : undefined
  const successField =
    successEntry?.representation._tag === 'Aggregate'
      ? successEntry.representation.fields.at(0)?.id
      : undefined
  const failureField =
    failureEntry?.representation._tag === 'Aggregate'
      ? failureEntry.representation.fields.at(0)?.id
      : undefined
  const resultShape =
    resultType?._tag === 'Nominal' ? Layout.callingShape(fn.layout, resultType.type) : undefined
  const outcomeShape = Layout.callingShape(fn.layout, effectType.type)
  const failureValueShape = Layout.callingShape(fn.layout, failureValueType)
  const successTag = resultUnion?.members.findIndex((member) => Type.equals(member, successType))
  const failureTag = resultUnion?.members.findIndex((member) => Type.equals(member, failureType))
  if (
    resultType?._tag !== 'Nominal' ||
    resultUnion === undefined ||
    resultField === undefined ||
    successField === undefined ||
    failureField === undefined ||
    resultShape === undefined ||
    outcomeShape === undefined ||
    failureValueShape === undefined ||
    successTag === undefined ||
    successTag < 0 ||
    failureTag === undefined ||
    failureTag < 0
  )
    return undefined
  const outcome = fn.alloc(outcomeType)
  const destination = fn.alloc(resultType)
  fn.emit(
    Object.freeze({
      _tag: 'ReifyEffect' as const,
      destination,
      outcome,
      effect,
      runner,
      runnerTypeArguments:
        effectType.storage?.realization.runnerArguments ??
        effectType.environment.instance.typeArguments,
      arguments: runtimeRequirementArguments(provided),
      outcomeType,
      resultType,
      resultField,
      resultUnion,
      successType,
      successField,
      successTag,
      failureType,
      failureField,
      failureTag,
      failureValueType,
      resultShape,
      outcomeShape,
      failureValueShape,
      type: resultType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({
    result: destination,
    resultType,
    resultField,
    resultUnion,
    successType,
    successField,
    failureType,
    failureField,
    failureValueType,
  })
}

const callableEffectResult = (
  fn: FunctionLowering,
  callable: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  if (callable.target._tag !== 'DeclarationCallableTarget') return undefined
  const typeArguments =
    callable.environment?.callable.typeArguments ??
    callable.storage?.realization.targetArguments ??
    Object.freeze([])
  return fn.effectResults.get(instanceText(callable.target.declaration, typeArguments))
}

const lowerEffectCatch = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
  runSpan: SourceSpan.SourceSpan,
  captured?: {
    readonly protected: Mir.LocalId
    readonly protectedType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
    readonly handler: Mir.LocalId
    readonly handlerType: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>
  },
): LoweredExpression | undefined => {
  if (expression.protected._tag === 'Unavailable') return undefined
  const protected_ =
    captured === undefined
      ? expression.protected._tag === 'ServiceEffectConstruct'
        ? lowerServiceEffectValue(fn, expression.protected, fn.providedRequirements)
        : lowerExpression(fn, expression.protected)
      : Object.freeze({ result: captured.protected })
  const protectedType =
    captured?.protectedType ??
    (protected_ === undefined ? undefined : fn.localTypes.at(protected_.result.ordinal))
  if (protected_ === undefined || protectedType?._tag !== 'EffectValue') return undefined

  // Both operands are formed before the protected Effect starts, matching ordinary call
  // evaluation even though the handler is invoked only on the selected failure path.
  const handler =
    captured === undefined
      ? lowerExpression(fn, expression.handler)
      : Object.freeze({ result: captured.handler })
  const handlerType =
    captured?.handlerType ??
    (handler === undefined ? undefined : fn.localTypes.at(handler.result.ordinal))
  const handlerEffectType =
    handlerType?._tag === 'CallableValue' ? callableEffectResult(fn, handlerType) : undefined
  if (
    handler === undefined ||
    handlerType?._tag !== 'CallableValue' ||
    handlerEffectType === undefined
  )
    return undefined
  const unusedHandlerDrop = (): ReadonlyArray<Mir.DropOperation> => {
    const cleanup = cleanupForLocal(
      fn,
      concreteCleanup(fn, Mir.semanticType(handlerType)),
      handlerType,
    )
    return cleanup._tag === 'NoCleanup'
      ? Object.freeze([])
      : Object.freeze([
          Object.freeze({
            _tag: 'Drop' as const,
            local: handler.result,
            cleanup,
            provenance: generated(runSpan),
          }),
        ])
  }

  const selected = fn.semantic(expression.selected)
  const protectedEffect = fn.semantic(expression.protected.type)
  const resultEffect = fn.semantic(expression.type)
  if (!Type.isNominal(selected) || !Type.isEffect(protectedEffect) || !Type.isEffect(resultEffect))
    return undefined
  const protectedFailures = Type.failureMembers(protectedEffect)
  const selectedOrdinal = protectedFailures.findIndex((failure) => Type.equals(failure, selected))
  if (selectedOrdinal < 0) return undefined

  const reified = reifyEffectValue(
    fn,
    protected_.result,
    protectedType,
    Type.result(protectedEffect.success, Type.failureValue(protectedFailures)),
    expression.span,
  )
  if (reified === undefined) return undefined
  const resultUnionType: Extract<Mir.Type, { readonly _tag: 'Union' }> = Object.freeze({
    _tag: 'Union',
    type: reified.resultUnion,
  })
  const resultUnion = fn.alloc(resultUnionType)
  fn.emit(
    Object.freeze({
      _tag: 'Project' as const,
      destination: resultUnion,
      source: reified.result,
      field: reified.resultField,
      type: resultUnionType,
      provenance: generated(expression.span),
    }),
  )

  const successType = fn.type(resultEffect.success)
  const resultUnionShape = Layout.callingShape(fn.layout, reified.resultUnion)
  const successShape = Layout.callingShape(fn.layout, resultEffect.success)
  const failureValueMir = fn.type(reified.failureValueType)
  const propagationEffect = fn.effectOutcome
  const propagationType = propagationEffect === undefined ? undefined : fn.type(propagationEffect)
  const propagationShape =
    propagationEffect === undefined ? undefined : Layout.callingShape(fn.layout, propagationEffect)
  if (
    successType === undefined ||
    successType._tag === 'EffectOutcome' ||
    resultUnionShape === undefined ||
    successShape === undefined ||
    (failureValueMir?._tag !== 'Nominal' && failureValueMir?._tag !== 'Union') ||
    propagationEffect === undefined ||
    propagationType?._tag !== 'EffectOutcome' ||
    propagationShape === undefined
  )
    return undefined

  const declaration = fn.owner.function.declaration.id
  const outerMatch: Match.MatchId = Object.freeze({
    _tag: 'MatchId',
    function: declaration,
    span: expression.span,
  })
  const successArm: Match.ArmId = Object.freeze({
    _tag: 'MatchArmId',
    match: outerMatch,
    ordinal: 0,
  })
  const failureArm: Match.ArmId = Object.freeze({
    _tag: 'MatchArmId',
    match: outerMatch,
    ordinal: 1,
  })
  const successBinding: Match.BindingId = Object.freeze({
    _tag: 'PatternBindingId',
    arm: successArm,
    ordinal: 0,
  })
  const failureBinding: Match.BindingId = Object.freeze({
    _tag: 'PatternBindingId',
    arm: failureArm,
    ordinal: 0,
  })
  const success = fn.alloc(successType)
  const failure = fn.alloc(failureValueMir)

  const failureMembers =
    failureValueMir._tag === 'Nominal'
      ? Object.freeze([failureValueMir.type])
      : failureValueMir.type.members
  const innerSpan =
    SourceSpan.fromOffsets(
      expression.span.sourceId,
      expression.span.start,
      expression.span.start,
    ) ?? expression.span
  const innerMatch: Match.MatchId = Object.freeze({
    _tag: 'MatchId',
    function: declaration,
    span: innerSpan,
  })
  const innerArms: Array<Mir.MatchArm> = []
  for (const [ordinal, member] of failureMembers.entries()) {
    const armId: Match.ArmId = Object.freeze({
      _tag: 'MatchArmId',
      match: innerMatch,
      ordinal,
    })
    const bindingId: Match.BindingId = Object.freeze({
      _tag: 'PatternBindingId',
      arm: armId,
      ordinal: 0,
    })
    const memberType = fn.type(member)
    if (memberType?._tag !== 'Nominal') return undefined
    const bound = fn.alloc(memberType)
    const [selectedResult, selectedOperations] = fn.capture(() => {
      if (Type.equals(member, selected)) {
        const applied = fn.alloc(handlerEffectType)
        fn.emit(
          Object.freeze({
            _tag: 'ApplyCallable' as const,
            destination: applied,
            callable: handler.result,
            typeArguments:
              handlerType.environment?.callable.typeArguments ??
              handlerType.storage?.realization.targetArguments ??
              Object.freeze([]),
            captures: Object.freeze([]),
            arguments: Object.freeze([bound]),
            callableType: handlerType.type,
            access: handlerType.type.mode,
            evaluation: 'CalleeThenArguments' as const,
            realization: 'Environment' as const,
            type: handlerEffectType,
            provenance: generated(expression.span),
          }),
        )
        return lowerRunEffectValue(fn, applied, handlerEffectType, resultEffect.success, runSpan)
      }
      const target = Type.failureMembers(propagationEffect).findIndex((candidate) =>
        Type.equals(candidate, member),
      )
      const bottom = fn.type('never')
      if (target < 0 || bottom?._tag !== 'Bottom') return undefined
      const destination = fn.alloc(bottom)
      for (const drop of unusedHandlerDrop()) fn.emit(drop)
      const releases = propagationReleases(fn, runSpan)
      fn.emit(
        Object.freeze({
          _tag: 'PropagateEffectFailure' as const,
          source: bound,
          sourceType: memberType,
          propagationType,
          tagMappings: Object.freeze([Object.freeze({ source: 0, target: target + 1 })]),
          propagationLaneCount: propagationShape.laneCount,
          ...(releases.length === 0 ? {} : { releases }),
          type: bottom,
          provenance: generated(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    })
    if (selectedResult === undefined) return undefined
    innerArms.push(
      Object.freeze({
        id: armId,
        member,
        universal: false,
        before: Object.freeze(failureMembers.slice(ordinal)),
        after: Object.freeze(failureMembers.slice(ordinal + 1)),
        bindings: Object.freeze([
          Object.freeze({
            id: bindingId,
            destination: bound,
            path: Object.freeze([]),
            type: memberType,
            access: 'Move' as const,
            provenance: generated(expression.span),
          }),
        ]),
        selected: Object.freeze({
          access: 'Move' as const,
          operations: selectedOperations,
          result: selectedResult.result,
          cleanup: Object.freeze([]),
          endBorrow: false,
        }),
        provenance: generated(expression.span),
      }),
    )
  }
  const innerResult = fn.alloc(successType)
  const innerOperation: Mir.MatchOperation = Object.freeze({
    _tag: 'Match',
    id: innerMatch,
    destination: innerResult,
    scrutinee: failure,
    scrutineeType: failureValueMir,
    scrutineeShape: Layout.callingShape(fn.layout, reified.failureValueType) ?? resultUnionShape,
    access: 'Move',
    members: failureMembers,
    decisions: Object.freeze(
      failureMembers.map((member, ordinal) =>
        Object.freeze({
          member,
          candidates: Object.freeze([innerArms.at(ordinal)?.id].flatMap((id) => id ?? [])),
        }),
      ),
    ),
    arms: Object.freeze(innerArms),
    type: successType,
    resultShape: successShape,
    provenance: generated(expression.span),
  })
  const destination = fn.alloc(successType)
  fn.emit(
    Object.freeze({
      _tag: 'Match' as const,
      id: outerMatch,
      destination,
      scrutinee: resultUnion,
      scrutineeType: resultUnionType,
      scrutineeShape: resultUnionShape,
      access: 'Move' as const,
      members: reified.resultUnion.members,
      decisions: Object.freeze(
        reified.resultUnion.members.map((member) =>
          Object.freeze({
            member,
            candidates: Object.freeze([
              Type.equals(member, reified.successType) ? successArm : failureArm,
            ]),
          }),
        ),
      ),
      arms: Object.freeze([
        Object.freeze({
          id: successArm,
          member: reified.successType,
          universal: false,
          before: reified.resultUnion.members,
          after: Object.freeze([reified.failureType]),
          bindings: Object.freeze([
            Object.freeze({
              id: successBinding,
              destination: success,
              path: Object.freeze([reified.successField]),
              type: successType,
              access: 'Move' as const,
              provenance: generated(expression.span),
            }),
          ]),
          selected: Object.freeze({
            access: 'Move' as const,
            operations: unusedHandlerDrop(),
            result: success,
            cleanup: Object.freeze([]),
            endBorrow: false,
          }),
          provenance: generated(expression.span),
        }),
        Object.freeze({
          id: failureArm,
          member: reified.failureType,
          universal: false,
          before: Object.freeze([reified.failureType]),
          after: Object.freeze([]),
          bindings: Object.freeze([
            Object.freeze({
              id: failureBinding,
              destination: failure,
              path: Object.freeze([reified.failureField]),
              type: failureValueMir,
              access: 'Move' as const,
              provenance: generated(expression.span),
            }),
          ]),
          selected: Object.freeze({
            access: 'Move' as const,
            operations: Object.freeze([innerOperation]),
            result: innerResult,
            cleanup: Object.freeze([]),
            endBorrow: false,
          }),
          provenance: generated(expression.span),
        }),
      ]),
      type: successType,
      resultShape: successShape,
      provenance: generated(expression.span),
    }),
  )
  endRunLoans(fn, runSpan)
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
      ...((type._tag === 'CallableValue' &&
        type.storage !== undefined &&
        type.type.mode === 'Take') ||
      (type._tag === 'EffectValue' && type.storage !== undefined && type.type.access === 'Take')
        ? { consume: true as const }
        : {}),
      provenance: Object.freeze({ span: expression.span, generated: false }),
    }),
  )
  return Object.freeze({ result: destination })
}

const endLoans = (
  fn: FunctionLowering,
  loans: ReadonlyArray<Hir.BorrowId>,
  span: SourceSpan.SourceSpan,
): void => {
  for (const authored of loans) {
    const borrow = fn.recipeBorrow(authored)
    const held = fn.loanLocals.get(borrowKey(borrow))
    if (held === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'EndLoan',
        borrow,
        slice: held,
        provenance: generated(span),
      }),
    )
    fn.loanLocals.delete(borrowKey(borrow))
  }
}

const endRunLoans = (fn: FunctionLowering, span: SourceSpan.SourceSpan): void => {
  // A constructed effect holds its argument borrows until the run that consumes it: ownership
  // records that end site, so every lowering path for run must release them here.
  for (const loan of fn.ownership?.loans ?? []) {
    if (
      loan.origin !== 'EffectCapture' &&
      loan.origin !== 'CallableCapture' &&
      loan.origin !== 'ValueBorrow' &&
      loan.origin !== 'InterfaceOperand'
    )
      continue
    if (loan.endSpan.sourceId !== span.sourceId || loan.endSpan.end > span.end) {
      continue
    }
    endLoans(fn, [loan.id], span)
  }
}

const dropOwnedProvider = (
  fn: FunctionLowering,
  local_: Mir.LocalId,
  type: Type.Nominal,
  span: SourceSpan.SourceSpan,
): void => {
  const cleanup = concreteCleanup(fn, type)
  if (cleanup._tag === 'NoCleanup') return
  fn.emit(
    Object.freeze({
      _tag: 'Drop',
      local: local_,
      cleanup,
      provenance: generated(span),
    }),
  )
}

const endReturnedViewLoans = (fn: FunctionLowering, span: SourceSpan.SourceSpan): void => {
  for (const loan of fn.ownership?.loans ?? []) {
    if (loan.origin !== 'ReturnedView') continue
    if (
      loan.endSpan.sourceId !== span.sourceId ||
      loan.endSpan.start < span.start ||
      loan.endSpan.end > span.end
    ) {
      continue
    }
    endLoans(fn, [loan.id], span)
  }
}

const retainedEffectLoans = (
  fn: FunctionLowering,
  expression: Hir.Expression,
): ReadonlyArray<Hir.BorrowId> => {
  const retained = new Map<string, Hir.BorrowId>()
  for (const child of Hir.expressionTree(expression)) {
    if (child._tag === 'BindingReference') {
      for (const borrow of fn.effectLoanEnds.get(child.binding.ordinal) ?? [])
        retained.set(borrowKey(borrow), borrow)
    }
    if (child._tag === 'SliceBorrow' || child._tag === 'ValueBorrow') {
      retained.set(borrowKey(child.borrow), child.borrow)
      continue
    }
    if (
      (child._tag === 'BuiltinCall' || child._tag === 'BoundOperationCall') &&
      child.witnessEffectSite !== undefined
    ) {
      for (const loan of fn.ownership?.loans ?? []) {
        if (
          loan.origin === 'InterfaceOperand' &&
          loan.id.callSpan.sourceId === child.span.sourceId &&
          loan.id.callSpan.start === child.span.start &&
          loan.id.callSpan.end === child.span.end
        )
          retained.set(borrowKey(loan.id), loan.id)
      }
    }
    if (child._tag !== 'CallableSection') continue
    for (const capture of child.captures) {
      if (capture.value._tag !== 'SliceBorrow' && capture.value._tag !== 'ValueBorrow') continue
      retained.set(borrowKey(capture.value.borrow), capture.value.borrow)
    }
  }
  return Object.freeze([...retained.values()])
}

const borrowedWriteRoot = (
  fn: FunctionLowering,
  root: Hir.BorrowedWritePlace['root'],
): Mir.LocalId | undefined =>
  root._tag === 'ParameterSliceRoot'
    ? fn.parameterLocals.get(root.parameter.ordinal)
    : fn.bindingLocals.get(root.binding.ordinal)

const lowerServiceEffectValue = (
  fn: FunctionLowering,
  subject: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>,
  availableRequirements: ReadonlyArray<ProvidedRequirement>,
): LoweredExpression | undefined => {
  const provided = availableRequirements.find(
    (requirement) =>
      requirement.role === subject.role &&
      Type.equals(requirement.capability, subject.service) &&
      (subject.access === 'Shared' ||
        requirement.access === 'Exclusive' ||
        requirement.access === 'Take'),
  )
  if (provided?.witness._tag !== 'SourceConformanceWitness' || provided.local === undefined)
    return undefined
  const target = DeclarationIndex.witnessOperation(provided.witness, subject.operation)
  if (target === undefined) return undefined
  const loweredArguments = subject.arguments.map((argument) => lowerExpression(fn, argument))
  if (loweredArguments.some((argument) => argument === undefined)) return undefined
  const typeArguments = provided.witness.typeArguments
  const effectResult = fn.effectResults.get(instanceText(target, typeArguments))
  if (effectResult === undefined) return undefined
  const effect = fn.alloc(effectResult)
  fn.emit(
    Object.freeze({
      _tag: 'Call',
      destination: effect,
      target,
      typeArguments,
      arguments: Object.freeze([
        provided.local,
        ...loweredArguments.flatMap((argument) =>
          argument === undefined ? [] : [argument.result],
        ),
      ]),
      type: effectResult,
      provenance: authored(subject.span),
    }),
  )
  return Object.freeze({ result: effect })
}

const lowerReifiedEffectRecipe = (
  fn: FunctionLowering,
  subject: Hir.Expression,
  resultType: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): ReifiedEffect | undefined => {
  const recipe = effectRecipe(fn, subject)
  const forwarded = inlineForwardedRequirement(fn, recipe)
  if (forwarded !== undefined) {
    const provider = lowerExpression(fn, forwarded.provider)
    if (provider === undefined) return undefined
    const providerBorrow =
      forwarded.provider._tag === 'ValueBorrow'
        ? fn.recipeBorrow(forwarded.provider.borrow)
        : undefined
    let runtimeProvider = provider.result
    let ownedLoan: { readonly borrow: Hir.BorrowId; readonly slice: Mir.LocalId } | undefined
    if (
      forwarded.selection.access === 'Take' &&
      forwarded.selection.witness._tag === 'SourceConformanceWitness'
    ) {
      const providerType = fn.type(forwarded.selection.providerType)
      const referenceType = fn.type(Type.reference('Exclusive', forwarded.selection.providerType))
      if (providerType?._tag !== 'Nominal' || referenceType?._tag !== 'Reference') return undefined
      const borrow = fn.freshSyntheticBorrow(forwarded.provider.span)
      const reference = fn.alloc(referenceType)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination: reference,
          root: provider.result,
          selectors: Object.freeze([]),
          sourceType: providerType,
          type: referenceType,
          access: 'Exclusive',
          reborrow: false,
          suspendsParent: false,
          provenance: authored(forwarded.provider.span),
        }),
      )
      fn.loanLocals.set(borrowKey(borrow), reference)
      runtimeProvider = reference
      ownedLoan = Object.freeze({ borrow, slice: reference })
    }
    const reified = lowerReifiedEffectRecipe(
      fn,
      forwarded.binding.protected,
      resultType,
      span,
      Object.freeze([
        ...availableRequirements,
        Object.freeze({ ...forwarded.selection, local: runtimeProvider }),
      ]),
    )
    if (reified === undefined) return undefined
    const closeOwnedLoan =
      ownedLoan !== undefined && fn.loanLocals.delete(borrowKey(ownedLoan.borrow))
    endRunLoans(fn, span)
    if (providerBorrow !== undefined) endLoans(fn, [providerBorrow], span)
    if (ownedLoan !== undefined && closeOwnedLoan) {
      fn.emit(
        Object.freeze({
          _tag: 'EndLoan',
          borrow: ownedLoan.borrow,
          slice: ownedLoan.slice,
          provenance: generated(forwarded.provider.span),
        }),
      )
    }
    if (forwarded.selection.access === 'Take')
      dropOwnedProvider(
        fn,
        provider.result,
        forwarded.selection.providerType,
        forwarded.provider.span,
      )
    return reified
  }

  if (recipe._tag === 'EffectBindRequirement') {
    const selectedProvider = specializeProvider(fn, recipe.provider)
    if (selectedProvider === undefined) return undefined
    const ownedProvider =
      recipe.provider.selectionAccess !== 'Take'
        ? undefined
        : recipe.provider.binding !== undefined
          ? fn.bindingLocals.get(recipe.provider.binding.ordinal)
          : recipe.provider.parameter !== undefined
            ? fn.parameterLocals.get(recipe.provider.parameter.ordinal)
            : undefined
    let providerLoan: { readonly borrow: Hir.BorrowId; readonly slice: Mir.LocalId } | undefined
    let provided: ProvidedRequirement = selectedProvider
    if (selectedProvider.witness._tag === 'SourceConformanceWitness') {
      const providerAccess =
        recipe.provider.selectionAccess === 'Take'
          ? ('Exclusive' as const)
          : recipe.provider.selectionAccess
      const provider =
        recipe.provider.binding !== undefined
          ? fn.bindingLocals.get(recipe.provider.binding.ordinal)
          : recipe.provider.parameter !== undefined
            ? fn.parameterLocals.get(recipe.provider.parameter.ordinal)
            : undefined
      const forwardedReference =
        recipe.provider.parameter === undefined || provider === undefined
          ? undefined
          : fn.localTypes.at(provider.ordinal)
      const providerType = fn.type(selectedProvider.providerType)
      const referenceType = fn.type(Type.reference(providerAccess, selectedProvider.providerType))
      const authoredLoan = fn.ownership?.loans.find(
        (candidate) =>
          (candidate.origin === 'EffectCapture' ||
            candidate.origin === 'CallableCapture' ||
            candidate.origin === 'ValueBorrow') &&
          candidate.access === providerAccess &&
          candidate.startSpan.start === recipe.provider.span.start &&
          candidate.startSpan.end === recipe.provider.span.end,
      )
      const borrow = fn.beginRecipeBorrow(
        authoredLoan?.id ?? fn.freshSyntheticBorrow(recipe.provider.span),
      )
      if (
        provider !== undefined &&
        forwardedReference?._tag === 'Reference' &&
        forwardedReference.type.access === providerAccess &&
        Type.equals(forwardedReference.type.target, selectedProvider.providerType)
      ) {
        provided = Object.freeze({ ...selectedProvider, local: provider })
      } else {
        if (
          provider === undefined ||
          providerType?._tag !== 'Nominal' ||
          referenceType?._tag !== 'Reference' ||
          borrow === undefined
        )
          return undefined
        const reference = fn.alloc(referenceType)
        fn.emit(
          Object.freeze({
            _tag: 'BeginLoan',
            borrow,
            destination: reference,
            root: provider,
            selectors: Object.freeze([]),
            sourceType: providerType,
            type: referenceType,
            access: providerAccess,
            reborrow: false,
            suspendsParent: false,
            provenance: authored(recipe.provider.span),
          }),
        )
        fn.loanLocals.set(borrowKey(borrow), reference)
        provided = Object.freeze({ ...selectedProvider, local: reference })
        providerLoan = Object.freeze({ borrow, slice: reference })
      }
    }
    const reified = lowerReifiedEffectRecipe(
      fn,
      recipe.protected,
      resultType,
      span,
      Object.freeze([...availableRequirements, provided]),
    )
    if (reified === undefined) return undefined
    const closeProviderLoan =
      providerLoan !== undefined && fn.loanLocals.delete(borrowKey(providerLoan.borrow))
    endRunLoans(fn, span)
    if (providerLoan !== undefined && closeProviderLoan) {
      fn.emit(
        Object.freeze({
          _tag: 'EndLoan',
          borrow: providerLoan.borrow,
          slice: providerLoan.slice,
          provenance: generated(recipe.provider.span),
        }),
      )
    }
    if (ownedProvider !== undefined)
      dropOwnedProvider(fn, ownedProvider, selectedProvider.providerType, recipe.provider.span)
    return reified
  }

  const lowered =
    recipe._tag === 'ServiceEffectConstruct'
      ? lowerServiceEffectValue(fn, recipe, availableRequirements)
      : lowerExpression(fn, recipe)
  const effectType = lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
  if (lowered === undefined || effectType?._tag !== 'EffectValue') return undefined
  const reified = reifyEffectValue(
    fn,
    lowered.result,
    effectType,
    resultType,
    span,
    availableRequirements,
  )
  if (reified === undefined) return undefined
  endRunLoans(fn, span)
  if (recipe._tag === 'EffectConstruct' || recipe._tag === 'ServiceEffectConstruct')
    endLoans(fn, recipe.loanEnds, span)
  return reified
}

const lowerEffectExecution = (
  fn: FunctionLowering,
  subject: Hir.Expression,
  success: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): LoweredExpression | undefined => {
  if (subject._tag === 'BindingReference') {
    const storedRecipe = fn.effectRecipes.get(subject.binding.ordinal)
    if (storedRecipe !== undefined)
      return lowerEffectExecution(fn, storedRecipe, success, span, availableRequirements)
  }
  if (subject._tag === 'Move' && subject.subject._tag === 'BindingReference') {
    const storedRecipe = fn.effectRecipes.get(subject.subject.binding.ordinal)
    if (storedRecipe !== undefined)
      return lowerEffectExecution(fn, storedRecipe, success, span, availableRequirements)
  }

  const forwarded = inlineForwardedRequirement(fn, subject)
  if (forwarded !== undefined) {
    const provider = lowerExpression(fn, forwarded.provider)
    if (provider === undefined) return undefined
    const providerBorrow =
      forwarded.provider._tag === 'ValueBorrow'
        ? fn.recipeBorrow(forwarded.provider.borrow)
        : undefined
    let runtimeProvider = provider.result
    let ownedLoan: { readonly borrow: Hir.BorrowId; readonly slice: Mir.LocalId } | undefined
    if (
      forwarded.selection.access === 'Take' &&
      forwarded.selection.witness._tag === 'SourceConformanceWitness'
    ) {
      const providerType = fn.type(forwarded.selection.providerType)
      const referenceType = fn.type(Type.reference('Exclusive', forwarded.selection.providerType))
      if (providerType?._tag !== 'Nominal' || referenceType?._tag !== 'Reference') return undefined
      const borrow = fn.freshSyntheticBorrow(forwarded.provider.span)
      const reference = fn.alloc(referenceType)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination: reference,
          root: provider.result,
          selectors: Object.freeze([]),
          sourceType: providerType,
          type: referenceType,
          access: 'Exclusive',
          reborrow: false,
          suspendsParent: false,
          provenance: authored(forwarded.provider.span),
        }),
      )
      fn.loanLocals.set(borrowKey(borrow), reference)
      runtimeProvider = reference
      ownedLoan = Object.freeze({ borrow, slice: reference })
    }
    const result = lowerEffectExecution(
      fn,
      forwarded.binding.protected,
      success,
      span,
      Object.freeze([
        ...availableRequirements,
        Object.freeze({ ...forwarded.selection, local: runtimeProvider }),
      ]),
    )
    if (result === undefined) return undefined
    const closeOwnedLoan =
      ownedLoan !== undefined && fn.loanLocals.delete(borrowKey(ownedLoan.borrow))
    endRunLoans(fn, span)
    if (providerBorrow !== undefined) endLoans(fn, [providerBorrow], span)
    if (ownedLoan !== undefined && closeOwnedLoan) {
      fn.emit(
        Object.freeze({
          _tag: 'EndLoan',
          borrow: ownedLoan.borrow,
          slice: ownedLoan.slice,
          provenance: generated(forwarded.provider.span),
        }),
      )
    }
    if (forwarded.selection.access === 'Take')
      dropOwnedProvider(
        fn,
        provider.result,
        forwarded.selection.providerType,
        forwarded.provider.span,
      )
    if (
      forwarded.binding.protected._tag === 'EffectConstruct' ||
      forwarded.binding.protected._tag === 'ServiceEffectConstruct'
    )
      endLoans(fn, forwarded.binding.protected.loanEnds, span)
    return result
  }

  if (subject._tag === 'EffectBindRequirement') {
    const selectedProvider = specializeProvider(fn, subject.provider)
    if (selectedProvider === undefined) return undefined
    const ownedProvider =
      subject.provider.selectionAccess !== 'Take'
        ? undefined
        : subject.provider.binding !== undefined
          ? fn.bindingLocals.get(subject.provider.binding.ordinal)
          : subject.provider.parameter !== undefined
            ? fn.parameterLocals.get(subject.provider.parameter.ordinal)
            : undefined
    let providerLoan: { readonly borrow: Hir.BorrowId; readonly slice: Mir.LocalId } | undefined
    let provided: ProvidedRequirement = selectedProvider
    if (selectedProvider.witness._tag === 'SourceConformanceWitness') {
      const providerAccess =
        subject.provider.selectionAccess === 'Take'
          ? ('Exclusive' as const)
          : subject.provider.selectionAccess
      const provider =
        subject.provider.binding !== undefined
          ? fn.bindingLocals.get(subject.provider.binding.ordinal)
          : subject.provider.parameter !== undefined
            ? fn.parameterLocals.get(subject.provider.parameter.ordinal)
            : undefined
      const forwardedReference =
        subject.provider.parameter === undefined || provider === undefined
          ? undefined
          : fn.localTypes.at(provider.ordinal)
      const providerType = fn.type(selectedProvider.providerType)
      const referenceType = fn.type(
        Object.freeze({
          _tag: 'ReferenceType' as const,
          access: providerAccess,
          target: selectedProvider.providerType,
        }),
      )
      const authoredLoan = fn.ownership?.loans.find(
        (candidate) =>
          (candidate.origin === 'EffectCapture' ||
            candidate.origin === 'CallableCapture' ||
            candidate.origin === 'ValueBorrow') &&
          candidate.access === providerAccess &&
          candidate.startSpan.start === subject.provider.span.start &&
          candidate.startSpan.end === subject.provider.span.end,
      )
      const borrow = fn.beginRecipeBorrow(
        authoredLoan?.id ?? fn.freshSyntheticBorrow(subject.provider.span),
      )
      if (
        provider !== undefined &&
        forwardedReference?._tag === 'Reference' &&
        forwardedReference.type.access === providerAccess &&
        Type.equals(forwardedReference.type.target, selectedProvider.providerType)
      ) {
        provided = Object.freeze({ ...selectedProvider, local: provider })
      } else {
        if (
          provider === undefined ||
          providerType?._tag !== 'Nominal' ||
          referenceType?._tag !== 'Reference' ||
          borrow === undefined
        )
          return undefined
        const reference = fn.alloc(referenceType)
        fn.emit(
          Object.freeze({
            _tag: 'BeginLoan',
            borrow,
            destination: reference,
            root: provider,
            selectors: Object.freeze([]),
            sourceType: providerType,
            type: referenceType,
            access: providerAccess,
            reborrow: false,
            suspendsParent: false,
            provenance: authored(subject.provider.span),
          }),
        )
        fn.loanLocals.set(borrowKey(borrow), reference)
        provided = Object.freeze({ ...selectedProvider, local: reference })
        providerLoan = Object.freeze({ borrow, slice: reference })
      }
    }
    const result = lowerEffectExecution(
      fn,
      subject.protected,
      success,
      span,
      Object.freeze([...availableRequirements, provided]),
    )
    if (result === undefined) return undefined
    const closeProviderLoan =
      providerLoan !== undefined && fn.loanLocals.delete(borrowKey(providerLoan.borrow))
    endRunLoans(fn, span)
    if (
      subject.protected._tag === 'EffectConstruct' ||
      subject.protected._tag === 'ServiceEffectConstruct'
    )
      endLoans(fn, subject.protected.loanEnds, span)
    if (providerLoan !== undefined && closeProviderLoan) {
      fn.emit(
        Object.freeze({
          _tag: 'EndLoan',
          borrow: providerLoan.borrow,
          slice: providerLoan.slice,
          provenance: generated(subject.provider.span),
        }),
      )
    }
    if (ownedProvider !== undefined)
      dropOwnedProvider(fn, ownedProvider, selectedProvider.providerType, subject.provider.span)
    return result
  }

  if (subject._tag === 'ServiceEffectConstruct') {
    const lowered = lowerServiceEffectValue(fn, subject, availableRequirements)
    const effectResult =
      lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
    if (lowered === undefined || effectResult?._tag !== 'EffectValue') return undefined
    const result = lowerRunEffectValue(
      fn,
      lowered.result,
      effectResult,
      success,
      span,
      availableRequirements,
    )
    if (result !== undefined) {
      endRunLoans(fn, span)
      endLoans(fn, subject.loanEnds, span)
    }
    return result
  }

  if (subject._tag === 'BuiltinCall' && Type.isEffect(subject.type)) {
    const run = Object.freeze({ _tag: 'Run' as const, subject, type: success, span })
    return lowerExpression(fn, run)
  }

  const lowered = lowerExpression(fn, subject)
  const loweredType = lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
  if (lowered === undefined || loweredType?._tag !== 'EffectValue') return undefined
  return lowerRunEffectValue(fn, lowered.result, loweredType, success, span, availableRequirements)
}

/**
 * Redirects one bound operator to the provider's own function when specialization lands on a type
 * whose conformance maps the operation to source rather than to a sealed intrinsic.
 *
 * Operator syntax lowers values through the compiler-known scalar shape, while the bound records
 * the ordinary interface contract the operator spells. A source witness therefore receives each
 * operand exactly as that contract declares it: borrowed operands create call-scoped loans, while
 * value operands remain values. This is contract-directed lowering, not a witness-side adapter.
 */
const emitWitnessDispatch = (
  fn: FunctionLowering,
  target: DeclarationIndex.InterfaceWitnessTarget,
  argumentLocals: ReadonlyArray<Mir.LocalId>,
  operandTypes: ReadonlyArray<Mir.Type>,
  resultType: Mir.Type,
  span: SourceSpan.SourceSpan,
): Mir.LocalId | undefined => {
  const borrows: Array<{ readonly borrow: Hir.BorrowId; readonly local: Mir.LocalId }> = []
  const arguments_: Array<Mir.LocalId> = []
  for (const [ordinal, argument] of argumentLocals.entries()) {
    const operand = operandTypes.at(ordinal)
    const source = fn.localTypes.at(argument.ordinal)
    if (operand === undefined || source === undefined) return undefined
    if (operand._tag !== 'Reference') {
      arguments_.push(argument)
      continue
    }
    const borrow = fn.beginRecipeBorrow(
      Object.freeze({
        _tag: 'BorrowId' as const,
        function: fn.owner.function.declaration.id,
        callSpan: span,
        ordinal,
      }),
    )
    const destination = fn.alloc(operand)
    fn.emit(
      Object.freeze({
        _tag: 'BeginLoan',
        borrow,
        destination,
        root: argument,
        selectors: Object.freeze([]),
        sourceType: source,
        type: operand,
        access: operand.type.access,
        reborrow: false,
        suspendsParent: false,
        provenance: generated(span),
      }),
    )
    borrows.push(Object.freeze({ borrow, local: destination }))
    arguments_.push(destination)
  }
  const witnessArguments = sourceWitnessArguments(fn, target, arguments_, span)
  if (witnessArguments === undefined) return undefined
  const destination = fn.alloc(resultType)
  fn.emit(
    Object.freeze({
      _tag: 'Call',
      destination,
      target: target.implementation,
      // A conditional witness is one generic function per header, so the direct target carries the
      // arguments this specialization proved. Nothing else travels: a requirement's own witness is
      // reached through its own instance, never through a value handed to this call.
      typeArguments: target.typeArguments,
      arguments: witnessArguments.arguments,
      type: resultType,
      provenance: generated(span),
    }),
  )
  endWitnessReborrows(fn, witnessArguments.reborrows, span)
  for (const entry of borrows)
    fn.emit(
      Object.freeze({
        _tag: 'EndLoan',
        borrow: entry.borrow,
        slice: entry.local,
        provenance: generated(span),
      }),
    )
  return destination
}

const lowerInterfaceWitnessCall = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }>,
  argumentLocals: ReadonlyArray<Mir.LocalId>,
): Mir.LocalId | undefined => {
  const bound = expression.interfaceOperation
  if (bound === undefined) return undefined
  const provider = fn.semantic(bound.provider)
  const capability = fn.semantic(bound.capability)
  if (!Type.isNominal(capability)) return undefined
  const target = DeclarationIndex.interfaceWitnessTarget(
    fn.index,
    provider,
    capability,
    bound.operation,
  )
  const resultType = fn.type(expression.type)
  const operandTypes = bound.contract.operands.flatMap((operand) => {
    if (operand.type._tag !== 'Resolved') return []
    const type = fn.type(fn.semantic(operand.type.type))
    return type === undefined ? [] : [type]
  })
  if (
    target === undefined ||
    resultType === undefined ||
    operandTypes.length !== bound.contract.operands.length
  )
    return undefined
  return emitWitnessDispatch(fn, target, argumentLocals, operandTypes, resultType, expression.span)
}

interface WitnessArguments {
  readonly arguments: ReadonlyArray<Mir.LocalId>
  readonly reborrows: ReadonlyArray<{ readonly borrow: Hir.BorrowId; readonly local: Mir.LocalId }>
}

interface InterfaceOperands {
  readonly arguments: ReadonlyArray<Mir.LocalId>
  readonly borrows: ReadonlyArray<{ readonly borrow: Hir.BorrowId; readonly local: Mir.LocalId }>
}

const lowerInterfaceOperands = (
  fn: FunctionLowering,
  arguments_: ReadonlyArray<Hir.Expression>,
  operands: ReadonlyArray<DeclarationIndex.InterfaceOperandFact>,
  span: SourceSpan.SourceSpan,
): InterfaceOperands | undefined => {
  if (arguments_.length !== operands.length) return undefined
  const lowered: Array<Mir.LocalId> = []
  const borrows: Array<{ readonly borrow: Hir.BorrowId; readonly local: Mir.LocalId }> = []
  for (const [ordinal, argument] of arguments_.entries()) {
    const value = lowerExpression(fn, argument)
    const operand = operands.at(ordinal)
    if (value === undefined || operand?.type._tag !== 'Resolved') return undefined
    const expected = fn.type(fn.semantic(operand.type.type))
    const actual = fn.localTypes.at(value.result.ordinal)
    if (expected === undefined || actual === undefined) return undefined
    if (Type.equals(Mir.semanticType(actual), Mir.semanticType(expected))) {
      lowered.push(value.result)
      continue
    }
    if (
      expected._tag !== 'Reference' ||
      !Type.equals(Mir.semanticType(actual), expected.type.target)
    )
      return undefined
    const borrow = fn.beginRecipeBorrow(
      Object.freeze({
        _tag: 'BorrowId' as const,
        function: fn.owner.function.declaration.id,
        callSpan: span,
        ordinal,
      }),
    )
    const destination = fn.alloc(expected)
    fn.emit(
      Object.freeze({
        _tag: 'BeginLoan',
        borrow,
        destination,
        root: value.result,
        selectors: Object.freeze([]),
        sourceType: actual,
        type: expected,
        access: expected.type.access,
        reborrow: false,
        suspendsParent: false,
        provenance: generated(span),
      }),
    )
    fn.loanLocals.set(borrowKey(borrow), destination)
    lowered.push(destination)
    borrows.push(Object.freeze({ borrow, local: destination }))
  }
  return Object.freeze({ arguments: Object.freeze(lowered), borrows: Object.freeze(borrows) })
}

const sourceWitnessParameterTypes = (
  fn: FunctionLowering,
  target: DeclarationIndex.InterfaceWitnessTarget,
): ReadonlyArray<Mir.Type> | undefined => {
  const declaration = DeclarationIndex.byCanonical(fn.index, target.implementation)
  if (declaration?._tag !== 'FunctionDeclaration') return undefined
  const binders = declaration.typeParameters
    .filter((parameter) => parameter.duplicateOf === undefined)
    .map((parameter) => parameter.type)
  const substitution = Type.substitution(binders, target.typeArguments)
  if (substitution === undefined) return undefined
  const parameters = declaration.parameters.flatMap((parameter) => {
    if (parameter.declaredType._tag !== 'Resolved') return []
    const type = fn.type(Type.substitute(parameter.declaredType.type, substitution))
    return type === undefined ? [] : [type]
  })
  return parameters.length === declaration.parameters.length ? Object.freeze(parameters) : undefined
}

/** Realizes only access weakening already admitted by the compatibility actor. */
const sourceWitnessArguments = (
  fn: FunctionLowering,
  target: DeclarationIndex.InterfaceWitnessTarget,
  arguments_: ReadonlyArray<Mir.LocalId>,
  span: SourceSpan.SourceSpan,
): WitnessArguments | undefined => {
  const parameters = sourceWitnessParameterTypes(fn, target)
  if (parameters === undefined || parameters.length !== arguments_.length) return undefined
  const lowered: Array<Mir.LocalId> = []
  const reborrows: Array<{ readonly borrow: Hir.BorrowId; readonly local: Mir.LocalId }> = []
  for (const [ordinal, argument] of arguments_.entries()) {
    const actual = fn.localTypes.at(argument.ordinal)
    const expected = parameters.at(ordinal)
    if (actual === undefined || expected === undefined) return undefined
    if (Type.equals(Mir.semanticType(actual), Mir.semanticType(expected))) {
      lowered.push(argument)
      continue
    }
    const actualReference = actual._tag === 'Reference' || actual._tag === 'Slice'
    const expectedReference = expected._tag === 'Reference' || expected._tag === 'Slice'
    const sameTarget =
      actual._tag === 'Reference' && expected._tag === 'Reference'
        ? Type.equals(actual.type.target, expected.type.target)
        : actual._tag === 'Slice' && expected._tag === 'Slice'
          ? Type.equals(actual.type.element, expected.type.element)
          : false
    if (
      !actualReference ||
      !expectedReference ||
      !sameTarget ||
      actual.type.access !== 'Exclusive' ||
      expected.type.access !== 'Shared'
    )
      return undefined
    const borrow = fn.beginRecipeBorrow(
      Object.freeze({
        _tag: 'BorrowId' as const,
        function: fn.owner.function.declaration.id,
        callSpan: span,
        ordinal: arguments_.length + ordinal,
      }),
    )
    const destination = fn.alloc(expected)
    fn.emit(
      Object.freeze({
        _tag: 'BeginLoan',
        borrow,
        destination,
        root: argument,
        selectors: Object.freeze([]),
        sourceType: actual,
        type: expected,
        access: 'Shared',
        reborrow: true,
        suspendsParent: true,
        provenance: generated(span),
      }),
    )
    fn.loanLocals.set(borrowKey(borrow), destination)
    lowered.push(destination)
    reborrows.push(Object.freeze({ borrow, local: destination }))
  }
  return Object.freeze({ arguments: Object.freeze(lowered), reborrows: Object.freeze(reborrows) })
}

const endWitnessReborrows = (
  fn: FunctionLowering,
  reborrows: WitnessArguments['reborrows'],
  span: SourceSpan.SourceSpan,
): void => {
  for (const reborrow of reborrows)
    if (fn.loanLocals.delete(borrowKey(reborrow.borrow)))
      fn.emit(
        Object.freeze({
          _tag: 'EndLoan',
          borrow: reborrow.borrow,
          slice: reborrow.local,
          provenance: generated(span),
        }),
      )
}

const witnessEffectContract = (
  expression: Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' | 'BoundOperationCall' }>,
): DeclarationIndex.InterfaceOperationApplicationFact | undefined =>
  expression._tag === 'BoundOperationCall'
    ? expression.contract
    : expression.interfaceOperation?.contract

const lowerWitnessEffect = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' | 'BoundOperationCall' }>,
): LoweredExpression | undefined => {
  const site = expression.witnessEffectSite
  const contract = witnessEffectContract(expression)
  if (site === undefined || contract === undefined) return undefined
  const capability = fn.semantic(
    expression._tag === 'BoundOperationCall'
      ? expression.capability
      : (expression.interfaceOperation?.capability ?? 'never'),
  )
  const provider = fn.semantic(
    expression._tag === 'BoundOperationCall'
      ? expression.provider
      : (expression.interfaceOperation?.provider ?? 'never'),
  )
  if (!Type.isNominal(capability)) return undefined
  const target = DeclarationIndex.interfaceWitnessTarget(
    fn.index,
    provider,
    capability,
    expression._tag === 'BoundOperationCall'
      ? expression.operation
      : (expression.interfaceOperation?.operation ?? ''),
  )
  const intrinsic = DeclarationIndex.interfaceOperationIntrinsic(
    fn.index,
    provider,
    capability,
    expression._tag === 'BoundOperationCall'
      ? expression.operation
      : (expression.interfaceOperation?.operation ?? ''),
  )
  if (target === undefined && intrinsic?.rule._tag !== 'BuiltinRule') return undefined
  const type = witnessEffectValueType(fn.layout, fn.owner.key, site)
  if (type === undefined) return undefined
  const operands = lowerInterfaceOperands(
    fn,
    expression.arguments,
    contract.operands,
    expression.span,
  )
  if (operands === undefined) return undefined
  const destination = fn.alloc(type)
  const runner = Hir.effectRunnerId(fn.owner.key.declaration, site)
  fn.emit(
    Object.freeze({
      _tag: 'MakeEffect',
      destination,
      runner,
      runnerTypeArguments: fn.owner.key.typeArguments,
      captures: Object.freeze(
        operands.arguments.map((source, ordinal) =>
          Object.freeze({
            source,
            access: type.environment.fields.at(ordinal)?.access ?? ('Take' as const),
          }),
        ),
      ),
      type,
      provenance: generated(expression.span),
    }),
  )
  const key = baseRunnerKey(fn.owner.key, site)
  if (!fn.generatedRunners.some((candidate) => candidate.specializationKey === key))
    fn.generatedRunners.push(
      Object.freeze({
        _tag: 'WitnessEffectRunner',
        id: runner,
        owner: fn.owner,
        expression,
        ...(target === undefined ? {} : { target }),
        ...(intrinsic?.rule._tag === 'BuiltinRule' ? { intrinsic } : {}),
        type,
        specializationKey: key,
        providedRequirements: Object.freeze([]),
      }),
    )
  return Object.freeze({ result: destination })
}

/**
 * Redirects one bound operation call to the provider's own function, the fallback the operator path
 * reaches through `lowerInterfaceWitnessCall`.
 *
 * Named bound calls already elaborated and ownership-checked their arguments against the literal
 * applied contract. Lowering therefore forwards those locals unchanged; introducing any borrow or
 * move here would restore a second hidden user-interface calling convention.
 */
const lowerBoundWitnessCall = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'BoundOperationCall' }>,
  provider: Type.Type,
  capability: Type.Nominal,
  argumentLocals: ReadonlyArray<Mir.LocalId>,
): Mir.LocalId | undefined => {
  const target = DeclarationIndex.interfaceWitnessTarget(
    fn.index,
    provider,
    capability,
    expression.operation,
  )
  const resultType = fn.type(expression.type)
  if (target === undefined || resultType === undefined) return undefined
  const witnessArguments = sourceWitnessArguments(fn, target, argumentLocals, expression.span)
  if (witnessArguments === undefined) return undefined
  const destination = fn.alloc(resultType)
  fn.emit(
    Object.freeze({
      _tag: 'Call',
      destination,
      target: target.implementation,
      typeArguments: target.typeArguments,
      arguments: witnessArguments.arguments,
      type: resultType,
      provenance: generated(expression.span),
    }),
  )
  endWitnessReborrows(fn, witnessArguments.reborrows, expression.span)
  return destination
}

/**
 * Reads through one shared interface operand only for the legacy value-shaped builtin selected by
 * an intrinsic conformance. The operand may be a fresh borrow or an already-borrowed parameter;
 * ordinary source witnesses never enter this bridge.
 */
const lowerIntrinsicWitnessOperand = (
  fn: FunctionLowering,
  argument: Hir.Expression,
  operand: DeclarationIndex.InterfaceOperandFact | undefined,
): LoweredExpression | undefined => {
  const lowered = lowerExpression(fn, argument)
  if (lowered === undefined || operand?.type._tag !== 'Resolved') return lowered
  const contractType = fn.semantic(operand.type.type)
  const sourceType = fn.localTypes.at(lowered.result.ordinal)
  if (
    !Type.isReference(contractType) ||
    contractType.access !== 'Shared' ||
    sourceType?._tag !== 'Reference' ||
    !Type.equals(sourceType.type, contractType)
  )
    return lowered
  const type = fn.type(contractType.target)
  if (type === undefined) return undefined
  const destination = fn.alloc(type)
  fn.emit(
    Object.freeze({
      _tag: 'ReadPlace',
      destination,
      root: lowered.result,
      selectors: Object.freeze([]),
      type,
      provenance: generated(argument.span),
    }),
  )
  return Object.freeze({ result: destination })
}

function lowerExpression(
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredExpression | undefined {
  const lower = (): LoweredExpression | undefined => {
    const lowered = lowerExpressionInner(fn, expression)
    endReturnedViewLoans(fn, expression.span)
    return lowered
  }
  // The replay substitution must remain live through the wrapper's automatic loan endings, not
  // only through the Run case itself: a returned view can share the Run span with the replayed
  // protected recipe.
  return expression._tag === 'Run' ? fn.withRecipeReplay(lower) : lower()
}

function lowerExpressionInner(
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredExpression | undefined {
  switch (expression._tag) {
    case 'IntegerLiteral': {
      const type = fn.type(expression.type)
      if (type === undefined || !Type.isBuiltin(Mir.semanticType(type))) return undefined
      const destination = fn.alloc(type)
      // Lowering is the first phase that holds the selected target, and every engine reads the MIR
      // it produces, so this is where a pointer-width fact becomes one exact number.
      const value =
        expression.targetConstant === undefined
          ? expression.value
          : TargetConstant.value(
              expression.targetConstant,
              TargetConstant.pointerBits(fn.layout.target),
            )
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type,
          value,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'FloatingLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== expression.type) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type,
          value: expression.bits,
          provenance: authored(expression.span),
        }),
      )
      return { result: destination }
    }
    case 'StaticStringLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'String') return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'StaticString',
          destination,
          data: expression.data.id,
          byteLength: expression.data.bytes.length,
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'RuntimeStringView': {
      const source = lowerExpression(fn, expression.source)
      const sourceType = source === undefined ? undefined : fn.localTypes.at(source.result.ordinal)
      const type = fn.type(expression.type)
      if (
        source === undefined ||
        sourceType?._tag !== 'Slice' ||
        !Type.equals(sourceType.type, Type.slice('Shared', 'u8')) ||
        type?._tag !== 'String'
      )
        return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'StringFromUtf8Unchecked',
          destination,
          bytes: source.result,
          heldLoans: expression.heldLoans,
          authorization: 'Unsafe',
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'StringEquality': {
      const left = lowerExpression(fn, expression.left)
      const right = lowerExpression(fn, expression.right)
      const leftType = left === undefined ? undefined : fn.localTypes.at(left.result.ordinal)
      const rightType = right === undefined ? undefined : fn.localTypes.at(right.result.ordinal)
      if (
        left === undefined ||
        right === undefined ||
        leftType?._tag !== 'String' ||
        rightType?._tag !== 'String'
      )
        return undefined
      const destination = fn.alloc(bool)
      fn.emit(
        Object.freeze({
          _tag: 'StringEqualsExact',
          destination,
          left: left.result,
          right: right.result,
          negated: expression.negated,
          type: bool,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'StaticByteViewLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'Slice') return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'StaticView',
          destination,
          data: expression.data.id,
          length: expression.data.bytes.length,
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'UnitLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'Nominal' || !Type.equals(type.type, Type.unit)) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Construct',
          destination,
          type,
          fields: Object.freeze([]),
          provenance: authored(expression.span),
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
    case 'CharacterLiteral': {
      const destination = fn.alloc(character)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type: character,
          value: expression.value,
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
    case 'Replace': {
      // Swap one writable place: the old value reads out before the replacement commits, and
      // both halves ride the existing checked place operations.
      const place = expression.place
      const root =
        place._tag === 'BorrowedWritePlace'
          ? borrowedWriteRoot(fn, place.root)
          : fn.bindingLocals.get(place.root.ordinal)
      const rootType = root === undefined ? undefined : fn.localTypes.at(root.ordinal)
      const type = fn.type(place.type)
      if (root === undefined || rootType === undefined || type === undefined) return undefined
      const selectors =
        place._tag === 'BorrowedWritePlace'
          ? lowerBorrowedWriteSelectors(fn, place.selectors)
          : lowerWriteSelectors(fn, place.selectors)
      if (selectors === undefined) return undefined
      fn.emit(
        Object.freeze({
          _tag: 'CheckPlace',
          root,
          selectors,
          type,
          provenance: authored(place.span),
        }),
      )
      const value = lowerExpression(fn, expression.value)
      if (value === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ReadPlace',
          destination,
          root,
          selectors,
          type,
          consume: true,
          provenance: authored(expression.span),
        }),
      )
      fn.emit(
        Object.freeze({
          _tag: 'WritePlace',
          root,
          selectors,
          source: value.result,
          rootType,
          type,
          mutable: true,
          replacement: 'Copy',
          commit: 'AfterCleanup',
          provenance: authored(expression.span),
        }),
      )
      return { result: destination }
    }
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
      const call = fn.call(expression.span)
      const directType =
        directSection !== undefined
          ? directCallableSectionValueType(fn, directSection, expression.substitution)
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
      let typeArguments: ReadonlyArray<Type.GenericArgument> = Object.freeze([])
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
            call?.target.typeArguments ??
            directType.environment?.callable.typeArguments ??
            Object.freeze(
              [...expression.substitution.values()].map((argument) =>
                fn.semanticArgument(argument),
              ),
            )
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
      const type =
        call?.resultEffect === undefined
          ? fn.type(expression.type)
          : effectValueByIdentity(fn.layout, call.resultEffect)
      if (!lowered || type === undefined || callableType === undefined) return undefined
      const definition =
        callable === undefined ? undefined : fn.callableDefinitions.get(callable.ordinal)
      const realizedTarget = target ?? definition?.target
      if (
        realizedTarget?._tag === 'BuiltinCallableTarget' &&
        Scalar.isCheckedOperation(realizedTarget.operation) &&
        type._tag === 'Union'
      ) {
        const sourceScalar = Scalar.find(realizedTarget.actor)
        const valueScalar = Scalar.conversionTarget(realizedTarget.operation) ?? sourceScalar
        const scalarOperation = sourceScalar?.operations.find(
          (operation) => operation.code === realizedTarget.operation,
        )
        const realizedCaptures = definition?.captures ?? captures
        const ordered: Array<Mir.LocalId | undefined> = Array.from({
          length: scalarOperation?.arity ?? 0,
        })
        for (const capture of realizedCaptures) ordered[capture.parameterOrdinal] = capture.source
        for (const argument of arguments_) {
          const empty = ordered.indexOf(undefined)
          if (empty >= 0) ordered[empty] = argument
        }
        const operands = ordered.filter((operand): operand is Mir.LocalId => operand !== undefined)
        const first = operands.at(0)
        const sourceType = first === undefined ? undefined : fn.localTypes.at(first.ordinal)
        if (
          sourceScalar?.category !== 'Integer' ||
          valueScalar?.category !== 'Integer' ||
          scalarOperation === undefined ||
          operands.length !== scalarOperation.arity ||
          sourceType?._tag !== sourceScalar.spelling ||
          operands.some((operand) => fn.localTypes.at(operand.ordinal)?._tag !== sourceType._tag)
        )
          return undefined
        const success = Type.some(valueScalar.spelling)
        const failure = Type.none
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'CheckedInteger' as const,
            operation: scalarOperation.code,
            destination,
            operands: Object.freeze(operands),
            sourceType,
            valueType: Object.freeze({ _tag: valueScalar.spelling }),
            type,
            success,
            failure,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
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
          access: callableType.mode,
          evaluation: expression.evaluation,
          realization: callable === undefined ? 'DirectErasedSection' : expression.realization,
          type,
          provenance: authored(expression.span),
        }),
      )
      for (const capture of directSection?.captures ?? []) {
        if (capture.value._tag !== 'SliceBorrow' && capture.value._tag !== 'ValueBorrow') continue
        const borrow = fn.recipeBorrow(capture.value.borrow)
        const held = fn.loanLocals.get(borrowKey(borrow))
        if (held === undefined) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'EndLoan',
            borrow,
            slice: held,
            provenance: generated(expression.span),
          }),
        )
        fn.loanLocals.delete(borrowKey(borrow))
      }
      return Object.freeze({ result: destination })
    }
    case 'EffectConstruct': {
      const call = fn.call(expression.span)
      const typeArguments =
        call?.target.typeArguments ??
        expression.typeArguments.map((argument) => fn.semanticArgument(argument))
      const resultType =
        (call?.resultEffect === undefined
          ? undefined
          : effectValueByIdentity(fn.layout, call.resultEffect)) ??
        fn.effectResults.get(instanceText(expression.target, typeArguments))
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
          typeArguments: Object.freeze(typeArguments),
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
      for (const [ordinal, capture] of expression.captures.entries()) {
        const source =
          capture.binding === undefined
            ? capture.parameter === undefined
              ? undefined
              : fn.parameterLocals.get(capture.parameter.ordinal)
            : fn.bindingLocals.get(capture.binding.ordinal)
        if (source === undefined) return undefined
        const access = type.environment.fields.at(ordinal)?.access
        if (access === undefined) return undefined
        captures.push(Object.freeze({ source, access }))
      }
      const destination = fn.alloc(type)
      const runner = Hir.effectRunnerId(fn.owner.key.declaration, expression.site)
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
            candidate.specializationKey === baseRunnerKey(fn.owner.key, expression.site),
        )
      ) {
        fn.generatedRunners.push(
          Object.freeze({
            _tag: 'BlockEffectRunner',
            id: runner,
            owner: fn.owner,
            block: expression,
            type,
            specializationKey: baseRunnerKey(fn.owner.key, expression.site),
            providedRequirements: Object.freeze([]),
          }),
        )
      }
      return Object.freeze({ result: destination })
    }
    case 'EffectCatch':
      return lowerCatchEffectValue(fn, expression)
    case 'EffectResult':
      return undefined
    case 'Run': {
      return fn.withRecipeReplay(() => {
        const resultRecipe = effectRecipe(fn, expression.subject)
        if (resultRecipe?._tag === 'EffectCatch')
          return lowerEffectCatch(fn, resultRecipe, expression.span)
        if (resultRecipe?._tag === 'EffectResult') {
          const reified = lowerReifiedEffectRecipe(
            fn,
            resultRecipe.protected,
            expression.type,
            expression.span,
          )
          return reified === undefined ? undefined : Object.freeze({ result: reified.result })
        }
        if (
          resultRecipe !== undefined &&
          inlineForwardedRequirement(fn, resultRecipe) !== undefined
        )
          return lowerEffectExecution(fn, resultRecipe, expression.type, expression.span)
        if (
          resultRecipe?._tag === 'CallableApply' &&
          !Type.isEffect(fn.semantic(expression.type)) &&
          callableRecipe(fn, resultRecipe.callee) !== undefined
        )
          return lowerEffectExecution(fn, resultRecipe, expression.type, expression.span)
        if (resultRecipe?._tag === 'ServiceEffectConstruct')
          return lowerEffectExecution(fn, resultRecipe, expression.type, expression.span)
        const recipe = resultRecipe
        // Compiler-backed effects lower directly from their recipe. Lowering the effect expression
        // first would form every borrowed argument twice before the dedicated operation is emitted.
        const loweredSubject =
          recipe?._tag === 'BuiltinCall' && recipe.witnessEffectSite === undefined
            ? undefined
            : lowerExpression(fn, expression.subject)
        const effectValueType =
          loweredSubject === undefined ? undefined : fn.localTypes.at(loweredSubject.result.ordinal)
        if (loweredSubject !== undefined && effectValueType?._tag === 'EffectValue') {
          const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
            _tag: 'EffectOutcome',
            type: effectValueType.type,
          })
          const structuralSuccess = fn.semantic(expression.type)
          const successType = Type.isEffect(structuralSuccess)
            ? effectValueByIdentity(
                fn.layout,
                effectValueType.environment.successEffectIdentity ?? '',
              )
            : fn.type(expression.type)
          if (successType === undefined || successType._tag === 'EffectOutcome') return undefined
          const outcome = fn.alloc(outcomeType)
          const destination = fn.alloc(successType)
          const propagationType =
            Type.failureMembers(effectValueType.type).length === 0 || fn.effectOutcome === undefined
              ? undefined
              : fn.type(fn.effectOutcome)
          if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome')
            return undefined
          const tagMappings = Type.failureMembers(effectValueType.type).flatMap(
            (failure, source) => {
              const target =
                propagationType === undefined
                  ? undefined
                  : Type.failureMembers(propagationType.type).findIndex((candidate) =>
                      Type.equals(candidate, failure),
                    )
              return target === undefined || target < 0
                ? []
                : [Object.freeze({ source: source + 1, target: target + 1 })]
            },
          )
          if (tagMappings.length !== Type.failureMembers(effectValueType.type).length)
            return undefined
          const propagationShape =
            propagationType === undefined
              ? undefined
              : Layout.callingShape(fn.layout, propagationType.type)
          const releases = propagationReleases(fn, expression.span)
          const failureEnds = propagationLoanEnds(fn, expression.span)
          const provided = requirementsFor(fn.providedRequirements, effectValueType.type)
          const providedRunner =
            provided === undefined || provided.length === 0
              ? undefined
              : ensureProvidedRunner(fn, effectValueType, provided)
          if (provided !== undefined && provided.length > 0 && providedRunner === undefined)
            return undefined
          const baseRunner =
            effectValueType.storage?.realization.runner ??
            Hir.effectRunnerId(
              effectValueType.environment.instance.declaration,
              effectValueType.site,
            )
          const baseRunnerTypeArguments =
            effectValueType.storage?.realization.runnerArguments ??
            effectValueType.environment.instance.typeArguments
          fn.emit(
            Object.freeze({
              _tag: 'RunEffectValue',
              destination,
              outcome,
              effect: loweredSubject.result,
              runner: providedRunner ?? baseRunner,
              runnerTypeArguments: baseRunnerTypeArguments,
              ...(providedRunner === undefined
                ? {}
                : {
                    runnerBase: Object.freeze({
                      declaration: baseRunner,
                      typeArguments: baseRunnerTypeArguments,
                    }),
                  }),
              providers: providerBindings(provided),
              arguments: runtimeRequirementArguments(provided),
              outcomeType,
              ...(propagationType === undefined ? {} : { propagationType }),
              tagMappings: Object.freeze(tagMappings),
              propagationLaneCount: propagationShape?.laneCount ?? 0,
              ...(Type.failureMembers(effectValueType.type).length === 0 || failureEnds.length === 0
                ? {}
                : { failureLoanEnds: failureEnds }),
              ...(propagationType === undefined || releases.length === 0 ? {} : { releases }),
              type: successType,
              provenance: authored(expression.span),
            }),
          )
          endRunLoans(fn, expression.span)
          const storedBinding =
            expression.subject._tag === 'BindingReference'
              ? expression.subject.binding.ordinal
              : expression.subject._tag === 'Move' &&
                  expression.subject.subject._tag === 'BindingReference'
                ? expression.subject.subject.binding.ordinal
                : undefined
          if (storedBinding !== undefined) {
            endLoans(fn, fn.effectLoanEnds.get(storedBinding) ?? [], expression.span)
            fn.effectLoanEnds.delete(storedBinding)
          }
          if (expression.subject._tag === 'EffectConstruct')
            endLoans(fn, expression.subject.loanEnds, expression.span)
          return Object.freeze({ result: destination })
        }
        if (recipe?._tag === 'EffectBindRequirement' && recipe.protected._tag !== 'BuiltinCall')
          return lowerEffectExecution(fn, recipe, expression.type, expression.span)
        if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'EffectSuspend') {
          const deferred = recipe.arguments.at(0)
          return deferred === undefined
            ? undefined
            : lowerEffectExecution(fn, deferred, expression.type, expression.span)
        }
        if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'StorageAcquire') {
          const [layoutExpression] = recipe.arguments
          if (layoutExpression === undefined || fn.effectOutcome === undefined) return undefined
          const loweredLayout = lowerExpression(fn, layoutExpression)
          const type = fn.type(expression.type)
          const propagationType = fn.type(fn.effectOutcome)
          const failureTag = Type.failureMembers(fn.effectOutcome).findIndex((failure) =>
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
        if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'HostWrite') {
          const [streamExpression, bytesExpression] = recipe.arguments
          if (
            streamExpression === undefined ||
            bytesExpression === undefined ||
            fn.effectOutcome === undefined
          )
            return undefined
          const stream = lowerExpression(fn, streamExpression)
          const bytes = lowerExpression(fn, bytesExpression)
          const type = fn.type(expression.type)
          const propagationType = fn.type(fn.effectOutcome)
          const failureTag = Type.failureMembers(fn.effectOutcome).findIndex((failure) =>
            Type.equals(failure, Type.streamWriteFailure),
          )
          if (
            stream === undefined ||
            bytes === undefined ||
            type?._tag !== 'Nominal' ||
            !Type.equals(type.type, Type.unit) ||
            propagationType?._tag !== 'EffectOutcome' ||
            failureTag < 0
          )
            return undefined
          const destination = fn.alloc(type)
          fn.emit(
            Object.freeze({
              _tag: 'HostWrite' as const,
              destination,
              stream: stream.result,
              bytes: bytes.result,
              type,
              failure: Type.streamWriteFailure,
              propagationType,
              failureTag: failureTag + 1,
              provenance: authored(expression.span),
            }),
          )
          return Object.freeze({ result: destination })
        }
        if (recipe?._tag === 'BuiltinCall' && isOsOperation(recipe.operation)) {
          const arguments_: Array<Mir.LocalId> = []
          for (const argument of recipe.arguments) {
            const lowered = lowerExpression(fn, argument)
            if (lowered === undefined) return undefined
            arguments_.push(lowered.result)
          }
          const type = fn.type(expression.type)
          if (type === undefined) return undefined
          const destination = fn.alloc(type)
          fn.emit(
            Object.freeze({
              _tag: 'OsCall' as const,
              operation: recipe.intrinsic,
              destination,
              arguments: Object.freeze(arguments_),
              type,
              provenance: authored(expression.span),
            }),
          )
          for (const authored of recipe.loanEnds) {
            const borrow = fn.recipeBorrow(authored)
            const held = fn.loanLocals.get(borrowKey(borrow))
            if (held === undefined) continue
            fn.emit(
              Object.freeze({
                _tag: 'EndLoan' as const,
                borrow,
                slice: held,
                provenance: generated(expression.span),
              }),
            )
            fn.loanLocals.delete(borrowKey(borrow))
          }
          return Object.freeze({ result: destination })
        }
        if (recipe?._tag === 'EffectBindRequirement') {
          const provider =
            recipe.provider.binding !== undefined
              ? fn.bindingLocals.get(recipe.provider.binding.ordinal)
              : recipe.provider.parameter !== undefined
                ? fn.parameterLocals.get(recipe.provider.parameter.ordinal)
                : undefined
          if (provider === undefined) return undefined
          const selectedProvider = specializeProvider(fn, recipe.provider)
          if (selectedProvider === undefined) return undefined
          const loweredProtected = lowerExpression(fn, recipe.protected)
          const protectedType =
            loweredProtected === undefined
              ? undefined
              : fn.localTypes.at(loweredProtected.result.ordinal)
          if (loweredProtected === undefined || protectedType?._tag !== 'EffectValue')
            return undefined
          if (selectedProvider.witness._tag !== 'SourceConformanceWitness') {
            const result = lowerRunEffectValue(
              fn,
              loweredProtected.result,
              protectedType,
              expression.type,
              expression.span,
              Object.freeze([...fn.providedRequirements, selectedProvider]),
            )
            if (result !== undefined) {
              endRunLoans(fn, expression.span)
              if (recipe.protected._tag === 'EffectConstruct')
                endLoans(fn, recipe.protected.loanEnds, expression.span)
              if (recipe.provider.selectionAccess === 'Take')
                dropOwnedProvider(fn, provider, selectedProvider.providerType, recipe.provider.span)
            }
            return result
          }
          const providerAccess =
            recipe.provider.selectionAccess === 'Take'
              ? ('Exclusive' as const)
              : recipe.provider.selectionAccess
          const providerType = fn.type(selectedProvider.providerType)
          const referenceType = fn.type(
            Object.freeze({
              _tag: 'ReferenceType' as const,
              access: providerAccess,
              target: selectedProvider.providerType,
            }),
          )
          const loan = fn.ownership?.loans.find(
            (candidate) =>
              candidate.origin === 'EffectCapture' &&
              candidate.access === providerAccess &&
              candidate.startSpan.start === recipe.provider.span.start &&
              candidate.startSpan.end === recipe.provider.span.end,
          )
          const authoredBorrow =
            loan?.id ??
            (recipe.provider.selectionAccess === 'Take'
              ? fn.freshSyntheticBorrow(recipe.provider.span)
              : undefined)
          const borrow =
            authoredBorrow === undefined ? undefined : fn.beginRecipeBorrow(authoredBorrow)
          if (
            providerType?._tag !== 'Nominal' ||
            referenceType?._tag !== 'Reference' ||
            borrow === undefined
          )
            return undefined
          const reference = fn.alloc(referenceType)
          fn.emit(
            Object.freeze({
              _tag: 'BeginLoan',
              borrow,
              destination: reference,
              root: provider,
              selectors: Object.freeze([]),
              sourceType: providerType,
              type: referenceType,
              access: providerAccess,
              reborrow: false,
              suspendsParent: false,
              provenance: authored(recipe.provider.span),
            }),
          )
          fn.loanLocals.set(borrowKey(borrow), reference)
          const provided: ProvidedRequirement = Object.freeze({
            ...selectedProvider,
            local: reference,
          })
          const result = lowerRunEffectValue(
            fn,
            loweredProtected.result,
            protectedType,
            expression.type,
            expression.span,
            Object.freeze([...fn.providedRequirements, provided]),
          )
          if (result === undefined) return undefined
          const closeProviderLoan = fn.loanLocals.delete(borrowKey(borrow))
          endRunLoans(fn, expression.span)
          if (recipe.protected._tag === 'EffectConstruct')
            endLoans(fn, recipe.protected.loanEnds, expression.span)
          if (closeProviderLoan)
            fn.emit(
              Object.freeze({
                _tag: 'EndLoan',
                borrow,
                slice: reference,
                provenance: generated(recipe.provider.span),
              }),
            )
          if (recipe.provider.selectionAccess === 'Take')
            dropOwnedProvider(fn, provider, selectedProvider.providerType, recipe.provider.span)
          return result
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
        if (Type.failureMembers(recipe.type).length > 0) {
          const propagationType =
            fn.effectOutcome === undefined ? undefined : fn.type(fn.effectOutcome)
          const propagationShape =
            fn.effectOutcome === undefined
              ? undefined
              : Layout.callingShape(fn.layout, fn.effectOutcome)
          if (propagationType?._tag !== 'EffectOutcome' || propagationShape === undefined)
            return undefined
          const tagMappings = Type.failureMembers(recipe.type).flatMap((failure, source) => {
            const target = Type.failureMembers(propagationType.type).findIndex((candidate) =>
              Type.equals(candidate, failure),
            )
            return target < 0 ? [] : [Object.freeze({ source: source + 1, target: target + 1 })]
          })
          if (tagMappings.length !== Type.failureMembers(recipe.type).length) return undefined
          const failureEnds = propagationLoanEnds(fn, expression.span)
          const releases = propagationReleases(fn, expression.span)
          fn.emit(
            Object.freeze({
              _tag: 'RunEffect',
              destination,
              outcome,
              target: recipe.target,
              typeArguments: Object.freeze(
                recipe.typeArguments.map((argument) => fn.semanticArgument(argument)),
              ),
              arguments: Object.freeze(arguments_),
              outcomeType,
              propagationType,
              ...(failureEnds.length === 0 ? {} : { failureLoanEnds: failureEnds }),
              ...(releases.length === 0 ? {} : { releases }),
              tagMappings: Object.freeze(tagMappings),
              propagationLaneCount: propagationShape.laneCount,
              type: successType,
              provenance: authored(expression.span),
            }),
          )
          // The effect held its argument borrows for exactly this run; end them here.
          for (const authored of recipe.loanEnds) {
            const borrow = fn.recipeBorrow(authored)
            const held = fn.loanLocals.get(borrowKey(borrow))
            if (held === undefined) continue
            fn.emit(
              Object.freeze({
                _tag: 'EndLoan',
                borrow,
                slice: held,
                provenance: generated(expression.span),
              }),
            )
            fn.loanLocals.delete(borrowKey(borrow))
          }
          return Object.freeze({ result: destination })
        }
        fn.emit(
          Object.freeze({
            _tag: 'Call',
            destination: outcome,
            target: recipe.target,
            typeArguments: Object.freeze(
              recipe.typeArguments.map((argument) => fn.semanticArgument(argument)),
            ),
            arguments: Object.freeze(arguments_),
            type: outcomeType,
            provenance: authored(expression.span),
          }),
        )
        for (const authored of recipe.loanEnds) {
          const borrow = fn.recipeBorrow(authored)
          const held = fn.loanLocals.get(borrowKey(borrow))
          if (held === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan',
              borrow,
              slice: held,
              provenance: generated(expression.span),
            }),
          )
          fn.loanLocals.delete(borrowKey(borrow))
        }
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
      })
    }
    case 'UnionConvert': {
      const source = lowerExpression(fn, expression.source)
      // Effect access is a semantic ownership coercion. Hidden construction identity has already
      // selected one concrete EffectValue layout, so the runtime representation is unchanged.
      if (expression.conversion === 'EffectAccess') return source
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
      // Canonical union member order can change under substitution (parameter keys sort
      // differently from concrete keys), so the mapping recomputes at the instantiation.
      const substituted = TypeCompatibility.check(
        fn.semantic(expression.sourceType),
        fn.semantic(expression.target),
      )
      const mappings =
        substituted._tag === 'Inject' || substituted._tag === 'Widen'
          ? substituted.mappings
          : expression.mappings
      fn.emit(
        Object.freeze({
          _tag: 'ConvertUnion',
          destination,
          source: source.result,
          sourceType,
          targetType,
          conversion: expression.conversion,
          mappings,
          sourceShape,
          targetShape,
          access: expression.access,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'ShortCircuit': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'bool') return undefined
      const left = lowerExpression(fn, expression.left)
      if (left === undefined) return undefined
      // The right operand's operations stay nested so that the engines can emit them under the
      // branch instead of before it. It is pure by elaboration, so nothing there needs releasing
      // on the path that skips it.
      const [right, rightOperations] = fn.capture(() => lowerExpression(fn, expression.right))
      if (right === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ShortCircuit',
          operator: expression.operator,
          destination,
          left: left.result,
          right: Object.freeze({ operations: rightOperations, result: right.result }),
          type,
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
      const armStates = new Map<number, DelayedEffectState>()
      const branchState = delayedEffectState(fn)
      for (const arm of expression.arms) {
        if (!arm.reachable) continue
        restoreDelayedEffectState(fn, branchState)
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
        armStates.set(arm.id.ordinal, delayedEffectState(fn))
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
      restoreDelayedEffectState(fn, branchState)
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
      for (const key of branchState.loanLocals.keys()) {
        const endedOnEveryPath = decisions.every((decision) => {
          const candidates = decision.candidates.flatMap((candidate) => {
            const arm = arms.find((entry) => entry.id.ordinal === candidate.ordinal)
            const armState = armStates.get(candidate.ordinal)
            return arm === undefined || armState === undefined ? [] : [{ arm, armState }]
          })
          return (
            candidates.length === decision.candidates.length &&
            candidates.some(({ arm }) => arm.guard === undefined) &&
            candidates.every(({ armState }) => !armState.loanLocals.has(key))
          )
        })
        if (endedOnEveryPath) fn.loanLocals.delete(key)
      }
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
      const representation = Layout.entry(fn.layout, type.type)?.representation
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
        const declared =
          representation?._tag === 'Aggregate'
            ? representation.fields.find(
                (candidate) =>
                  candidate.id.ordinal === field.field.ordinal &&
                  candidate.id.struct.sourceId === field.field.struct.sourceId &&
                  candidate.id.struct.ordinal === field.field.struct.ordinal,
              )
            : undefined
        const stored =
          declared === undefined
            ? undefined
            : (storedCallableValueType(fn.layout, declared.type)?.storage ??
              storedEffectValueType(fn.layout, declared.type)?.storage)
        return value === undefined
          ? []
          : [
              Object.freeze({
                field: field.field,
                value,
                ...(stored === undefined ? {} : { stored }),
              }),
            ]
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
          : expression.root._tag === 'ParameterSliceRoot'
            ? local(expression.root.parameter.ordinal)
            : fn.patternLocals.get(patternKey(expression.root.binding))
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
      const borrow = fn.beginRecipeBorrow(expression.borrow)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination,
          root,
          selectors: Object.freeze([]),
          sourceType,
          type,
          access: expression.access,
          reborrow: expression.reborrow,
          suspendsParent: expression.suspendsParent,
          provenance: authored(expression.span),
        }),
      )
      fn.loanLocals.set(borrowKey(borrow), destination)
      return Object.freeze({ result: destination })
    }
    case 'ValueBorrow': {
      const root =
        expression.root._tag === 'BindingSliceRoot'
          ? fn.bindingLocals.get(expression.root.binding.ordinal)
          : expression.root._tag === 'ParameterSliceRoot'
            ? local(expression.root.parameter.ordinal)
            : fn.patternLocals.get(patternKey(expression.root.binding))
      const sourceType = fn.type(expression.source)
      const type = fn.type(expression.type)
      if (root === undefined || sourceType === undefined || type?._tag !== 'Reference') {
        return undefined
      }
      const destination = fn.alloc(type)
      const borrow = fn.beginRecipeBorrow(expression.borrow)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination,
          root,
          selectors: Object.freeze(
            expression.path.map((field) =>
              Object.freeze({
                _tag: 'FieldSelector' as const,
                field,
                provenance: authored(expression.span),
              }),
            ),
          ),
          sourceType,
          type,
          access: expression.access,
          reborrow: false,
          suspendsParent: false,
          provenance: authored(expression.span),
        }),
      )
      fn.loanLocals.set(borrowKey(borrow), destination)
      return Object.freeze({ result: destination })
    }
    case 'SliceLength': {
      const slice = lowerExpression(fn, expression.slice)
      const sliceType = slice === undefined ? undefined : fn.localTypes.at(slice.result.ordinal)
      if (
        slice === undefined ||
        sliceType === undefined ||
        !Type.isSlice(Mir.semanticType(sliceType))
      ) {
        return undefined
      }
      const destination = fn.alloc(usize)
      fn.emit(
        Object.freeze({
          _tag: 'SliceLength',
          destination,
          slice: slice.result,
          type: usize,
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
      const call = fn.call(expression.span)
      const type =
        (call?.resultEffect === undefined
          ? undefined
          : effectValueByIdentity(fn.layout, call.resultEffect)) ?? fn.type(expression.type)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          typeArguments: Object.freeze(
            call?.target.typeArguments ??
              expression.typeArguments.map((argument) => fn.semanticArgument(argument)),
          ),
          arguments: Object.freeze(argumentLocals),
          type,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      for (const authored of expression.loanEnds) {
        const borrow = fn.recipeBorrow(authored)
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
    case 'BoundOperationCall': {
      if (expression.witnessEffectSite !== undefined) return lowerWitnessEffect(fn, expression)
      // The bound named the operation; the specialization names the witness. Only here is the type
      // argument known, so only here can the conformance say which compiler-known operation the
      // call runs — two providers of one interface may answer one operation with two unrelated
      // instructions, and an operator's width-neutral lowering cannot stand in for that.
      const capability = fn.semantic(expression.capability)
      const provider = fn.semantic(expression.provider)
      if (!Type.isNominal(capability)) return undefined
      const selected = DeclarationIndex.interfaceOperationIntrinsic(
        fn.index,
        provider,
        capability,
        expression.operation,
      )
      // A witness names either a sealed intrinsic or a function of the provider's own actor, and
      // which one it names is the conformance's business rather than the call's. The operator path
      // has always read both; this call reads the second one here.
      if (selected?.rule._tag !== 'BuiltinRule') {
        const argumentLocals: Array<Mir.LocalId> = []
        for (const argument of expression.arguments) {
          const lowered = lowerExpression(fn, argument)
          if (lowered === undefined) return undefined
          argumentLocals.push(lowered.result)
        }
        const result = lowerBoundWitnessCall(fn, expression, provider, capability, argumentLocals)
        if (result === undefined) return undefined
        for (const authored of expression.loanEnds) {
          const borrow = fn.recipeBorrow(authored)
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
        return Object.freeze({ result })
      }
      // The call the witness names, at this call's own span: the enclosing `lowerExpression` ends
      // this span's returned-view loans once, after the operation it selected has been lowered.
      return lowerExpressionInner(
        fn,
        Object.freeze({
          _tag: 'BuiltinCall',
          operation: selected.rule.operation,
          intrinsic: selected.id,
          typeArguments: Object.freeze([]),
          arguments: expression.arguments,
          intrinsicWitnessOperands: expression.contract.operands,
          loanEnds: expression.loanEnds,
          heldLoans: Object.freeze([]),
          type: expression.type,
          span: expression.span,
        }),
      )
    }
    case 'BuiltinCall': {
      if (expression.witnessEffectSite !== undefined) return lowerWitnessEffect(fn, expression)
      const argumentLocals: Array<Mir.LocalId> = []
      for (const [ordinal, argument] of expression.arguments.entries()) {
        const lowered =
          expression.intrinsicWitnessOperands === undefined
            ? lowerExpression(fn, argument)
            : lowerIntrinsicWitnessOperand(
                fn,
                argument,
                expression.intrinsicWitnessOperands.at(ordinal),
              )
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      const finishBuiltin = (result: Mir.LocalId): { readonly result: Mir.LocalId } => {
        const slot = argumentLocals.at(0)
        const inherited =
          expression.operation === 'SlotWrite' ||
          expression.operation === 'SlotTake' ||
          expression.operation === 'SlotCopy' ||
          expression.operation === 'SlotDrop'
            ? slot === undefined
              ? []
              : (fn.slotLoans.get(slot.ordinal) ?? [])
            : []
        const endings = new Map(
          [...expression.loanEnds, ...inherited].map((authored) => {
            const borrow = fn.recipeBorrow(authored)
            return [borrowKey(borrow), borrow] as const
          }),
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
      const witnessCall = lowerInterfaceWitnessCall(fn, expression, argumentLocals)
      if (witnessCall !== undefined) return finishBuiltin(witnessCall)
      if (expression.operation === 'LayoutOf') {
        const raw = expression.typeArguments.at(0)
        const element = raw === undefined ? undefined : fn.semantic(raw)
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
      if (
        expression.operation === 'EffectSuspend' ||
        expression.operation === 'StorageAcquire' ||
        expression.operation === 'HostWrite'
      )
        return undefined
      if (expression.operation === 'RawBufferFrom') {
        const [allocation, count] = argumentLocals
        const type = fn.type(expression.type)
        const raw = Type.isRawBuffer(expression.type)
          ? Type.typeArgumentAt(expression.type, 0)
          : undefined
        const element = raw === undefined ? undefined : fn.semantic(raw)
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
      if (expression.operation === 'RawBufferRead') {
        const [buffer, index] = argumentLocals
        const type = fn.type(expression.type)
        if (buffer === undefined || index === undefined || type === undefined) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferRead' as const,
            destination,
            buffer,
            index,
            element: fn.semantic(expression.type),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferView' || expression.operation === 'RawBufferViewMut') {
        const [buffer, offset, length] = argumentLocals
        const type = fn.type(expression.type)
        const element = Type.isSlice(expression.type) ? expression.type.element : undefined
        const semanticElement = element === undefined ? undefined : fn.semantic(element)
        const elementLayout =
          semanticElement === undefined ? undefined : Layout.entry(fn.layout, semanticElement)
        if (
          buffer === undefined ||
          offset === undefined ||
          length === undefined ||
          type?._tag !== 'Slice' ||
          semanticElement === undefined ||
          elementLayout === undefined
        ) {
          return undefined
        }
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferView' as const,
            destination,
            buffer,
            offset,
            length,
            element: semanticElement,
            stride:
              Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
            access: expression.operation === 'RawBufferView' ? 'Shared' : 'Exclusive',
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferSlot') {
        const [buffer, index] = argumentLocals
        const type = fn.type(expression.type)
        const element = Type.isSlot(expression.type)
          ? Type.typeArgumentAt(expression.type, 0)
          : undefined
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
            element: fn.semantic(element),
            type,
            provenance: authored(expression.span),
          }),
        )
        fn.slotLoans.set(destination.ordinal, expression.heldLoans)
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferCopy') {
        const [buffer, offset, source, length] = argumentLocals
        const sourceArgument = expression.arguments.at(2)
        const sourceType = sourceArgument?._tag === 'Unavailable' ? undefined : sourceArgument?.type
        const element =
          sourceType !== undefined && Type.isSlice(sourceType) ? sourceType.element : undefined
        const semanticElement = element === undefined ? undefined : fn.semantic(element)
        const elementLayout =
          semanticElement === undefined ? undefined : Layout.entry(fn.layout, semanticElement)
        const type = fn.type(expression.type)
        if (
          buffer === undefined ||
          offset === undefined ||
          source === undefined ||
          length === undefined ||
          semanticElement === undefined ||
          elementLayout === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferCopy' as const,
            destination,
            buffer,
            offset,
            source,
            length,
            element: semanticElement,
            stride:
              Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
            retainsSource: Mir.isStructurallyCopy(fn.layout, semanticElement),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferFill') {
        const [buffer, offset, length, value] = argumentLocals
        const type = fn.type(expression.type)
        if (
          buffer === undefined ||
          offset === undefined ||
          length === undefined ||
          value === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferFill' as const,
            destination,
            buffer,
            offset,
            length,
            value,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotWrite') {
        const [slot, value] = argumentLocals
        const slotArgument = expression.arguments.at(0)
        const slotType = slotArgument?._tag === 'Unavailable' ? undefined : slotArgument?.type
        const slotElement =
          slotType !== undefined && Type.isSlot(slotType)
            ? Type.typeArgumentAt(slotType, 0)
            : undefined
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
            element: fn.semantic(slotElement),
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
            element: fn.semantic(expression.type),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotCopy') {
        const [slot] = argumentLocals
        const type = fn.type(expression.type)
        if (slot === undefined || type === undefined) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotCopy' as const,
            destination,
            slot,
            element: fn.semantic(expression.type),
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
          slotType !== undefined && Type.isSlot(slotType)
            ? Type.typeArgumentAt(slotType, 0)
            : undefined
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
            element: fn.semantic(element),
            cleanup: concreteCleanup(fn, fn.semantic(element)),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (isOsOperation(expression.operation)) return undefined
      if (expression.operation === 'StringFromUtf8Unchecked') return undefined
      if (expression.operation === 'StringUtf8Bytes') {
        const [string] = argumentLocals
        const stringType = string === undefined ? undefined : fn.localTypes.at(string.ordinal)
        const type = fn.type(expression.type)
        if (string === undefined || stringType?._tag !== 'String' || type?._tag !== 'Slice')
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'StringUtf8Bytes',
            destination,
            string,
            heldLoans: expression.heldLoans,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'StringByteLength') {
        const [string] = argumentLocals
        const stringType = string === undefined ? undefined : fn.localTypes.at(string.ordinal)
        if (string === undefined || stringType?._tag !== 'String') return undefined
        const destination = fn.alloc(usize)
        fn.emit(
          Object.freeze({
            _tag: 'StringByteLength',
            destination,
            string,
            type: usize,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'StringEqualsExact') return undefined
      const conversionTarget = Scalar.conversionTarget(expression.operation)
      if (Scalar.isCheckedOperation(expression.operation)) {
        const [first] = argumentLocals
        const sourceType = first === undefined ? undefined : fn.localTypes.at(first.ordinal)
        const semanticSource = sourceType === undefined ? undefined : Mir.semanticType(sourceType)
        const sourceScalar =
          typeof semanticSource === 'string' ? Scalar.find(semanticSource) : undefined
        const valueScalar = conversionTarget ?? sourceScalar
        const targetType = fn.type(expression.type)
        if (
          first === undefined ||
          sourceScalar?.category !== 'Integer' ||
          valueScalar?.category !== 'Integer' ||
          sourceType?._tag !== sourceScalar.spelling ||
          targetType?._tag !== 'Union' ||
          argumentLocals.some((local) => fn.localTypes.at(local.ordinal)?._tag !== sourceType._tag)
        )
          return undefined
        const success = Type.some(valueScalar.spelling)
        const failure = Type.none
        if (
          !targetType.type.members.some((member) => Type.equals(member, success)) ||
          !targetType.type.members.some((member) => Type.equals(member, failure))
        )
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'CheckedInteger' as const,
            operation: expression.operation,
            destination,
            operands: Object.freeze(argumentLocals),
            sourceType,
            valueType: Object.freeze({ _tag: valueScalar.spelling }),
            type: targetType,
            success,
            failure,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (conversionTarget !== undefined) {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const semanticSource = sourceType === undefined ? undefined : Mir.semanticType(sourceType)
        const sourceScalar =
          typeof semanticSource === 'string' ? Scalar.find(semanticSource) : undefined
        const targetType = fn.type(expression.type)
        if (
          source === undefined ||
          (sourceScalar?.category !== 'Integer' && sourceScalar?.category !== 'Floating') ||
          sourceType?._tag !== sourceScalar.spelling ||
          targetType?._tag !== conversionTarget.spelling
        )
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag:
              sourceScalar.category === 'Integer'
                ? ('ConvertInteger' as const)
                : ('ConvertScalar' as const),
            destination,
            source,
            sourceType,
            type: targetType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      const floatConversionTarget = Scalar.floatConversionTarget(expression.operation)
      if (floatConversionTarget !== undefined) {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const semanticSource = sourceType === undefined ? undefined : Mir.semanticType(sourceType)
        const sourceScalar =
          typeof semanticSource === 'string' ? Scalar.find(semanticSource) : undefined
        const targetType = fn.type(expression.type)
        if (
          source === undefined ||
          sourceScalar === undefined ||
          sourceScalar.category === 'Boolean' ||
          sourceType?._tag !== sourceScalar.spelling ||
          targetType?._tag !== floatConversionTarget.spelling
        )
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'ConvertScalar' as const,
            destination,
            source,
            sourceType,
            type: targetType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'ToBits' || expression.operation === 'FromBits') {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const targetType = fn.type(expression.type)
        if (source === undefined || sourceType === undefined || targetType === undefined)
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'ReinterpretScalar' as const,
            destination,
            source,
            sourceType: sourceType as Mir.ScalarType,
            type: targetType as Mir.ScalarType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'Sin' || expression.operation === 'Cos') {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const targetType = fn.type(expression.type)
        if (source === undefined || sourceType === undefined || targetType === undefined)
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'FloatTranscendental' as const,
            operation: expression.operation,
            destination,
            source,
            sourceType: sourceType as Mir.ScalarType,
            type: targetType as Mir.ScalarType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (
        expression.operation === 'IsNaN' ||
        expression.operation === 'IsInfinite' ||
        expression.operation === 'IsFinite' ||
        expression.operation === 'IsNormal' ||
        expression.operation === 'IsSubnormal' ||
        expression.operation === 'IsSignNegative' ||
        expression.operation === 'Sqrt' ||
        (expression.operation === 'Negate' &&
          argumentLocals.some((local) => {
            const type = fn.localTypes.at(local.ordinal)
            const semantic = type === undefined ? undefined : Mir.semanticType(type)
            return typeof semantic === 'string' && Scalar.find(semantic)?.category === 'Floating'
          }))
      ) {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const targetType = fn.type(expression.type)
        if (source === undefined || sourceType === undefined || targetType === undefined)
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'FloatUnary' as const,
            operation: expression.operation,
            destination,
            source,
            sourceType: sourceType as Mir.ScalarType,
            type: targetType as Mir.ScalarType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (
        expression.operation === 'Not' ||
        expression.operation === 'Negate' ||
        expression.operation === 'BitNot' ||
        expression.operation === 'WrappingNegate' ||
        expression.operation === 'SaturatingNegate'
      ) {
        const [subject] = argumentLocals
        if (subject === undefined) return undefined
        const operandType = fn.localTypes.at(subject.ordinal)
        if (operandType === undefined) return undefined
        const semanticOperand = Mir.semanticType(operandType)
        const scalar =
          typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
        if (expression.operation !== 'Not' && scalar?.category !== 'Integer') return undefined
        const pointerBits = fn.layout.target.pointerSize === 4 ? 32 : 64
        const constant =
          expression.operation === 'BitNot' && scalar?.category === 'Integer'
            ? scalar.signedness === 'Signed'
              ? -1n
              : Scalar.range(scalar, pointerBits).maximum
            : 0n
        const zero = fn.alloc(operandType)
        fn.emit(
          Object.freeze({
            _tag: 'Literal',
            destination: zero,
            type: operandType,
            value: constant,
            provenance: Object.freeze({ span: expression.span, generated: true }),
          }),
        )
        const destination = fn.alloc(operandType)
        fn.emit(
          Object.freeze({
            _tag: 'Binary',
            operator:
              expression.operation === 'Not'
                ? 'Equals'
                : expression.operation === 'BitNot'
                  ? 'BitXor'
                  : expression.operation === 'WrappingNegate'
                    ? 'WrappingSubtract'
                    : expression.operation === 'SaturatingNegate'
                      ? 'SaturatingSubtract'
                      : 'Subtract',
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
  const specialized = Type.substitute(type, fn.substitution)
  const resolveRepresented = (candidate: Type.Type): Ownership.CleanupPlan | undefined => {
    const concrete = Type.substitute(candidate, fn.substitution)
    if (!Type.isRepresented(concrete)) return undefined
    const value =
      storedCallableValueType(fn.layout, concrete) ??
      storedEffectValueType(fn.layout, concrete) ??
      representedValueType(fn.layout, fn.opaqueRealizations, concrete, new Map())
    return value?._tag === 'CallableValue'
      ? value.storage?._tag === 'StoredCallableField'
        ? Ownership.realizedCallableCleanup(fn.index, value.storage.realization)
        : callableLocalCleanup(fn, value)
      : value?._tag === 'EffectValue'
        ? effectLocalCleanup(fn, value, new Set())
        : undefined
  }
  const realized = resolveRepresented(specialized)
  if (realized !== undefined) return realized
  return Ownership.specializeCleanup(
    Ownership.cleanupPlan(fn.index, specialized, seen),
    new Map(),
    (nested) => resolveRepresented(nested) ?? Ownership.cleanupPlan(fn.index, nested, seen),
  )
}

function effectLocalCleanup(
  fn: FunctionLowering,
  effectValue: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  seen: ReadonlySet<string>,
): Ownership.CleanupPlan {
  const identity =
    effectValue.storage?.realization.runnerIdentity ??
    Instances.effectIdentity(effectValue.environment.instance, effectValue.site)
  if (seen.has(identity)) return Object.freeze({ _tag: 'NoCleanup', type: effectValue.type })
  const next = new Set(seen).add(identity)
  let laneOffset = 0
  const slots = effectValue.environment.fields
    .flatMap((field, ordinal) => {
      const nested =
        field.effectIdentity === undefined
          ? undefined
          : effectValueByIdentity(fn.layout, field.effectIdentity)
      const callable =
        field.callableIdentity === undefined || !Type.isCallable(field.type)
          ? undefined
          : callableValueByIdentity(fn.layout, field.callableIdentity, field.type)
      const laneCount =
        field.representation === 'Borrow'
          ? 1
          : callable === undefined
            ? nested === undefined
              ? (Layout.callingShape(fn.layout, field.type)?.laneCount ?? 0)
              : Layout.effectEnvironmentLanes(fn.layout, nested.environment).length
            : callable.environment === undefined
              ? 0
              : Layout.callableEnvironmentLanes(fn.layout, callable.environment).length
      const currentOffset = laneOffset
      laneOffset += laneCount
      const realizationOrdinal =
        effectValue.storage?.realization.environment.at(ordinal)?.ordinal ?? ordinal
      const storedOwned =
        effectValue.storage?.realization.cleanup.unrunLanes.includes(realizationOrdinal) ?? false
      if (effectValue.storage === undefined ? field.representation === 'Borrow' : !storedOwned)
        return []
      const fieldCleanup =
        callable === undefined
          ? nested === undefined
            ? concreteCleanup(fn, field.type)
            : effectLocalCleanup(fn, nested, next)
          : callableLocalCleanup(fn, callable)
      return fieldCleanup._tag === 'NoCleanup' && effectValue.storage === undefined
        ? []
        : [
            Object.freeze({
              ordinal: realizationOrdinal,
              laneOffset: currentOffset,
              laneCount,
              cleanup: fieldCleanup,
            }),
          ]
    })
    .reverse()
  return slots.length === 0
    ? Object.freeze({ _tag: 'NoCleanup', type: effectValue.type })
    : Object.freeze({
        _tag: 'EffectCleanup',
        type: effectValue.type,
        site: effectValue.site,
        slots: Object.freeze(slots),
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
  if (localType._tag === 'EffectValue') {
    return effectLocalCleanup(fn, localType, new Set())
  }
  if (localType._tag !== 'CallableValue') {
    return specialized
  }
  if (localType.storage === undefined) return callableLocalCleanup(fn, localType)
  if (specialized._tag !== 'CallableCleanup') return specialized
  const fields = localType.environment?.fields ?? []
  return Object.freeze({
    _tag: 'CallableCleanup',
    type: localType.type,
    environment:
      localType.environment === undefined
        ? specialized.environment
        : Object.freeze({
            _tag: 'CallableEnvironmentIdentity',
            identity: Instances.callableEnvironmentIdentity(localType.environment.callable),
          }),
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

/**
 * The Drop operations a propagating failure must execute before it leaves this function:
 * every owner the ownership phase saw live at the run site, resolved to this function's
 * locals. Sites without a local here belong to a different compiled body and are skipped.
 */
const propagationReleases = (
  fn: FunctionLowering,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Mir.DropOperation> => {
  const exit = fn.ownership?.exits.find(
    (candidate) =>
      candidate.kind === 'Propagation' &&
      candidate.span.start === span.start &&
      candidate.span.end === span.end,
  )
  if (exit === undefined) return Object.freeze([])
  return Object.freeze(
    exit.releases.flatMap((release): ReadonlyArray<Mir.DropOperation> => {
      if (release.cleanup._tag === 'NoCleanup') return []
      const site = release.binding.site
      const local =
        site._tag === 'Let'
          ? fn.bindingLocals.get(site.binding.ordinal)
          : site._tag === 'Parameter'
            ? fn.parameterLocals.get(site.parameter.ordinal)
            : undefined
      const localType = local === undefined ? undefined : fn.localTypes.at(local.ordinal)
      if (local === undefined || localType === undefined) return []
      return [
        Object.freeze({
          _tag: 'Drop' as const,
          local,
          cleanup: cleanupForLocal(fn, release.binding.cleanup, localType),
          provenance: generated(span),
        }),
      ]
    }),
  )
}

/**
 * The caller-owned loans live at a run site. A typed failure exits before the success-path
 * operations that normally end them, so the run carries an explicit failure-only cleanup path.
 */
const dependencyOrderedLoanEndings = (
  fn: FunctionLowering,
  endings: ReadonlyArray<Mir.EndLoanOperation>,
): ReadonlyArray<Mir.EndLoanOperation> => {
  const depth = (key: string): number => {
    let current = fn.loanParents.get(key)
    const seen = new Set<string>([key])
    let result = 0
    while (current !== undefined && !seen.has(current)) {
      seen.add(current)
      result += 1
      current = fn.loanParents.get(current)
    }
    return result
  }
  return Object.freeze(
    [...endings].sort((left, right) => {
      const leftKey = borrowKey(left.borrow)
      const rightKey = borrowKey(right.borrow)
      return depth(rightKey) - depth(leftKey) || leftKey.localeCompare(rightKey)
    }),
  )
}

const propagationLoanEnds = (
  fn: FunctionLowering,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Mir.EndLoanOperation> =>
  dependencyOrderedLoanEndings(
    fn,
    [...fn.loanLocals.entries()].flatMap(([key, slice]): ReadonlyArray<Mir.EndLoanOperation> => {
      const borrow = fn.loanIds.get(key)
      return borrow === undefined
        ? []
        : [
            Object.freeze({
              _tag: 'EndLoan' as const,
              borrow,
              slice,
              provenance: generated(span),
            }),
          ]
    }),
  )

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
    environment: Object.freeze({
      _tag: 'CallableEnvironmentIdentity',
      identity: Instances.callableEnvironmentIdentity(environment.callable),
    }),
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
  if (exit?.kind === 'Return') {
    endLoans(
      fn,
      (fn.ownership?.loans ?? []).map((loan) => loan.id),
      exit.span,
    )
  }
  for (const release of exit?.releases ?? []) {
    if (release.binding.site._tag !== 'Let') continue
    const ordinal = release.binding.site.binding.ordinal
    endLoans(fn, fn.effectLoanEnds.get(ordinal) ?? [], exit?.span ?? release.binding.liveTo)
    fn.effectLoanEnds.delete(ordinal)
  }
  for (const release of exit?.releases ?? []) {
    const site = release.binding.site
    const dropped =
      site._tag === 'Parameter'
        ? fn.parameterLocals.get(site.parameter.ordinal)
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

const withoutLoanEndings = (
  operations: ReadonlyArray<Mir.Operation>,
  loans: ReadonlySet<string>,
): ReadonlyArray<Mir.Operation> =>
  Object.freeze(
    operations.flatMap((operation): ReadonlyArray<Mir.Operation> => {
      if (operation._tag === 'EndLoan' && loans.has(borrowKey(operation.borrow))) return []
      if (operation._tag === 'ShortCircuit')
        return [
          Object.freeze({
            ...operation,
            right: Object.freeze({
              ...operation.right,
              operations: withoutLoanEndings(operation.right.operations, loans),
            }),
          }),
        ]
      if (operation._tag === 'Match')
        return [
          Object.freeze({
            ...operation,
            arms: Object.freeze(
              operation.arms.map((arm) =>
                Object.freeze({
                  ...arm,
                  ...(arm.guard === undefined
                    ? {}
                    : {
                        guard: Object.freeze({
                          ...arm.guard,
                          operations: withoutLoanEndings(arm.guard.operations, loans),
                        }),
                      }),
                  selected: Object.freeze({
                    ...arm.selected,
                    operations: withoutLoanEndings(arm.selected.operations, loans),
                  }),
                }),
              ),
            ),
          }),
        ]
      return [operation]
    }),
  )

interface DelayedLoopLoan {
  readonly key: string
  readonly borrow: Hir.BorrowId
  readonly slice: Mir.LocalId
}

const delayedLoopLoans = (
  fn: FunctionLowering,
  keys: ReadonlySet<string>,
  entry: DelayedEffectState,
): ReadonlyArray<DelayedLoopLoan> => {
  return Object.freeze(
    [...keys].flatMap((key): ReadonlyArray<DelayedLoopLoan> => {
      const borrow = fn.loanIds.get(key)
      const slice = entry.loanLocals.get(key)
      return borrow === undefined || slice === undefined
        ? []
        : [Object.freeze({ key, borrow, slice })]
    }),
  )
}

const terminalLoopLoanEndings = (
  loans: ReadonlyArray<DelayedLoopLoan>,
  outcome: Extract<Mir.Outcome, { readonly _tag: 'Return' | 'Trap' }>,
): ReadonlyArray<Extract<Mir.Operation, { readonly _tag: 'EndLoan' }>> =>
  Object.freeze(
    loans.map((loan) =>
      Object.freeze({
        _tag: 'EndLoan' as const,
        borrow: loan.borrow,
        slice: loan.slice,
        provenance: generated(outcome.provenance.span),
      }),
    ),
  )

/** Adds loop-entry loans to failure exits after their authored success endings move past the loop. */
const withDelayedFailureLoanEndings = (
  fn: FunctionLowering,
  operations: ReadonlyArray<Mir.Operation>,
  loans: ReadonlyArray<DelayedLoopLoan>,
): ReadonlyArray<Mir.Operation> =>
  Object.freeze(
    operations.map((operation): Mir.Operation => {
      if (operation._tag === 'ShortCircuit')
        return Object.freeze({
          ...operation,
          right: Object.freeze({
            ...operation.right,
            operations: withDelayedFailureLoanEndings(fn, operation.right.operations, loans),
          }),
        })
      if (operation._tag === 'Match')
        return Object.freeze({
          ...operation,
          arms: Object.freeze(
            operation.arms.map((arm) =>
              Object.freeze({
                ...arm,
                ...(arm.guard === undefined
                  ? {}
                  : {
                      guard: Object.freeze({
                        ...arm.guard,
                        operations: withDelayedFailureLoanEndings(fn, arm.guard.operations, loans),
                      }),
                    }),
                selected: Object.freeze({
                  ...arm.selected,
                  operations: withDelayedFailureLoanEndings(fn, arm.selected.operations, loans),
                }),
              }),
            ),
          ),
        })
      if (
        (operation._tag !== 'RunEffect' &&
          operation._tag !== 'RunEffectValue' &&
          operation._tag !== 'RunStaticEffect') ||
        Type.failureMembers(operation.outcomeType.type).length === 0
      )
        return operation
      const existing = new Set(
        (operation.failureLoanEnds ?? []).map((ending) => borrowKey(ending.borrow)),
      )
      const appended = loans.flatMap(
        (loan): ReadonlyArray<Mir.EndLoanOperation> =>
          existing.has(loan.key)
            ? []
            : [
                Object.freeze({
                  _tag: 'EndLoan' as const,
                  borrow: loan.borrow,
                  slice: loan.slice,
                  provenance: generated(operation.provenance.span),
                }),
              ],
      )
      return appended.length === 0
        ? operation
        : Object.freeze({
            ...operation,
            failureLoanEnds: dependencyOrderedLoanEndings(fn, [
              ...(operation.failureLoanEnds ?? []),
              ...appended,
            ]),
          })
    }),
  )

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
    const initializerType =
      'type' in statement.initializer ? fn.semantic(statement.initializer.type) : undefined
    const transferredEffect = movedEffectRecipe(fn, statement.initializer)
    if (
      transferredEffect !== undefined &&
      initializerType !== undefined &&
      Type.isEffect(initializerType)
    ) {
      fn.effectRecipes.delete(transferredEffect.source)
      fn.effectLoanEnds.delete(transferredEffect.source)
      fn.effectRecipes.set(statement.binding.ordinal, transferredEffect.recipe)
      if (transferredEffect.loanEnds.length > 0)
        fn.effectLoanEnds.set(statement.binding.ordinal, transferredEffect.loanEnds)
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
    const staticCallable = callableRecipe(fn, statement.initializer)
    const staticCallableType =
      staticCallable === undefined ? undefined : fn.semantic(staticCallable.type)
    const callableSchema =
      staticCallableType !== undefined && Type.isCallable(staticCallableType)
        ? staticCallableType.schema
        : initializerType !== undefined && Type.isCallable(initializerType)
          ? initializerType.schema
          : undefined
    if (
      staticCallable !== undefined &&
      callableSchema !== undefined &&
      (callableSchema.binders.length > 0 ||
        callableSchema.constraints.length > 0 ||
        callableSchema.evidence.length > 0)
    ) {
      fn.callableRecipes.set(statement.binding.ordinal, statement.initializer)
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
    const forwardedRequirement = inlineForwardedRequirement(fn, statement.initializer)
    const forwardedResult = inlineForwardedEffectResult(fn, statement.initializer)
    const forwardedResultEffect =
      forwardedRequirement === undefined
        ? undefined
        : fn.call(statement.initializer.span)?.resultEffect
    const forwardedRequirementNeedsRecipe =
      forwardedRequirement !== undefined &&
      (forwardedResultEffect === undefined ||
        effectValueByIdentity(fn.layout, forwardedResultEffect) === undefined)
    if (
      forwardedResult !== undefined ||
      forwardedRequirementNeedsRecipe ||
      statement.initializer._tag === 'ServiceEffectConstruct' ||
      (statement.initializer._tag === 'EffectConstruct' &&
        fn.call(statement.initializer.span)?.resultEffect === undefined &&
        fn.effectResults.get(
          instanceText(
            statement.initializer.target,
            statement.initializer.typeArguments.map((argument) => fn.semanticArgument(argument)),
          ),
        ) === undefined) ||
      statement.initializer._tag === 'EffectResult' ||
      statement.initializer._tag === 'EffectBindRequirement' ||
      (statement.initializer._tag === 'BuiltinCall' && Type.isEffect(statement.initializer.type))
    ) {
      fn.effectRecipes.set(statement.binding.ordinal, forwardedResult ?? statement.initializer)
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
      const destinationType = fn.localTypes.at(destination.ordinal)
      if (destinationType?._tag === 'EffectValue') {
        const retained = retainedEffectLoans(fn, statement.initializer)
        if (retained.length > 0) fn.effectLoanEnds.set(statement.binding.ordinal, retained)
      }
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

  if (statement._tag === 'Evaluate') {
    const [evaluated, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (evaluated === undefined) return undefined
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
        ? borrowedWriteRoot(fn, place.root)
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
      endReturnedViewLoans(fn, statement.span)
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
    const droppedRecipe = callableRecipe(fn, droppedExpression)
    const droppedRecipeType =
      droppedRecipe === undefined ? undefined : fn.semantic(droppedRecipe.type)
    if (
      droppedRecipeType !== undefined &&
      Type.isCallable(droppedRecipeType) &&
      droppedRecipeType.schema !== undefined
    ) {
      if (droppedExpression._tag === 'BindingReference')
        fn.callableRecipes.delete(droppedExpression.binding.ordinal)
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
    const [lowered, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (lowered === undefined) return undefined
    const localType = fn.localTypes.at(lowered.result.ordinal)
    if (localType === undefined) return undefined
    const droppedBinding =
      droppedExpression._tag === 'BindingReference' ? droppedExpression.binding.ordinal : undefined
    const bindingFact =
      droppedBinding !== undefined
        ? Ownership.allBindings(fn.ownership).find(
            (binding) =>
              binding.site._tag === 'Let' && binding.site.binding.ordinal === droppedBinding,
          )
        : undefined
    const ownershipLoanReleases = (fn.ownership?.loans ?? []).flatMap((loan) => {
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
    const retainedLoanReleases = (
      droppedBinding === undefined ? [] : (fn.effectLoanEnds.get(droppedBinding) ?? [])
    ).flatMap((borrow) => {
      const slice = fn.loanLocals.get(borrowKey(borrow))
      if (slice === undefined) return []
      fn.loanLocals.delete(borrowKey(borrow))
      return [
        Object.freeze({
          _tag: 'EndLoan' as const,
          borrow,
          slice,
          provenance: generated(statement.span),
        }),
      ]
    })
    if (droppedBinding !== undefined) fn.effectLoanEnds.delete(droppedBinding)
    const loanReleases = [...ownershipLoanReleases, ...retainedLoanReleases].filter(
      (release, ordinal, releases) =>
        releases.findIndex(
          (candidate) => borrowKey(candidate.borrow) === borrowKey(release.borrow),
        ) === ordinal,
    )
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
            cleanup: cleanupForLocal(
              fn,
              bindingFact === undefined
                ? concreteCleanup(fn, Mir.semanticType(localType))
                : bindingFact.cleanup,
              localType,
            ),
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
    const branchState = delayedEffectState(fn)
    const beforeTaken = new Set(
      fn.regions.flatMap((region) => (region === undefined ? [] : [region.id.ordinal])),
    )
    const loweredTaken = lowerSequence(
      fn,
      statement.taken,
      exits,
      ownerLoop,
      Object.freeze({
        _tag: 'Forward',
        target: following,
        provenance: generated(statement.span),
      }),
      taken,
      exits.armEnds.get(`${spanKey(statement.span)}:Taken`),
    )
    if (loweredTaken === undefined) return undefined
    const takenState = delayedEffectState(fn)
    const takenRegions = fn.regions.flatMap((region) =>
      region !== undefined && !beforeTaken.has(region.id.ordinal) ? [region.id.ordinal] : [],
    )
    restoreDelayedEffectState(fn, branchState)
    const beforeOtherwise = new Set(
      fn.regions.flatMap((region) => (region === undefined ? [] : [region.id.ordinal])),
    )
    const loweredOtherwise = lowerSequence(
      fn,
      statement.otherwise,
      exits,
      ownerLoop,
      Object.freeze({
        _tag: 'Forward',
        target: following,
        provenance: generated(statement.span),
      }),
      otherwise,
      exits.armEnds.get(`${spanKey(statement.span)}:Otherwise`),
    )
    if (loweredOtherwise === undefined) return undefined
    const otherwiseState = delayedEffectState(fn)
    const otherwiseRegions = fn.regions.flatMap((region) =>
      region !== undefined && !beforeOtherwise.has(region.id.ordinal) ? [region.id.ordinal] : [],
    )
    restoreDelayedEffectState(fn, branchState)
    const branchEndings = (
      state: DelayedEffectState,
      other: DelayedEffectState,
    ): ReadonlyArray<Extract<Mir.Operation, { readonly _tag: 'EndLoan' }>> =>
      Object.freeze(
        (fn.ownership?.loans ?? []).flatMap((loan) => {
          const key = borrowKey(loan.id)
          const held = state.loanLocals.get(key)
          return held !== undefined && !other.loanLocals.has(key)
            ? [
                Object.freeze({
                  _tag: 'EndLoan' as const,
                  borrow: loan.id,
                  slice: held,
                  provenance: generated(statement.span),
                }),
              ]
            : []
        }),
      )
    const takenEndings = branchEndings(takenState, otherwiseState)
    const otherwiseEndings = branchEndings(otherwiseState, takenState)
    for (const key of branchState.loanLocals.keys()) {
      if (!takenState.loanLocals.has(key) || !otherwiseState.loanLocals.has(key))
        fn.loanLocals.delete(key)
    }
    for (const [regions, releases] of [
      [takenRegions, takenEndings],
      [otherwiseRegions, otherwiseEndings],
    ] as const)
      if (releases.length > 0) {
        const branchEnd = fn.reserve()
        for (const ordinal of regions) {
          const region = fn.regions.at(ordinal)
          if (
            region === undefined ||
            (region._tag !== 'OperationRegion' && region._tag !== 'CleanupRegion') ||
            region.outcome._tag !== 'Forward' ||
            region.outcome.target.ordinal !== following.ordinal
          )
            continue
          fn.regions[ordinal] = Object.freeze({
            ...region,
            outcome: Object.freeze({ ...region.outcome, target: branchEnd }),
          })
        }
        fn.publish(
          Object.freeze({
            _tag: 'CleanupRegion',
            id: branchEnd,
            ...ownerFields(ownerLoop),
            releases,
            outcome: Object.freeze({
              _tag: 'Forward',
              target: following,
              provenance: generated(statement.span),
            }),
          }),
        )
      }
    if (lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined)
      return undefined
    return id
  }

  if (statement._tag === 'While') {
    const loop: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.loop.ordinal })
    const conditionId = fn.reserve()
    const bodyId = fn.reserve()
    const following = fn.reserve()
    const entryState = delayedEffectState(fn)
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
    const loopState = delayedEffectState(fn)
    const loweredBody = lowerSequence(
      fn,
      statement.body,
      exits,
      loop,
      repeat,
      bodyId,
      exits.loopFallthroughs.get(statement.loop.ordinal),
    )
    if (loweredBody === undefined) return undefined
    const bodyState = delayedEffectState(fn)
    const delayedLoanKeys = new Set(
      [...entryState.loanLocals.keys()].filter(
        (key) => !loopState.loanLocals.has(key) || !bodyState.loanLocals.has(key),
      ),
    )
    const delayedLoans = delayedLoopLoans(fn, delayedLoanKeys, entryState)
    if (delayedLoanKeys.size > 0) {
      const loopFamily = new Set<number>([loop.ordinal])
      let changed = true
      while (changed) {
        changed = false
        for (const region of fn.regions) {
          if (
            region?._tag !== 'LoopRegion' ||
            region.parent === undefined ||
            !loopFamily.has(region.parent.ordinal) ||
            loopFamily.has(region.loop.ordinal)
          )
            continue
          loopFamily.add(region.loop.ordinal)
          changed = true
        }
      }
      for (const region of fn.regions) {
        if (region === undefined || !loopFamily.has(region.ownerLoop?.ordinal ?? -1)) continue
        if (region._tag === 'OperationRegion') {
          const operations = withDelayedFailureLoanEndings(
            fn,
            withoutLoanEndings(region.operations, delayedLoanKeys),
            delayedLoans,
          )
          const terminalEndings =
            region.outcome._tag === 'Return' || region.outcome._tag === 'Trap'
              ? terminalLoopLoanEndings(delayedLoans, region.outcome)
              : []
          fn.regions[region.id.ordinal] = Object.freeze({
            ...region,
            operations: Object.freeze([...operations, ...terminalEndings]),
          })
        } else if (region._tag === 'CleanupRegion') {
          const releases = withoutLoanEndings(region.releases, delayedLoanKeys).flatMap(
            (release) => (release._tag === 'Drop' || release._tag === 'EndLoan' ? [release] : []),
          )
          const terminalEndings =
            region.outcome._tag === 'Return' || region.outcome._tag === 'Trap'
              ? terminalLoopLoanEndings(delayedLoans, region.outcome)
              : []
          fn.regions[region.id.ordinal] = Object.freeze({
            ...region,
            releases: Object.freeze([...terminalEndings, ...releases]),
          })
        }
      }
    }
    restoreDelayedEffectState(fn, loopState)
    for (const key of delayedLoanKeys) {
      const held = entryState.loanLocals.get(key)
      if (held !== undefined) fn.loanLocals.set(key, held)
    }
    const continuation = delayedLoans.length === 0 ? following : fn.reserve()
    if (delayedLoans.length > 0) {
      const [, releases] = fn.capture(() => {
        for (const loan of delayedLoans) {
          const slice = fn.loanLocals.get(loan.key)
          if (slice === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan',
              borrow: loan.borrow,
              slice,
              provenance: generated(statement.span),
            }),
          )
          fn.loanLocals.delete(loan.key)
        }
      })
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id: following,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: continuation,
            provenance: generated(statement.span),
          }),
        }),
      )
    }
    if (lowerSequence(fn, rest, exits, ownerLoop, terminal, continuation, armExit) === undefined)
      return undefined
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
    const specializedFailure = fn.semantic(statement.failure)
    if (Type.isNever(specializedFailure)) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Trap',
            reason: 'unreachable failure of never',
            provenance: generated(statement.span),
          }),
        }),
      )
      return id
    }
    const [failedValue, operations] = fn.capture(() => {
      const failed = lowerExpression(fn, statement.expression)
      const outcomeType = fn.effectOutcome === undefined ? undefined : fn.type(fn.effectOutcome)
      if (
        failed === undefined ||
        outcomeType?._tag !== 'EffectOutcome' ||
        (!Type.isNominal(specializedFailure) && !Type.isUnion(specializedFailure))
      )
        return undefined
      const destination = fn.alloc(outcomeType)
      if (Type.isNominal(specializedFailure)) {
        const tag = Type.failureMembers(outcomeType.type).findIndex((failure) =>
          Type.equals(failure, specializedFailure),
        )
        if (tag < 0) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'PackEffectOutcome' as const,
            destination,
            source: failed.result,
            tag: tag + 1,
            type: outcomeType,
            provenance: authored(statement.span),
          }),
        )
      } else {
        const sourceType = fn.type(specializedFailure)
        if (sourceType?._tag !== 'Union') return undefined
        const mappings = specializedFailure.members.flatMap((member, source) => {
          const target = Type.failureMembers(outcomeType.type).findIndex((failure) =>
            Type.equals(failure, member),
          )
          return target < 0 ? [] : [Object.freeze({ source, target: target + 1 })]
        })
        if (mappings.length !== specializedFailure.members.length) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'PackEffectFailureUnion' as const,
            destination,
            source: failed.result,
            sourceType,
            mappings: Object.freeze(mappings),
            type: outcomeType,
            provenance: authored(statement.span),
          }),
        )
      }
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
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.MirFunction => {
  const fn = instance.function
  const plan = planFor(ownership, fn)

  if (plan !== undefined && plan.verdict._tag === 'Violation') {
    return trapFunction(instance, 'ownership violation', plan.verdict.cause.span)
  }

  const contract = fn.contract
  const parameterTypes =
    contract._tag === 'Contract'
      ? instance.specialization.parameters.flatMap((specialized, ordinal) => {
          const type = contract.parameters.at(ordinal) ?? specialized
          if (Type.isEffect(specialized)) {
            const identity = Instances.parameterEffectIdentity(fn, instance.key, ordinal)
            const effectValue =
              identity === undefined ? undefined : effectValueByIdentity(layout, identity)
            return effectValue === undefined ? [] : [effectValue]
          }
          if (
            Type.isRepresented(specialized) &&
            Type.isCallable(specialized.contract) &&
            Type.isExactRepresentationArgument(specialized.representation.argument) &&
            Type.isCallableIdentityArgument(specialized.representation.argument.identity)
          ) {
            const callable = callableValueByIdentity(
              layout,
              specialized.representation.argument.identity,
              specialized.contract,
            )
            return callable === undefined ? [] : [callable]
          }
          if (Type.isCallable(specialized)) {
            const identity = Instances.parameterCallableIdentity(fn, instance.key, ordinal)
            const callable =
              identity === undefined
                ? undefined
                : callableValueByIdentity(layout, identity, specialized)
            return callable === undefined ? [] : [callable]
          }
          const lowered =
            storedCallableValueType(layout, specialized) ??
            storedEffectValueType(layout, specialized) ??
            representedValueType(layout, opaqueRealizations, type, instance.substitution) ??
            mirType(type, instance.substitution)
          return lowered === undefined ? [] : [lowered]
        })
      : Array.from({ length: fn.declaration.parameterCount }, () => i32)
  const effectOutcome =
    contract._tag === 'Contract' && contract.functionKind === 'Effect'
      ? Type.effectWithRows(
          instance.specialization.result,
          instance.specialization.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
          'Shared',
          instance.specialization.requirementRow ??
            RowAlgebra.concrete(Type.requirementRowPolicy(), []),
        )
      : undefined
  const returnedBlock = contract._tag === 'Contract' ? returnedEffectBlock(fn) : undefined
  const hiddenEffectResult =
    returnedBlock === undefined ? undefined : effectValueType(layout, instance.key, returnedBlock)
  const specializedEffectResult =
    instance.resultEffect === undefined
      ? undefined
      : effectValueByIdentity(layout, instance.resultEffect)
  const resultType =
    specializedEffectResult ??
    hiddenEffectResult ??
    (contract._tag === 'Contract'
      ? (storedCallableValueType(layout, effectOutcome ?? instance.specialization.result) ??
        storedEffectValueType(layout, effectOutcome ?? instance.specialization.result) ??
        representedValueType(
          layout,
          opaqueRealizations,
          effectOutcome ?? instance.specialization.result,
          new Map(),
        ) ??
        mirType(effectOutcome ?? instance.specialization.result))
      : i32)
  if (resultType === undefined) {
    return trapFunction(instance, 'unavailable contract type', bodySpan(fn))
  }

  const lowering = new FunctionLowering(
    layout,
    index,
    parameterTypes,
    plan,
    instance.substitution,
    effectOutcome,
    instance,
    instances,
    calls,
    effectResults,
    generatedRunners,
    opaqueRealizations,
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
  spec: GeneratedBlockEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.MirFunction | undefined => {
  const { owner, block, type } = spec
  const id = spec.id
  const instance: Instances.InstanceKey = Object.freeze({
    _tag: 'InstanceKey',
    declaration: id,
    typeArguments: owner.key.typeArguments,
    contractRow: Object.freeze([
      ...owner.key.contractRow,
      `effect-site:${Hir.executableSiteKey(block.site)}`,
      ...spec.providedRequirements.map(providedContractEntry),
    ]),
  })
  const captureParameterTypes = type.environment.fields.flatMap((field) => {
    if (field.effectIdentity !== undefined) {
      const effectValue = effectValueByIdentity(layout, field.effectIdentity)
      return effectValue === undefined ? [] : [effectValue]
    }
    if (field.callableIdentity !== undefined && Type.isCallable(field.type)) {
      const callable = callableValueByIdentity(layout, field.callableIdentity, field.type)
      return callable === undefined ? [] : [callable]
    }
    const lowered = mirType(field.type)
    if (lowered === undefined) return []
    if (field.representation === 'Value') return [lowered]
    if (field.access !== 'Shared' && field.access !== 'Exclusive') return []
    return [
      Object.freeze({ _tag: 'EffectBorrow' as const, type: field.type, access: field.access }),
    ]
  })
  if (captureParameterTypes.length !== block.captures.length) return undefined
  const parameterizedRequirements = spec.providedRequirements.filter(
    (requirement) => requirement.witness._tag === 'SourceConformanceWitness',
  )
  const requirementParameterTypes = parameterizedRequirements.flatMap((requirement) => {
    const type = mirType(
      Object.freeze({
        _tag: 'ReferenceType' as const,
        access: requirement.access === 'Take' ? ('Exclusive' as const) : requirement.access,
        target: requirement.providerType,
      }),
    )
    return type === undefined ? [] : [type]
  })
  if (requirementParameterTypes.length !== parameterizedRequirements.length) return undefined
  const parameterTypes = Object.freeze([...captureParameterTypes, ...requirementParameterTypes])
  const plan = planFor(ownership, owner.function)
  const lowering = new FunctionLowering(
    layout,
    index,
    parameterTypes,
    plan,
    owner.substitution,
    type.type,
    owner,
    instances,
    calls,
    effectResults,
    generatedRunners,
    opaqueRealizations,
    Object.freeze(
      spec.providedRequirements.map((requirement) => {
        const ordinal = parameterizedRequirements.indexOf(requirement)
        return Object.freeze({
          ...requirement,
          ...(ordinal < 0 ? {} : { local: local(captureParameterTypes.length + ordinal) }),
        })
      }),
    ),
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
  if (entry === undefined || lowering.regions.some((region) => region === undefined)) {
    return undefined
  }
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
    effectRunner: Object.freeze({
      base: Object.freeze({
        declaration: Hir.effectRunnerId(type.environment.instance.declaration, type.site),
        typeArguments: type.environment.instance.typeArguments,
      }),
      providers: Object.freeze(
        spec.providedRequirements.map((requirement) =>
          Object.freeze({
            capability: requirement.capability,
            providerType: requirement.providerType,
            witness: requirement.witness,
            role: requirement.role,
            requirementAccess: requirement.requirementAccess,
            access: requirement.access,
          }),
        ),
      ),
    }),
  })
}

const lowerCatchEffectRunner = (
  spec: GeneratedCatchEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.MirFunction | undefined => {
  const parameterizedRequirements = spec.providedRequirements.filter(
    (requirement) => requirement.witness._tag === 'SourceConformanceWitness',
  )
  const requirementParameterTypes = parameterizedRequirements.flatMap((requirement) => {
    const type = mirType(
      Type.reference(
        requirement.access === 'Take' ? ('Exclusive' as const) : requirement.access,
        requirement.providerType,
      ),
    )
    return type === undefined ? [] : [type]
  })
  if (requirementParameterTypes.length !== parameterizedRequirements.length) return undefined
  const captureParameterTypes = Object.freeze([spec.protectedType, spec.handlerType])
  const parameterTypes = Object.freeze([...captureParameterTypes, ...requirementParameterTypes])
  const instance: Instances.InstanceKey = Object.freeze({
    _tag: 'InstanceKey',
    declaration: spec.id,
    typeArguments: spec.owner.key.typeArguments,
    contractRow: Object.freeze([
      ...spec.owner.key.contractRow,
      `effect-site:${Hir.executableSiteKey(spec.type.site)}`,
      ...spec.providedRequirements.map(providedContractEntry),
    ]),
  })
  const lowering = new FunctionLowering(
    layout,
    index,
    parameterTypes,
    planFor(ownership, spec.owner.function),
    spec.owner.substitution,
    spec.type.type,
    spec.owner,
    instances,
    calls,
    effectResults,
    generatedRunners,
    opaqueRealizations,
    Object.freeze(
      spec.providedRequirements.map((requirement) => {
        const ordinal = parameterizedRequirements.indexOf(requirement)
        return Object.freeze({
          ...requirement,
          ...(ordinal < 0 ? {} : { local: local(captureParameterTypes.length + ordinal) }),
        })
      }),
    ),
  )
  const region = lowering.reserve()
  const [success, operations] = lowering.capture(() =>
    lowerEffectCatch(lowering, spec.expression, spec.expression.span, {
      protected: local(0),
      protectedType: spec.protectedType,
      handler: local(1),
      handlerType: spec.handlerType,
    }),
  )
  const result: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: spec.type.type,
  })
  if (success === undefined) return undefined
  const returned = lowering.alloc(result)
  const packed: Mir.Operation = Object.freeze({
    _tag: 'PackEffectOutcome',
    destination: returned,
    source: success.result,
    tag: 0,
    type: result,
    provenance: generated(spec.expression.span),
  })
  lowering.publish(
    Object.freeze({
      _tag: 'OperationRegion',
      id: region,
      operations: Object.freeze([...operations, packed]),
      outcome: Object.freeze({
        _tag: 'Return',
        value: returned,
        provenance: generated(spec.expression.span),
      }),
    }),
  )
  return Object.freeze({
    _tag: 'MirFunction',
    id: spec.id,
    instance,
    parameterCount: parameterTypes.length,
    localTypes: Object.freeze([...lowering.localTypes]),
    result,
    entry: region,
    regions: Object.freeze(
      lowering.regions.flatMap((candidate) => (candidate === undefined ? [] : [candidate])),
    ),
    effectRunner: Object.freeze({
      base: Object.freeze({
        declaration: Hir.effectRunnerId(spec.type.environment.instance.declaration, spec.type.site),
        typeArguments: spec.type.environment.instance.typeArguments,
      }),
      providers: Object.freeze(
        spec.providedRequirements.map((requirement) =>
          Object.freeze({
            capability: requirement.capability,
            providerType: requirement.providerType,
            witness: requirement.witness,
            role: requirement.role,
            requirementAccess: requirement.requirementAccess,
            access: requirement.access,
          }),
        ),
      ),
    }),
  })
}

const lowerWitnessEffectRunner = (
  spec: GeneratedWitnessEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.MirFunction | undefined => {
  const parameterTypes = spec.type.environment.fields.flatMap((field) => {
    const type = mirType(field.type)
    return type === undefined ? [] : [type]
  })
  if (parameterTypes.length !== spec.type.environment.fields.length) return undefined
  const parameterizedRequirements = spec.providedRequirements.filter(
    (requirement) => requirement.witness._tag === 'SourceConformanceWitness',
  )
  const requirementParameterTypes = parameterizedRequirements.flatMap((requirement) => {
    const type = mirType(
      Type.reference(
        requirement.access === 'Take' ? ('Exclusive' as const) : requirement.access,
        requirement.providerType,
      ),
    )
    return type === undefined ? [] : [type]
  })
  if (requirementParameterTypes.length !== parameterizedRequirements.length) return undefined
  const allParameters = Object.freeze([...parameterTypes, ...requirementParameterTypes])
  const instance: Instances.InstanceKey = Object.freeze({
    _tag: 'InstanceKey',
    declaration: spec.id,
    typeArguments: spec.owner.key.typeArguments,
    contractRow: Object.freeze([
      ...spec.owner.key.contractRow,
      `witness-effect-site:${Hir.executableSiteKey(spec.type.site)}`,
    ]),
  })
  const lowering = new FunctionLowering(
    layout,
    index,
    allParameters,
    planFor(ownership, spec.owner.function),
    spec.owner.substitution,
    spec.type.type,
    spec.owner,
    instances,
    calls,
    effectResults,
    generatedRunners,
    opaqueRealizations,
    Object.freeze(
      spec.providedRequirements.map((requirement) => {
        const ordinal = parameterizedRequirements.indexOf(requirement)
        return Object.freeze({
          ...requirement,
          ...(ordinal < 0 ? {} : { local: local(parameterTypes.length + ordinal) }),
        })
      }),
    ),
  )
  const region = lowering.reserve()
  const [returned, operations] = lowering.capture((): Mir.LocalId | undefined => {
    let success: LoweredExpression | undefined
    let reborrows: WitnessArguments['reborrows'] = Object.freeze([])
    if (spec.target !== undefined) {
      const declaration = DeclarationIndex.byCanonical(index, spec.target.implementation)
      if (declaration?._tag !== 'FunctionDeclaration') return undefined
      const arguments_ = sourceWitnessArguments(
        lowering,
        spec.target,
        parameterTypes.map((_, ordinal) => local(ordinal)),
        spec.expression.span,
      )
      if (arguments_ === undefined) return undefined
      reborrows = arguments_.reborrows
      if (declaration.functionKind === 'Ordinary') {
        const binders = declaration.typeParameters
          .filter((parameter) => parameter.duplicateOf === undefined)
          .map((parameter) => parameter.type)
        const substitution = Type.substitution(binders, spec.target.typeArguments)
        const result =
          substitution === undefined || declaration.returnType._tag !== 'Resolved'
            ? undefined
            : lowering.type(Type.substitute(declaration.returnType.type, substitution))
        if (result === undefined) return undefined
        const destination = lowering.alloc(result)
        lowering.emit(
          Object.freeze({
            _tag: 'Call',
            destination,
            target: spec.target.implementation,
            typeArguments: spec.target.typeArguments,
            arguments: arguments_.arguments,
            type: result,
            provenance: generated(spec.expression.span),
          }),
        )
        success = Object.freeze({ result: destination })
      } else {
        const effectType = effectResults.get(
          instanceText(spec.target.implementation, spec.target.typeArguments),
        )
        if (effectType === undefined) return undefined
        const effect = lowering.alloc(effectType)
        lowering.emit(
          Object.freeze({
            _tag: 'Call',
            destination: effect,
            target: spec.target.implementation,
            typeArguments: spec.target.typeArguments,
            arguments: arguments_.arguments,
            type: effectType,
            provenance: generated(spec.expression.span),
          }),
        )
        success = lowerRunEffectValue(
          lowering,
          effect,
          effectType,
          spec.type.type.success,
          spec.expression.span,
        )
      }
    } else if (spec.intrinsic?.rule._tag === 'BuiltinRule') {
      const contract = witnessEffectContract(spec.expression)
      if (contract === undefined) return undefined
      success = lowerExpressionInner(
        lowering,
        Object.freeze({
          _tag: 'BuiltinCall',
          operation: spec.intrinsic.rule.operation,
          intrinsic: spec.intrinsic.id,
          typeArguments: Object.freeze([]),
          arguments: Object.freeze(
            contract.operands.map((operand) =>
              Object.freeze({
                _tag: 'ParameterReference' as const,
                parameter: operand.parameter.id,
                type: operand.type._tag === 'Resolved' ? operand.type.type : 'never',
                span: spec.expression.span,
              }),
            ),
          ),
          intrinsicWitnessOperands: contract.operands,
          loanEnds: Object.freeze([]),
          heldLoans: Object.freeze([]),
          type: spec.type.type.success,
          span: spec.expression.span,
        }),
      )
    }
    if (success === undefined) return undefined
    endWitnessReborrows(lowering, reborrows, spec.expression.span)
    const outcome: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
      _tag: 'EffectOutcome',
      type: spec.type.type,
    })
    const destination = lowering.alloc(outcome)
    lowering.emit(
      Object.freeze({
        _tag: 'PackEffectOutcome',
        destination,
        source: success.result,
        tag: 0,
        type: outcome,
        provenance: generated(spec.expression.span),
      }),
    )
    return destination
  })
  if (returned === undefined) return undefined
  lowering.publish(
    Object.freeze({
      _tag: 'OperationRegion',
      id: region,
      operations,
      outcome: Object.freeze({
        _tag: 'Return',
        value: returned,
        provenance: generated(spec.expression.span),
      }),
    }),
  )
  return Object.freeze({
    _tag: 'MirFunction',
    id: spec.id,
    instance,
    parameterCount: allParameters.length,
    localTypes: Object.freeze([...lowering.localTypes]),
    result: Object.freeze({ _tag: 'EffectOutcome', type: spec.type.type }),
    entry: region,
    regions: Object.freeze(
      lowering.regions.flatMap((candidate) => (candidate === undefined ? [] : [candidate])),
    ),
    effectRunner: Object.freeze({
      base: Object.freeze({
        declaration: Hir.effectRunnerId(spec.type.environment.instance.declaration, spec.type.site),
        typeArguments: spec.type.environment.instance.typeArguments,
      }),
      providers: Object.freeze(
        spec.providedRequirements.map((requirement) =>
          Object.freeze({
            capability: requirement.capability,
            providerType: requirement.providerType,
            witness: requirement.witness,
            role: requirement.role,
            requirementAccess: requirement.requirementAccess,
            access: requirement.access,
          }),
        ),
      ),
    }),
  })
}

/** Lowers the discovered instances into one MIR program module in discovery order. */
export const lowerProgram = (
  discovery: Instances.Discovery,
  ownership: ReadonlyMap<string, Ownership.ModuleOwnership>,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.Module => {
  const staticDataById = new Map<
    string,
    Extract<
      Hir.Expression,
      { readonly _tag: 'StaticStringLiteral' | 'StaticByteViewLiteral' }
    >['data']
  >()
  for (const instance of discovery.instances) {
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (expression._tag === 'StaticStringLiteral' || expression._tag === 'StaticByteViewLiteral')
        staticDataById.set(expression.data.id, expression.data)
    }
  }
  const staticData = Object.freeze(
    [...staticDataById.values()].sort((left, right) => left.id.localeCompare(right.id)),
  )
  const effectResults = new Map<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>()
  const generatedRunners: Array<GeneratedEffectRunner> = []
  for (const instance of discovery.instances) {
    const block = returnedEffectBlock(instance.function)
    if (block === undefined) continue
    const type = effectValueType(layout, instance.key, block)
    if (type !== undefined) {
      effectResults.set(instanceText(instance.key.declaration, instance.key.typeArguments), type)
      generatedRunners.push(
        Object.freeze({
          _tag: 'BlockEffectRunner',
          id: Hir.effectRunnerId(instance.key.declaration, block.site),
          owner: instance,
          block,
          type,
          specializationKey: baseRunnerKey(instance.key, block.site),
          providedRequirements: Object.freeze([]),
        }),
      )
    }
  }
  const functions = discovery.instances.map((instance) =>
    lowerInstance(
      instance,
      ownership.get(instance.key.declaration.module),
      layout,
      index,
      discovery.instances,
      discovery.calls,
      effectResults,
      generatedRunners,
      opaqueRealizations,
    ),
  )
  const loweredRunners: Array<{
    readonly spec: GeneratedEffectRunner
    readonly runner: Mir.MirFunction
  }> = []
  for (let ordinal = 0; ordinal < generatedRunners.length; ordinal += 1) {
    const generated = generatedRunners.at(ordinal)
    if (generated === undefined) continue
    const runner =
      generated._tag === 'BlockEffectRunner'
        ? lowerEffectRunner(
            generated,
            ownership.get(generated.owner.key.declaration.module),
            layout,
            index,
            discovery.instances,
            discovery.calls,
            effectResults,
            generatedRunners,
            opaqueRealizations,
          )
        : generated._tag === 'CatchEffectRunner'
          ? lowerCatchEffectRunner(
              generated,
              ownership.get(generated.owner.key.declaration.module),
              layout,
              index,
              discovery.instances,
              discovery.calls,
              effectResults,
              generatedRunners,
              opaqueRealizations,
            )
          : lowerWitnessEffectRunner(
              generated,
              ownership.get(generated.owner.key.declaration.module),
              layout,
              index,
              discovery.instances,
              discovery.calls,
              effectResults,
              generatedRunners,
              opaqueRealizations,
            )
    if (runner !== undefined) loweredRunners.push(Object.freeze({ spec: generated, runner }))
  }
  // Lowering a provided parent can discover provided children after their open bases were already
  // visited. Filter only after the worklist reaches its fixed point so backends never compile an
  // unreachable open runner that still calls another open runner without provider arguments.
  const unresolvedOpenBase = (spec: GeneratedEffectRunner): boolean => {
    const entryOwnsRunner =
      discovery.entry._tag === 'Resolved' &&
      discovery.entry.kind === 'Effect' &&
      instanceText(spec.owner.key.declaration, spec.owner.key.typeArguments) ===
        instanceText(discovery.entry.key.declaration, discovery.entry.key.typeArguments)
    return (
      !entryOwnsRunner &&
      spec.providedRequirements.length === 0 &&
      Type.requirementMembers(spec.type.type).length > 0
    )
  }
  const runnerKey = (
    declaration: DeclarationIndex.CanonicalId,
    typeArguments: ReadonlyArray<Type.GenericArgument>,
  ): string => instanceText(declaration, typeArguments)
  const retainedRunners = new Set(
    loweredRunners
      .filter(({ spec }) => !unresolvedOpenBase(spec))
      .map(({ spec }) => runnerKey(spec.id, spec.owner.key.typeArguments)),
  )
  const retainReferencedRunners = (fn: Mir.MirFunction): boolean => {
    let changed = false
    for (const operation of Mir.operations(fn)) {
      if (
        operation._tag !== 'RunEffectValue' &&
        operation._tag !== 'RunStaticEffect' &&
        operation._tag !== 'ReifyEffect'
      )
        continue
      const key = runnerKey(operation.runner, operation.runnerTypeArguments)
      if (!retainedRunners.has(key)) {
        retainedRunners.add(key)
        changed = true
      }
    }
    return changed
  }
  for (const fn of functions) retainReferencedRunners(fn)
  let retainedChanged = true
  while (retainedChanged) {
    retainedChanged = false
    for (const { spec, runner } of loweredRunners) {
      if (!retainedRunners.has(runnerKey(spec.id, spec.owner.key.typeArguments))) continue
      if (retainReferencedRunners(runner)) retainedChanged = true
    }
  }
  functions.push(
    ...loweredRunners.flatMap(({ spec, runner }) => {
      return retainedRunners.has(runnerKey(spec.id, spec.owner.key.typeArguments)) ? [runner] : []
    }),
  )
  if (discovery.entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'MirModule',
      module: discovery.rootModule,
      intrinsics: discovery.intrinsics,
      entry: Object.freeze({ _tag: 'UnavailableEntry', reason: discovery.entry.reason }),
      layout,
      staticData,
      functions: Object.freeze(functions),
    })
  }
  const resolvedEntry = discovery.entry
  let entry: Mir.Entry
  if (resolvedEntry.kind === 'Ordinary') {
    entry = Object.freeze({
      _tag: 'OrdinaryEntry',
      target: resolvedEntry.key,
      machine: resolvedEntry.key,
    })
  } else {
    const target = functions.find(
      (fn) =>
        instanceText(fn.instance.declaration, fn.instance.typeArguments) ===
        instanceText(resolvedEntry.key.declaration, resolvedEntry.key.typeArguments),
    )
    const runnerSpec = generatedRunners.find(
      (candidate) =>
        instanceText(candidate.owner.key.declaration, candidate.owner.key.typeArguments) ===
        instanceText(resolvedEntry.key.declaration, resolvedEntry.key.typeArguments),
    )
    const runner =
      runnerSpec === undefined
        ? undefined
        : functions.find((fn) =>
            Mir.matchesInstance(fn, runnerSpec.id, resolvedEntry.key.typeArguments),
          )
    if (target?.result._tag !== 'EffectValue' || runner?.result._tag !== 'EffectOutcome') {
      throw new RangeError('Effect entry lowering lost its constructor or runner')
    }
    const adapterId = effectEntryAdapterId(discovery.rootModule)
    const adapterKey: Instances.InstanceKey = Object.freeze({
      _tag: 'InstanceKey',
      declaration: adapterId,
      typeArguments: Object.freeze([]),
      contractRow: Object.freeze(['generated:effect-entry']),
    })
    const span = target.regions
      .flatMap((region) =>
        region._tag === 'OperationRegion'
          ? region.operations.map((operation) => operation.provenance.span)
          : region._tag === 'CleanupRegion'
            ? region.releases.map((operation) => operation.provenance.span)
            : [region.provenance.span],
      )
      .at(0)
    if (span === undefined) throw new RangeError('Effect entry lowering lost source provenance')
    const failures = resolvedEntry.failures.map((failure, ordinal) =>
      Object.freeze({
        tag: ordinal + 1,
        type: failure.type,
        report: failure.report,
        payload: local(ordinal + 3),
        cleanup: Ownership.cleanupPlan(index, failure.type),
      }),
    )
    const effect = local(1)
    const outcome = local(2)
    const status = local(0)
    functions.push(
      Object.freeze({
        _tag: 'MirFunction',
        id: adapterId,
        instance: adapterKey,
        parameterCount: 0,
        localTypes: Object.freeze([
          i32,
          target.result,
          runner.result,
          ...failures.map((failure) =>
            Object.freeze({ _tag: 'Nominal' as const, type: failure.type }),
          ),
        ]),
        result: i32,
        entry: Object.freeze({ _tag: 'Region', ordinal: 0 }),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: Object.freeze({ _tag: 'Region' as const, ordinal: 0 }),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'CloseEffectEntry' as const,
                destination: status,
                effect,
                outcome,
                target: resolvedEntry.key.declaration,
                runner: runner.id,
                typeArguments: resolvedEntry.key.typeArguments,
                effectType: target.result,
                outcomeType: runner.result,
                failures: Object.freeze(failures),
                type: i32,
                provenance: generated(span),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: status,
              provenance: generated(span),
            }),
          }),
        ]),
      }),
    )
    entry = Object.freeze({
      _tag: 'EffectEntry',
      target: resolvedEntry.key,
      machine: adapterKey,
      requirements: resolvedEntry.requirements,
      failures: Object.freeze(
        resolvedEntry.failures.map((failure, ordinal) =>
          Object.freeze({ tag: ordinal + 1, type: failure.type, report: failure.report }),
        ),
      ),
    })
  }
  return Object.freeze({
    _tag: 'MirModule',
    module: discovery.rootModule,
    intrinsics: discovery.intrinsics,
    entry,
    layout,
    staticData,
    functions: Object.freeze(functions),
  })
}
