import { generated, indexExits } from './CleanupEmission.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type { LoweredExpression } from './EffectLowering.js'
import { lowerEffectCatch, lowerRunEffectComposite, lowerRunEffectValue } from './EffectLowering.js'
import type {} from './Forwarding.js'
import { FunctionLowering } from './FunctionLowering.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as TypeInference from './internal/TypeInference.js'
import type * as Layout from './Layout.js'
import type { ExecutableEffectType } from './Lower.js'
import { i32, local, mirType } from './Lower.js'
import type {} from './LowerExpression.js'
import { lowerExpressionInner } from './LowerExpression.js'
import { lowerSequence } from './LowerStatements.js'
import type * as Mir from './Mir.js'
import type * as OpaqueRealization from './OpaqueRealization.js'
import type * as Ownership from './Ownership.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import type {
  GeneratedBlockEffectRunner,
  GeneratedCatchEffectRunner,
  GeneratedEffectRunner,
  GeneratedWitnessEffectRunner,
} from './ValueType.js'
import {
  callableValueByIdentity,
  effectValueByIdentity,
  effectValueType,
  instanceText,
  providedContractEntry,
  representedValueType,
  storedCallableValueType,
  storedEffectValueType,
} from './ValueType.js'
import type { WitnessArguments } from './WitnessLowering.js'
import {
  endWitnessReborrows,
  sourceWitnessArguments,
  witnessEffectContract,
} from './WitnessLowering.js'

export const trapFunction = (
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

export const planFor = (
  ownership: Ownership.ModuleOwnership | undefined,
  fn: Hir.HirFunction,
): Ownership.FunctionOwnership | undefined =>
  ownership?.functions.find(
    (candidate) => candidate.declaration.id.ordinal === fn.declaration.id.ordinal,
  )

export const bodySpan = (fn: Hir.HirFunction): SourceSpan.SourceSpan =>
  fn.statements.at(-1)?.span ?? fn.declaration.syntax.span

export const returnedEffectBlock = (
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

export const lowerInstance = (
  instance: Instances.Instance,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, ExecutableEffectType>,
  generatedRunners: Array<GeneratedEffectRunner>,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.MirFunction => {
  const fn = instance.function
  const plan = planFor(ownership, fn)

  if (plan !== undefined && plan.verdict._tag === 'Violation') {
    return trapFunction(instance, 'ownership violation', plan.verdict.cause.span)
  }

  const contract = fn.contract
  let parameterTypes: Mir.Type[]
  if (contract._tag === 'Contract') {
    parameterTypes = instance.specialization.parameters.flatMap((specialized, ordinal) => {
      const type = contract.parameters.at(ordinal) ?? specialized
      const representedEffect =
        Type.isRepresented(specialized) &&
        Type.isEffect(specialized.contract) &&
        Type.isExactRepresentationArgument(specialized.representation.argument) &&
        Type.isEffectIdentityArgument(specialized.representation.argument.identity)
      if (Type.isEffect(specialized) || representedEffect) {
        const representation = Instances.parameterEffectRepresentationArgument(
          fn,
          instance.key,
          ordinal,
        )
        if (
          Type.isEffect(specialized) &&
          representation !== undefined &&
          Type.isCompositeEffectRepresentationArgument(representation)
        ) {
          const composite = representedValueType(
            layout,
            opaqueRealizations,
            Type.represented(specialized, specialized, representation),
            instance.substitution,
          )
          if (composite !== undefined) return [composite]
        }
        const identity =
          representation !== undefined && Type.isEffectIdentityArgument(representation)
            ? representation.identity
            : undefined
        const effectValue =
          identity === undefined ? undefined : effectValueByIdentity(layout, identity)
        if (effectValue !== undefined) return [effectValue]
        if (Type.isEffect(specialized)) return []
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
        mirType(type, instance.substitution, layout)
      return lowered === undefined ? [] : [lowered]
    })
  } else {
    parameterTypes = Array.from({ length: fn.declaration.parameterCount }, () => i32)
  }
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
  const terminalStatement = fn.statements.at(-1)
  const returnedExpression =
    terminalStatement?._tag === 'Return' && 'type' in terminalStatement.expression
      ? terminalStatement.expression
      : undefined
  const hiddenEffectResult =
    returnedBlock === undefined ? undefined : effectValueType(layout, instance.key, returnedBlock)
  const hiddenCompositeResult =
    returnedExpression === undefined
      ? undefined
      : representedValueType(
          layout,
          opaqueRealizations,
          returnedExpression.type,
          instance.substitution,
        )
  const specializedEffectResult =
    instance.resultEffect === undefined
      ? undefined
      : effectValueByIdentity(layout, instance.resultEffect)
  const resultType =
    specializedEffectResult ??
    hiddenEffectResult ??
    hiddenCompositeResult ??
    (contract._tag === 'Contract'
      ? (storedCallableValueType(layout, effectOutcome ?? instance.specialization.result) ??
        storedEffectValueType(layout, effectOutcome ?? instance.specialization.result) ??
        representedValueType(
          layout,
          opaqueRealizations,
          effectOutcome ?? instance.specialization.result,
          new Map(),
        ) ??
        mirType(effectOutcome ?? instance.specialization.result, new Map(), layout))
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

export const lowerEffectRunner = (
  spec: GeneratedBlockEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, ExecutableEffectType>,
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
    if (Type.isRepresented(field.type)) {
      const represented = representedValueType(layout, opaqueRealizations, field.type, new Map())
      return represented === undefined ? [] : [represented]
    }
    // The layout resolves scalar-enum nominals to their Enum representation; without it a
    // captured enum lowers as a bare Nominal and every enum operation in the runner body fails.
    const lowered = mirType(field.type, new Map(), layout)
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

export const lowerCatchEffectRunner = (
  spec: GeneratedCatchEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, ExecutableEffectType>,
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

export const lowerWitnessEffectRunner = (
  spec: GeneratedWitnessEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, ExecutableEffectType>,
  generatedRunners: Array<GeneratedEffectRunner>,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.MirFunction | undefined => {
  const parameterTypes = spec.type.environment.fields.flatMap((field) => {
    // The layout resolves scalar-enum nominals to their Enum representation, exactly as in
    // lowerEffectRunner — without it a captured enum silently fails every enum operation.
    const type = mirType(field.type, new Map(), layout)
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
      const declaration = DeclarationFacts.byCanonical(index, spec.target.implementation)
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
        const substitution = TypeInference.substitution(binders, spec.target.typeArguments)
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
        success =
          effectType._tag === 'EffectValue'
            ? lowerRunEffectValue(
                lowering,
                effect,
                effectType,
                spec.type.type.success,
                spec.expression.span,
              )
            : lowerRunEffectComposite(
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
