import {
  authored,
  callableLocalCleanup,
  concreteCleanup,
  generated,
  lowerBorrowedWriteSelectors,
  lowerBorrowSelectors,
  lowerWriteSelectors,
  propagationLoanEnds,
  propagationReleases,
  specializedCleanup,
} from './CleanupEmission.js'
import * as CleanupPlan from './CleanupPlan.js'
import * as ConformanceProof from './ConformanceProof.js'
import type { LoweredExpression } from './EffectLowering.js'
import {
  borrowedWriteRoot,
  endLoans,
  endReturnedViewLoans,
  endRunLoans,
  lowerCatchEffectValue,
  lowerEffectCatch,
  lowerEffectExecution,
  lowerPlace,
  lowerReifiedEffectRecipe,
  lowerRunEffectComposite,
} from './EffectLowering.js'
import type {} from './EntryAssembly.js'
import type {} from './Forwarding.js'
import {
  callableRecipe,
  delayedEffectState,
  effectRecipe,
  inlineForwardedRequirement,
  restoreDelayedEffectState,
} from './Forwarding.js'
import type { FunctionLowering } from './FunctionLowering.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type { DelayedEffectState } from './Lower.js'
import { bool, borrowKey, character, isOsOperation, local, patternKey, usize } from './Lower.js'
import { lowerBuiltinExpression } from './LowerBuiltin.js'
import * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'
import * as TargetConstant from './TargetConstant.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'
import {
  baseRunnerKey,
  callableValueByIdentity,
  callableValueType,
  directCallableSectionValueType,
  effectCompositeShape,
  effectValueByIdentity,
  effectValueType,
  ensureProvidedRunner,
  functionItemValueType,
  instanceText,
  providerBindings,
  requirementsFor,
  runtimeRequirementArguments,
  storedCallableValueType,
  storedEffectValueType,
} from './ValueType.js'
import { lowerBoundWitnessCall, lowerWitnessEffect } from './WitnessLowering.js'

export function lowerExpression(
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

export function lowerExpressionInner(
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
    case 'EnumMember': {
      const type = fn.type(expression.type)
      if (
        type?._tag !== 'Enum' ||
        type.representation.enum.module !== expression.enum.module ||
        type.representation.enum.name !== expression.enum.name
      )
        return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'EnumConstant',
          destination,
          enum: expression.enum,
          member: expression.member,
          discriminant: expression.discriminant,
          representation: type.representation,
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'EnumValue': {
      const value = lowerExpression(fn, expression.value)
      const sourceType = value === undefined ? undefined : fn.localTypes.at(value.result.ordinal)
      const type = fn.type(expression.type)
      if (
        value === undefined ||
        sourceType?._tag !== 'Enum' ||
        type?._tag !== expression.type ||
        sourceType.representation.enum.module !== expression.enum.module ||
        sourceType.representation.enum.name !== expression.enum.name ||
        sourceType.representation.scalar !== expression.type
      )
        return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'EnumValue',
          destination,
          source: value.result,
          enum: expression.enum,
          representation: sourceType.representation,
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'EnumEquality': {
      const left = lowerExpression(fn, expression.left)
      const right = lowerExpression(fn, expression.right)
      const leftType = left === undefined ? undefined : fn.localTypes.at(left.result.ordinal)
      const rightType = right === undefined ? undefined : fn.localTypes.at(right.result.ordinal)
      if (
        left === undefined ||
        right === undefined ||
        leftType?._tag !== 'Enum' ||
        rightType?._tag !== 'Enum' ||
        leftType.representation.enum.module !== expression.enum.module ||
        leftType.representation.enum.name !== expression.enum.name ||
        rightType.representation.enum.module !== expression.enum.module ||
        rightType.representation.enum.name !== expression.enum.name
      )
        return undefined
      const destination = fn.alloc(bool)
      fn.emit(
        Object.freeze({
          _tag: 'EnumEquality',
          destination,
          left: left.result,
          right: right.result,
          enum: expression.enum,
          negated: expression.negated,
          representation: leftType.representation,
          type: bool,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
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
        readonly access: Type.CaptureAccess
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
          typeArguments: Object.freeze([...Layout.callableTargetArguments(type.environment)]),
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
        readonly access: Type.CaptureAccess
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
      const definition =
        callable === undefined ? undefined : fn.callableDefinitions.get(callable.ordinal)
      const realizedTarget = target ?? definition?.target
      const declaredEffectResult =
        realizedTarget?._tag === 'DeclarationCallableTarget'
          ? fn.effectResults.get(instanceText(realizedTarget.declaration, typeArguments))
          : undefined
      const type =
        (call?.resultEffect === undefined
          ? undefined
          : effectValueByIdentity(fn.layout, call.resultEffect)) ??
        declaredEffectResult ??
        fn.type(expression.type)
      if (!lowered || type === undefined || callableType === undefined) return undefined
      if (
        realizedTarget?._tag === 'BuiltinCallableTarget' &&
        Scalar.isCheckedOperation(realizedTarget.operation) &&
        type._tag === 'Union'
      ) {
        const actorScalar = Scalar.find(realizedTarget.actor)
        const scalarOperation = actorScalar?.operations.find(
          (operation) => operation.code === realizedTarget.operation,
        )
        const sourceScalar = Scalar.find(
          scalarOperation?.parameters?.at(0) ?? actorScalar?.spelling ?? '',
        )
        const valueScalar =
          realizedTarget.operation === 'CheckedConvertToChar'
            ? actorScalar
            : (Scalar.conversionTarget(realizedTarget.operation) ?? actorScalar)
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
          (valueScalar?.category !== 'Integer' && valueScalar?.category !== 'Character') ||
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
            _tag: 'CheckedScalar' as const,
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
        readonly access: Type.CaptureAccess
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
          if (
            expression.subject._tag === 'EffectConstruct' ||
            ((expression.subject._tag === 'BuiltinCall' ||
              expression.subject._tag === 'BoundOperationCall') &&
              expression.subject.witnessEffectSite !== undefined)
          )
            endLoans(fn, expression.subject.loanEnds, expression.span)
          return Object.freeze({ result: destination })
        }
        if (loweredSubject !== undefined && effectValueType?._tag === 'EffectComposite') {
          const result = lowerRunEffectComposite(
            fn,
            loweredSubject.result,
            effectValueType,
            expression.type,
            expression.span,
          )
          if (result !== undefined) endRunLoans(fn, expression.span)
          return result
        }
        if (recipe?._tag === 'EffectBindRequirement')
          return lowerEffectExecution(fn, recipe, expression.type, expression.span)
        if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'EffectSuspend') {
          const deferred = recipe.arguments.at(0)
          return deferred === undefined
            ? undefined
            : lowerEffectExecution(fn, deferred, expression.type, expression.span)
        }
        if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'ExecutionDrive') {
          const [executionExpression, branchExpression, completeExpression, suspendExpression] =
            recipe.arguments
          if (
            executionExpression === undefined ||
            branchExpression === undefined ||
            completeExpression === undefined ||
            suspendExpression === undefined
          )
            return undefined
          const execution = lowerExpression(fn, executionExpression)
          const branch = lowerExpression(fn, branchExpression)
          const onComplete = lowerExpression(fn, completeExpression)
          const onSuspend = lowerExpression(fn, suspendExpression)
          const type = fn.type(expression.type)
          const executionType =
            execution === undefined ? undefined : fn.localTypes.at(execution.result.ordinal)
          if (
            execution === undefined ||
            branch === undefined ||
            onComplete === undefined ||
            onSuspend === undefined ||
            executionType?._tag !== 'Nominal' ||
            !Type.isExecution(executionType.type) ||
            type?._tag !== 'Nominal' ||
            !Type.equals(type.type, Type.unit)
          )
            return undefined
          const destination = fn.alloc(type)
          const drivenResult = Type.typeArgumentAt(executionType.type, 0)
          const result = drivenResult === undefined ? undefined : fn.type(drivenResult)
          if (result === undefined || result._tag === 'EffectOutcome') return undefined
          const resultLocal = fn.alloc(result)
          const representedArguments = recipe.typeArguments.map((argument) =>
            fn.semanticArgument(argument),
          )
          const callableTypeArguments = (ordinal: number): ReadonlyArray<Type.GenericArgument> => {
            const argument = representedArguments.at(ordinal)
            return argument !== undefined &&
              Type.isExactRepresentationArgument(argument) &&
              Type.isCallableIdentityArgument(argument.identity)
              ? argument.identity.typeArguments
              : Object.freeze([])
          }
          const callbackCleanup = (local: Mir.LocalId): CleanupPlan.CleanupPlan => {
            const localType = fn.localTypes.at(local.ordinal)
            return localType?._tag === 'CallableValue'
              ? callableLocalCleanup(fn, localType)
              : concreteCleanup(
                  fn,
                  localType === undefined ? Type.unit : Mir.semanticType(localType),
                )
          }
          fn.emit(
            Object.freeze({
              _tag: 'ExecutionDrive' as const,
              destination,
              result: resultLocal,
              execution: execution.result,
              branch: branch.result,
              onComplete: onComplete.result,
              onSuspend: onSuspend.result,
              executionAccess: 'Take' as const,
              branchAccess: 'Take' as const,
              completionAccess: 'Take' as const,
              suspensionAccess: 'Take' as const,
              completionCleanup: callbackCleanup(onComplete.result),
              suspensionCleanup: callbackCleanup(onSuspend.result),
              completionTypeArguments: callableTypeArguments(2),
              suspensionTypeArguments: callableTypeArguments(3),
              type,
              provenance: authored(expression.span),
            }),
          )
          return Object.freeze({ result: destination })
        }
        if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'ExecutionPark') {
          const [registerExpression] = recipe.arguments
          if (registerExpression === undefined) return undefined
          const register = lowerExpression(fn, registerExpression)
          const type = fn.type(expression.type)
          const guardArgument = recipe.typeArguments.at(0)
          const semanticGuard =
            guardArgument === undefined ? undefined : fn.semanticArgument(guardArgument)
          const representation = recipe.typeArguments.at(1)
          const semanticRepresentation =
            representation === undefined ? undefined : fn.semanticArgument(representation)
          const guardType =
            semanticGuard !== undefined &&
            (Type.isTypeArgument(semanticGuard) ||
              (typeof semanticGuard !== 'string' && semanticGuard._tag === 'RepresentedType'))
              ? semanticGuard
              : undefined
          const registrationIdentity =
            semanticRepresentation !== undefined &&
            Type.isExactRepresentationArgument(semanticRepresentation) &&
            Type.isCallableIdentityArgument(semanticRepresentation.identity)
              ? semanticRepresentation.identity
              : undefined
          const registrationTarget = registrationIdentity?.target
          const registrationArguments = registrationIdentity?.typeArguments ?? Object.freeze([])
          const resultCallableCandidates =
            registrationTarget?._tag === 'Declaration'
              ? fn.instances.flatMap((candidate) => {
                  if (
                    candidate.key.declaration.module !== registrationTarget.module ||
                    candidate.key.declaration.name !== registrationTarget.name ||
                    !registrationArguments.every((argument, ordinal) => {
                      const candidateArgument = candidate.key.typeArguments.at(ordinal)
                      return (
                        candidateArgument !== undefined &&
                        Type.equalsGenericArgument(argument, candidateArgument)
                      )
                    }) ||
                    candidate.resultCallable === undefined
                  )
                    return []
                  return [candidate.resultCallable]
                })
              : Object.freeze([])
          const resultCallable = resultCallableCandidates.at(0)
          const unambiguousResultCallable =
            resultCallable !== undefined &&
            resultCallableCandidates.every((candidate) =>
              Type.equalsGenericArgument(resultCallable, candidate),
            )
              ? resultCallable
              : undefined
          const guard =
            guardType === undefined
              ? undefined
              : (fn.type(guardType) ??
                (Type.isCallable(guardType) && unambiguousResultCallable !== undefined
                  ? (() => {
                      const realized = callableValueByIdentity(
                        fn.layout,
                        unambiguousResultCallable,
                        guardType,
                      )
                      return realized === undefined
                        ? undefined
                        : Object.freeze({ ...realized, type: guardType })
                    })()
                  : undefined))
          const registerType =
            register === undefined ? undefined : fn.localTypes.at(register.result.ordinal)
          if (
            register === undefined ||
            registerType?._tag !== 'CallableValue' ||
            guard === undefined ||
            guard._tag === 'EffectOutcome' ||
            type?._tag !== 'Nominal' ||
            !Type.equals(type.type, Type.unit)
          )
            return undefined
          const destination = fn.alloc(type)
          const guardLocal = fn.alloc(guard)
          const registrationTypeArguments =
            semanticRepresentation !== undefined &&
            Type.isExactRepresentationArgument(semanticRepresentation) &&
            Type.isCallableIdentityArgument(semanticRepresentation.identity)
              ? semanticRepresentation.identity.typeArguments
              : Object.freeze([])
          fn.emit(
            Object.freeze({
              _tag: 'ExecutionPark' as const,
              destination,
              guard: guardLocal,
              register: register.result,
              registerAccess: 'Take' as const,
              guardCleanup:
                guard._tag === 'CallableValue'
                  ? callableLocalCleanup(fn, guard)
                  : concreteCleanup(fn, Mir.semanticType(guard)),
              registerCleanup: callableLocalCleanup(fn, registerType),
              registrationTypeArguments,
              type,
              provenance: authored(expression.span),
            }),
          )
          return Object.freeze({ result: destination })
        }
        if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'StorageAcquire') {
          const [layoutExpression] = recipe.arguments
          if (layoutExpression === undefined || fn.effectOutcome === undefined) return undefined
          const loweredLayout = lowerExpression(fn, layoutExpression)
          const type = fn.type(expression.type)
          const propagationType = fn.type(fn.effectOutcome)
          const failureTag = Type.failureMembers(fn.effectOutcome).findIndex((failure) =>
            Type.equals(failure, Type.storageFailure),
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
              failure: Type.storageFailure,
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
          endLoans(fn, recipe.loanEnds, expression.span)
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
          endLoans(fn, recipe.loanEnds, expression.span)
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
        endLoans(fn, recipe.loanEnds, expression.span)
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
      const substituted = TypeCompatibility.check(
        fn.semantic(expression.sourceType),
        fn.semantic(expression.target),
      )
      // An open `A | B` conversion may disappear when both parameters specialize to the same
      // concrete type. The specialized program then carries no runtime union tag at this site.
      if (substituted._tag === 'Exact' || substituted._tag === 'Bottom') return source
      const sourceShape = Layout.callingShape(fn.layout, fn.semantic(expression.sourceType))
      const targetShape = Layout.callingShape(fn.layout, fn.semantic(expression.target))
      if (
        source === undefined ||
        sourceShape === undefined ||
        targetShape === undefined ||
        sourceType === undefined ||
        sourceType._tag === 'EffectOutcome' ||
        targetType?._tag !== 'Union'
      ) {
        return undefined
      }
      const destination = fn.alloc(targetType)
      // Canonical union member order can change under substitution (parameter keys sort
      // differently from concrete keys), so the mapping recomputes at the instantiation.
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
      const resultShape =
        resultType?._tag === 'EffectComposite'
          ? effectCompositeShape(fn.layout, resultType)
          : Layout.callingShape(fn.layout, fn.semantic(expression.type))
      if (
        scrutinee === undefined ||
        scrutineeType === undefined ||
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
      const specializeMember = (member: Match.CoverageIdentity): Match.CoverageIdentity =>
        member._tag === 'StructuralTypeMember'
          ? Match.structuralMember(fn.semantic(member.type))
          : member
      const members = Object.freeze(expression.members.map(specializeMember))
      const specializedCoverage = Match.cover(
        members,
        expression.arms.map((arm) =>
          Object.freeze({
            ...(arm.member === undefined ? {} : { member: specializeMember(arm.member) }),
            universal: arm.universal,
            guarded: arm.guard !== undefined,
          }),
        ),
      )
      const arms: Array<Mir.MatchArm> = []
      const armStates = new Map<number, DelayedEffectState>()
      const branchState = delayedEffectState(fn)
      for (const [armOrdinal, arm] of expression.arms.entries()) {
        const transition = specializedCoverage.transitions.at(armOrdinal)
        if (!arm.reachable || transition?.reachable !== true) continue
        restoreDelayedEffectState(fn, branchState)
        const member = arm.member === undefined ? undefined : specializeMember(arm.member)
        const before = transition.before
        const after = transition.after
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
        const [selectedResult, selectedOperations] = fn.capture(() => {
          const lowered = lowerExpression(fn, arm.result)
          if (lowered === undefined || resultType._tag !== 'EffectComposite') return lowered
          const selectedType = fn.localTypes.at(lowered.result.ordinal)
          if (selectedType?._tag !== 'EffectValue') return undefined
          const selectedIdentity = Instances.effectIdentity(
            selectedType.environment.instance,
            selectedType.site,
          )
          const alternative = resultType.alternatives.findIndex(
            (candidate) =>
              Instances.effectIdentity(candidate.environment.instance, candidate.site) ===
              selectedIdentity,
          )
          if (alternative < 0) return undefined
          const packed = fn.alloc(resultType)
          fn.emit(
            Object.freeze({
              _tag: 'PackEffectComposite',
              destination: packed,
              source: lowered.result,
              alternative,
              type: resultType,
              provenance: authored(arm.result.span),
            }),
          )
          return Object.freeze({ result: packed })
        })
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
                  arm.universal ||
                  (arm.member !== undefined && Match.identityEquals(arm.member, member)),
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
      const mirDecisions = decisions.map((decision) =>
        Object.freeze({
          member: decision.member,
          candidates: decision.candidates,
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
          retainsBindings: false,
          members: Object.freeze(members),
          decisions: Object.freeze(mirDecisions),
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
      const temporary =
        expression.root._tag === 'TemporarySliceRoot'
          ? lowerExpression(fn, expression.root.value)
          : undefined
      const root =
        expression.root._tag === 'BindingSliceRoot'
          ? fn.bindingLocals.get(expression.root.binding.ordinal)
          : expression.root._tag === 'ParameterSliceRoot'
            ? local(expression.root.parameter.ordinal)
            : expression.root._tag === 'PatternSliceRoot'
              ? fn.patternLocals.get(patternKey(expression.root.binding))
              : temporary?.result
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
      const selectors = lowerBorrowSelectors(fn, expression.selectors)
      if (selectors === undefined) return undefined
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination,
          root,
          selectors,
          sourceType,
          type,
          access: expression.access,
          reborrow: expression.reborrow,
          suspendsParent: expression.suspendsParent,
          provenance: authored(expression.span),
        }),
      )
      if (expression.root._tag === 'TemporarySliceRoot') {
        fn.temporaryBorrowOwners.set(
          borrowKey(borrow),
          Object.freeze({
            local: root,
            cleanup: CleanupPlan.cleanupPlan(fn.index, fn.semantic(expression.source)),
            span: expression.root.owner.span,
          }),
        )
      }
      fn.loanLocals.set(borrowKey(borrow), destination)
      return Object.freeze({ result: destination })
    }
    case 'ValueBorrow': {
      const temporary =
        expression.root._tag === 'TemporarySliceRoot'
          ? lowerExpression(fn, expression.root.value)
          : undefined
      const root =
        expression.root._tag === 'BindingSliceRoot'
          ? fn.bindingLocals.get(expression.root.binding.ordinal)
          : expression.root._tag === 'ParameterSliceRoot'
            ? local(expression.root.parameter.ordinal)
            : expression.root._tag === 'PatternSliceRoot'
              ? fn.patternLocals.get(patternKey(expression.root.binding))
              : temporary?.result
      const sourceType = fn.type(expression.source)
      const type = fn.type(expression.type)
      if (root === undefined || sourceType === undefined || type?._tag !== 'Reference') {
        return undefined
      }
      const destination = fn.alloc(type)
      const borrow = fn.beginRecipeBorrow(expression.borrow)
      const selectors = lowerBorrowSelectors(fn, expression.selectors)
      if (selectors === undefined) return undefined
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination,
          root,
          selectors,
          sourceType,
          type,
          access: expression.access,
          reborrow: false,
          suspendsParent: false,
          provenance: authored(expression.span),
        }),
      )
      if (expression.root._tag === 'TemporarySliceRoot') {
        fn.temporaryBorrowOwners.set(
          borrowKey(borrow),
          Object.freeze({
            local: root,
            cleanup: CleanupPlan.cleanupPlan(fn.index, fn.semantic(expression.source)),
            span: expression.root.owner.span,
          }),
        )
      }
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
      const typeArguments = Object.freeze(
        call?.target.typeArguments ??
          expression.typeArguments.map((argument) => fn.semanticArgument(argument)),
      )
      const type =
        (call?.resultEffect === undefined
          ? undefined
          : effectValueByIdentity(fn.layout, call.resultEffect)) ??
        fn.effectResults.get(instanceText(expression.target, typeArguments)) ??
        fn.type(expression.type)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          typeArguments,
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
      const selected = ConformanceProof.interfaceOperationIntrinsic(
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
          loanEnds: expression.loanEnds,
          heldLoans: Object.freeze([]),
          type: expression.type,
          span: expression.span,
        }),
      )
    }
    case 'BuiltinCall':
      return lowerBuiltinExpression(fn, expression)
    case 'Unavailable':
      return undefined
  }
}
