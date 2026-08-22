import { authored, concreteCleanup, generated } from './CleanupEmission.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type { LoweredExpression } from './EffectLowering.js'
import type {} from './EntryAssembly.js'
import type {} from './Forwarding.js'
import type { FunctionLowering } from './FunctionLowering.js'
import type * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as Layout from './Layout.js'
import * as LocalSharedAllocationProvenance from './LocalSharedAllocationProvenance.js'
import * as LocalSharedControlBlock from './LocalSharedControlBlock.js'
import { borrowKey, isOsOperation, usize } from './Lower.js'
import * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'
import * as Type from './Type.js'
import {
  lowerBuiltinArguments,
  lowerInterfaceWitnessCall,
  lowerWitnessEffect,
} from './WitnessLowering.js'

export const lowerBuiltinExpression = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }>,
): LoweredExpression | undefined => {
  if (expression.witnessEffectSite !== undefined) return lowerWitnessEffect(fn, expression)
  const intrinsic = Intrinsic.findOperationById(expression.intrinsic)
  if (intrinsic === undefined || !Intrinsic.isBuiltinOperation(intrinsic)) return undefined
  const argumentLocals = lowerBuiltinArguments(fn, expression, intrinsic)
  if (argumentLocals === undefined) return undefined
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
  if (expression.operation === 'LayoutOf' || expression.operation === 'SharedLayout') {
    const raw = expression.typeArguments.at(0)
    const element = raw === undefined ? undefined : fn.semantic(raw)
    const elementLayout = element === undefined ? undefined : Layout.entry(fn.layout, element)
    const sharedBlock =
      expression.operation === 'SharedLayout' &&
      element !== undefined &&
      elementLayout !== undefined
        ? LocalSharedControlBlock.plan(fn.layout.target, element, elementLayout)
        : undefined
    const layoutEntry = Layout.entry(fn.layout, Type.layout)
    const type = fn.type(Type.layout)
    if (
      elementLayout === undefined ||
      (sharedBlock !== undefined && sharedBlock._tag !== 'LocalSharedControlBlockPlan') ||
      layoutEntry?.representation._tag !== 'Aggregate' ||
      type?._tag !== 'Nominal'
    )
      return undefined
    const fields: Array<{
      readonly field: DeclarationFacts.FieldId
      readonly value: Mir.LocalId
    }> = []
    for (const field of layoutEntry.representation.fields) {
      const value = fn.alloc(usize)
      fn.emit(
        Object.freeze({
          _tag: 'Literal' as const,
          destination: value,
          type: usize,
          value: BigInt(
            field.name === 'bytes'
              ? (sharedBlock?.size ?? elementLayout.size)
              : (sharedBlock?.alignment ?? elementLayout.alignment),
          ),
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
        stride: Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
        elementAlignment: elementLayout.alignment,
        type,
        provenance: authored(expression.span),
      }),
    )
    return finishBuiltin(destination)
  }
  if (expression.operation === 'SharedFromAllocation') {
    const [allocation, value] = argumentLocals
    const type = fn.type(expression.type)
    const raw = Type.isSharedCore(expression.type)
      ? Type.typeArgumentAt(expression.type, 0)
      : undefined
    const element = raw === undefined ? undefined : fn.semantic(raw)
    const elementLayout = element === undefined ? undefined : Layout.entry(fn.layout, element)
    const block =
      element === undefined || elementLayout === undefined
        ? undefined
        : LocalSharedControlBlock.plan(fn.layout.target, element, elementLayout)
    const allocationProvenance = LocalSharedAllocationProvenance.find(
      fn.layout.localSharedAllocationProvenance,
      fn.owner.key,
      expression,
    )
    const allocationElementLayout =
      allocationProvenance === undefined
        ? undefined
        : Layout.entry(fn.layout, allocationProvenance.element)
    const allocationBlock =
      allocationProvenance === undefined || allocationElementLayout === undefined
        ? undefined
        : LocalSharedControlBlock.plan(
            fn.layout.target,
            allocationProvenance.element,
            allocationElementLayout,
          )
    if (
      allocation === undefined ||
      value === undefined ||
      type?._tag !== 'Nominal' ||
      !Type.isSharedCore(type.type) ||
      element === undefined ||
      block?._tag !== 'LocalSharedControlBlockPlan' ||
      allocationBlock?._tag !== 'LocalSharedControlBlockPlan'
    )
      return undefined
    const destination = fn.alloc(type)
    fn.emit(
      Object.freeze({
        _tag: 'SharedFromAllocation' as const,
        destination,
        allocation,
        value,
        element,
        block,
        allocationBlock,
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
        stride: Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
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
        stride: Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
        retainsSource: Mir.isCopy(fn.layout, semanticElement),
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
      slotType !== undefined && Type.isSlot(slotType) ? Type.typeArgumentAt(slotType, 0) : undefined
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
      slotType !== undefined && Type.isSlot(slotType) ? Type.typeArgumentAt(slotType, 0) : undefined
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
    const valueScalar =
      expression.operation === 'CheckedConvertToChar'
        ? Scalar.character
        : (conversionTarget ?? sourceScalar)
    const targetType = fn.type(expression.type)
    if (
      first === undefined ||
      sourceScalar?.category !== 'Integer' ||
      (valueScalar?.category !== 'Integer' && valueScalar?.category !== 'Character') ||
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
        _tag: 'CheckedScalar' as const,
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
      sourceScalar === undefined ||
      sourceScalar?.category === 'Boolean' ||
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
    const scalar = typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
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
    return finishBuiltin(destination)
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
  return finishBuiltin(destination)
}
