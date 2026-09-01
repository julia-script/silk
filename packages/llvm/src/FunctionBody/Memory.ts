import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as AddrSpace from '../AddrSpace.js'
import * as Alignment from '../Alignment.js'
import * as ByteString from '../ByteString.js'
import * as Constant from '../Constant.js'
import type * as FunctionBodyDescription from '../internal/FunctionBodyDescription.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import * as Handle from '../internal/Handle.js'
import { invalidInput, invalidState, type LlvmError } from '../LlvmError.js'
import * as MemoryAccess from '../MemoryAccess.js'
import * as Type from '../Type.js'
import type * as Value from '../Value.js'
import type { FunctionBody, Instruction } from './_internal.js'

/**
 * Element count, alignment, address space, and `inalloca` settings for {@link alloca}.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface AllocaOptions {
  readonly count?: Value.Input
  readonly alignment?: Alignment.Alignment
  readonly addressSpace?: AddrSpace.AddrSpace
  readonly inAlloca?: boolean
}

/**
 * Bounds promises and optional in-range index for get-element-pointer operations.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface GetElementPtrOptions {
  readonly inbounds?: boolean
  readonly inrange?: number
}

/**
 * Memory-access settings plus mandatory failure ordering for {@link compareExchange}.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface CompareExchangeOptions extends MemoryAccess.Input {
  readonly failureOrdering: Exclude<
    MemoryAccess.AtomicOrdering,
    'none' | 'unordered' | 'release' | 'acq_rel'
  >
  readonly weak?: boolean
}

/** @internal */
const accessInfo = (input: MemoryAccess.Input): FunctionBodyDescription.MemoryInfo => {
  const access = MemoryAccess.make(input)
  return Object.freeze({
    kind: access.kind,
    alignment: access.alignment,
    syncScope: access.syncScope,
    ordering: access.ordering,
  })
}

/**
 * Appends stack allocation for a sized element type and optional integer count.
 *
 * @category instructions
 * @since 0.0.0
 */
export const alloca = Effect.fnUntraced(function* (
  self: FunctionBody,
  allocationType: Type.Type,
  name?: ByteString.ByteString | Uint8Array | string,
  options: AllocaOptions = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const addressSpace = options.addressSpace ?? AddrSpace.defaultAddrSpace
  const pointerType = yield* Type.pointer(builder, addressSpace)
  const count =
    options.count ?? (yield* Constant.integerUnsigned(builder, yield* Type.integer(builder, 32), 1))
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.alloca', (draft, module) =>
    Result.gen(function* () {
      const allocationTypeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        allocationType,
        'Type',
        'FunctionBody.alloca',
      )
      const allocation = yield* FunctionBodyState.typeAt(
        module,
        allocationTypeIndex,
        'FunctionBody.alloca',
      )
      if (
        allocation._tag === 'Simple' &&
        (allocation.tag === 'Void' || allocation.tag === 'Label')
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.alloca',
            message: 'alloca requires an allocatable value type',
            input: allocationType,
          }),
        )
      }
      if (allocation._tag === 'Function') {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.alloca',
            message: 'alloca cannot allocate a function type',
            input: allocationType,
          }),
        )
      }
      const countValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        count,
        'FunctionBody.alloca',
      )
      if (
        !(yield* FunctionBodyState.isIntegerType(module, countValue.type, 'FunctionBody.alloca'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.alloca',
            message: 'alloca count must have integer type',
            input: count,
          }),
        )
      }
      const resultType = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        pointerType,
        'Type',
        'FunctionBody.alloca',
      )
      return (yield* FunctionBodyState.appendResult(draft, resultType, name, (result, finalName) =>
        Object.freeze({
          _tag: 'Alloca',
          allocationType: allocationTypeIndex,
          count: countValue.operand,
          addressSpace: addressSpace.value,
          alignment: options.alignment ?? Alignment.defaultAlignment,
          inAlloca: options.inAlloca ?? false,
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/**
 * Appends a typed load from a pointer or vector of pointers after ordering validation.
 *
 * @category instructions
 * @since 0.0.0
 */
export const load = Effect.fnUntraced(function* (
  self: FunctionBody,
  valueType: Type.Type,
  pointer: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: MemoryAccess.Input = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const access = accessInfo(options)
  yield* MemoryAccess.validateLoadOrdering(access.ordering)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.load', (draft, module) =>
    Result.gen(function* () {
      const pointerValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        pointer,
        'FunctionBody.load',
      )
      if (
        !(yield* FunctionBodyState.isPointerType(module, pointerValue.type, 'FunctionBody.load'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.load',
            message: 'load requires a pointer operand',
            input: pointer,
          }),
        )
      }
      const type = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        valueType,
        'Type',
        'FunctionBody.load',
      )
      return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
        Object.freeze({
          _tag: 'Load',
          valueType: type,
          pointer: pointerValue.operand,
          access,
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/**
 * Appends a store to a pointer after validating atomic ordering and alignment settings.
 *
 * @category instructions
 * @since 0.0.0
 */
export const store = Effect.fnUntraced(function* (
  self: FunctionBody,
  value: Value.Input,
  pointer: Value.Input,
  options: MemoryAccess.Input = {},
): Effect.fn.Return<Instruction, LlvmError> {
  const access = accessInfo(options)
  yield* MemoryAccess.validateStoreOrdering(access.ordering)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.store', (draft, module) =>
    Result.gen(function* () {
      const stored = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.store',
      )
      const destination = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        pointer,
        'FunctionBody.store',
      )
      if (
        !(yield* FunctionBodyState.isPointerType(module, destination.type, 'FunctionBody.store'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.store',
            message: 'store requires a pointer destination',
            input: pointer,
          }),
        )
      }
      return yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'Store',
          value: stored.operand,
          pointer: destination.operand,
          access,
          result: undefined,
          name: ByteString.empty,
        }),
      )
    }),
  )
})

/** @internal */
const gepPlan = Effect.fnUntraced(function* (
  self: FunctionBody,
  sourceType: Type.Type,
  base: Value.Input,
  indices: ReadonlyArray<Value.Input>,
  options: GetElementPtrOptions,
): Effect.fn.Return<
  {
    readonly sourceType: number
    readonly base: FunctionBodyDescription.Operand
    readonly indices: ReadonlyArray<FunctionBodyDescription.Operand>
    readonly pointerType: Type.Type
    readonly pointerScalarType: Type.Type
    readonly baseIsVector: boolean
    readonly vector: { readonly length: number; readonly scalable: boolean } | undefined
  },
  LlvmError
> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.getElementPtr',
    (draft, module) =>
      Result.gen(function* () {
        if (indices.length === 0) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.getElementPtr',
              message: 'getelementptr requires at least one index',
              input: indices,
            }),
          )
        }
        if (
          options.inrange !== undefined &&
          (!Number.isSafeInteger(options.inrange) ||
            options.inrange < 0 ||
            options.inrange >= indices.length)
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.getElementPtr',
              message: 'inrange must identify an existing GEP index',
              input: options.inrange,
            }),
          )
        }
        const source = yield* Handle.resolve(
          draft.builder,
          draft.moduleOwner,
          sourceType,
          'Type',
          'FunctionBody.getElementPtr',
        )
        const pointer = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          base,
          'FunctionBody.getElementPtr',
        )
        const pointerDescription = yield* FunctionBodyState.typeAt(
          module,
          pointer.type,
          'FunctionBody.getElementPtr',
        )
        const pointerScalar =
          pointerDescription._tag === 'Vector'
            ? yield* FunctionBodyState.typeAt(
                module,
                pointerDescription.child,
                'FunctionBody.getElementPtr',
              )
            : pointerDescription
        if (pointerScalar._tag !== 'Pointer') {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.getElementPtr',
              message: 'getelementptr base must be a pointer or vector of pointers',
              input: base,
            }),
          )
        }
        const pointerType = module.types.handles[pointer.type]
        const pointerScalarType =
          module.types.handles[
            pointerDescription._tag === 'Vector' ? pointerDescription.child : pointer.type
          ]
        if (pointerType === undefined || pointerScalarType === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'FunctionBody.getElementPtr',
              message: 'GEP pointer type handle is missing',
              state: base,
            }),
          )
        }
        let current = source
        let vector =
          pointerDescription._tag === 'Vector'
            ? { length: pointerDescription.length, scalable: pointerDescription.scalable }
            : undefined
        const resolved: Array<FunctionBodyDescription.Operand> = []
        for (let position = 0; position < indices.length; position += 1) {
          const input = indices[position]
          if (input === undefined) continue
          const index = yield* FunctionBodyState.resolveOperand(
            draft,
            module,
            input,
            'FunctionBody.getElementPtr',
          )
          const indexType = yield* FunctionBodyState.typeAt(
            module,
            index.type,
            'FunctionBody.getElementPtr',
          )
          const indexScalar =
            indexType._tag === 'Vector'
              ? yield* FunctionBodyState.typeAt(
                  module,
                  indexType.child,
                  'FunctionBody.getElementPtr',
                )
              : indexType
          if (indexScalar._tag !== 'Integer') {
            return yield* Result.fail(
              invalidInput({
                operation: 'FunctionBody.getElementPtr',
                message: 'getelementptr indices must be integer scalars or vectors',
                input: input,
              }),
            )
          }
          if (indexType._tag === 'Vector') {
            const shape = { length: indexType.length, scalable: indexType.scalable }
            if (
              vector !== undefined &&
              (vector.length !== shape.length || vector.scalable !== shape.scalable)
            ) {
              return yield* Result.fail(
                invalidInput({
                  operation: 'FunctionBody.getElementPtr',
                  message: 'Vector GEP operands must have one vector shape',
                  input: input,
                }),
              )
            }
            vector = shape
          }
          if (position === 0) {
            resolved.push(index.operand)
            continue
          }
          const aggregate = yield* FunctionBodyState.typeAt(
            module,
            current,
            'FunctionBody.getElementPtr',
          )
          if (aggregate._tag === 'Array' || aggregate._tag === 'Vector') {
            current = aggregate.child
            resolved.push(index.operand)
            continue
          }
          let body: { readonly fields: ReadonlyArray<number>; readonly packed: boolean } | undefined
          if (aggregate._tag === 'Structure') body = aggregate
          else if (aggregate._tag === 'NamedStructure') body = aggregate.body
          if (body === undefined || index.operand._tag !== 'Constant') {
            return yield* Result.fail(
              invalidInput({
                operation: 'FunctionBody.getElementPtr',
                message: 'Structure GEP indices must be exact integer constants',
                input: input,
              }),
            )
          }
          const constant = module.constants.descriptions[index.operand.constant]
          const field =
            constant?._tag === 'Integer' && constant.bitPattern <= BigInt(Number.MAX_SAFE_INTEGER)
              ? body.fields[Number(constant.bitPattern)]
              : undefined
          if (field === undefined) {
            return yield* Result.fail(
              invalidInput({
                operation: 'FunctionBody.getElementPtr',
                message: 'Structure GEP index is outside the selected aggregate',
                input: input,
              }),
            )
          }
          current = field
          resolved.push(index.operand)
        }
        return {
          sourceType: source,
          base: pointer.operand,
          indices: Object.freeze(resolved),
          pointerType,
          pointerScalarType,
          baseIsVector: pointerDescription._tag === 'Vector',
          vector,
        }
      }),
  )
})

/**
 * Appends scalar or vector `getelementptr` with fully typed dynamic indices.
 *
 * **Gotchas**
 *
 * Aggregate paths, integer index types, vector shapes, and `inrange` bounds are validated
 * before the instruction is appended. Use {@link structuredGetElementPtr} for numeric field paths.
 *
 * @category instructions
 * @since 0.0.0
 */
export const getElementPtr = Effect.fnUntraced(function* (
  self: FunctionBody,
  sourceType: Type.Type,
  base: Value.Input,
  indices: ReadonlyArray<Value.Input>,
  name?: ByteString.ByteString | Uint8Array | string,
  options: GetElementPtrOptions = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const plan = yield* gepPlan(self, sourceType, base, indices, options)
  const builder = yield* FunctionBodyState.builder(self)
  let resultType = plan.pointerType
  if (plan.vector !== undefined && !plan.baseIsVector) {
    resultType = plan.vector.scalable
      ? yield* Type.scalableVector(builder, plan.pointerScalarType, plan.vector.length)
      : yield* Type.vector(builder, plan.pointerScalarType, plan.vector.length)
  }
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.getElementPtr', (draft) =>
    Result.gen(function* () {
      const resultTypeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        resultType,
        'Type',
        'FunctionBody.getElementPtr',
      )
      return (yield* FunctionBodyState.appendResult(
        draft,
        resultTypeIndex,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'GetElementPtr',
            sourceType: plan.sourceType,
            base: plan.base,
            indices: plan.indices,
            inbounds: options.inbounds ?? false,
            inrange: options.inrange,
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Converts numeric aggregate indices to constants and delegates to {@link getElementPtr}.
 *
 * @category instructions
 * @since 0.0.0
 */
export const structuredGetElementPtr = Effect.fnUntraced(function* (
  self: FunctionBody,
  sourceType: Type.Type,
  base: Value.Input,
  fields: ReadonlyArray<number>,
  name?: ByteString.ByteString | Uint8Array | string,
  options: GetElementPtrOptions = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const i32 = yield* Type.integer(builder, 32)
  const exact = [0, ...fields]
  const indices: Array<Constant.Constant> = []
  for (const field of exact) {
    if (!Number.isSafeInteger(field) || field < 0) {
      return yield* invalidInput({
        operation: 'FunctionBody.structuredGetElementPtr',
        message: 'Structured GEP fields must be non-negative integers',
        input: field,
      })
    }
    indices.push(yield* Constant.integerUnsigned(builder, i32, field))
  }
  return yield* getElementPtr(self, sourceType, base, indices, name, options)
})

/**
 * Appends an atomic fence with acquire-or-stronger validated ordering.
 *
 * @category instructions
 * @since 0.0.0
 */
export const fence = Effect.fnUntraced(function* (
  self: FunctionBody,
  ordering: Exclude<MemoryAccess.AtomicOrdering, 'none' | 'unordered' | 'monotonic'>,
  syncScope: MemoryAccess.SyncScope = 'system',
): Effect.fn.Return<Instruction, LlvmError> {
  yield* MemoryAccess.validateFenceOrdering(ordering)
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.fence', (draft) =>
    FunctionBodyState.appendInstruction(
      draft,
      Object.freeze({
        _tag: 'Fence',
        syncScope,
        ordering,
        result: undefined,
        name: ByteString.empty,
      }),
    ),
  )
})

/**
 * Appends `cmpxchg` and returns LLVM's `{ value, i1 }` result structure.
 *
 * **Gotchas**
 *
 * The failure ordering may not contain release semantics and may not be stronger than the
 * success ordering. Pointer, expected, and replacement types must agree.
 *
 * @category instructions
 * @since 0.0.0
 */
export const compareExchange = Effect.fnUntraced(function* (
  self: FunctionBody,
  pointer: Value.Input,
  comparison: Value.Input,
  replacement: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: CompareExchangeOptions = { failureOrdering: 'monotonic' },
): Effect.fn.Return<Value.Value, LlvmError> {
  const access = accessInfo({ ...options, ordering: options.ordering ?? 'monotonic' })
  yield* MemoryAccess.validateCompareExchange(access.ordering, options.failureOrdering)
  const builder = yield* FunctionBodyState.builder(self)
  const comparisonType = yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.compareExchange',
    (draft, module) =>
      Result.gen(function* () {
        const value = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          comparison,
          'FunctionBody.compareExchange',
        )
        const handle = module.types.handles[value.type]
        if (handle === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'FunctionBody.compareExchange',
              message: 'Comparison type handle is missing',
              state: comparison,
            }),
          )
        }
        return handle
      }),
  )
  const resultType = yield* Type.structure(builder, [
    comparisonType,
    yield* Type.integer(builder, 1),
  ])
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.compareExchange',
    (draft, module) =>
      Result.gen(function* () {
        const address = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          pointer,
          'FunctionBody.compareExchange',
        )
        const expected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          comparison,
          'FunctionBody.compareExchange',
        )
        const desired = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          replacement,
          'FunctionBody.compareExchange',
        )
        if (
          !(yield* FunctionBodyState.isPointerType(
            module,
            address.type,
            'FunctionBody.compareExchange',
          )) ||
          expected.type !== desired.type
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.compareExchange',
              message: 'cmpxchg requires a pointer and matching comparison/replacement values',
              input: { pointer, comparison, replacement },
            }),
          )
        }
        const type = yield* Handle.resolve(
          draft.builder,
          draft.moduleOwner,
          resultType,
          'Type',
          'FunctionBody.compareExchange',
        )
        return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
          Object.freeze({
            _tag: 'CompareExchange',
            pointer: address.operand,
            comparison: expected.operand,
            replacement: desired.operand,
            access,
            failureOrdering: options.failureOrdering,
            weak: options.weak ?? false,
            result,
            name: finalName,
          }),
        )).value
      }),
  )
})

/**
 * Appends a type-compatible atomic read-modify-write with at least monotonic ordering.
 *
 * @category instructions
 * @since 0.0.0
 */
export const atomicRmw = Effect.fnUntraced(function* (
  self: FunctionBody,
  operation: MemoryAccess.AtomicOperation,
  pointer: Value.Input,
  value: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: MemoryAccess.Input = { ordering: 'monotonic' },
): Effect.fn.Return<Value.Value, LlvmError> {
  const access = accessInfo({ ...options, ordering: options.ordering ?? 'monotonic' })
  yield* MemoryAccess.validateRmwOrdering(access.ordering)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.atomicRmw', (draft, module) =>
    Result.gen(function* () {
      const address = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        pointer,
        'FunctionBody.atomicRmw',
      )
      const operand = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.atomicRmw',
      )
      if (
        !(yield* FunctionBodyState.isPointerType(module, address.type, 'FunctionBody.atomicRmw'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.atomicRmw',
            message: 'atomicrmw requires a pointer operand',
            input: pointer,
          }),
        )
      }
      const floating =
        operation === 'fadd' || operation === 'fsub' || operation === 'fmax' || operation === 'fmin'
      if (
        floating
          ? !(yield* FunctionBodyState.isFloatingType(
              module,
              operand.type,
              'FunctionBody.atomicRmw',
            ))
          : !(yield* FunctionBodyState.isIntegerType(
              module,
              operand.type,
              'FunctionBody.atomicRmw',
            )) && operation !== 'xchg'
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.atomicRmw',
            message: 'atomicrmw operation is incompatible with the value type',
            input: { operation, value },
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        operand.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'AtomicRmw',
            operation,
            pointer: address.operand,
            value: operand.operand,
            access,
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Appends `va_arg` from a pointer-like list value and explicit result type.
 *
 * @category instructions
 * @since 0.0.0
 */
export const vaArg = Effect.fnUntraced(function* (
  self: FunctionBody,
  list: Value.Input,
  valueType: Type.Type,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.vaArg', (draft, module) =>
    Result.gen(function* () {
      const source = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        list,
        'FunctionBody.vaArg',
      )
      if (!(yield* FunctionBodyState.isPointerType(module, source.type, 'FunctionBody.vaArg'))) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.vaArg',
            message: 'va_arg requires a pointer list operand',
            input: list,
          }),
        )
      }
      const type = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        valueType,
        'Type',
        'FunctionBody.vaArg',
      )
      return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
        Object.freeze({
          _tag: 'VaArg',
          list: source.operand,
          valueType: type,
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})
