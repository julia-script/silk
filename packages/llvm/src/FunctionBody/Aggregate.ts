import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as ByteString from '../ByteString.js'
import * as Constant from '../Constant.js'
import type * as BuilderState from '../internal/BuilderState.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import * as Handle from '../internal/Handle.js'
import { invalidInput, invalidState, type LlvmError } from '../LlvmError.js'
import * as Type from '../Type.js'
import type * as Value from '../Value.js'
import { type FunctionBody, sameOperands } from './_internal.js'

/** @internal */
const aggregatePath = (
  module: BuilderState.MutableState,
  root: number,
  indices: ReadonlyArray<number>,
  operation: string,
): Result.Result<number, LlvmError> =>
  Result.gen(function* () {
    if (indices.length === 0) {
      return yield* Result.fail(
        invalidInput({ operation, message: 'Aggregate paths cannot be empty', input: indices }),
      )
    }
    let current = root
    for (const index of indices) {
      if (!Number.isSafeInteger(index) || index < 0) {
        return yield* Result.fail(
          invalidInput({
            operation,
            message: 'Aggregate indices must be non-negative integers',
            input: index,
          }),
        )
      }
      const description = yield* FunctionBodyState.typeAt(module, current, operation)
      if (description._tag === 'Array' || description._tag === 'Vector') {
        const length =
          description._tag === 'Array' ? description.length : BigInt(description.length)
        if (BigInt(index) >= length) {
          return yield* Result.fail(
            invalidInput({
              operation,
              message: 'Aggregate index is outside the aggregate',
              input: index,
            }),
          )
        }
        current = description.child
        continue
      }
      let fields: ReadonlyArray<number> | undefined
      if (description._tag === 'Structure') fields = description.fields
      else if (description._tag === 'NamedStructure') fields = description.body?.fields
      const field = fields?.[index]
      if (field === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation,
            message: 'Aggregate path does not select a field',
            input: indices,
          }),
        )
      }
      current = field
    }
    return current
  })

/**
 * Extracts a value along a non-empty, statically indexed structure path.
 *
 * @category instructions
 * @since 0.0.0
 */
export const extractValue = Effect.fnUntraced(function* (
  self: FunctionBody,
  aggregate: Value.Input,
  indices: ReadonlyArray<number>,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.extractValue', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        aggregate,
        'FunctionBody.extractValue',
      )
      const resultType = yield* aggregatePath(
        module,
        resolved.type,
        indices,
        'FunctionBody.extractValue',
      )
      return (yield* FunctionBodyState.appendResult(draft, resultType, name, (result, finalName) =>
        Object.freeze({
          _tag: 'ExtractValue',
          aggregate: resolved.operand,
          indices: Object.freeze([...indices]),
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/**
 * Inserts a same-typed value along a validated aggregate path.
 *
 * @category instructions
 * @since 0.0.0
 */
export const insertValue = Effect.fnUntraced(function* (
  self: FunctionBody,
  aggregate: Value.Input,
  element: Value.Input,
  indices: ReadonlyArray<number>,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.insertValue', (draft, module) =>
    Result.gen(function* () {
      const aggregateValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        aggregate,
        'FunctionBody.insertValue',
      )
      const elementValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        element,
        'FunctionBody.insertValue',
      )
      const selected = yield* aggregatePath(
        module,
        aggregateValue.type,
        indices,
        'FunctionBody.insertValue',
      )
      if (elementValue.type !== selected) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.insertValue',
            message: 'Inserted value does not match the aggregate path type',
            input: { aggregate, element, indices },
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        aggregateValue.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'InsertValue',
            aggregate: aggregateValue.operand,
            element: elementValue.operand,
            indices: Object.freeze([...indices]),
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Builds an aggregate from a poison seed and a complete, exact sequence of element values.
 *
 * @category instructions
 * @since 0.0.0
 */
export const buildAggregate = Effect.fnUntraced(function* (
  self: FunctionBody,
  type: Type.Type,
  elements: ReadonlyArray<Value.Input>,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Input, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const shape = yield* Type.aggregateShape(builder, type)
  const expectedLength = shape.length === undefined ? shape.fields.length : Number(shape.length)
  if (elements.length !== expectedLength) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'FunctionBody.buildAggregate',
        message: 'Aggregate element count does not match its type',
        input: elements,
      }),
    )
  }
  let aggregate: Value.Input = yield* Constant.poison(builder, type)
  for (let index = 0; index < elements.length; index += 1) {
    const element = elements[index]
    if (element === undefined) continue
    aggregate = yield* insertValue(
      self,
      aggregate,
      element,
      [index],
      index === elements.length - 1 ? name : undefined,
    )
  }
  return aggregate
})

/**
 * Extracts one vector lane using a scalar integer index.
 *
 * @category instructions
 * @since 0.0.0
 */
export const extractElement = Effect.fnUntraced(function* (
  self: FunctionBody,
  vector: Value.Input,
  index: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.extractElement',
    (draft, module) =>
      Result.gen(function* () {
        const source = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          vector,
          'FunctionBody.extractElement',
        )
        const sourceType = yield* FunctionBodyState.typeAt(
          module,
          source.type,
          'FunctionBody.extractElement',
        )
        const selected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          index,
          'FunctionBody.extractElement',
        )
        const selectedType = yield* FunctionBodyState.typeAt(
          module,
          selected.type,
          'FunctionBody.extractElement',
        )
        if (sourceType._tag !== 'Vector' || selectedType._tag !== 'Integer') {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.extractElement',
              message: 'extractelement requires a vector and scalar integer index',
              input: { vector, index },
            }),
          )
        }
        return (yield* FunctionBodyState.appendResult(
          draft,
          sourceType.child,
          name,
          (result, finalName) =>
            Object.freeze({
              _tag: 'ExtractElement',
              vector: source.operand,
              index: selected.operand,
              result,
              name: finalName,
            }),
        )).value
      }),
  )
})

/**
 * Inserts a matching scalar into one vector lane selected by an integer index.
 *
 * @category instructions
 * @since 0.0.0
 */
export const insertElement = Effect.fnUntraced(function* (
  self: FunctionBody,
  vector: Value.Input,
  element: Value.Input,
  index: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.insertElement',
    (draft, module) =>
      Result.gen(function* () {
        const source = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          vector,
          'FunctionBody.insertElement',
        )
        const sourceType = yield* FunctionBodyState.typeAt(
          module,
          source.type,
          'FunctionBody.insertElement',
        )
        const inserted = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          element,
          'FunctionBody.insertElement',
        )
        const selected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          index,
          'FunctionBody.insertElement',
        )
        const selectedType = yield* FunctionBodyState.typeAt(
          module,
          selected.type,
          'FunctionBody.insertElement',
        )
        if (
          sourceType._tag !== 'Vector' ||
          inserted.type !== sourceType.child ||
          selectedType._tag !== 'Integer'
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.insertElement',
              message: 'insertelement operands do not match the vector shape',
              input: { vector, element, index },
            }),
          )
        }
        return (yield* FunctionBodyState.appendResult(
          draft,
          source.type,
          name,
          (result, finalName) =>
            Object.freeze({
              _tag: 'InsertElement',
              vector: source.operand,
              element: inserted.operand,
              index: selected.operand,
              result,
              name: finalName,
            }),
        )).value
      }),
  )
})

/**
 * Shuffles two same-shaped vectors with a fixed integer mask of compatible length.
 *
 * @category instructions
 * @since 0.0.0
 */
export const shuffleVector = Effect.fnUntraced(function* (
  self: FunctionBody,
  left: Value.Input,
  right: Value.Input,
  mask: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const plan = yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.shuffleVector',
    (draft, module) =>
      Result.gen(function* () {
        const values = yield* sameOperands(draft, module, left, right, 'FunctionBody.shuffleVector')
        const vector = yield* FunctionBodyState.typeAt(
          module,
          values.leftValue.type,
          'FunctionBody.shuffleVector',
        )
        const selected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          mask,
          'FunctionBody.shuffleVector',
        )
        const maskType = yield* FunctionBodyState.typeAt(
          module,
          selected.type,
          'FunctionBody.shuffleVector',
        )
        if (
          vector._tag !== 'Vector' ||
          maskType._tag !== 'Vector' ||
          !(yield* FunctionBodyState.isIntegerType(
            module,
            selected.type,
            'FunctionBody.shuffleVector',
          )) ||
          vector.scalable !== maskType.scalable
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.shuffleVector',
              message: 'shufflevector requires compatible vectors and an integer vector mask',
              input: { left, right, mask },
            }),
          )
        }
        const child = module.types.handles[vector.child]
        if (child === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'FunctionBody.shuffleVector',
              message: 'Vector child type handle is missing',
              state: vector,
            }),
          )
        }
        return {
          left: values.leftValue.operand,
          right: values.rightValue.operand,
          mask: selected.operand,
          child,
          length: maskType.length,
          scalable: maskType.scalable,
        }
      }),
  )
  const resultType = plan.scalable
    ? yield* Type.scalableVector(builder, plan.child, plan.length)
    : yield* Type.vector(builder, plan.child, plan.length)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.shuffleVector', (draft) =>
    Result.gen(function* () {
      const resultTypeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        resultType,
        'Type',
        'FunctionBody.shuffleVector',
      )
      return (yield* FunctionBodyState.appendResult(
        draft,
        resultTypeIndex,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'ShuffleVector',
            left: plan.left,
            right: plan.right,
            mask: plan.mask,
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Broadcasts a matching scalar across every lane of a fixed or scalable vector.
 *
 * @category instructions
 * @since 0.0.0
 */
export const splatVector = Effect.fnUntraced(function* (
  self: FunctionBody,
  vectorType: Type.Type,
  element: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const shape = yield* Type.aggregateShape(builder, vectorType)
  if (shape.length === undefined || shape.fields.length !== 1) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'FunctionBody.splatVector',
        message: 'splatVector requires a vector result type',
        input: vectorType,
      }),
    )
  }
  const child = shape.fields[0]
  if (child === undefined) {
    return yield* Effect.fail(
      invalidState({
        operation: 'FunctionBody.splatVector',
        message: 'Vector child is missing',
        state: vectorType,
      }),
    )
  }
  const scalarVector = shape.scalable
    ? yield* Type.scalableVector(builder, child, 1)
    : yield* Type.vector(builder, child, 1)
  const i32 = yield* Type.integer(builder, 32)
  const maskType = shape.scalable
    ? yield* Type.scalableVector(builder, i32, Number(shape.length))
    : yield* Type.vector(builder, i32, Number(shape.length))
  const zero = yield* Constant.integerUnsigned(builder, i32, 0)
  const scalarPoison = yield* Constant.poison(builder, scalarVector)
  const mask = yield* Constant.splat(builder, maskType, zero)
  const inserted = yield* insertElement(self, scalarPoison, element, zero)
  return yield* shuffleVector(self, inserted, scalarPoison, mask, name)
})
