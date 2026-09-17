import * as Alignment from '@silklang/llvm/Alignment'
import * as NativePlace from './NativePlace.js'
import type * as Mir from './Mir.js'
import type * as NativeType from './NativeType.js'
import type * as NativeLanePointer from './NativeLanePointer.js'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'

/** One invocation's source result lanes and separate private failure metadata. */
export interface NativeResult {
  readonly values: ReadonlyArray<Value.Input>
  readonly diagnostic?: Value.Input
}

/** A private call preserves canonical memory; explicit transports still deliver scalar lanes. */
export type Received =
  | NativeResult
  | { readonly place: NativePlace.NativePlace; readonly diagnostic?: Value.Input }

export const materialize = Effect.fnUntraced(function* (
  context: NativePlace.Context,
  self: Received,
  name: string,
): Effect.fn.Return<NativeResult, LlvmError.LlvmError> {
  if (!('place' in self)) return self
  return {
    values: yield* NativePlace.loadLanes(self.place, context, name),
    ...(self.diagnostic === undefined ? {} : { diagnostic: self.diagnostic }),
  }
})

/** The private metadata aggregate follows source lanes without changing their layout. */
export interface Shape {
  readonly resultLaneCount: number
  readonly diagnosticResult: boolean
}

/** Reads a result that cannot own failure metadata, rejecting an accidental discard. */
export const sourceValues = (self: NativeResult): ReadonlyArray<Value.Input> => {
  if (self.diagnostic !== undefined)
    throw new RangeError('Native source-only result cannot discard diagnostic ownership')
  return self.values
}

/** Unpacks a call result, optionally after a suspension status field. */
export const unpack = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  shape: Shape,
  value: Value.Input | undefined,
  name: string,
  status: 'Synchronous' | 'SuspensionStep' = 'Synchronous',
): Effect.fn.Return<NativeResult, LlvmError.LlvmError> {
  const offset = status === 'SuspensionStep' ? 1 : 0
  const count = shape.resultLaneCount + (shape.diagnosticResult ? 1 : 0)
  if (count === 0) return Object.freeze({ values: Object.freeze([]) })
  if (value === undefined) throw new RangeError('Native result lost its declared value')
  if (count === 1 && offset === 0 && !shape.diagnosticResult)
    return Object.freeze({ values: Object.freeze([value]) })
  const values: Array<Value.Input> = []
  for (let ordinal = 0; ordinal < shape.resultLaneCount; ordinal += 1)
    values.push(
      yield* FunctionBody.extractValue(body, value, [offset + ordinal], `${name}_${ordinal}`),
    )
  const diagnostic = shape.diagnosticResult
    ? yield* FunctionBody.extractValue(
        body,
        value,
        [offset + shape.resultLaneCount],
        `${name}_diagnostic`,
      )
    : undefined
  return Object.freeze({
    values: Object.freeze(values),
    ...(diagnostic === undefined ? {} : { diagnostic }),
  })
})

/** Validates the complete result instead of silently dropping private metadata. */
export const fields = (self: NativeResult, shape: Shape): ReadonlyArray<Value.Input> => {
  if (self.values.length !== shape.resultLaneCount)
    throw new RangeError('Native result does not match its source lane count')
  if ((self.diagnostic !== undefined) !== shape.diagnosticResult)
    throw new RangeError('Native result does not match its diagnostic ownership shape')
  return Object.freeze([
    ...self.values,
    ...(self.diagnostic === undefined ? [] : [self.diagnostic]),
  ])
}

/** Packs a synchronous private result; an empty result denotes a void return. */
export const pack = Effect.fnUntraced(function* (
  self: NativeResult,
  context: { readonly body: FunctionBody.FunctionBody },
  shape: Shape,
  resultType: LlvmType.Type,
  name: string,
): Effect.fn.Return<Value.Input | undefined, LlvmError.LlvmError> {
  const values = fields(self, shape)
  if (values.length === 0) return undefined
  if (values.length === 1 && !shape.diagnosticResult) return values.at(0)
  return yield* FunctionBody.buildAggregate(context.body, resultType, values, name)
})

/** Caller-owned storage for a recursively growing private result and its failure metadata. */
export type Storage =
  | {
      readonly _tag: 'Lanes'
      readonly type: LlvmType.Type
      readonly fields: ReadonlyArray<LlvmType.Type>
      readonly parameter: number
    }
  | {
      readonly _tag: 'Canonical'
      readonly diagnosticType?: LlvmType.Type
      readonly type: LlvmType.Type
      readonly parameter: number
      readonly logicalType: Mir.Type
      readonly alignment: number
      readonly types: NativeType.LoweringContext
      readonly lanePointers: NativeLanePointer.Context
    }

export interface Transport {
  readonly resultLaneCount: number
  readonly diagnosticResult?: LlvmType.Type
  readonly resultStorage?: Storage
}

/** Allocates one invocation's result record outside loops; callees initialize every field. */
export const allocate = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  target: Pick<Transport, 'resultStorage'>,
  name: string,
) {
  return target.resultStorage === undefined
    ? undefined
    : yield* FunctionBody.alloca(body, target.resultStorage.type, name, {
        placement: 'entry',
        ...(target.resultStorage._tag === 'Canonical'
          ? { alignment: yield* Alignment.fromByteUnits(target.resultStorage.alignment) }
          : {}),
      })
})

/** Appends the temporary result destination after source and observation arguments. */
export const argumentsFor = (
  target: Pick<Transport, 'resultStorage'>,
  arguments_: ReadonlyArray<Value.Input>,
  address: Value.Input | undefined,
): ReadonlyArray<Value.Input> => {
  if (target.resultStorage === undefined) {
    if (address !== undefined) throw new RangeError('Direct native result acquired storage')
    return arguments_
  }
  if (address === undefined || arguments_.length !== target.resultStorage.parameter)
    throw new RangeError('Indirect native result lost its destination parameter')
  return [...arguments_, address]
}

/** Stores result lanes individually, avoiding an unbounded aggregate SSA construction chain. */
export const store = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  storage: Storage,
  address: Value.Input,
  values: ReadonlyArray<Value.Input>,
  name: string,
) {
  if (storage._tag === 'Canonical') {
    if (storage.diagnosticType !== undefined) {
      const diagnostic = values.at(-1)
      if (diagnostic === undefined)
        throw new RangeError('Canonical result lost its diagnostic owner')
      yield* storeDiagnostic(body, storage, address, diagnostic, name)
    }
    return yield* NativePlace.storeLanes(
      NativePlace.make(storage.types.program.layout, storage.logicalType, address),
      { body, types: storage.types, lanePointers: storage.lanePointers },
      storage.diagnosticType === undefined ? values : values.slice(0, -1),
      name,
    )
  }
  if (values.length !== storage.fields.length)
    throw new RangeError('Indirect native result does not match its storage fields')
  for (const [ordinal, value] of values.entries()) {
    yield* FunctionBody.store(
      body,
      value,
      yield* FunctionBody.structuredGetElementPtr(
        body,
        storage.type,
        address,
        [ordinal],
        `${name}_${ordinal}_ptr`,
      ),
    )
  }
})

/** Reads a completed invocation from its declared direct or caller-owned result transport. */
export const read = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  target: Transport,
  value: Value.Input | undefined,
  address: Value.Input | undefined,
  name: string,
  status: 'Synchronous' | 'SuspensionStep' = 'Synchronous',
): Effect.fn.Return<NativeResult, LlvmError.LlvmError> {
  if (target.resultStorage === undefined)
    return yield* unpack(
      body,
      {
        resultLaneCount: target.resultLaneCount,
        diagnosticResult: target.diagnosticResult !== undefined,
      },
      value,
      name,
      status,
    )
  if (address === undefined) throw new RangeError('Indirect native result lost its storage')
  const storage = target.resultStorage
  if (storage._tag === 'Canonical')
    return yield* materialize(
      { body, types: storage.types, lanePointers: storage.lanePointers },
      yield* readValue(body, target, value, address, name, status),
      name,
    )
  const values: Array<Value.Input> = []
  for (const [ordinal, type] of storage.fields.entries()) {
    values.push(
      yield* FunctionBody.load(
        body,
        type,
        yield* FunctionBody.structuredGetElementPtr(
          body,
          target.resultStorage.type,
          address,
          [ordinal],
          `${name}_${ordinal}_ptr`,
        ),
        `${name}_${ordinal}`,
      ),
    )
  }
  const diagnostic = target.diagnosticResult === undefined ? undefined : values.at(-1)
  if (target.diagnosticResult !== undefined && diagnostic === undefined)
    throw new RangeError('Indirect native result lost its diagnostic owner')
  return {
    values: values.slice(0, target.resultLaneCount),
    ...(diagnostic === undefined ? {} : { diagnostic }),
  }
})

/** Suspension status is independent from a caller-owned aggregate result. */
export const status = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  target: Pick<Transport, 'resultStorage'>,
  value: Value.Input | undefined,
  name: string,
) {
  if (value === undefined) throw new RangeError('Native suspension step lost its status')
  return target.resultStorage === undefined
    ? yield* FunctionBody.extractValue(body, value, [0], name)
    : value
})

/** Receives canonical private results without crossing a scalar-lane boundary. */
export const place = (
  target: Transport,
  address: Value.Input | undefined,
): NativePlace.NativePlace | undefined => {
  const storage = target.resultStorage
  if (storage?._tag !== 'Canonical') return undefined
  if (address === undefined) throw new RangeError('Canonical result lost its destination')
  return NativePlace.make(storage.types.program.layout, storage.logicalType, address)
}

/** Reads the logical private result, retaining stored aggregates instead of flattening them. */
export const readValue = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  target: Transport,
  value: Value.Input | undefined,
  address: Value.Input | undefined,
  name: string,
  status: 'Synchronous' | 'SuspensionStep' = 'Synchronous',
): Effect.fn.Return<Received, LlvmError.LlvmError> {
  const storage = target.resultStorage
  if (storage?._tag !== 'Canonical') return yield* read(body, target, value, address, name, status)
  const stored = place(target, address)
  if (stored === undefined || address === undefined)
    throw new RangeError('Canonical result lost its place')
  const diagnostic =
    storage.diagnosticType === undefined
      ? undefined
      : yield* FunctionBody.load(
          body,
          storage.diagnosticType,
          yield* FunctionBody.structuredGetElementPtr(
            body,
            storage.type,
            address,
            [1],
            `${name}_diagnostic_ptr`,
          ),
          `${name}_diagnostic`,
        )
  return { place: stored, ...(diagnostic === undefined ? {} : { diagnostic }) }
})

export const storeDiagnostic = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  storage: Extract<Storage, { readonly _tag: 'Canonical' }>,
  address: Value.Input,
  diagnostic: Value.Input,
  name: string,
) {
  if (storage.diagnosticType === undefined)
    throw new RangeError('Canonical result has no diagnostic field')
  yield* FunctionBody.store(
    body,
    diagnostic,
    yield* FunctionBody.structuredGetElementPtr(
      body,
      storage.type,
      address,
      [1],
      `${name}_diagnostic_ptr`,
    ),
  )
})
