import * as LlvmBlock from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as DeclarationFacts from './DeclarationFacts.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as NativeDebug from './NativeDebug.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeOperationContext from './NativeOperationContext.js'
import * as NativePlace from './NativePlace.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeTermination from './NativeTermination.js'
import * as Type from './Type.js'

/** Native services needed to resolve a checked logical place to its original allocation. */
export type Context = Pick<
  NativeOperationContext.Context,
  | 'builder'
  | 'body'
  | 'program'
  | 'i32'
  | 'pointer'
  | 'usizeType'
  | 'lanePointers'
  | 'storage'
  | 'debug'
  | 'termination'
>

/** Resolves each selector in order, preserving native field layout and slice allocation boundaries. */
export const resolve = Effect.fnUntraced(function* (
  context: Context,
  root: Mir.LocalId,
  selectors: ReadonlyArray<Mir.PlaceSelector>,
  name: string,
  descriptor = false,
) {
  const { builder, body, program, i32, pointer, usizeType, lanePointers, debug } = context
  const nativeStorage = context.storage
  const rootType = nativeStorage.fn.localTypes.at(root.ordinal)
  if (rootType === undefined) throw new RangeError('Native place lost its root type')
  const rootSemantic = Mir.semanticType(rootType)
  let selected = !descriptor && Type.isReference(rootSemantic) ? rootSemantic.target : rootSemantic
  let rootBase: Value.Input
  if (!descriptor && Type.isReference(rootSemantic)) {
    rootBase = yield* FunctionBody.cast(
      body,
      'inttoptr',
      yield* NativeStorage.readScalar(nativeStorage, root),
      pointer,
      `${name}_root`,
    )
  } else {
    yield* NativeStorage.ensureAddressRoot(nativeStorage, root)
    rootBase = yield* NativeStorage.addressOf(nativeStorage, root)
  }
  let variant:
    | Extract<Layout.Representation, { readonly _tag: 'NominalUnion' }>['variants'][number]
    | undefined
  let trapBlock: LlvmBlock.Block
  let projected: Value.Input = rootBase
  for (const [ordinal, selector] of selectors.entries()) {
    const selectedLayout = Layout.entry(program.layout, selected)
    const tag = `${name}_${ordinal}`
    if (selector._tag === 'VariantSelector') {
      if (selectedLayout?.representation._tag === 'Union') {
        const member = selectedLayout.representation.members.find(
          (candidate) => candidate.ordinal === selector.ordinal,
        )
        if (member === undefined) throw new RangeError('Native place lost its union member')
        projected = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          projected,
          selectedLayout.representation.payloadOffset,
          `${tag}_payload`,
        )
        selected = member.type
      } else if (selectedLayout?.representation._tag === 'NominalUnion') {
        variant = selectedLayout.representation.variants.find(
          (candidate) => candidate.ordinal === selector.ordinal,
        )
        if (variant === undefined) throw new RangeError('Native place lost its nominal variant')
        projected = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          projected,
          selectedLayout.representation.payloadOffset,
          `${tag}_payload`,
        )
      } else throw new RangeError('Native place lost its variant layout')
      continue
    }
    if (selector._tag === 'FieldSelector') {
      const fields =
        variant?.fields ??
        (selectedLayout?.representation._tag === 'Aggregate'
          ? selectedLayout.representation.fields
          : [])
      const field = fields.find((candidate) =>
        DeclarationFacts.sameFieldId(candidate.id, selector.field),
      )
      if (field === undefined) throw new RangeError('LLVM borrow field lost its field layout')
      projected = yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        projected,
        field.offset,
        `${tag}_field`,
      )
      selected = field.type
      variant = undefined
      continue
    }
    let index: Value.Input
    let length: Value.Input
    let stride: number
    if (selector._tag === 'SliceElementSelector') {
      if (selectedLayout?.representation._tag !== 'Slice')
        throw new RangeError('LLVM borrowed slice field lost its descriptor layout')
      const descriptorLayout = selectedLayout.representation
      // Crossing a slice descriptor changes the allocation being addressed. Prefix field
      // offsets belong to the descriptor; suffix selectors belong to its backing elements.
      const base: Value.Input = yield* FunctionBody.load(body, pointer, projected, `${tag}_data`)
      length = yield* FunctionBody.load(
        body,
        usizeType ?? i32,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          projected,
          descriptorLayout.length.offset,
          `${tag}_length_ptr`,
        ),
        `${tag}_length`,
      )
      projected = base
      index = yield* NativeStorage.readScalar(nativeStorage, selector.index)
      stride = descriptorLayout.stride
      selected = descriptorLayout.element
    } else {
      if (selector._tag !== 'ElementSelector' || selectedLayout?.representation._tag !== 'Repeated')
        throw new RangeError('LLVM borrow element lost its repeated layout')
      const repeated = selectedLayout.representation
      selected = repeated.element
      stride = repeated.stride
      if (selector.index._tag === 'Proven') {
        projected = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          projected,
          selector.index.value * stride,
          `${tag}_element`,
        )
        continue
      }
      index = yield* NativeStorage.readScalar(nativeStorage, selector.index.local)
      length = yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(selector.length))
    }
    trapBlock = yield* NativeTermination.trapBlock(
      context.termination,
      'index out of bounds',
      selector.provenance.span,
    )
    const inBounds = yield* FunctionBody.integerCompare(
      body,
      'ult',
      index,
      length,
      `${tag}_in_bounds`,
    )
    yield* NativeDebug.locate(
      debug,
      selector.provenance.span,
      yield* Value.instruction(body, inBounds),
    )
    const continuation = yield* LlvmBlock.make(body, `${tag}_ok`)
    yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
    yield* LlvmBlock.setInsertionPoint(body, continuation)
    const offset = yield* FunctionBody.binary(
      body,
      'mul',
      index,
      yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(stride)),
      `${tag}_offset`,
    )
    projected = yield* NativeLanePointer.lanePointer(
      lanePointers,
      body,
      projected,
      offset,
      `${tag}_element`,
    )
  }
  return NativePlace.stored(program.layout, selected, projected)
})
