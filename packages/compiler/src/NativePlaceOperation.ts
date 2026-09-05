import * as LlvmBlock from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as DeclarationFacts from './DeclarationFacts.js'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeArith from './NativeArith.js'
import * as NativeDebug from './NativeDebug.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeOwnedPlace from './NativeOwnedPlace.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeTermination from './NativeTermination.js'
import * as NativeType from './NativeType.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SilkType from './Type.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'Move'
      | 'SetInitialized'
      | 'BeginLoan'
      | 'EndLoan'
      | 'SliceLength'
      | 'ConvertUnion'
      | 'Construct'
      | 'ConstructUnionVariant'
      | 'ConstructArray'
      | 'Project'
      | 'ReadPlace'
      | 'CheckPlace'
      | 'WritePlace'
  }
>

const candidateCondition = Effect.fnUntraced(function* (
  context: Context,
  indices: NativeOwnedPlace.Candidate['indices'],
  tag: string,
) {
  let condition: Value.Input | undefined
  for (const [ordinal, index] of indices.entries()) {
    const expected = yield* Constant.integerUnsigned(
      context.builder,
      context.usizeType ?? context.i32,
      BigInt(index.value),
    )
    const equal = yield* FunctionBody.integerCompare(
      context.body,
      'eq',
      NativeStorage.readScalar(context.storage, index.local),
      expected,
      `${tag}_${ordinal}_index`,
    )
    condition =
      condition === undefined
        ? equal
        : yield* FunctionBody.binary(context.body, 'and', condition, equal, `${tag}_${ordinal}_all`)
  }
  return condition
})

export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const {
    arith,
    body,
    builder,
    debug,
    entry,
    i32,
    lanePointers,
    pointer,
    program,
    storage: nativeStorage,
    types,
    usizeType,
  } = context
  let trapBlock: LlvmBlock.Block | undefined
  let checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'SetInitialized': {
      const type = entry.fn.localTypes.at(operation.flag.ordinal)
      const lane = type === undefined ? undefined : NativeType.lanesFor(types, type).at(0)
      if (type?._tag !== 'bool' || lane === undefined)
        throw new RangeError('Ownership initialization flag must have a boolean lane')
      const value = yield* Constant.integerUnsigned(
        builder,
        NativeType.laneType(types, lane),
        operation.initialized ? 1n : 0n,
      )
      const values = Object.freeze([value])
      nativeStorage.locals.set(operation.flag.ordinal, values)
      yield* NativeStorage.storeMutable(nativeStorage, operation.flag, values)
      break
    }
    case 'Move': {
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType?._tag === 'Bottom') {
        const destinationType = entry.fn.localTypes.at(operation.destination.ordinal)
        if (destinationType === undefined)
          throw new RangeError('Bottom move lost its destination type')
        const placeholders: Array<Value.Input> = []
        for (const lane of NativeType.lanesFor(types, destinationType)) {
          placeholders.push(yield* Constant.nullValue(builder, NativeType.laneType(types, lane)))
        }
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(placeholders))
        break
      }
      nativeStorage.locals.set(
        operation.destination.ordinal,
        NativeStorage.readLocal(nativeStorage, operation.source),
      )
      break
    }
    case 'BeginLoan': {
      const descriptor = Mir.borrowsDescriptor(operation)
      if (!descriptor && operation.sourceType._tag === 'Slice') {
        nativeStorage.locals.set(
          operation.destination.ordinal,
          NativeStorage.readLocal(nativeStorage, operation.root),
        )
        break
      }
      const rootType = entry.fn.localTypes.at(operation.root.ordinal)
      const rootSemantic = rootType === undefined ? undefined : Mir.semanticType(rootType)
      if (rootSemantic === undefined)
        throw new RangeError('LLVM borrow formation lost its root type')
      if (!descriptor && SilkType.isSlice(rootSemantic)) {
        const [selector, ...suffixSelectors] = operation.selectors
        const [base, length] = NativeStorage.readLocal(nativeStorage, operation.root)
        if (
          selector?._tag !== 'SliceElementSelector' ||
          base === undefined ||
          length === undefined ||
          operation.type._tag !== 'Reference'
        ) {
          throw new RangeError('LLVM slice borrow lost its canonical lanes')
        }
        trapBlock = yield* NativeTermination.trapBlock(
          context.termination,
          'index out of bounds',
          selector.provenance.span,
        )
        const index = NativeStorage.readScalar(nativeStorage, selector.index)
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `borrow${checkOrdinal}_in_bounds`,
        )
        yield* NativeDebug.locate(
          debug,
          selector.provenance.span,
          yield* Value.instruction(body, inBounds),
        )
        const continuation = yield* LlvmBlock.make(body, `borrow${checkOrdinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continuation)
        const sliceLayout = Layout.entry(program.layout, rootSemantic)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('LLVM slice borrow lost its compiler layout')
        }
        const elementOffset = yield* FunctionBody.binary(
          body,
          'mul',
          index,
          yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(sliceLayout.representation.stride),
          ),
          `borrow${operation.destination.ordinal}_element_offset`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({
                _tag: 'ElementSelector',
                index: candidate.index.value,
              }),
            )
          } else {
            throw new RangeError('LLVM nested runtime slice borrow is not canonical')
          }
        }
        const staticOffset = LayoutVerify.laneOffset(
          program.layout,
          rootSemantic.element,
          staticSelectors,
        )
        if (staticOffset === undefined) {
          throw new RangeError('LLVM slice borrow lost its selected layout')
        }
        const offset =
          staticOffset === 0
            ? elementOffset
            : yield* FunctionBody.binary(
                body,
                'add',
                elementOffset,
                yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(staticOffset)),
                `borrow${operation.destination.ordinal}_static_offset`,
              )
        const projected = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          base,
          offset,
          `borrow${operation.destination.ordinal}_projected`,
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([projected]))
        checkOrdinal += 1
        break
      }
      let selected =
        !descriptor && SilkType.isReference(rootSemantic) ? rootSemantic.target : rootSemantic
      let rootBase: Value.Input | undefined
      if (!descriptor && SilkType.isReference(rootSemantic)) {
        const address = NativeStorage.readLocal(nativeStorage, operation.root).at(0)
        if (address === undefined) throw new RangeError('LLVM rootBase borrow lost its reference address')
        rootBase = yield* FunctionBody.cast(body, 'inttoptr', address, pointer,
          `borrow${operation.destination.ordinal}_base`)
      } else {
        yield* NativeStorage.materializeAddressRoot(nativeStorage, operation.root)
        rootBase = nativeStorage.addressStorage.get(operation.root.ordinal)
      }
      if (rootBase === undefined) throw new RangeError('LLVM borrow formation lost its root')
      let projected: Value.Input = rootBase
      for (const [ordinal, selector] of operation.selectors.entries()) {
        const selectedLayout = Layout.entry(program.layout, selected)
        const tag = `borrow${operation.destination.ordinal}_${ordinal}`
        if (selector._tag === 'FieldSelector') {
          if (selectedLayout?.representation._tag !== 'Aggregate')
            throw new RangeError('LLVM borrow field lost its aggregate layout')
          const field = selectedLayout.representation.fields.find((candidate) =>
            DeclarationFacts.sameFieldId(candidate.id, selector.field))
          if (field === undefined) throw new RangeError('LLVM borrow field lost its field layout')
          projected = yield* NativeLanePointer.lanePointer(lanePointers, body, projected, field.offset, `${tag}_field`)
          selected = field.type
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
          length = yield* FunctionBody.load(body, usizeType ?? i32,
            yield* NativeLanePointer.lanePointer(lanePointers, body, projected,
              descriptorLayout.length.offset, `${tag}_length_ptr`), `${tag}_length`)
          projected = base
          index = NativeStorage.readScalar(nativeStorage, selector.index)
          stride = descriptorLayout.stride
          selected = descriptorLayout.element
        } else {
          if (selector._tag !== 'ElementSelector' || selectedLayout?.representation._tag !== 'Repeated')
            throw new RangeError('LLVM borrow element lost its repeated layout')
          const repeated = selectedLayout.representation
          selected = repeated.element
          stride = repeated.stride
          if (selector.index._tag === 'Proven') {
            projected = yield* NativeLanePointer.lanePointer(lanePointers, body, projected,
              selector.index.value * stride, `${tag}_element`)
            continue
          }
          index = NativeStorage.readScalar(nativeStorage, selector.index.local)
          length = yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(selector.length))
        }
        trapBlock = yield* NativeTermination.trapBlock(context.termination,
          'index out of bounds', selector.provenance.span)
        const inBounds = yield* FunctionBody.integerCompare(body, 'ult', index, length, `${tag}_in_bounds`)
        yield* NativeDebug.locate(debug, selector.provenance.span, yield* Value.instruction(body, inBounds))
        const continuation = yield* LlvmBlock.make(body, `${tag}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continuation)
        const offset = yield* FunctionBody.binary(body, 'mul', index,
          yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(stride)), `${tag}_offset`)
        projected = yield* NativeLanePointer.lanePointer(lanePointers, body, projected, offset, `${tag}_element`)
        checkOrdinal += 1
      }
      if (operation.type._tag === 'Reference') {
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([projected]))
        break
      }
      if (operation.sourceType._tag !== 'FixedArray') {
        throw new RangeError('LLVM slice formation requires an array root')
      }
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          projected,
          yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(operation.sourceType.type.length),
          ),
        ]),
      )
      break
    }
    case 'EndLoan':
      break
    case 'SliceLength': {
      const length = NativeStorage.readLocal(nativeStorage, operation.slice).at(1)
      if (length === undefined) throw new RangeError('LLVM slice lost its length lane')
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([length]))
      break
    }
    case 'ConvertUnion': {
      const source = NativeStorage.readLocal(nativeStorage, operation.source)
      const targetWidth = operation.targetShape.laneCount
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const sourceLanes = operation.sourceShape.lanes
      const targetLanes = operation.targetShape.lanes
      if (operation.conversion === 'Inject') {
        const mapping = operation.mappings.at(0)
        if (mapping === undefined) {
          throw new RangeError('LLVM union injection has no member map')
        }
        const tag = yield* Constant.integerSigned(builder, i32, BigInt(mapping.targetOrdinal))
        const payload: Array<Value.Input> = []
        for (let ordinal = 0; ordinal < Math.max(0, targetWidth - 1); ordinal += 1) {
          const targetLane = targetLanes.at(ordinal + 1)
          if (targetLane === undefined) {
            throw new RangeError('LLVM union injection lost a target payload lane')
          }
          const input = source.at(ordinal)
          const sourceLane = sourceLanes.at(ordinal)
          payload.push(
            input === undefined || sourceLane === undefined
              ? yield* Constant.nullValue(builder, NativeType.laneType(types, targetLane))
              : yield* NativeArith.coerceLane(
                  arith.lane,
                  input,
                  sourceLane,
                  targetLane,
                  `union${operation.destination.ordinal}_${ordinal}_inject`,
                ),
          )
        }
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
        break
      }
      const sourceTag = source.at(0)
      if (sourceTag === undefined) {
        throw new RangeError('LLVM union widening has no source tag')
      }
      let tag: Value.Input = zero
      for (const [ordinal, mapping] of operation.mappings.entries()) {
        const sourceOrdinal = yield* Constant.integerSigned(
          builder,
          i32,
          BigInt(mapping.sourceOrdinal),
        )
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          sourceTag,
          sourceOrdinal,
          `union${operation.destination.ordinal}_${ordinal}_matches`,
        )
        const targetOrdinal = yield* Constant.integerSigned(
          builder,
          i32,
          BigInt(mapping.targetOrdinal),
        )
        tag = yield* FunctionBody.select(
          body,
          matches,
          targetOrdinal,
          tag,
          `union${operation.destination.ordinal}_${ordinal}_tag`,
        )
      }
      const payload: Array<Value.Input> = []
      for (let ordinal = 0; ordinal < Math.max(0, targetWidth - 1); ordinal += 1) {
        const targetLane = targetLanes.at(ordinal + 1)
        if (targetLane === undefined) {
          throw new RangeError('LLVM union widening lost a target payload lane')
        }
        const input = source.at(ordinal + 1)
        const sourceLane = sourceLanes.at(ordinal + 1)
        payload.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, NativeType.laneType(types, targetLane))
            : yield* NativeArith.coerceLane(
                arith.lane,
                input,
                sourceLane,
                targetLane,
                `union${operation.destination.ordinal}_${ordinal}_widen`,
              ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
      break
    }
    case 'Construct':
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze(
          operation.fields.flatMap((field) => [
            ...NativeStorage.readLocal(nativeStorage, field.value),
          ]),
        ),
      )
      break
    case 'ConstructUnionVariant': {
      const targetLanes = NativeType.lanesFor(types, operation.type)
      const tagLane = targetLanes.at(0)
      if (tagLane === undefined) throw new RangeError('LLVM nominal union lost its tag lane')
      const tag = yield* Constant.integerSigned(builder, i32, BigInt(operation.variantOrdinal))
      const sourceValues = operation.fields.flatMap((field) => [
        ...NativeStorage.readLocal(nativeStorage, field.value),
      ])
      const sourceLanes = operation.fields.flatMap((field) => {
        const fieldType = entry.fn.localTypes.at(field.value.ordinal)
        return fieldType === undefined ? [] : [...NativeType.lanesFor(types, fieldType)]
      })
      const payload: Array<Value.Input> = []
      for (let ordinal = 1; ordinal < targetLanes.length; ordinal += 1) {
        const targetLane = targetLanes.at(ordinal)
        if (targetLane === undefined) throw new RangeError('LLVM nominal union lost a payload lane')
        const input = sourceValues.at(ordinal - 1)
        const sourceLane = sourceLanes.at(ordinal - 1)
        payload.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, NativeType.laneType(types, targetLane))
            : yield* NativeArith.coerceLane(
                arith.lane,
                input,
                sourceLane,
                targetLane,
                `nominal_union${operation.destination.ordinal}_${ordinal - 1}`,
              ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
      break
    }
    case 'ConstructArray':
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze(
          operation.elements.flatMap((element) => [
            ...NativeStorage.readLocal(nativeStorage, element),
          ]),
        ),
      )
      break
    case 'Project': {
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType === undefined) {
        throw new RangeError('Backend projection lost its source type')
      }
      const sourceLanes = NativeType.valueLanesFor(types, sourceType)
      const sourceValues = NativeStorage.readLocal(nativeStorage, operation.source)
      const projected = sourceLanes.flatMap((lane, index) => {
        const first = lane.path.at(0)
        const selected = sourceValues.at(index)
        return first !== undefined &&
          first._tag === 'FieldId' &&
          selected !== undefined &&
          DeclarationFacts.sameFieldId(first, operation.field)
          ? [selected]
          : []
      })
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(projected))
      break
    }
    case 'ReadPlace': {
      const sourceType = entry.fn.localTypes.at(operation.root.ordinal)
      if (sourceType === undefined) {
        throw new RangeError('Backend place read lost its root type')
      }
      const sourceSemantic = Mir.semanticType(sourceType)
      if (
        !SilkType.isReference(sourceSemantic) &&
        !SilkType.isSlice(sourceSemantic) &&
        operation.selectors.every(
          (selector) =>
            selector._tag === 'FieldSelector' ||
            selector._tag === 'VariantSelector' ||
            (selector._tag === 'ElementSelector' && selector.index._tag === 'Proven'),
        )
      ) {
        const place = NativeOwnedPlace.make(program.layout, sourceSemantic, operation.selectors)
        if (place === undefined) throw new RangeError('Owned read lost its verified projection')
        const selected = yield* NativeOwnedPlace.read(
          place,
          arith.lane,
          NativeStorage.readLocal(nativeStorage, operation.root),
          `owned_read${operation.destination.ordinal}`,
        )
        nativeStorage.locals.set(operation.destination.ordinal, selected)
        break
      }
      if (SilkType.isReference(sourceSemantic)) {
        // Resolve the selected value to one checked address, then load each calling lane.
        const address = NativeStorage.readLocal(nativeStorage, operation.root).at(0)
        if (address === undefined) throw new RangeError('LLVM reference read lost its address')
        const base = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `reference_read${operation.destination.ordinal}_base`,
        )
        let selected: SilkType.Type = sourceSemantic.target
        let staticOffset = 0
        let dynamicOffset: Value.Input | undefined
        let runtimeOrdinal = 0
        for (const selector of operation.selectors) {
          const selectedLayout = Layout.entry(program.layout, selected)
          if (selector._tag === 'FieldSelector') {
            if (selectedLayout?.representation._tag !== 'Aggregate')
              throw new RangeError('LLVM reference read field lost its aggregate layout')
            const field = selectedLayout.representation.fields.find((candidate) =>
              DeclarationFacts.sameFieldId(candidate.id, selector.field),
            )
            if (field === undefined)
              throw new RangeError('LLVM reference read lost its field layout')
            staticOffset += field.offset
            selected = field.type
            continue
          }
          if (
            selector._tag !== 'ElementSelector' ||
            selectedLayout?.representation._tag !== 'Repeated'
          )
            throw new RangeError('LLVM reference read element lost its repeated layout')
          if (selector.index._tag === 'Proven') {
            staticOffset += selector.index.value * selectedLayout.representation.stride
          } else {
            const index = NativeStorage.readScalar(nativeStorage, selector.index.local)
            const length = yield* Constant.integerUnsigned(
              builder,
              usizeType ?? i32,
              BigInt(selector.length),
            )
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
              `reference_read${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
            )
            yield* NativeDebug.locate(
              debug,
              selector.provenance.span,
              yield* Value.instruction(body, inBounds),
            )
            const continuation = yield* LlvmBlock.make(
              body,
              `reference_read${checkOrdinal}_${runtimeOrdinal}_ok`,
            )
            yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
            yield* LlvmBlock.setInsertionPoint(body, continuation)
            const scaled = yield* FunctionBody.binary(
              body,
              'mul',
              index,
              yield* Constant.integerUnsigned(
                builder,
                usizeType ?? i32,
                BigInt(selectedLayout.representation.stride),
              ),
              `reference_read${operation.destination.ordinal}_${runtimeOrdinal}_scaled`,
            )
            dynamicOffset =
              dynamicOffset === undefined
                ? scaled
                : yield* FunctionBody.binary(
                    body,
                    'add',
                    dynamicOffset,
                    scaled,
                    `reference_read${operation.destination.ordinal}_${runtimeOrdinal}_offset`,
                  )
            runtimeOrdinal += 1
          }
          selected = selectedLayout.representation.element
        }
        if (staticOffset !== 0) {
          const constant = yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(staticOffset),
          )
          dynamicOffset =
            dynamicOffset === undefined
              ? constant
              : yield* FunctionBody.binary(
                  body,
                  'add',
                  dynamicOffset,
                  constant,
                  `reference_read${operation.destination.ordinal}_static_offset`,
                )
        }
        const projected =
          dynamicOffset === undefined
            ? base
            : yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                base,
                dynamicOffset,
                `reference_read${operation.destination.ordinal}_projected`,
              )
        const values: Array<Value.Input> = []
        for (const [ordinal, lane] of NativeType.lanesFor(types, operation.type).entries()) {
          const offset = LayoutVerify.laneOffset(program.layout, selected, lane.path)
          if (offset === undefined) throw new RangeError('LLVM reference read lost a lane offset')
          values.push(
            yield* FunctionBody.load(
              body,
              NativeType.laneType(types, lane),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                projected,
                offset,
                `reference_read${operation.destination.ordinal}_${ordinal}_ptr`,
              ),
              `reference_read${operation.destination.ordinal}_${ordinal}`,
            ),
          )
        }
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
        if (runtimeOrdinal > 0) checkOrdinal += 1
        break
      }
      if (SilkType.isSlice(sourceSemantic)) {
        const [selector, ...suffixSelectors] = operation.selectors
        if (selector?._tag !== 'SliceElementSelector') {
          throw new RangeError('LLVM slice read lost its runtime element selector')
        }
        const [base, length] = NativeStorage.readLocal(nativeStorage, operation.root)
        if (base === undefined || length === undefined) {
          throw new RangeError('LLVM slice read lost its address or length lane')
        }
        trapBlock = yield* NativeTermination.trapBlock(
          context.termination,
          'index out of bounds',
          selector.provenance.span,
        )
        const index = NativeStorage.readScalar(nativeStorage, selector.index)
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `slice${checkOrdinal}_in_bounds`,
        )
        yield* NativeDebug.locate(
          debug,
          selector.provenance.span,
          yield* Value.instruction(body, inBounds),
        )
        const continueBlock = yield* LlvmBlock.make(body, `slice${checkOrdinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
        const sliceLayout = Layout.entry(program.layout, sourceSemantic)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('LLVM slice read lost its compiler layout')
        }
        const stride = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(sliceLayout.representation.stride),
        )
        const elementOffset = yield* FunctionBody.binary(
          body,
          'mul',
          index,
          stride,
          `slice${checkOrdinal}_element_offset`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({
                _tag: 'ElementSelector',
                index: candidate.index.value,
              }),
            )
          } else {
            throw new RangeError('LLVM nested runtime slice place is not canonical')
          }
        }
        const selectedValues: Array<Value.Input> = []
        for (const [laneOrdinal, lane] of NativeType.lanesFor(types, operation.type).entries()) {
          const staticOffset = LayoutVerify.laneOffset(
            program.layout,
            sourceSemantic.element,
            Object.freeze([...staticSelectors, ...lane.path]),
          )
          if (staticOffset === undefined) {
            throw new RangeError(`LLVM slice read lost lane ${laneOrdinal}`)
          }
          const offset =
            staticOffset === 0
              ? elementOffset
              : yield* FunctionBody.binary(
                  body,
                  'add',
                  elementOffset,
                  yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(staticOffset)),
                  `slice${checkOrdinal}_${laneOrdinal}_offset`,
                )
          const address = yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            offset,
            `slice${checkOrdinal}_${laneOrdinal}_ptr`,
          )
          selectedValues.push(
            yield* FunctionBody.load(
              body,
              NativeType.laneType(types, lane),
              address,
              `slice${checkOrdinal}_${laneOrdinal}`,
            ),
          )
        }
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
        checkOrdinal += 1
        break
      }
      const sourceValues = NativeStorage.readLocal(nativeStorage, operation.root)
      const runtimeSelectors = operation.selectors.flatMap((selector, ordinal) =>
        selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
          ? [
              Object.freeze({
                local: selector.index.local,
                length: selector.length,
                span: selector.provenance.span,
                ordinal,
              }),
            ]
          : [],
      )
      for (const [runtimeOrdinal, selector] of runtimeSelectors.entries()) {
        trapBlock = yield* NativeTermination.trapBlock(
          context.termination,
          'index out of bounds',
          selector.span,
        )
        const limit = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(selector.length),
        )
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          NativeStorage.readScalar(nativeStorage, selector.local),
          limit,
          `index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
        )
        const instruction = yield* Value.instruction(body, inBounds)
        yield* NativeDebug.locate(debug, selector.span, instruction)
        const continueBlock = yield* LlvmBlock.make(
          body,
          `index${checkOrdinal}_${runtimeOrdinal}_ok`,
        )
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
      }

      const candidates = NativeOwnedPlace.candidates(
        program.layout,
        sourceSemantic,
        operation.selectors,
      )
      let selectedValues: ReadonlyArray<Value.Input> | undefined
      for (const [ordinal, candidate] of candidates.entries()) {
        const values = yield* NativeOwnedPlace.read(
          candidate.place,
          arith.lane,
          sourceValues,
          `read${checkOrdinal}_${ordinal}`,
        )
        const condition = yield* candidateCondition(
          context,
          candidate.indices,
          `read${checkOrdinal}_${ordinal}`,
        )
        if (selectedValues === undefined || condition === undefined) selectedValues = values
        else {
          const selected: Array<Value.Input> = []
          for (const [lane, value] of values.entries()) {
            const previous = selectedValues.at(lane)
            if (previous === undefined)
              throw new RangeError('Owned read candidates disagree on lane count')
            selected.push(
              yield* FunctionBody.select(
                body,
                condition,
                value,
                previous,
                `read${checkOrdinal}_${ordinal}_${lane}`,
              ),
            )
          }
          selectedValues = selected
        }
      }
      if (selectedValues === undefined) {
        // Zero-length arrays have already branched to the bounds trap. Keep the unreachable
        // continuation well typed without reading any source lane.
        const empty: Array<Value.Input> = []
        for (const lane of NativeType.lanesFor(types, operation.type))
          empty.push(yield* Constant.nullValue(builder, NativeType.laneType(types, lane)))
        selectedValues = empty
      }
      checkOrdinal += 1
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
      break
    }
    case 'CheckPlace': {
      const rootType = entry.fn.localTypes.at(operation.root.ordinal)
      if (rootType?._tag === 'Slice') {
        const selector = operation.selectors.at(0)
        const length = NativeStorage.readLocal(nativeStorage, operation.root).at(1)
        if (selector?._tag !== 'SliceElementSelector' || length === undefined) {
          throw new RangeError('LLVM slice write check lost its canonical lanes')
        }
        trapBlock = yield* NativeTermination.trapBlock(
          context.termination,
          'index out of bounds',
          selector.provenance.span,
        )
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          NativeStorage.readScalar(nativeStorage, selector.index),
          length,
          `write_slice${checkOrdinal}_in_bounds`,
        )
        yield* NativeDebug.locate(
          debug,
          selector.provenance.span,
          yield* Value.instruction(body, inBounds),
        )
        const continueBlock = yield* LlvmBlock.make(body, `write_slice${checkOrdinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
        checkOrdinal += 1
        break
      }
      const runtimeSelectors = operation.selectors.flatMap((selector, ordinal) =>
        selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
          ? [
              Object.freeze({
                local: selector.index.local,
                length: selector.length,
                span: selector.provenance.span,
                ordinal,
              }),
            ]
          : [],
      )
      for (const [runtimeOrdinal, selector] of runtimeSelectors.entries()) {
        trapBlock = yield* NativeTermination.trapBlock(
          context.termination,
          'index out of bounds',
          selector.span,
        )
        const limit = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(selector.length),
        )
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          NativeStorage.readScalar(nativeStorage, selector.local),
          limit,
          `write_index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
        )
        const instruction = yield* Value.instruction(body, inBounds)
        yield* NativeDebug.locate(debug, selector.span, instruction)
        const continueBlock = yield* LlvmBlock.make(
          body,
          `write_index${checkOrdinal}_${runtimeOrdinal}_ok`,
        )
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
      }
      checkOrdinal += 1
      break
    }
    case 'WritePlace': {
      if (operation.rootType._tag === 'Reference') {
        if (operation.rootType.type.access !== 'Exclusive')
          throw new RangeError('LLVM reference write requires exclusive access')
        // Resolve the selected value address once, then store each calling lane.
        const address = NativeStorage.readLocal(nativeStorage, operation.root).at(0)
        if (address === undefined) throw new RangeError('LLVM reference write lost its address')
        const base = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `reference_write${operation.source.ordinal}_base`,
        )
        let selected: SilkType.Type = operation.rootType.type.target
        let staticOffset = 0
        let dynamicOffset: Value.Input | undefined
        let runtimeOrdinal = 0
        for (const selector of operation.selectors) {
          const selectedLayout = Layout.entry(program.layout, selected)
          if (selector._tag === 'FieldSelector') {
            if (selectedLayout?.representation._tag !== 'Aggregate')
              throw new RangeError('LLVM reference write field lost its aggregate layout')
            const field = selectedLayout.representation.fields.find((candidate) =>
              DeclarationFacts.sameFieldId(candidate.id, selector.field),
            )
            if (field === undefined)
              throw new RangeError('LLVM reference write lost its field layout')
            staticOffset += field.offset
            selected = field.type
            continue
          }
          if (
            selector._tag !== 'ElementSelector' ||
            selectedLayout?.representation._tag !== 'Repeated'
          )
            throw new RangeError('LLVM reference write element lost its repeated layout')
          if (selector.index._tag === 'Proven') {
            staticOffset += selector.index.value * selectedLayout.representation.stride
          } else {
            const scaled = yield* FunctionBody.binary(
              body,
              'mul',
              NativeStorage.readScalar(nativeStorage, selector.index.local),
              yield* Constant.integerUnsigned(
                builder,
                usizeType ?? i32,
                BigInt(selectedLayout.representation.stride),
              ),
              `reference_write${operation.source.ordinal}_${runtimeOrdinal}_scaled`,
            )
            dynamicOffset =
              dynamicOffset === undefined
                ? scaled
                : yield* FunctionBody.binary(
                    body,
                    'add',
                    dynamicOffset,
                    scaled,
                    `reference_write${operation.source.ordinal}_${runtimeOrdinal}_offset`,
                  )
            runtimeOrdinal += 1
          }
          selected = selectedLayout.representation.element
        }
        if (staticOffset !== 0) {
          const constant = yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(staticOffset),
          )
          dynamicOffset =
            dynamicOffset === undefined
              ? constant
              : yield* FunctionBody.binary(
                  body,
                  'add',
                  dynamicOffset,
                  constant,
                  `reference_write${operation.source.ordinal}_static_offset`,
                )
        }
        const projected =
          dynamicOffset === undefined
            ? base
            : yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                base,
                dynamicOffset,
                `reference_write${operation.source.ordinal}_projected`,
              )
        const values = NativeStorage.readLocal(nativeStorage, operation.source)
        for (const [ordinal, lane] of NativeType.lanesFor(types, operation.type).entries()) {
          const value = values.at(ordinal)
          const offset = LayoutVerify.laneOffset(program.layout, selected, lane.path)
          if (value === undefined || offset === undefined)
            throw new RangeError('LLVM reference write lost a lane offset')
          yield* FunctionBody.store(
            body,
            value,
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              projected,
              offset,
              `reference_write${operation.source.ordinal}_${ordinal}_ptr`,
            ),
          )
        }
        yield* NativeStorage.reloadAddressRoots(nativeStorage)
        break
      }
      if (operation.rootType._tag === 'Slice') {
        const [selector, ...suffixSelectors] = operation.selectors
        const [base] = NativeStorage.readLocal(nativeStorage, operation.root)
        if (selector?._tag !== 'SliceElementSelector' || base === undefined) {
          throw new RangeError('LLVM slice write lost its canonical address lane')
        }
        const sliceLayout = Layout.entry(program.layout, operation.rootType.type)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('LLVM slice write lost its compiler layout')
        }
        const stride = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(sliceLayout.representation.stride),
        )
        const elementOffset = yield* FunctionBody.binary(
          body,
          'mul',
          NativeStorage.readScalar(nativeStorage, selector.index),
          stride,
          `write_slice${checkOrdinal}_element_offset`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({
                _tag: 'ElementSelector',
                index: candidate.index.value,
              }),
            )
          } else {
            throw new RangeError('LLVM nested runtime slice write is not canonical')
          }
        }
        const sourceValues = NativeStorage.readLocal(nativeStorage, operation.source)
        for (const [laneOrdinal, lane] of NativeType.lanesFor(types, operation.type).entries()) {
          const staticOffset = LayoutVerify.laneOffset(
            program.layout,
            operation.rootType.type.element,
            Object.freeze([...staticSelectors, ...lane.path]),
          )
          const stored = sourceValues.at(laneOrdinal)
          if (staticOffset === undefined || stored === undefined) {
            throw new RangeError(`LLVM slice write lost lane ${laneOrdinal}`)
          }
          const offset =
            staticOffset === 0
              ? elementOffset
              : yield* FunctionBody.binary(
                  body,
                  'add',
                  elementOffset,
                  yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(staticOffset)),
                  `write_slice${checkOrdinal}_${laneOrdinal}_offset`,
                )
          yield* FunctionBody.store(
            body,
            stored,
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              base,
              offset,
              `write_slice${checkOrdinal}_${laneOrdinal}_ptr`,
            ),
          )
        }
        checkOrdinal += 1
        yield* NativeStorage.reloadAddressRoots(nativeStorage)
        break
      }
      const rootValues = NativeStorage.readLocal(nativeStorage, operation.root)
      const sourceValues = NativeStorage.readLocal(nativeStorage, operation.source)
      const candidates = NativeOwnedPlace.candidates(
        program.layout,
        Mir.semanticType(operation.rootType),
        operation.selectors,
      )
      let updated = rootValues
      for (const [ordinal, candidate] of candidates.entries()) {
        const values = yield* NativeOwnedPlace.write(
          candidate.place,
          arith.lane,
          updated,
          sourceValues,
          `write${checkOrdinal}_${ordinal}`,
        )
        const condition = yield* candidateCondition(
          context,
          candidate.indices,
          `write${checkOrdinal}_${ordinal}`,
        )
        if (condition === undefined) updated = values
        else {
          const selected = [...updated]
          for (const slot of candidate.place.slots) {
            const previous = updated.at(slot)
            const value = values.at(slot)
            if (previous === undefined || value === undefined)
              throw new RangeError('Owned write lost its original lane')
            selected[slot] = yield* FunctionBody.select(
              body,
              condition,
              value,
              previous,
              `write${checkOrdinal}_${ordinal}_${slot}`,
            )
          }
          updated = selected
        }
      }
      checkOrdinal += 1
      const frozen = Object.freeze(updated)
      nativeStorage.locals.set(operation.root.ordinal, frozen)
      yield* NativeStorage.storeMutable(nativeStorage, operation.root, frozen)
      if (nativeStorage.addressRoots.has(operation.root.ordinal)) {
        yield* NativeStorage.storeAddressValues(
          nativeStorage,
          operation.root.ordinal,
          frozen,
          `write_addr${operation.root.ordinal}`,
        )
      }
      break
    }
  }
  context.state.checkOrdinal = checkOrdinal
})
