import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeFunction from './NativeFunction.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type { LoweringContext } from './NativeOperation.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SilkType from './Type.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'Move'
      | 'BeginLoan'
      | 'EndLoan'
      | 'SliceLength'
      | 'ConvertUnion'
      | 'Construct'
      | 'ConstructArray'
      | 'Project'
      | 'ReadPlace'
      | 'CheckPlace'
      | 'WritePlace'
  }
>

export const emit = Effect.fnUntraced(function* (context: LoweringContext, operation: Operation) {
  const {
    addressStorage,
    body,
    builder,
    coerceLane,
    constantBytePointer,
    entry,
    i32,
    lanePointers,
    laneType,
    lanesFor,
    locals,
    locate,
    materializeAddressRoot,
    pointer,
    program,
    storeMutable,
    usizeType,
  } = context
  const initialTrapBlock = context.state.trapBlock
  let trapBlock = initialTrapBlock
  let checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'Move': {
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType?._tag === 'Bottom') {
        const destinationType = entry.fn.localTypes.at(operation.destination.ordinal)
        if (destinationType === undefined)
          throw new RangeError('Bottom move lost its destination type')
        const placeholders: Array<Value.Input> = []
        for (const lane of lanesFor(destinationType)) {
          placeholders.push(yield* Constant.nullValue(builder, laneType(lane)))
        }
        locals.set(operation.destination.ordinal, Object.freeze(placeholders))
        break
      }
      locals.set(operation.destination.ordinal, NativeFunction.readLocal(locals, operation.source))
      break
    }
    case 'BeginLoan': {
      if (operation.sourceType._tag === 'Slice') {
        locals.set(operation.destination.ordinal, NativeFunction.readLocal(locals, operation.root))
        break
      }
      const rootType = entry.fn.localTypes.at(operation.root.ordinal)
      const rootSemantic = rootType === undefined ? undefined : Mir.semanticType(rootType)
      if (rootSemantic === undefined)
        throw new RangeError('LLVM borrow formation lost its root type')
      if (SilkType.isSlice(rootSemantic)) {
        const [selector, ...suffixSelectors] = operation.selectors
        const [base, length] = NativeFunction.readLocal(locals, operation.root)
        if (
          selector?._tag !== 'SliceElementSelector' ||
          base === undefined ||
          length === undefined ||
          operation.type._tag !== 'Reference'
        ) {
          throw new RangeError('LLVM slice borrow lost its canonical lanes')
        }
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const index = NativeFunction.readScalar(locals, selector.index)
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `borrow${checkOrdinal}_in_bounds`,
        )
        yield* locate(selector.provenance.span, yield* Value.instruction(body, inBounds))
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
        locals.set(operation.destination.ordinal, Object.freeze([projected]))
        checkOrdinal += 1
        break
      }
      let selected = SilkType.isReference(rootSemantic) ? rootSemantic.target : rootSemantic
      let staticOffset = 0
      const dynamicOffsets: Array<{
        readonly local: Mir.LocalId
        readonly stride: number
        readonly length: number
        readonly span: SourceSpan.SourceSpan
      }> = []
      for (const selector of operation.selectors) {
        const selectedLayout = Layout.entry(program.layout, selected)
        if (selector._tag === 'FieldSelector') {
          if (selectedLayout?.representation._tag !== 'Aggregate')
            throw new RangeError('LLVM borrow field lost its aggregate layout')
          const field = selectedLayout.representation.fields.find(
            (candidate) =>
              candidate.id.ordinal === selector.field.ordinal &&
              candidate.id.struct.sourceId === selector.field.struct.sourceId &&
              candidate.id.struct.ordinal === selector.field.struct.ordinal,
          )
          if (field === undefined) throw new RangeError('LLVM borrow field lost its field layout')
          staticOffset += field.offset
          selected = field.type
          continue
        }
        if (
          selector._tag !== 'ElementSelector' ||
          selectedLayout?.representation._tag !== 'Repeated'
        )
          throw new RangeError('LLVM borrow element lost its repeated layout')
        if (selector.index._tag === 'Proven') {
          staticOffset += selector.index.value * selectedLayout.representation.stride
        } else {
          dynamicOffsets.push(
            Object.freeze({
              local: selector.index.local,
              stride: selectedLayout.representation.stride,
              length: selector.length,
              span: selector.provenance.span,
            }),
          )
        }
        selected = selectedLayout.representation.element
      }
      let dynamicOffset: Value.Input | undefined
      for (const [ordinal, offset] of dynamicOffsets.entries()) {
        const index = NativeFunction.readScalar(locals, offset.local)
        const length = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(offset.length),
        )
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `borrow${checkOrdinal}_${ordinal}_in_bounds`,
        )
        yield* locate(offset.span, yield* Value.instruction(body, inBounds))
        const continuation = yield* LlvmBlock.make(body, `borrow${checkOrdinal}_${ordinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continuation)
        const scaled = yield* FunctionBody.binary(
          body,
          'mul',
          index,
          yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(offset.stride)),
          `borrow${operation.destination.ordinal}_${ordinal}_scaled`,
        )
        dynamicOffset =
          dynamicOffset === undefined
            ? scaled
            : yield* FunctionBody.binary(
                body,
                'add',
                dynamicOffset,
                scaled,
                `borrow${operation.destination.ordinal}_${ordinal}_offset`,
              )
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
                `borrow${operation.destination.ordinal}_static_offset`,
              )
      }
      let rootBase: Value.Input | undefined
      if (SilkType.isReference(rootSemantic)) {
        const address = NativeFunction.readLocal(locals, operation.root).at(0)
        if (address === undefined)
          throw new RangeError('LLVM projected borrow lost its reference address')
        rootBase = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `borrow${operation.destination.ordinal}_base`,
        )
      } else {
        yield* materializeAddressRoot(operation.root)
        rootBase = addressStorage.get(operation.root.ordinal)
      }
      if (rootBase === undefined) throw new RangeError('LLVM borrow formation lost its root')
      const projected =
        dynamicOffset === undefined
          ? rootBase
          : yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              rootBase,
              dynamicOffset,
              `borrow${operation.destination.ordinal}_projected`,
            )
      if (operation.type._tag === 'Reference') {
        locals.set(operation.destination.ordinal, Object.freeze([projected]))
        break
      }
      if (operation.sourceType._tag !== 'FixedArray') {
        throw new RangeError('LLVM slice formation requires an array root')
      }
      locals.set(
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
      const length = NativeFunction.readLocal(locals, operation.slice).at(1)
      if (length === undefined) throw new RangeError('LLVM slice lost its length lane')
      locals.set(operation.destination.ordinal, Object.freeze([length]))
      break
    }
    case 'ConvertUnion': {
      const source = NativeFunction.readLocal(locals, operation.source)
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
              ? yield* Constant.nullValue(builder, laneType(targetLane))
              : yield* coerceLane(
                  input,
                  sourceLane,
                  targetLane,
                  `union${operation.destination.ordinal}_${ordinal}_inject`,
                ),
          )
        }
        locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
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
            ? yield* Constant.nullValue(builder, laneType(targetLane))
            : yield* coerceLane(
                input,
                sourceLane,
                targetLane,
                `union${operation.destination.ordinal}_${ordinal}_widen`,
              ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
      break
    }
    case 'Construct':
      locals.set(
        operation.destination.ordinal,
        Object.freeze(
          operation.fields.flatMap((field) => [...NativeFunction.readLocal(locals, field.value)]),
        ),
      )
      break
    case 'ConstructArray':
      locals.set(
        operation.destination.ordinal,
        Object.freeze(
          operation.elements.flatMap((element) => [...NativeFunction.readLocal(locals, element)]),
        ),
      )
      break
    case 'Project': {
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType === undefined) {
        throw new RangeError('Backend projection lost its source type')
      }
      const sourceLanes = lanesFor(sourceType)
      const sourceValues = NativeFunction.readLocal(locals, operation.source)
      const projected = sourceLanes.flatMap((lane, index) => {
        const first = lane.path.at(0)
        const selected = sourceValues.at(index)
        return first !== undefined &&
          first._tag === 'FieldId' &&
          selected !== undefined &&
          first.ordinal === operation.field.ordinal &&
          first.struct.sourceId === operation.field.struct.sourceId &&
          first.struct.ordinal === operation.field.struct.ordinal
          ? [selected]
          : []
      })
      locals.set(operation.destination.ordinal, Object.freeze(projected))
      break
    }
    case 'ReadPlace': {
      const sourceType = entry.fn.localTypes.at(operation.root.ordinal)
      if (sourceType === undefined) {
        throw new RangeError('Backend place read lost its root type')
      }
      const sourceSemantic = Mir.semanticType(sourceType)
      if (SilkType.isReference(sourceSemantic)) {
        // The place lives on the referenced target: static field offsets off the
        // borrow's address, one load per lane of the projected value.
        const address = NativeFunction.readLocal(locals, operation.root).at(0)
        if (address === undefined) throw new RangeError('LLVM reference read lost its address')
        const base = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `reference_read${operation.destination.ordinal}_base`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of operation.selectors) {
          if (candidate._tag !== 'FieldSelector')
            throw new RangeError('LLVM reference place supports only field selectors')
          staticSelectors.push(candidate.field)
        }
        const target = sourceSemantic.target
        const values: Array<Value.Input> = []
        for (const [ordinal, lane] of lanesFor(operation.type).entries()) {
          const offset = LayoutVerify.laneOffset(program.layout, target, [
            ...staticSelectors,
            ...lane.path,
          ])
          if (offset === undefined) throw new RangeError('LLVM reference read lost a lane offset')
          values.push(
            yield* FunctionBody.load(
              body,
              laneType(lane),
              yield* constantBytePointer(
                base,
                offset,
                `reference_read${operation.destination.ordinal}_${ordinal}_ptr`,
              ),
              `reference_read${operation.destination.ordinal}_${ordinal}`,
            ),
          )
        }
        locals.set(operation.destination.ordinal, Object.freeze(values))
        break
      }
      if (SilkType.isSlice(sourceSemantic)) {
        const [selector, ...suffixSelectors] = operation.selectors
        if (selector?._tag !== 'SliceElementSelector') {
          throw new RangeError('LLVM slice read lost its runtime element selector')
        }
        const [base, length] = NativeFunction.readLocal(locals, operation.root)
        if (base === undefined || length === undefined) {
          throw new RangeError('LLVM slice read lost its address or length lane')
        }
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const index = NativeFunction.readScalar(locals, selector.index)
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `slice${checkOrdinal}_in_bounds`,
        )
        yield* locate(selector.provenance.span, yield* Value.instruction(body, inBounds))
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
        for (const [laneOrdinal, lane] of lanesFor(operation.type).entries()) {
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
              laneType(lane),
              address,
              `slice${checkOrdinal}_${laneOrdinal}`,
            ),
          )
        }
        locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
        checkOrdinal += 1
        break
      }
      const sourceLanes = lanesFor(sourceType)
      const sourceValues = NativeFunction.readLocal(locals, operation.root)
      const destinationLanes = lanesFor(operation.type)
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
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const limit = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(selector.length),
        )
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          NativeFunction.readScalar(locals, selector.local),
          limit,
          `index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
        )
        const instruction = yield* Value.instruction(body, inBounds)
        yield* locate(selector.span, instruction)
        const continueBlock = yield* LlvmBlock.make(
          body,
          `index${checkOrdinal}_${runtimeOrdinal}_ok`,
        )
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
      }

      const selectedValues: Array<Value.Input> = []
      for (const [destinationOrdinal, destinationLane] of destinationLanes.entries()) {
        const candidates = sourceLanes.flatMap((sourceLane, sourceOrdinal) => {
          if (sourceLane.path.length !== operation.selectors.length + destinationLane.path.length) {
            return []
          }
          const runtimeElements: Array<{
            readonly local: Mir.LocalId
            readonly element: number
          }> = []
          for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
            const physical = sourceLane.path.at(selectorOrdinal)
            if (physical === undefined) return []
            if (selector._tag === 'FieldSelector') {
              if (
                physical._tag !== 'FieldId' ||
                physical.ordinal !== selector.field.ordinal ||
                physical.struct.sourceId !== selector.field.struct.sourceId ||
                physical.struct.ordinal !== selector.field.struct.ordinal
              ) {
                return []
              }
            } else {
              if (physical._tag !== 'ElementSelector') return []
              if (selector.index._tag === 'Proven' && physical.index !== selector.index.value) {
                return []
              }
              if (selector.index._tag === 'Runtime') {
                runtimeElements.push(
                  Object.freeze({
                    local: selector.index.local,
                    element: physical.index,
                  }),
                )
              }
            }
          }
          const suffix = sourceLane.path.slice(operation.selectors.length)
          const sameSuffix = suffix.every((physical, ordinal) => {
            const expected = destinationLane.path.at(ordinal)
            return expected !== undefined && LayoutVerify.selectorEquals(physical, expected)
          })
          const selected = sourceValues.at(sourceOrdinal)
          return sameSuffix && selected !== undefined
            ? [Object.freeze({ value: selected, runtimeElements })]
            : []
        })
        const first = candidates.at(0)
        if (
          first === undefined &&
          operation.selectors.some(
            (selector) => selector._tag === 'ElementSelector' && selector.length === 0,
          )
        ) {
          selectedValues.push(yield* Constant.integerSigned(builder, i32, 0n))
          continue
        }
        if (first === undefined) {
          throw new RangeError(`Backend could not realize place-read lane ${destinationOrdinal}`)
        }
        let selected = first.value
        for (const [candidateOrdinal, candidate] of candidates.slice(1).entries()) {
          let condition: Value.Input | undefined
          for (const [elementOrdinal, element] of candidate.runtimeElements.entries()) {
            const expected = yield* Constant.integerUnsigned(
              builder,
              usizeType ?? i32,
              BigInt(element.element),
            )
            const equal = yield* FunctionBody.integerCompare(
              body,
              'eq',
              NativeFunction.readScalar(locals, element.local),
              expected,
              `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_${elementOrdinal}`,
            )
            condition =
              condition === undefined
                ? equal
                : yield* FunctionBody.binary(
                    body,
                    'and',
                    condition,
                    equal,
                    `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_${elementOrdinal}_all`,
                  )
          }
          if (condition !== undefined) {
            selected = yield* FunctionBody.select(
              body,
              condition,
              candidate.value,
              selected,
              `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_value`,
            )
          }
        }
        selectedValues.push(selected)
      }
      checkOrdinal += 1
      locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
      break
    }
    case 'CheckPlace': {
      const rootType = entry.fn.localTypes.at(operation.root.ordinal)
      if (rootType?._tag === 'Slice') {
        const selector = operation.selectors.at(0)
        const length = NativeFunction.readLocal(locals, operation.root).at(1)
        if (selector?._tag !== 'SliceElementSelector' || length === undefined) {
          throw new RangeError('LLVM slice write check lost its canonical lanes')
        }
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          NativeFunction.readScalar(locals, selector.index),
          length,
          `write_slice${checkOrdinal}_in_bounds`,
        )
        yield* locate(selector.provenance.span, yield* Value.instruction(body, inBounds))
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
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const limit = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(selector.length),
        )
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          NativeFunction.readScalar(locals, selector.local),
          limit,
          `write_index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
        )
        const instruction = yield* Value.instruction(body, inBounds)
        yield* locate(selector.span, instruction)
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
        // Writing through the borrow stores each value lane at its target offset.
        const address = NativeFunction.readLocal(locals, operation.root).at(0)
        if (address === undefined) throw new RangeError('LLVM reference write lost its address')
        const base = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `reference_write${operation.source.ordinal}_base`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of operation.selectors) {
          if (candidate._tag !== 'FieldSelector')
            throw new RangeError('LLVM reference place supports only field selectors')
          staticSelectors.push(candidate.field)
        }
        const target = operation.rootType.type.target
        const values = NativeFunction.readLocal(locals, operation.source)
        for (const [ordinal, lane] of lanesFor(operation.type).entries()) {
          const value = values.at(ordinal)
          const offset = LayoutVerify.laneOffset(program.layout, target, [
            ...staticSelectors,
            ...lane.path,
          ])
          if (value === undefined || offset === undefined)
            throw new RangeError('LLVM reference write lost a lane offset')
          yield* FunctionBody.store(
            body,
            value,
            yield* constantBytePointer(
              base,
              offset,
              `reference_write${operation.source.ordinal}_${ordinal}_ptr`,
            ),
          )
        }
        break
      }
      if (operation.rootType._tag === 'Slice') {
        const [selector, ...suffixSelectors] = operation.selectors
        const [base] = NativeFunction.readLocal(locals, operation.root)
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
          NativeFunction.readScalar(locals, selector.index),
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
        const sourceValues = NativeFunction.readLocal(locals, operation.source)
        for (const [laneOrdinal, lane] of lanesFor(operation.type).entries()) {
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
        break
      }
      const rootLanes = lanesFor(operation.rootType)
      const rootValues = NativeFunction.readLocal(locals, operation.root)
      const sourceLanes = lanesFor(operation.type)
      const sourceValues = NativeFunction.readLocal(locals, operation.source)
      if (operation.selectors.length === 0) {
        locals.set(operation.root.ordinal, sourceValues)
        yield* storeMutable(operation.root, sourceValues)
        break
      }
      const updated: Array<Value.Input> = []
      for (const [rootOrdinal, rootLane] of rootLanes.entries()) {
        const previous = rootValues.at(rootOrdinal)
        if (previous === undefined) throw new RangeError('Mutable root lost a lane')
        const runtimeElements: Array<{
          readonly local: Mir.LocalId
          readonly element: number
        }> = []
        let matches = true
        for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
          const physical = rootLane.path.at(selectorOrdinal)
          if (physical === undefined) {
            matches = false
            break
          }
          if (selector._tag === 'FieldSelector') {
            if (
              physical._tag !== 'FieldId' ||
              physical.ordinal !== selector.field.ordinal ||
              physical.struct.sourceId !== selector.field.struct.sourceId ||
              physical.struct.ordinal !== selector.field.struct.ordinal
            ) {
              matches = false
              break
            }
          } else if (selector._tag === 'SliceElementSelector') {
            matches = false
            break
          } else if (physical._tag !== 'ElementSelector') {
            matches = false
            break
          } else if (selector.index._tag === 'Proven') {
            if (physical.index !== selector.index.value) {
              matches = false
              break
            }
          } else {
            runtimeElements.push(
              Object.freeze({
                local: selector.index.local,
                element: physical.index,
              }),
            )
          }
        }
        if (!matches) {
          updated.push(previous)
          continue
        }
        const suffix = rootLane.path.slice(operation.selectors.length)
        const sourceOrdinal = sourceLanes.findIndex(
          (lane) =>
            lane.path.length === suffix.length &&
            lane.path.every((physical, ordinal) => {
              const expected = suffix.at(ordinal)
              return expected !== undefined && LayoutVerify.selectorEquals(physical, expected)
            }),
        )
        const replacement = sourceValues.at(sourceOrdinal)
        if (replacement === undefined) {
          throw new RangeError(`Backend could not realize place-write lane ${rootOrdinal}`)
        }
        let condition: Value.Input | undefined
        for (const [elementOrdinal, element] of runtimeElements.entries()) {
          const expected = yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(element.element),
          )
          const equal = yield* FunctionBody.integerCompare(
            body,
            'eq',
            NativeFunction.readScalar(locals, element.local),
            expected,
            `write_index${checkOrdinal}_${rootOrdinal}_${elementOrdinal}`,
          )
          condition =
            condition === undefined
              ? equal
              : yield* FunctionBody.binary(
                  body,
                  'and',
                  condition,
                  equal,
                  `write_index${checkOrdinal}_${rootOrdinal}_${elementOrdinal}_all`,
                )
        }
        updated.push(
          condition === undefined
            ? replacement
            : yield* FunctionBody.select(
                body,
                condition,
                replacement,
                previous,
                `write_index${checkOrdinal}_${rootOrdinal}_value`,
              ),
        )
      }
      checkOrdinal += 1
      const frozen = Object.freeze(updated)
      locals.set(operation.root.ordinal, frozen)
      yield* storeMutable(operation.root, frozen)
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
