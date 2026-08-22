import type * as DeclarationFacts from './DeclarationFacts.js'
import * as Diagnostic from './Diagnostic.js'
import * as FieldRealization from './FieldRealization.js'
import { alignUp } from './internal/Align.js'
import * as Packing from './internal/Packing.js'
import type {
  CallingScalar,
  Catalog,
  CatalogEntry,
  Entry,
  Plan,
  Representation,
  Selector,
  Violation,
} from './Layout.js'
import {
  callingShape,
  callingShapes,
  catalogEntry,
  entry,
  neverEntry,
  referenceEntry,
  scalarEntry,
  sliceEntry,
  stringEntry,
  unionEntry,
} from './Layout.js'
import * as Target from './Target.js'
import * as Type from './Type.js'

const representationEquals = (left: Representation, right: Representation): boolean => {
  if (left._tag !== right._tag) return false
  if (left._tag === 'SignedInteger')
    return right._tag === 'SignedInteger' && left.bits === right.bits
  if (left._tag === 'UnsignedInteger')
    return right._tag === 'UnsignedInteger' && left.bits === right.bits
  if (left._tag === 'Floating')
    return right._tag === 'Floating' && left.bits === right.bits && right.ieee
  if (left._tag === 'Boolean') {
    return (
      right._tag === 'Boolean' &&
      left.bits === right.bits &&
      left.falseValue === right.falseValue &&
      left.trueValue === right.trueValue
    )
  }
  if (left._tag === 'CallableEnvironment') {
    return (
      right._tag === 'CallableEnvironment' &&
      FieldRealization.equals(left.realization, right.realization) &&
      left.tailPadding === right.tailPadding &&
      left.fields.length === right.fields.length &&
      left.fields.every((field, ordinal) => {
        const other = right.fields.at(ordinal)
        return (
          other !== undefined &&
          field.ordinal === other.ordinal &&
          field.parameterOrdinal === other.parameterOrdinal &&
          field.access === other.access &&
          Type.equals(field.type, other.type) &&
          field.offset === other.offset &&
          field.size === other.size &&
          field.alignment === other.alignment &&
          field.padding === other.padding &&
          field.representation === other.representation
        )
      })
    )
  }
  if (left._tag === 'StoredEffectEnvironment') {
    return (
      right._tag === 'StoredEffectEnvironment' &&
      FieldRealization.equals(left.realization, right.realization) &&
      left.tailPadding === right.tailPadding &&
      left.fields.length === right.fields.length &&
      left.fields.every((field, ordinal) => {
        const other = right.fields.at(ordinal)
        return (
          other !== undefined &&
          field.capture === other.capture &&
          field.source === other.source &&
          field.ordinal === other.ordinal &&
          field.access === other.access &&
          Type.equals(field.type, other.type) &&
          field.offset === other.offset &&
          field.size === other.size &&
          field.alignment === other.alignment &&
          field.padding === other.padding &&
          field.representation === other.representation &&
          field.effectIdentity === other.effectIdentity &&
          ((field.callableIdentity === undefined && other.callableIdentity === undefined) ||
            (field.callableIdentity !== undefined &&
              other.callableIdentity !== undefined &&
              Type.equalsGenericArgument(field.callableIdentity, other.callableIdentity)))
        )
      })
    )
  }
  if (left._tag === 'Repeated') {
    return (
      right._tag === 'Repeated' &&
      Type.equals(left.element, right.element) &&
      left.length === right.length &&
      left.stride === right.stride
    )
  }
  if (left._tag === 'Slice') {
    return (
      right._tag === 'Slice' &&
      Type.equals(left.element, right.element) &&
      left.address.bits === right.address.bits &&
      left.address.offset === right.address.offset &&
      left.address.size === right.address.size &&
      left.address.alignment === right.address.alignment &&
      left.length.offset === right.length.offset &&
      left.addressPadding === right.addressPadding &&
      left.tailPadding === right.tailPadding &&
      left.stride === right.stride
    )
  }
  if (left._tag === 'String') {
    return (
      right._tag === 'String' &&
      left.storage.provenance === right.storage.provenance &&
      left.storage.bits === right.storage.bits &&
      left.storage.offset === right.storage.offset &&
      left.storage.size === right.storage.size &&
      left.storage.alignment === right.storage.alignment &&
      left.byteLength.type === right.byteLength.type &&
      left.byteLength.offset === right.byteLength.offset &&
      left.byteLength.size === right.byteLength.size &&
      left.storagePadding === right.storagePadding &&
      left.tailPadding === right.tailPadding
    )
  }
  if (left._tag === 'Reference') {
    return (
      right._tag === 'Reference' &&
      Type.equals(left.target, right.target) &&
      left.address.bits === right.address.bits &&
      left.address.offset === right.address.offset &&
      left.address.size === right.address.size &&
      left.address.alignment === right.address.alignment
    )
  }
  if (left._tag === 'Union') {
    return (
      right._tag === 'Union' &&
      left.payloadOffset === right.payloadOffset &&
      left.payloadSize === right.payloadSize &&
      left.payloadAlignment === right.payloadAlignment &&
      left.tagPadding === right.tagPadding &&
      left.tailPadding === right.tailPadding &&
      left.members.length === right.members.length &&
      left.members.every((member, ordinal) => {
        const other = right.members.at(ordinal)
        return (
          other !== undefined &&
          Type.equals(member.type, other.type) &&
          member.ordinal === other.ordinal &&
          member.size === other.size &&
          member.alignment === other.alignment
        )
      })
    )
  }
  const cleanupHooksEqual = (
    leftHook: Extract<Representation, { readonly _tag: 'Aggregate' }>['cleanupHook'],
    rightHook: Extract<Representation, { readonly _tag: 'Aggregate' }>['cleanupHook'],
  ): boolean =>
    leftHook === undefined
      ? rightHook === undefined
      : rightHook !== undefined &&
        leftHook.hook.module === rightHook.hook.module &&
        leftHook.hook.name === rightHook.hook.name &&
        leftHook.typeArguments.length === rightHook.typeArguments.length &&
        leftHook.typeArguments.every((argument, ordinal) => {
          const other = rightHook.typeArguments.at(ordinal)
          return other !== undefined && Type.equalsGenericArgument(argument, other)
        })
  return (
    right._tag === 'Aggregate' &&
    cleanupHooksEqual(left.cleanupHook, right.cleanupHook) &&
    left.tailPadding === right.tailPadding &&
    left.fields.length === right.fields.length &&
    left.fields.every((field, index) => {
      const other = right.fields[index]
      return (
        other !== undefined &&
        field.id.ordinal === other.id.ordinal &&
        field.name === other.name &&
        Type.equals(field.type, other.type) &&
        field.offset === other.offset &&
        field.size === other.size &&
        field.alignment === other.alignment &&
        field.padding === other.padding
      )
    })
  )
}

const executablePlanEquals = (left: Entry['executable'], right: Entry['executable']): boolean =>
  left === undefined
    ? right === undefined
    : right !== undefined &&
      left._tag === right._tag &&
      left.fields.length === right.fields.length &&
      left.fields.every((field, ordinal) => {
        const other = right.fields.at(ordinal)
        return (
          other !== undefined &&
          field.capture === other.capture &&
          Type.equals(field.type, other.type) &&
          field.access === other.access &&
          field.representation === other.representation &&
          field.offset === other.offset &&
          field.size === other.size &&
          field.alignment === other.alignment &&
          field.padding === other.padding &&
          field.effectIdentity === other.effectIdentity &&
          (field.callableIdentity === undefined
            ? other.callableIdentity === undefined
            : other.callableIdentity !== undefined &&
              Type.equalsGenericArgument(field.callableIdentity, other.callableIdentity))
        )
      })

const invalid = (
  rule: Violation['rule'],
  type: DeclarationFacts.SemanticType,
  detail: string,
): Violation => Object.freeze({ _tag: 'LayoutViolation', rule, type, detail })

const verifyEntry = (
  target: Target.Target,
  candidate: Entry,
  available: ReadonlyMap<string, Entry>,
): ReadonlyArray<Violation> => {
  if (Type.isBuiltin(candidate.type)) {
    const expected = scalarEntry(target, candidate.type)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidScalar',
            candidate.type,
            `${Type.encode(candidate.type)} does not match the canonical scalar layout`,
          ),
        ])
  }
  if (Type.isFixedArray(candidate.type)) {
    const element = Type.isBuiltin(candidate.type.element)
      ? scalarEntry(target, candidate.type.element)
      : available.get(Type.key(candidate.type.element))
    if (element === undefined || candidate.representation._tag !== 'Repeated') {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has no repeated-element representation`,
        ),
      ])
    }
    const stride = alignUp(element.size, element.alignment)
    const size = stride * candidate.type.length
    return candidate.representation.length === candidate.type.length &&
      Type.equals(candidate.representation.element, candidate.type.element) &&
      candidate.representation.stride === stride &&
      candidate.size === size &&
      candidate.alignment === element.alignment
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical repeated layout facts`,
          ),
        ])
  }
  if (Type.isString(candidate.type)) {
    const expected = stringEntry(target)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            'string does not match the canonical UTF-8 storage-provenance layout',
          ),
        ])
  }
  if (Type.isSlice(candidate.type)) {
    const element = Type.isBuiltin(candidate.type.element)
      ? scalarEntry(target, candidate.type.element)
      : available.get(Type.key(candidate.type.element))
    if (element === undefined) {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has no element layout`,
        ),
      ])
    }
    const expected = sliceEntry(target, candidate.type, element)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical slice layout facts`,
          ),
        ])
  }
  if (Type.isReference(candidate.type)) {
    const expected = referenceEntry(target, candidate.type)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidScalar',
            candidate.type,
            `${Type.encode(candidate.type)} does not match the canonical reference layout`,
          ),
        ])
  }
  if (Type.isUnion(candidate.type)) {
    const members = candidate.type.members.flatMap((member): ReadonlyArray<Entry> => {
      const memberLayout = available.get(Type.key(member))
      return memberLayout === undefined ? [] : [memberLayout]
    })
    if (members.length !== candidate.type.members.length) {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has unavailable union members`,
        ),
      ])
    }
    const expected = unionEntry(candidate.type, Object.freeze(members))
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical union layout facts`,
          ),
        ])
  }
  if (Type.isNever(candidate.type)) {
    const canonical = neverEntry()
    return candidate.size === canonical.size &&
      candidate.alignment === canonical.alignment &&
      representationEquals(candidate.representation, canonical.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            'never must use its zero-sized uninhabited placeholder layout',
          ),
        ])
  }
  if (Type.isRepresented(candidate.type)) {
    if (candidate.executable !== undefined) {
      return candidate.representation._tag === 'Aggregate' &&
        candidate.representation.fields.length === 0 &&
        candidate.representation.tailPadding === candidate.size &&
        candidate.size >= 0 &&
        candidate.alignment >= 1
        ? Object.freeze([])
        : Object.freeze([
            invalid(
              'InvalidAggregate',
              candidate.type,
              `${Type.encode(candidate.type)} has non-canonical executable environment facts`,
            ),
          ])
    }
    if (candidate.representation._tag === 'StoredEffectEnvironment') {
      const violations: Array<Violation> = []
      const expected = candidate.representation.fields.map((field, ordinal) => {
        const borrowed = field.representation === 'Borrow'
        const executable =
          field.effectIdentity !== undefined || field.callableIdentity !== undefined
        const fieldLayout =
          borrowed || executable
            ? undefined
            : Type.isBuiltin(field.type)
              ? scalarEntry(target, field.type)
              : available.get(Type.key(field.type))
        return Object.freeze({
          value: ordinal,
          size: borrowed ? target.pointerSize : executable ? field.size : (fieldLayout?.size ?? 0),
          alignment: borrowed
            ? target.pointerAlignment
            : executable
              ? field.alignment
              : (fieldLayout?.alignment ?? 1),
          available: borrowed || executable || fieldLayout !== undefined,
        })
      })
      const packed = Packing.pack(expected)
      for (const [ordinal, field] of candidate.representation.fields.entries()) {
        const slot = candidate.representation.realization.environment.at(ordinal)
        const facts = expected.at(ordinal)
        const placement = packed.fields.at(ordinal)
        if (
          slot === undefined ||
          slot.ordinal !== field.capture ||
          slot.source !== field.source ||
          slot.sourceOrdinal !== field.ordinal ||
          slot.access !== field.access ||
          !(
            Type.equals(slot.type, field.type) ||
            (field.effectIdentity !== undefined &&
              Type.isEffect(slot.type) &&
              Type.isEffect(field.type) &&
              Type.equals(
                Type.effectWithRows(
                  slot.type.success,
                  slot.type.failureRow,
                  field.type.access,
                  slot.type.requirementRow,
                ),
                field.type,
              ))
          ) ||
          slot.effectIdentity !== field.effectIdentity ||
          (slot.callableIdentity === undefined && field.callableIdentity !== undefined) ||
          (slot.callableIdentity !== undefined &&
            (field.callableIdentity === undefined ||
              !Type.equalsGenericArgument(slot.callableIdentity, field.callableIdentity))) ||
          facts === undefined ||
          placement === undefined ||
          !facts.available ||
          facts.alignment < 1 ||
          facts.size < 0 ||
          field.offset !== placement.offset ||
          field.size !== facts.size ||
          field.alignment !== facts.alignment ||
          field.padding !== placement.padding
        ) {
          violations.push(
            invalid(
              'InvalidAggregate',
              candidate.type,
              `Effect capture ${field.capture} has non-canonical physical facts`,
            ),
          )
        }
      }
      if (
        candidate.size !== packed.size ||
        candidate.alignment !== packed.alignment ||
        candidate.representation.tailPadding !== packed.tailPadding
      ) {
        violations.push(
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical stored Effect environment size or alignment`,
          ),
        )
      }
      return Object.freeze(violations)
    }
    if (candidate.representation._tag !== 'CallableEnvironment') {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has no concrete callable environment`,
        ),
      ])
    }
    const violations: Array<Violation> = []
    const expected = candidate.representation.fields.map((field, ordinal) => {
      const borrowed = field.representation === 'Borrow'
      const fieldLayout = borrowed
        ? undefined
        : Type.isBuiltin(field.type)
          ? scalarEntry(target, field.type)
          : available.get(Type.key(field.type))
      return Object.freeze({
        value: ordinal,
        size: borrowed ? target.pointerSize : (fieldLayout?.size ?? 0),
        alignment: borrowed ? target.pointerAlignment : (fieldLayout?.alignment ?? 1),
        available: borrowed || fieldLayout !== undefined,
      })
    })
    const packed = Packing.pack(expected)
    for (const [ordinal, field] of candidate.representation.fields.entries()) {
      const capture = candidate.representation.realization.captures.at(ordinal)
      const facts = expected.at(ordinal)
      const placement = packed.fields.at(ordinal)
      if (
        capture === undefined ||
        capture.ordinal !== field.ordinal ||
        capture.parameterOrdinal !== field.parameterOrdinal ||
        capture.access !== field.access ||
        !Type.equals(capture.type, field.type) ||
        facts === undefined ||
        placement === undefined ||
        !facts.available ||
        field.offset !== placement.offset ||
        field.size !== facts.size ||
        field.alignment !== facts.alignment ||
        field.padding !== placement.padding
      ) {
        violations.push(
          invalid(
            'InvalidAggregate',
            candidate.type,
            `callable capture ${field.ordinal} has non-canonical physical facts`,
          ),
        )
      }
    }
    if (
      candidate.size !== packed.size ||
      candidate.alignment !== packed.alignment ||
      candidate.representation.tailPadding !== packed.tailPadding
    ) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has non-canonical callable environment size or alignment`,
        ),
      )
    }
    return Object.freeze(violations)
  }
  if (Type.isSharedCore(candidate.type) && candidate.representation._tag === 'Reference') {
    const address = candidate.representation.address
    return candidate.copy === false &&
      candidate.size === target.pointerSize &&
      candidate.alignment === target.pointerAlignment &&
      Type.equals(candidate.representation.target, candidate.type) &&
      address.bits === (target.pointerSize === 4 ? 32 : 64) &&
      address.offset === 0 &&
      address.size === target.pointerSize &&
      address.alignment === target.pointerAlignment
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has a non-canonical opaque local-shared handle`,
          ),
        ])
  }
  if (candidate.representation._tag !== 'Aggregate') {
    return Object.freeze([
      invalid(
        'InvalidAggregate',
        candidate.type,
        `${Type.encode(candidate.type)} is nominal but not aggregate`,
      ),
    ])
  }
  const violations: Array<Violation> = []
  const cleanupHook = candidate.representation.cleanupHook
  if (
    cleanupHook !== undefined &&
    (cleanupHook.hook.module.length === 0 ||
      cleanupHook.hook.name.length === 0 ||
      cleanupHook.typeArguments.some(
        (argument) => !Type.isRuntimeConcreteGenericArgument(argument),
      ))
  ) {
    violations.push(
      invalid(
        'InvalidAggregate',
        candidate.type,
        `${Type.encode(candidate.type)} has a non-canonical cleanup hook`,
      ),
    )
  }
  const layouts = candidate.representation.fields.map((field) =>
    Type.isBuiltin(field.type)
      ? scalarEntry(target, field.type)
      : available.get(Type.key(field.type)),
  )
  const packed = Packing.pack(
    layouts.map((fieldLayout, ordinal) => ({
      value: ordinal,
      size: fieldLayout?.size ?? 0,
      alignment: fieldLayout?.alignment ?? 1,
    })),
  )
  let previousOrdinal = -1
  for (const [ordinal, field] of candidate.representation.fields.entries()) {
    const fieldLayout = layouts.at(ordinal)
    if (field.id.ordinal <= previousOrdinal) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `field ${field.name} is out of declaration order`,
        ),
      )
    }
    if (fieldLayout === undefined) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `field ${field.name} has no available dependency layout`,
        ),
      )
      previousOrdinal = field.id.ordinal
      continue
    }
    const placement = packed.fields.at(ordinal)
    if (placement === undefined) continue
    if (
      field.offset !== placement.offset ||
      field.padding !== placement.padding ||
      field.size !== fieldLayout.size ||
      field.alignment !== fieldLayout.alignment
    ) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `field ${field.name} has non-canonical physical facts`,
        ),
      )
    }
    previousOrdinal = field.id.ordinal
  }
  if (
    candidate.alignment !== packed.alignment ||
    candidate.size !== packed.size ||
    candidate.representation.tailPadding !== packed.tailPadding
  ) {
    violations.push(
      invalid(
        'InvalidAggregate',
        candidate.type,
        `${Type.encode(candidate.type)} has non-canonical size or alignment`,
      ),
    )
  }
  return Object.freeze(violations)
}

const commonViolations = (
  target: Target.Target,
  entries: ReadonlyArray<CatalogEntry>,
): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  if (!Target.isCanonical(target)) {
    violations.push(
      Object.freeze({
        _tag: 'LayoutViolation',
        rule: 'NonCanonicalTarget',
        detail: `target ${target.id} does not match its canonical profile`,
      }),
    )
  }
  const available = new Map(
    entries.flatMap((candidate) =>
      candidate._tag === 'LayoutEntry' ? [[Type.key(candidate.type), candidate] as const] : [],
    ),
  )
  const seen = new Set<string>()
  let previous: DeclarationFacts.SemanticType | undefined
  for (const candidate of entries) {
    const key = Type.key(candidate.type)
    if (seen.has(key)) {
      violations.push(
        invalid(
          'DuplicateType',
          candidate.type,
          `layout contains duplicate ${Type.encode(candidate.type)} entry`,
        ),
      )
    }
    if (previous !== undefined && Type.compare(previous, candidate.type) > 0) {
      violations.push(
        invalid(
          'NonCanonicalOrder',
          candidate.type,
          `${Type.encode(candidate.type)} follows ${Type.encode(previous)} out of canonical order`,
        ),
      )
    }
    if (candidate._tag === 'LayoutEntry') {
      violations.push(...verifyEntry(target, candidate, available))
    }
    seen.add(key)
    previous = candidate.type
  }
  return Object.freeze(violations)
}

const fieldIdEquals = (left: DeclarationFacts.FieldId, right: DeclarationFacts.FieldId): boolean =>
  left.ordinal === right.ordinal &&
  left.struct.sourceId === right.struct.sourceId &&
  left.struct.ordinal === right.struct.ordinal

/** Compares two compiler-planned physical selectors. */
export const selectorEquals = (left: Selector, right: Selector): boolean =>
  left._tag === 'ElementSelector'
    ? right._tag === 'ElementSelector' && left.index === right.index
    : left._tag === 'CallableCaptureSelector'
      ? right._tag === 'CallableCaptureSelector' && left.ordinal === right.ordinal
      : left._tag === 'EffectCaptureSelector'
        ? right._tag === 'EffectCaptureSelector' && left.ordinal === right.ordinal
        : left._tag === 'UnionTagSelector'
          ? right._tag === 'UnionTagSelector'
          : left._tag === 'UnionPayloadSelector'
            ? right._tag === 'UnionPayloadSelector' && left.slot === right.slot
            : left._tag === 'SliceAddressSelector'
              ? right._tag === 'SliceAddressSelector'
              : left._tag === 'SliceLengthSelector'
                ? right._tag === 'SliceLengthSelector'
                : left._tag === 'StringStorageSelector'
                  ? right._tag === 'StringStorageSelector'
                  : left._tag === 'StringByteLengthSelector'
                    ? right._tag === 'StringByteLengthSelector'
                    : left._tag === 'ReferenceAddressSelector'
                      ? right._tag === 'ReferenceAddressSelector'
                      : right._tag === 'FieldId' && fieldIdEquals(left, right)

/** Resolves one compiler-planned scalar lane to its byte offset within a logical value. */
export const laneOffset = (
  self: Plan,
  root: DeclarationFacts.SemanticType,
  path: ReadonlyArray<Selector>,
): number | undefined => {
  let current: DeclarationFacts.SemanticType = root
  let offset = 0
  for (const [ordinal, selector] of path.entries()) {
    const candidate = entry(self, current)
    if (candidate === undefined) return undefined
    if (selector._tag === 'FieldId') {
      if (candidate.representation._tag !== 'Aggregate') return undefined
      const field = candidate.representation.fields.find((item) => fieldIdEquals(item.id, selector))
      if (field === undefined) return undefined
      offset += field.offset
      current = field.type
      continue
    }
    if (selector._tag === 'ElementSelector') {
      if (candidate.representation._tag !== 'Repeated') return undefined
      if (selector.index < 0 || selector.index >= candidate.representation.length) return undefined
      offset += selector.index * candidate.representation.stride
      current = candidate.representation.element
      continue
    }
    if (selector._tag === 'CallableCaptureSelector') {
      if (candidate.executable?._tag === 'Callable') {
        const field = candidate.executable.fields.find(
          (capture) => capture.capture === selector.ordinal,
        )
        if (field === undefined) return undefined
        offset += field.offset
        current = field.type
        continue
      }
      if (candidate.representation._tag !== 'CallableEnvironment') return undefined
      const field = candidate.representation.fields.find(
        (capture) => capture.ordinal === selector.ordinal,
      )
      if (field === undefined) return undefined
      offset += field.offset
      current = field.type
      continue
    }
    if (selector._tag === 'EffectCaptureSelector') {
      if (candidate.executable?._tag === 'Effect') {
        const field = candidate.executable.fields.find(
          (capture) => capture.capture === selector.ordinal,
        )
        if (field === undefined) return undefined
        offset += field.offset
        current = field.type
        continue
      }
      if (candidate.representation._tag !== 'StoredEffectEnvironment') return undefined
      const field = candidate.representation.fields.find(
        (capture) => capture.capture === selector.ordinal,
      )
      if (field === undefined) return undefined
      offset += field.offset
      current = field.type
      continue
    }
    if (selector._tag === 'UnionTagSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'Union'
        ? offset
        : undefined
    }
    if (selector._tag === 'UnionPayloadSelector') {
      if (ordinal !== path.length - 1 || candidate.representation._tag !== 'Union') {
        return undefined
      }
      const shape = callingShape(self, current)
      if (shape?.tree._tag !== 'SumShape') return undefined
      let payloadOffset = 0
      for (let slot = 0; slot <= selector.slot; slot += 1) {
        const type = shape.tree.payloadTypes.at(slot)
        if (type === undefined) return undefined
        const scalar = entry(self, type)
        if (scalar === undefined) return undefined
        payloadOffset = alignUp(payloadOffset, scalar.alignment)
        if (slot === selector.slot) {
          return offset + candidate.representation.payloadOffset + payloadOffset
        }
        payloadOffset += scalar.size
      }
      return undefined
    }
    if (selector._tag === 'SliceAddressSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'Slice'
        ? offset + candidate.representation.address.offset
        : undefined
    }
    if (selector._tag === 'StringStorageSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'String'
        ? offset + candidate.representation.storage.offset
        : undefined
    }
    if (selector._tag === 'StringByteLengthSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'String'
        ? offset + candidate.representation.byteLength.offset
        : undefined
    }
    if (selector._tag === 'ReferenceAddressSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'Reference'
        ? offset + candidate.representation.address.offset
        : undefined
    }
    return ordinal === path.length - 1 && candidate.representation._tag === 'Slice'
      ? offset + candidate.representation.length.offset
      : undefined
  }
  return offset
}

const callingScalarEquals = (left: CallingScalar, right: CallingScalar): boolean =>
  typeof left === 'string'
    ? left === right
    : typeof right !== 'string' &&
      Type.equals(left.element, right.element) &&
      left.bits === right.bits

const verifyCallingShapes = (self: Plan): ReadonlyArray<Violation> => {
  const expected = callingShapes(
    self.target,
    self.entries,
    self.entries.map((entry) => entry.type),
    self.effectEnvironments,
    self.callableEnvironments,
  )
  const violations: Array<Violation> = []
  for (const entry of self.entries) {
    const actual = callingShape(self, entry.type)
    const canonical = expected.find((candidate) => Type.equals(candidate.type, entry.type))
    const matches =
      actual !== undefined &&
      canonical !== undefined &&
      actual.laneCount === canonical.laneCount &&
      actual.lanes.length === canonical.lanes.length &&
      actual.lanes.every((lane, laneIndex) => {
        const other = canonical.lanes.at(laneIndex)
        return (
          other !== undefined &&
          callingScalarEquals(lane.type, other.type) &&
          lane.path.length === other.path.length &&
          lane.path.every((selector, selectorIndex) => {
            const otherSelector = other.path.at(selectorIndex)
            return otherSelector !== undefined && selectorEquals(selector, otherSelector)
          })
        )
      })
    if (!matches) {
      violations.push(
        invalid(
          'InvalidCallingShape',
          entry.type,
          `${Type.encode(entry.type)} does not match its canonical scalar-lane shape`,
        ),
      )
    }
  }
  if (self.callingShapes.length < self.entries.length) {
    violations.push(
      Object.freeze({
        _tag: 'LayoutViolation',
        rule: 'InvalidCallingShape',
        detail: 'calling-shape collection does not match the reachable layout entries',
      }),
    )
  }
  return Object.freeze(violations)
}

const verifyLiteralVerdicts = (self: Plan): ReadonlyArray<Violation> => {
  const bits: 32 | 64 = self.target.pointerSize === 4 ? 32 : 64
  const maximum = bits === 32 ? 4294967295n : 18446744073709551615n
  const violations: Array<Violation> = []
  const unavailable = self.literalVerdicts.filter(
    (verdict) => verdict._tag === 'UnavailableUsizeLiteral',
  )
  for (const verdict of self.literalVerdicts) {
    const expectedTag =
      verdict.value >= 0n && verdict.value <= maximum
        ? 'AvailableUsizeLiteral'
        : 'UnavailableUsizeLiteral'
    if (verdict.bits !== bits || verdict._tag !== expectedTag) {
      violations.push(
        Object.freeze({
          _tag: 'LayoutViolation',
          rule: 'InvalidLiteralVerdict',
          type: 'usize',
          detail: `${verdict.value.toString()} has a non-canonical ${verdict.bits}-bit verdict`,
        }),
      )
    }
  }
  if (
    self.diagnostics.length !== unavailable.length ||
    unavailable.some((verdict) =>
      self.diagnostics.every(
        (diagnostic) =>
          diagnostic.code !== Diagnostic.usizeTargetOutOfRangeCode ||
          diagnostic.span.sourceId !== verdict.span.sourceId ||
          diagnostic.span.start !== verdict.span.start ||
          diagnostic.span.end !== verdict.span.end ||
          diagnostic.reason._tag !== 'UsizeTargetOutOfRange' ||
          diagnostic.reason.spelling !== verdict.value.toString() ||
          diagnostic.reason.target !== self.target.id ||
          diagnostic.reason.bits !== bits,
      ),
    )
  ) {
    violations.push(
      Object.freeze({
        _tag: 'LayoutViolation',
        rule: 'InvalidLiteralVerdict',
        type: 'usize',
        detail: 'target literal diagnostics do not match unavailable verdicts',
      }),
    )
  }
  return Object.freeze(violations)
}

const verifyStaticData = (self: Plan): ReadonlyArray<Violation> => {
  const expectedBits = self.target.pointerSize === 4 ? 32 : 64
  const valid = (self.staticData ?? []).every((placement, ordinal, all) => {
    const previous = ordinal === 0 ? undefined : all.at(ordinal - 1)
    return (
      (previous === undefined || previous.data.id < placement.data.id) &&
      placement.alignment === 1 &&
      placement.addressBits === expectedBits &&
      placement.lengthBits === expectedBits &&
      placement.data.bytes.every((byte) => Number.isInteger(byte) && byte >= 0 && byte <= 255)
    )
  })
  return valid
    ? Object.freeze([])
    : Object.freeze([
        Object.freeze({
          _tag: 'LayoutViolation' as const,
          rule: 'InvalidCallingShape' as const,
          detail: 'static data placements are not canonical immutable target data',
        }),
      ])
}

/** Verifies canonical target, ordering, uniqueness, representation, and ABI facts. */
export const verify = (self: Plan): ReadonlyArray<Violation> =>
  Object.freeze([
    ...commonViolations(self.target, self.entries),
    ...verifyCallingShapes(self),
    ...verifyLiteralVerdicts(self),
    ...verifyStaticData(self),
  ])

/** Verifies all available entries and deterministic ordering within a nominal catalog. */
export const verifyCatalog = (self: Catalog): ReadonlyArray<Violation> =>
  commonViolations(self.target, self.entries)

/** Verifies that every planned nominal layout is exactly the catalog decision. */
export const verifyAgainstCatalog = (self: Plan, catalog: Catalog): ReadonlyArray<Violation> =>
  Object.freeze(
    self.entries.flatMap((candidate) => {
      if (
        Type.isBuiltin(candidate.type) ||
        Type.isFixedArray(candidate.type) ||
        Type.isReference(candidate.type)
      )
        return []
      const expected = catalogEntry(catalog, candidate.type)
      return expected?._tag === 'LayoutEntry' &&
        candidate.copy === expected.copy &&
        candidate.size === expected.size &&
        candidate.alignment === expected.alignment &&
        representationEquals(candidate.representation, expected.representation) &&
        executablePlanEquals(candidate.executable, expected.executable)
        ? []
        : [
            invalid(
              'CatalogMismatch',
              candidate.type,
              `${Type.encode(candidate.type)} differs from its catalog entry`,
            ),
          ]
    }),
  )
