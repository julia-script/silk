import * as Hir from './Hir.js'
import type {
  CallingScalar,
  Catalog,
  Entry,
  Plan,
  Representation,
  UnavailableEntry,
} from './Layout.js'
import * as Target from './Target.js'
import * as Type from './Type.js'

const representationText = (representation: Representation): string =>
  representation._tag === 'SignedInteger'
    ? `signed-i${representation.bits}`
    : representation._tag === 'UnsignedInteger'
      ? `unsigned-i${representation.bits}`
      : representation._tag === 'Floating'
        ? `float${representation.bits}`
        : representation._tag === 'Boolean'
          ? `bool-i${representation.bits} false=${representation.falseValue} true=${representation.trueValue}`
          : representation._tag === 'CallableEnvironment'
            ? `callable-environment target=${
                representation.realization.target._tag === 'Declaration'
                  ? `${representation.realization.target.module}.${representation.realization.target.name}`
                  : `${representation.realization.target.actor}.${representation.realization.target.operation}`
              } environment=${representation.realization.environment === undefined ? 'none' : Type.callableEnvironmentKey(representation.realization.environment)} tail-padding=${representation.tailPadding}`
            : representation._tag === 'StoredEffectEnvironment'
              ? `stored-effect-environment runner=${representation.realization.runner.module}.${representation.realization.runner.name} identity=${representation.realization.runnerIdentity} access=${representation.realization.access.toLowerCase()} suspendable=${representation.realization.suspendable ? 'yes' : 'no'} tail-padding=${representation.tailPadding}`
              : representation._tag === 'Repeated'
                ? `repeated element=${Type.encode(representation.element)} length=${representation.length} stride=${representation.stride}`
                : representation._tag === 'Slice'
                  ? `slice element=${Type.encode(representation.element)} address=i${representation.address.bits}@${representation.address.offset}/${representation.address.size}/${representation.address.alignment} length=usize@${representation.length.offset}/${representation.length.size} address-padding=${representation.addressPadding} tail-padding=${representation.tailPadding} stride=${representation.stride}`
                  : representation._tag === 'String'
                    ? `string storage=${representation.storage.provenance}:i${representation.storage.bits}@${representation.storage.offset}/${representation.storage.size}/${representation.storage.alignment} byte-length=usize@${representation.byteLength.offset}/${representation.byteLength.size} storage-padding=${representation.storagePadding} tail-padding=${representation.tailPadding}`
                    : representation._tag === 'Reference'
                      ? `reference target=${Type.encode(representation.target)} address=i${representation.address.bits}@${representation.address.offset}/${representation.address.size}/${representation.address.alignment}`
                      : representation._tag === 'Union'
                        ? `union tag=i${representation.tag.bits} payload-offset=${representation.payloadOffset} payload-size=${representation.payloadSize} payload-align=${representation.payloadAlignment} tag-padding=${representation.tagPadding} tail-padding=${representation.tailPadding}`
                        : `aggregate cleanup-hook=${
                            representation.cleanupHook === undefined
                              ? 'none'
                              : `${representation.cleanupHook.hook.module}.${representation.cleanupHook.hook.name}<${representation.cleanupHook.typeArguments.map(Type.encodeGenericArgument).join(',')}>`
                          } tail-padding=${representation.tailPadding}`

const entryLines = (candidate: Entry): ReadonlyArray<string> => [
  `layout ${Type.encode(candidate.type)} size=${candidate.size} align=${candidate.alignment} repr=${representationText(candidate.representation)}${candidate.executable === undefined ? '' : ` executable=${candidate.executable._tag.toLowerCase()}`}`,
  ...(candidate.executable !== undefined
    ? candidate.executable.fields.map(
        (field) =>
          `  ${candidate.executable?._tag.toLowerCase()}-capture ${field.capture}: ${Type.encode(field.type)} access=${field.access.toLowerCase()} representation=${field.representation.toLowerCase()} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
      )
    : candidate.representation._tag === 'Aggregate'
      ? candidate.representation.fields.map(
          (field) =>
            `  field ${field.id.ordinal} ${field.name}: ${Type.encode(field.type)} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
        )
      : candidate.representation._tag === 'CallableEnvironment'
        ? candidate.representation.fields.map(
            (field) =>
              `  capture ${field.ordinal}->p${field.parameterOrdinal}: ${Type.encode(field.type)} access=${field.access.toLowerCase()} representation=${field.representation.toLowerCase()} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
          )
        : candidate.representation._tag === 'StoredEffectEnvironment'
          ? candidate.representation.fields.map(
              (field) =>
                `  effect-capture ${field.capture} ${field.source.toLowerCase()}${field.ordinal}: ${Type.encode(field.type)} access=${field.access.toLowerCase()} representation=${field.representation.toLowerCase()} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
            )
          : candidate.representation._tag === 'Repeated'
            ? [
                `  elements ${Type.encode(candidate.representation.element)} count=${candidate.representation.length} stride=${candidate.representation.stride}`,
              ]
            : candidate.representation._tag === 'Slice'
              ? [
                  `  address Address<${Type.encode(candidate.representation.element)}> bits=${candidate.representation.address.bits} offset=${candidate.representation.address.offset} size=${candidate.representation.address.size} align=${candidate.representation.address.alignment}`,
                  `  length usize offset=${candidate.representation.length.offset} size=${candidate.representation.length.size} stride=${candidate.representation.stride}`,
                ]
              : candidate.representation._tag === 'String'
                ? [
                    `  storage StringUtf8 bits=${candidate.representation.storage.bits} offset=${candidate.representation.storage.offset} size=${candidate.representation.storage.size} align=${candidate.representation.storage.alignment}`,
                    `  byte-length usize offset=${candidate.representation.byteLength.offset} size=${candidate.representation.byteLength.size}`,
                  ]
                : candidate.representation._tag === 'Reference'
                  ? [
                      `  address Address<${Type.encode(candidate.representation.target)}> bits=${candidate.representation.address.bits} offset=0 size=${candidate.representation.address.size} align=${candidate.representation.address.alignment}`,
                    ]
                  : candidate.representation._tag === 'Union'
                    ? candidate.representation.members.map(
                        (member) =>
                          `  member ${member.ordinal} ${Type.encode(member.type)} size=${member.size} align=${member.alignment}`,
                      )
                    : []),
]

/** Deterministic textual encoding of a complete runtime layout plan. */
const callingScalarText = (scalar: CallingScalar): string =>
  typeof scalar === 'string' ? scalar : `Address<${Type.encode(scalar.element)},i${scalar.bits}>`

export const encode = (self: Plan): string =>
  [
    `target ${Target.encode(self.target)}`,
    ...self.entries.flatMap(entryLines),
    ...self.effectEnvironments.map((environment) =>
      environment._tag === 'UnavailableEffectEnvironment'
        ? `effect-environment ${environment.instance.declaration.module}.${environment.instance.declaration.name}@${Hir.executableSiteLabel(environment.site)} unavailable=${environment.reason}`
        : `effect-environment ${environment.instance.declaration.module}.${environment.instance.declaration.name}@${Hir.executableSiteLabel(environment.site)} size=${environment.size} align=${environment.alignment} fields=${environment.fields.map((field) => `${field.source.toLowerCase()}${field.ordinal}:${field.access.toLowerCase()}:${field.representation.toLowerCase()}@${field.offset}`).join(',') || 'none'}`,
    ),
    ...self.callableEnvironments.map((environment) => {
      const callable = environment.callable
      const identity = `${callable.owner.declaration.module}.${callable.owner.declaration.name}@${Hir.executableSiteLabel(callable.site)}`
      return environment._tag === 'UnavailableCallableEnvironment'
        ? `callable-environment ${identity} unavailable=${environment.reason} view=code@${environment.view.codeOffset},env@${environment.view.environmentOffset},size=${environment.view.size}`
        : `callable-environment ${identity} mode=${callable.mode.toLowerCase()} size=${environment.size} align=${environment.alignment} fields=${environment.fields.map((field) => `capture${field.ordinal}->p${field.parameterOrdinal}:${field.access.toLowerCase()}:${field.representation.toLowerCase()}@${field.offset}`).join(',') || 'none'} view=code@${environment.view.codeOffset},env@${environment.view.environmentOffset},size=${environment.view.size}`
    }),
    ...self.callingShapes.map(
      (shape) =>
        `calling ${Type.encode(shape.type)} lanes=${shape.laneCount}${
          shape.laneCount === 0
            ? ''
            : ` ${shape.lanes
                .map(
                  (lane) =>
                    `${callingScalarText(lane.type)}[${lane.path
                      .map((selector) =>
                        selector._tag === 'ElementSelector'
                          ? `[${selector.index}]`
                          : selector._tag === 'CallableCaptureSelector'
                            ? `capture[${selector.ordinal}]`
                            : selector._tag === 'EffectCaptureSelector'
                              ? `effect-capture[${selector.ordinal}]`
                              : selector._tag === 'UnionTagSelector'
                                ? 'tag'
                                : selector._tag === 'UnionPayloadSelector'
                                  ? `payload[${selector.slot}]`
                                  : selector._tag === 'SliceAddressSelector'
                                    ? 'address'
                                    : selector._tag === 'SliceLengthSelector'
                                      ? 'length'
                                      : selector._tag === 'StringStorageSelector'
                                        ? 'storage'
                                        : selector._tag === 'StringByteLengthSelector'
                                          ? 'byte-length'
                                          : selector._tag === 'ReferenceAddressSelector'
                                            ? 'address'
                                            : `${selector.struct.sourceId}#${selector.struct.ordinal}.${selector.ordinal}`,
                      )
                      .join('.')}]`,
                )
                .join(',')}`
        }`,
    ),
    ...(self.staticData ?? []).map(
      (placement) =>
        `static-data ${placement.data.id} bytes=${placement.data.bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')} align=${placement.alignment} address=i${placement.addressBits} length=usize:i${placement.lengthBits}`,
    ),
    ...self.literalVerdicts.map(
      (verdict) =>
        `usize-literal ${verdict.value.toString()} bits=${verdict.bits} ${verdict._tag === 'AvailableUsizeLiteral' ? 'available' : `unavailable cause=${verdict.cause.code}`} [${verdict.span.start}, ${verdict.span.end})`,
    ),
    '',
  ].join('\n')

const unavailableText = (candidate: UnavailableEntry): string => {
  const reason =
    candidate.reason._tag === 'UnavailableDependency'
      ? `dependency=${Type.encode(candidate.reason.dependency)}`
      : `detail=${JSON.stringify(candidate.reason.detail)}`
  const cause =
    candidate.cause === undefined
      ? ''
      : ` cause=${candidate.cause.code}@${candidate.cause.span.sourceId}:${candidate.cause.span.start}-${candidate.cause.span.end}`
  return `layout ${Type.encode(candidate.type)} unavailable reason=${candidate.reason._tag} ${reason}${cause}`
}

/** Deterministic textual encoding of every nominal catalog fact. */
export const encodeCatalog = (self: Catalog): string =>
  [
    `target ${Target.encode(self.target)}`,
    ...self.entries.flatMap((candidate) =>
      candidate._tag === 'LayoutEntry' ? entryLines(candidate) : [unavailableText(candidate)],
    ),
    ...self.usizeConstants.map(
      (constant) =>
        `usize-constant ${constant.value.toString()} [${constant.span.sourceId}:${constant.span.start}, ${constant.span.end})`,
    ),
    '',
  ].join('\n')
