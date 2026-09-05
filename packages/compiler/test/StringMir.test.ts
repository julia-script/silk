import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Lifetime from '../src/Lifetime.js'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const source = `fn passthrough(value: string) -> string { return value }
pub fn main() -> i32 {
  let mut bytes: [u8; 3] = [104, 195, 169]
  unsafe {
    let runtime = Intrinsic.stringFromUtf8Unchecked(&bytes)
    let returned = passthrough(runtime)
    let raw = Intrinsic.stringUtf8Bytes(returned)
    let length = Intrinsic.stringByteLength(returned)
    if returned != "hé" { return 1 }
    return Intrinsic.usizeToI32(raw.length) + Intrinsic.usizeToI32(length)
  }
  return 1
}`

const lowered = () =>
  Analysis.ofSourceRealized('string/mir', encoder.encode(source), 'wasm32-unknown-unknown')

const mapOperations = (
  self: Mir.Module,
  map: (operation: Mir.Operation) => Mir.Operation | undefined,
): Mir.Module =>
  Object.freeze({
    ...self,
    functions: Object.freeze(
      self.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map((region) => {
              if (region._tag === 'OperationRegion') {
                return Object.freeze({
                  ...region,
                  operations: Object.freeze(
                    region.operations.flatMap((operation) => {
                      const result = map(operation)
                      return result === undefined ? [] : [result]
                    }),
                  ),
                })
              }
              if (region._tag === 'CleanupRegion') {
                return Object.freeze({
                  ...region,
                  releases: Object.freeze(
                    region.releases.flatMap((operation) => {
                      const result = map(operation)
                      if (result?._tag === 'Drop' || result?._tag === 'EndLoan') return [result]
                      return []
                    }),
                  ),
                })
              }
              return region
            }),
          ),
        }),
      ),
    ),
  })

it.effect('lowers every logical string path without reusing slice operations', () =>
  Effect.gen(function* () {
    const snapshot = yield* lowered()
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const mir = Analysis.loweredMir(snapshot)
    const operations = mir.functions.flatMap(MirVerification.operations)
    const tags = operations.map((operation) => operation._tag)

    assert.include(tags, 'StaticString')
    assert.include(tags, 'StringFromUtf8Unchecked')
    assert.include(tags, 'StringUtf8Bytes')
    assert.include(tags, 'StringByteLength')
    assert.include(tags, 'StringEqualsExact')
    assert.include(tags, 'Call')
    assert.include(tags, 'BeginLoan')
    assert.include(tags, 'EndLoan')
    assert.notInclude(
      operations
        .filter((operation) => operation._tag === 'StaticString')
        .map((operation) => operation._tag),
      'StaticView',
    )
    assert.isTrue(mir.functions.some((fn) => fn.localTypes.some((type) => type._tag === 'String')))

    const runtime = operations.find((operation) => operation._tag === 'StringFromUtf8Unchecked')
    assert.strictEqual(
      runtime?._tag === 'StringFromUtf8Unchecked' ? runtime.authorization : undefined,
      'Unsafe',
    )
    assert.strictEqual(
      runtime?._tag === 'StringFromUtf8Unchecked' ? runtime.heldLoans.length : undefined,
      1,
    )
    const encoded = MirEncoding.encode(mir)
    assert.include(encoded, 'static-string')
    assert.include(encoded, 'string-from-utf8-unchecked')
    assert.include(encoded, 'string-utf8-bytes')
    assert.include(encoded, 'string-byte-length')
    assert.include(encoded, 'string-not-equals-exact')
    assert.deepEqual(MirVerification.verify(mir), [])
  }),
)

it.effect('rejects forged, mutable, confused, unterminated, and call-mismatched string MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* lowered()
    const mir = Analysis.loweredMir(snapshot)
    const operations = mir.functions.flatMap(MirVerification.operations)
    const raw = operations.find((operation) => operation._tag === 'StringUtf8Bytes')
    if (raw?._tag !== 'StringUtf8Bytes') throw new RangeError('expected UTF-8 byte view')

    const forged = mapOperations(mir, (operation) =>
      operation._tag === 'StaticString'
        ? Object.freeze({ ...operation, data: 'text:00' })
        : operation,
    )
    const mutable = mapOperations(mir, (operation) =>
      operation._tag === 'StringUtf8Bytes'
        ? Object.freeze({
            ...operation,
            type: Object.freeze({
              _tag: 'Slice' as const,
              type: Type.slice('Exclusive', 'u8', Lifetime.staticLifetime),
            }),
          })
        : operation,
    )
    const confused = mapOperations(mir, (operation) =>
      operation._tag === 'StringByteLength'
        ? Object.freeze({ ...operation, string: raw.destination })
        : operation,
    )
    const unterminated = mapOperations(mir, (operation) =>
      operation._tag === 'EndLoan' ? undefined : operation,
    )
    const callMismatch = mapOperations(mir, (operation) =>
      operation._tag === 'Call' && operation.target.name === 'passthrough'
        ? Object.freeze({ ...operation, arguments: Object.freeze([raw.destination]) })
        : operation,
    )
    const stringShape = mir.layout.callingShapes.find((shape) => Type.isString(shape.type))
    const sliceShape = mir.layout.callingShapes.find(
      (shape) => Type.isSlice(shape.type) && shape.type.element === 'u8',
    )
    if (stringShape === undefined || sliceShape === undefined)
      throw new RangeError('expected string and byte-view calling shapes')
    const shapeMismatch: Mir.Module = Object.freeze({
      ...mir,
      layout: Object.freeze({
        ...mir.layout,
        callingShapes: Object.freeze(
          mir.layout.callingShapes.map((shape) =>
            Type.isString(shape.type)
              ? Object.freeze({
                  ...stringShape,
                  tree: sliceShape.tree,
                  lanes: sliceShape.lanes,
                })
              : shape,
          ),
        ),
      }),
    })

    for (const candidate of [forged, mutable, confused]) {
      assert.include(
        MirVerification.verify(candidate).map((violation) => violation.rule),
        'InvalidStringOperation',
      )
    }
    assert.include(
      MirVerification.verify(unterminated).map((violation) => violation.rule),
      'InvalidLoan',
    )
    assert.include(
      MirVerification.verify(callMismatch).map((violation) => violation.rule),
      'InvalidCallShape',
    )
    assert.include(
      MirVerification.verify(shapeMismatch).map((violation) => violation.rule),
      'InvalidLayout',
    )
  }),
)
