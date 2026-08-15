import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as Lower from '../src/Lower.js'
import * as Mir from '../src/Mir.js'
import * as OpaqueRealization from '../src/OpaqueRealization.js'
import type * as Ownership from '../src/Ownership.js'
import * as Target from '../src/Target.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const lowerStored = Effect.fnUntraced(function* (name: string, source: string) {
  const snapshot = yield* Analysis.ofSourceRealized(
    name,
    ascii(source),
    Target.wasm32UnknownUnknown.id,
  )
  const catalog = Layout.catalog(Target.wasm32UnknownUnknown, snapshot.index, snapshot.instances)
  const layout = Layout.plan(catalog, snapshot.instances)
  const ownership = Analysis.ownershipOf(snapshot, name) ?? unreachable('expected module ownership')
  return Lower.lowerProgram(
    snapshot.instances,
    new Map<string, Ownership.ModuleOwnership>([[name, ownership]]),
    layout,
    snapshot.index,
    OpaqueRealization.catalogOf(snapshot),
  )
})

const replaceDrop = (
  module: Mir.Module,
  target: Extract<Mir.Operation, { readonly _tag: 'Drop' }>,
  replacement: Extract<Mir.Operation, { readonly _tag: 'Drop' }>,
): Mir.Module =>
  Object.freeze({
    ...module,
    functions: Object.freeze(
      module.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map((region) =>
              region._tag === 'OperationRegion'
                ? Object.freeze({
                    ...region,
                    operations: Object.freeze(
                      region.operations.map((operation) =>
                        operation === target ? replacement : operation,
                      ),
                    ),
                  })
                : region._tag === 'CleanupRegion'
                  ? Object.freeze({
                      ...region,
                      releases: Object.freeze(
                        region.releases.map((operation) =>
                          operation === target ? replacement : operation,
                        ),
                      ),
                    })
                  : region,
            ),
          ),
        }),
      ),
    ),
  })

it.effect('rejects incomplete cleanup inside nested Effect and callable captures', () =>
  Effect.gen(function* () {
    const module = yield* lowerStored(
      'stored-effect-cleanup-verification/nested-executables',
      `struct Token { value: i32 }
struct Deferred<F: once Effect<i32>> { operation: F }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let effectToken = Token { value: 1 }
  let nested = effect { return consume(1, move effectToken) }
  let callableToken = Token { value: 2 }
  let transform = consume(move callableToken)
  let deferred = Deferred { operation: effect {
    let value = run move nested
    let ownedTransform = move transform
    return ownedTransform(value)
  } }
  return 0
}`,
    )
    assert.deepEqual(Mir.verify(module), [])
    const construct = module.functions
      .flatMap(Mir.operations)
      .find(
        (operation) =>
          operation._tag === 'Construct' &&
          operation.fields.some((field) => field.stored?._tag === 'StoredEffectField'),
      )
    const stored =
      construct?._tag === 'Construct'
        ? construct.fields.find((field) => field.stored?._tag === 'StoredEffectField')?.stored
        : undefined
    assert.strictEqual(stored?._tag, 'StoredEffectField')
    if (stored?._tag !== 'StoredEffectField') return
    assert.deepEqual(
      stored.realization.environment.map((field) => ({
        access: field.access,
        effect: field.effectIdentity !== undefined,
        callable: field.callableIdentity !== undefined,
      })),
      [
        { access: 'Take', effect: true, callable: false },
        { access: 'Take', effect: false, callable: true },
      ],
    )

    const drop = module.functions
      .flatMap(Mir.operations)
      .find(
        (operation): operation is Extract<Mir.Operation, { readonly _tag: 'Drop' }> =>
          operation._tag === 'Drop' &&
          operation.cleanup._tag === 'StructCleanup' &&
          operation.cleanup.fields.some((field) => field.cleanup._tag === 'EffectCleanup'),
      )
    assert.isDefined(drop)
    if (drop === undefined || drop.cleanup._tag !== 'StructCleanup') return
    const structCleanup = drop.cleanup
    const outerField = structCleanup.fields.find((field) => field.cleanup._tag === 'EffectCleanup')
    assert.strictEqual(outerField?.cleanup._tag, 'EffectCleanup')
    if (outerField?.cleanup._tag !== 'EffectCleanup') return
    const outer = outerField.cleanup
    assert.deepEqual(
      outer.slots.map((slot) => slot.cleanup._tag),
      ['CallableCleanup', 'EffectCleanup'],
    )
    const effectSlot = outer.slots.find((slot) => slot.cleanup._tag === 'EffectCleanup')
    const callableSlot = outer.slots.find((slot) => slot.cleanup._tag === 'CallableCleanup')
    assert.strictEqual(effectSlot?.cleanup._tag, 'EffectCleanup')
    assert.strictEqual(callableSlot?.cleanup._tag, 'CallableCleanup')
    if (
      effectSlot?.cleanup._tag !== 'EffectCleanup' ||
      callableSlot?.cleanup._tag !== 'CallableCleanup'
    )
      return

    const replaceOuter = (
      cleanup: Ownership.CleanupPlan,
      selected: Ownership.CleanupPlan,
    ): Extract<Mir.Operation, { readonly _tag: 'Drop' }> =>
      Object.freeze({
        ...drop,
        cleanup: Object.freeze({
          ...structCleanup,
          fields: Object.freeze(
            structCleanup.fields.map((field) =>
              field === outerField
                ? Object.freeze({
                    ...field,
                    cleanup: Object.freeze({
                      ...outer,
                      slots: Object.freeze(
                        outer.slots.map((slot) =>
                          slot.cleanup === selected ? Object.freeze({ ...slot, cleanup }) : slot,
                        ),
                      ),
                    }),
                  })
                : field,
            ),
          ),
        }),
      })

    const nestedEffectSlot =
      effectSlot.cleanup.slots.at(0) ?? unreachable('expected nested Effect cleanup slot')
    const malformed: ReadonlyArray<readonly [Ownership.CleanupPlan, Ownership.CleanupPlan]> = [
      [Object.freeze({ _tag: 'NoCleanup', type: effectSlot.cleanup.type }), effectSlot.cleanup],
      [
        Object.freeze({
          ...effectSlot.cleanup,
          slots: Object.freeze([
            Object.freeze({
              ...nestedEffectSlot,
              laneOffset: nestedEffectSlot.laneOffset + 1,
            }),
          ]),
        }),
        effectSlot.cleanup,
      ],
      [Object.freeze({ _tag: 'NoCleanup', type: callableSlot.cleanup.type }), callableSlot.cleanup],
      [Object.freeze({ ...callableSlot.cleanup, slots: Object.freeze([]) }), callableSlot.cleanup],
    ]
    for (const [cleanup, selected] of malformed) {
      assert.include(
        Mir.verify(replaceDrop(module, drop, replaceOuter(cleanup, selected))).map(
          (violation) => violation.rule,
        ),
        'InvalidAggregateOperation',
      )
    }
  }),
)
