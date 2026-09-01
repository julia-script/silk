import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('runtime-slice-mir/main', ascii(source))

const source = `import silk.usize as usize
fn inspect(values: &[i32], index: usize) -> i32 {
  return values[index] + usize.toI32(values.length)
}
fn replace(values: &mut [i32], index: usize) -> i32 {
  values[index] = values[index] + 1
  return values[index]
}
fn shared() -> i32 {
  let values = [10, 20, 30]
  return inspect(&values, 1)
}
pub fn main() -> i32 {
  let mut values = [1, 2, 3, 4, 5, 6]
  return shared() + replace(&mut values, 2)
}`

it.effect(
  'lowers logical slices, checked places, and ordered loan endings into an acyclic MIR',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot(source)
      assert.deepEqual(Analysis.diagnostics(self), [])
      const mir = Analysis.loweredMir(self)
      assert.deepEqual(MirVerification.verify(mir), [])

      const shared = mir.functions.find((fn) => fn.id.name === 'shared')
      const sharedOperations = shared === undefined ? [] : MirVerification.operations(shared)
      assert.deepEqual(
        sharedOperations
          .filter(
            (operation) =>
              operation._tag === 'BeginLoan' ||
              operation._tag === 'Call' ||
              operation._tag === 'EndLoan',
          )
          .map((operation) => operation._tag),
        ['BeginLoan', 'Call', 'EndLoan'],
      )

      const inspect = mir.functions.find((fn) => fn.id.name === 'inspect')
      const inspectOperations = inspect === undefined ? [] : MirVerification.operations(inspect)
      assert.strictEqual(
        inspectOperations.some((operation) => operation._tag === 'SliceLength'),
        true,
      )
      assert.strictEqual(
        inspectOperations.some(
          (operation) =>
            operation._tag === 'ReadPlace' &&
            operation.selectors.at(0)?._tag === 'SliceElementSelector',
        ),
        true,
      )

      const replace = mir.functions.find((fn) => fn.id.name === 'replace')
      if (replace === undefined) throw new RangeError('expected replace MIR function')
      const replaceOperations = MirVerification.operations(replace)
      const check = replaceOperations.findIndex((operation) => operation._tag === 'CheckPlace')
      const addition = replaceOperations.findIndex(
        (operation) => operation._tag === 'Binary' && operation.operator === 'Add',
      )
      const write = replaceOperations.findIndex((operation) => operation._tag === 'WritePlace')
      assert.isAtLeast(check, 0)
      assert.isAbove(addition, check)
      assert.isAbove(write, addition)

      assert.strictEqual(
        new Set(Mir.topologicalRegions(replace).map((region) => region.id.ordinal)).size,
        replace.regions.length,
      )
      assert.strictEqual(MirEncoding.encode(mir), MirEncoding.encode(Analysis.loweredMir(self)))
    }),
)

it.effect('rejects missing endings, consuming shared reads, and mismatched slice access', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source)
    const mir = Analysis.loweredMir(self)
    const sharedIndex = mir.functions.findIndex((fn) => fn.id.name === 'shared')
    const shared = mir.functions.at(sharedIndex)
    if (shared === undefined) throw new RangeError('expected shared MIR function')
    const missingEnd: Mir.MirFunction = Object.freeze({
      ...shared,
      regions: Object.freeze(
        shared.regions.map((region) =>
          region._tag === 'OperationRegion'
            ? Object.freeze({
                ...region,
                operations: Object.freeze(
                  region.operations.filter((operation) => operation._tag !== 'EndLoan'),
                ),
              })
            : region,
        ),
      ),
    })
    const missing: Mir.Module = Object.freeze({
      ...mir,
      functions: Object.freeze([
        ...mir.functions.slice(0, sharedIndex),
        missingEnd,
        ...mir.functions.slice(sharedIndex + 1),
      ]),
    })
    assert.include(
      MirVerification.verify(missing).map((violation) => violation.rule),
      'InvalidLoan',
    )

    const inspectIndex = mir.functions.findIndex((fn) => fn.id.name === 'inspect')
    const inspect = mir.functions.at(inspectIndex)
    if (inspect === undefined) throw new RangeError('expected inspect MIR function')
    const sharedRead: Mir.MirFunction = Object.freeze({
      ...inspect,
      regions: Object.freeze(
        inspect.regions.map((region) =>
          region._tag === 'OperationRegion'
            ? Object.freeze({
                ...region,
                operations: Object.freeze(
                  region.operations.map((operation): Mir.Operation =>
                    operation._tag === 'ReadPlace' &&
                    operation.selectors.some(
                      (selector) =>
                        selector._tag === 'SliceElementSelector' && selector.access === 'Shared',
                    )
                      ? Object.freeze({ ...operation, consume: true })
                      : operation,
                  ),
                ),
              })
            : region,
        ),
      ),
    })
    const consumingShared: Mir.Module = Object.freeze({
      ...mir,
      functions: Object.freeze([
        ...mir.functions.slice(0, inspectIndex),
        sharedRead,
        ...mir.functions.slice(inspectIndex + 1),
      ]),
    })
    assert.include(
      MirVerification.verify(consumingShared).map((violation) => violation.rule),
      'InvalidSliceOperation',
    )

    const wrongAccess: Mir.MirFunction = Object.freeze({
      ...inspect,
      regions: Object.freeze(
        inspect.regions.map((region) => {
          if (region._tag !== 'OperationRegion') return region
          return Object.freeze({
            ...region,
            operations: Object.freeze(
              region.operations.map((operation): Mir.Operation => {
                if (operation._tag !== 'ReadPlace') return operation
                return Object.freeze({
                  ...operation,
                  selectors: Object.freeze(
                    operation.selectors.map((selector) => {
                      if (selector._tag !== 'SliceElementSelector') return selector
                      return Object.freeze({ ...selector, access: 'Exclusive' })
                    }),
                  ),
                })
              }),
            ),
          })
        }),
      ),
    })
    const malformed: Mir.Module = Object.freeze({
      ...mir,
      functions: Object.freeze([
        ...mir.functions.slice(0, inspectIndex),
        wrongAccess,
        ...mir.functions.slice(inspectIndex + 1),
      ]),
    })
    assert.include(
      MirVerification.verify(malformed).map((violation) => violation.rule),
      'InvalidSliceOperation',
    )
  }),
)
