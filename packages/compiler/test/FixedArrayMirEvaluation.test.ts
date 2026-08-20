import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('fixed-array-mir/main', ascii(source))

it.effect('lowers construction and a mixed indexed-field chain to one checked place read', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Pair { left: i32 right: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 {
  let values = [Pair { left: 10, right: 11 }, Pair { left: 20, right: 21 }]
  return choose(move values, 1)
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(Mir.verify(mir), [])
    const choose = mir.functions.find((fn) => fn.id.name === 'choose')
    const operations = choose === undefined ? [] : Mir.operations(choose)
    const reads = operations.filter((operation) => operation._tag === 'ReadPlace')
    assert.strictEqual(reads.length, 1)
    assert.deepEqual(
      reads.at(0)?.selectors.map((selector) => selector._tag),
      ['ElementSelector', 'FieldSelector'],
    )
    assert.strictEqual(
      operations.some((operation) => operation._tag === 'Project'),
      false,
    )

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 20n)
    const placeRead = outcome.trace.find((event) => event._tag === 'PlaceRead')
    assert.strictEqual(placeRead?._tag, 'PlaceRead')
    if (placeRead?._tag !== 'PlaceRead') return
    assert.deepEqual(
      placeRead.selectors.map((selector) => selector._tag),
      ['Element', 'Field'],
    )
    const element = placeRead.selectors.at(0)
    assert.strictEqual(element?._tag, 'Element')
    if (element?._tag === 'Element') {
      assert.strictEqual(element.index, 1)
      assert.strictEqual(element.bounds, 'Checked')
      assert.strictEqual(element.array.length, 2)
    }
  }),
)

it.effect('preserves complete nested arrays through calls and returns', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn identity(values: [[i32; 2]; 2]) -> [[i32; 2]; 2] {
  return values
}
fn choose(values: [[i32; 2]; 2], outer: usize, inner: usize) -> i32 {
  return values[outer][inner]
}
pub fn main() -> i32 { return choose(identity([[1, 2], [3, 4]]), 1, 0) }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 3n)
    const binding = outcome.trace.find(
      (event) => event._tag === 'Binding' && event.value._tag === 'ArrayValue',
    )
    assert.strictEqual(binding?._tag, 'Binding')
    if (binding?._tag === 'Binding' && binding.value._tag === 'ArrayValue') {
      assert.strictEqual(binding.value.type.length, 2)
      assert.strictEqual(binding.value.elements.length, 2)
    }
  }),
)

it.effect(
  'traps negative, upper-bound, and zero-length dynamic indices at selector provenance',
  () =>
    Effect.gen(function* () {
      const negative =
        yield* snapshot(`fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 20], -1) }`)
      const upper =
        yield* snapshot(`fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 20], 2) }`)
      const empty =
        yield* snapshot(`fn choose(values: [i32; 0], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([], 0) }`)

      assert.deepEqual(
        Analysis.diagnostics(negative).map((diagnostic) => diagnostic.code),
        ['SEM0060'],
      )
      for (const [self, index, length] of [
        [upper, 2, 2],
        [empty, 0, 0],
      ] as const) {
        assert.deepEqual(Analysis.diagnostics(self), [])
        const outcome = Analysis.evaluate(self)
        assert.strictEqual(outcome._tag, 'Trap')
        if (outcome._tag !== 'Trap') continue
        assert.include(outcome.reason, `index ${index}`)
        assert.include(outcome.reason, `length ${length}`)
        assert.include(
          outcome.logicalPath.map((frame) => frame.function.name),
          'choose',
        )
        assert.isAbove(outcome.provenance.end, outcome.provenance.start)
        assert.strictEqual(
          outcome.trace.some((event) => event._tag === 'PlaceRead'),
          false,
        )
      }
    }),
)

it.effect('rejects incomplete array construction before evaluation', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('pub fn main() -> i32 { let values = [1, 2, 3] return values[0] }')
    const mir = Analysis.loweredMir(self)
    const malformed: Mir.Module = {
      ...mir,
      functions: mir.functions.map(
        (fn): Mir.MirFunction => ({
          ...fn,
          regions: fn.regions.map(
            (region): Mir.Region =>
              region._tag === 'OperationRegion'
                ? {
                    ...region,
                    operations: region.operations.map(
                      (operation): Mir.Operation =>
                        operation._tag === 'ConstructArray'
                          ? { ...operation, elements: operation.elements.slice(0, 2) }
                          : operation,
                    ),
                  }
                : region,
          ),
        }),
      ),
    }

    const violations = Mir.verify(malformed)
    assert.deepEqual(
      violations.map((violation) => violation.rule),
      ['InvalidAggregateOperation'],
    )
    const outcome = BootstrapEvaluation.evaluate(self.instances, malformed)
    assert.strictEqual(outcome._tag, 'Blocked')
    assert.strictEqual(outcome._tag === 'Blocked' ? outcome.reason._tag : undefined, 'InvalidMir')
    assert.deepEqual(outcome.trace, [])
    assert.strictEqual(Mir.encode(mir), Mir.encode(mir))
  }),
)
