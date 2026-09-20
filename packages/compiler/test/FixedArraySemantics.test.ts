import { records, type InspectedBody } from './support/records.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Tir from '../src/Tir.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string) =>
  AnalysisFixture.retainingMain('fixed-arrays/main', ascii(source))

const bindingInitializer = (
  body: InspectedBody | undefined,
  ordinal: number,
): Tir.Expression | undefined =>
  body?.statements.filter((statement) => statement._tag === 'Bind').at(ordinal)?.initializer

it.effect('infers non-empty arrays and contextually types empty and nested literals', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn inferred() -> i32 { let values = [10, 20, 30] return 0 }
fn empty() -> [i32; 0] { return [] }
fn nested() -> [[i32; 0]; 2] { return [[], []] }
fn take(values: [i32; 0]) -> i32 { return 7 }
pub fn main() -> i32 { return take([]) }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const functions = records(Analysis.rootAnalysis(self)).functions
    const inferred = bindingInitializer(functions.at(0), 0)
    assert.strictEqual(inferred?._tag, 'ArrayConstruct')
    if (inferred?._tag === 'ArrayConstruct') {
      assert.strictEqual(Type.encode(inferred.type), 'Array<i32, 3>')
      assert.strictEqual(inferred.elements.length, 3)
    }
    const empty = functions.at(1)?.returnedExpression
    const nestedArray = functions.at(2)?.returnedExpression
    assert.strictEqual(empty?._tag, 'ArrayConstruct')
    assert.strictEqual(nestedArray?._tag, 'ArrayConstruct')
    if (nestedArray?._tag === 'ArrayConstruct') {
      assert.strictEqual(
        nestedArray.elements.every((element) => element._tag === 'ArrayConstruct'),
        true,
      )
    }
    const main = Projections.tirOf(self, 'fixed-arrays/main')?.functions.at(4)
    assert.strictEqual(main === undefined ? undefined : Tir.returned(main)._tag, 'Call')
  }),
)

it.effect('retains every element while diagnosing empty context and exact mismatches', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn empty() -> i32 { let values = [] return 0 }
fn types() -> i32 { let values = [1, true, 3] return 0 }
fn length() -> [i32; 3] { return [1, 2] }
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0029', 'SEM0030', 'SEM0031'],
    )
    const types = bindingInitializer(records(Analysis.rootAnalysis(self)).functions.at(1), 0)
    assert.strictEqual(types?._tag, 'Unavailable')
  }),
)

it.effect('classifies constant and dynamic checked index places', () =>
  Effect.gen(function* () {
    const valid =
      yield* snapshot(`fn dynamic(values: [i32; 3], index: usize) -> i32 { return values[index] }
fn constant(values: [i32; 3]) -> i32 { return values[1] }
pub fn main() -> i32 { return constant([4, 5, 6]) }`)
    assert.deepEqual(Analysis.diagnostics(valid), [])
    const facts = records(Analysis.rootAnalysis(valid)).functions.flatMap((fn) =>
      Tir.expressionTree(fn.returnedExpression).filter(
        (fact): fact is Extract<Tir.Expression, { readonly _tag: 'IndexPlace' }> =>
          fact._tag === 'IndexPlace',
      ),
    )
    assert.deepEqual(
      facts.map((fact) => fact.bounds._tag),
      ['Runtime', 'Proven'],
    )
    const tir = Projections.tirOf(valid, 'fixed-arrays/main')?.functions.at(0)
    assert.strictEqual(tir === undefined ? undefined : Tir.returned(tir)._tag, 'IndexPlace')

    const invalid = yield* snapshot(`fn low(values: [i32; 3]) -> i32 { return values[-1] }
fn high(values: [i32; 3]) -> i32 { return values[3] }
fn wrong(values: [i32; 3]) -> i32 { return values[true] }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(
      Analysis.diagnostics(invalid).map((diagnostic) => diagnostic.code),
      ['SEM0060', 'SEM0034', 'SEM0033'],
    )
  }),
)

it.effect('derives Copy, whole-move, partial-move, and cleanup behavior from elements', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { kind: i32 }
fn copy(values: [i32; 2]) -> i32 { let again = values return values[0] }
fn readThenMove(tokens: [Token; 2], index: usize) -> i32 {
  let kind = tokens[index].kind
  let next = move tokens
  return kind
}
fn partial(tokens: [Token; 2], index: usize) -> Token { return move tokens[index] }
fn zero(tokens: [Token; 0]) -> i32 { let next = move tokens return 0 }
fn cleanup(tokens: [Token; 3]) -> i32 { return 0 }
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0002'],
    )
    const ownership = Analysis.ownershipOf(self, 'fixed-arrays/main')
    const copy = ownership?.functions.at(0)?.bindings.at(0)
    const moved = ownership?.functions.at(1)?.bindings.at(0)
    const zero = ownership?.functions.at(3)?.bindings.at(0)
    assert.strictEqual(copy?.category._tag, 'Copyable')
    assert.strictEqual(moved?.category._tag, 'MoveOnly')
    assert.notStrictEqual(moved?.movedAt, undefined)
    assert.strictEqual(zero?.category._tag, 'MoveOnly')

    const cleanup = ownership?.functions.at(4)?.exits.at(0)?.releases.at(0)?.cleanup
    assert.strictEqual(cleanup?._tag, 'ArrayCleanup')
    if (cleanup?._tag === 'ArrayCleanup') {
      assert.strictEqual(cleanup.length, 3)
      assert.strictEqual(cleanup.element._tag, 'StructCleanup')
    }
  }),
)
