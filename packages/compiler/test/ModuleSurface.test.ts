import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ModuleSurface from '../src/ModuleSurface.js'

const encoder = new TextEncoder()

const surface = Effect.fnUntraced(function* (source: string) {
  const analysis = yield* Analysis.ofSource('surface/Main', encoder.encode(source))
  return analysis.surfaces.get('surface/Main') ?? assert.fail('missing module surface')
})

it.effect('compares independently allocated equal facts exactly', () =>
  Effect.gen(function* () {
    const source = `pub fn answer(value: i32) -> i32 { return value }
pub struct Pair { pub left: i32 pub right: i32 }
pub const enabled: bool = true`
    const left = yield* surface(source)
    const right = yield* surface(source)

    assert.notStrictEqual(left, right)
    assert.strictEqual(ModuleSurface.equals(left, right), true)
    assert.strictEqual(left.canonical, right.canonical)
  }),
)

it.effect('ignores bodies and source positions while retaining header meaning', () =>
  Effect.gen(function* () {
    const left = yield* surface(`fn helper() -> i32 { return 1 }
pub fn answer() -> i32 { return 42 }`)
    const right = yield* surface(`fn helper() -> i32 { return 123456 }


pub fn answer() -> i32 { return 99 }`)

    assert.strictEqual(ModuleSurface.equals(left, right), true)
  }),
)

it.effect('distinguishes every cross-module observable header family', () =>
  Effect.gen(function* () {
    const cases = [
      ['pub fn answer() -> i32 { return 1 }', 'pub fn answer(value: i32) -> i32 { return value }'],
      ['pub fn answer() -> i32 { return 1 }', 'fn answer() -> i32 { return 1 }'],
      ['pub struct Pair { pub left: i32 }', 'pub struct Pair { pub left: i32 pub right: i32 }'],
      ['pub const answer: i32 = 1', 'pub const answer: i32 = 2'],
      [
        `struct Guard {}
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }`,
        'struct Guard {}',
      ],
      ['pub fn answer() -> Mystery { return 1 }', 'pub fn answer() -> Other { return 1 }'],
      [
        'pub fn ab() -> i32 { return 1 }',
        'pub fn a() -> i32 { return 1 } fn b() -> i32 { return 1 }',
      ],
    ] as const

    for (const [before, after] of cases) {
      const left = yield* surface(before)
      const right = yield* surface(after)
      assert.strictEqual(ModuleSurface.equals(left, right), false, `${before} should differ`)
    }
  }),
)

it.effect('keeps string distinct from an immutable byte view in module surfaces', () =>
  Effect.gen(function* () {
    const text = yield* surface('pub fn identity(value: string) -> string { return value }')
    const bytes = yield* surface('pub fn identity(value: &[u8]) -> &[u8] { return value }')

    assert.strictEqual(ModuleSurface.equals(text, bytes), false)
    assert.include(text.canonical, 'string')
    assert.include(bytes.canonical, 'slice:Shared<builtin:u8>')
    // Pins the canonical surface encoding byte-for-byte; an encoding change must be deliberate.
    assert.strictEqual(
      text.canonical,
      '13:ModuleSurface12:surface/Main650:5:Array639:19:FunctionDeclaration35:18:DeclarationOrdinal11:6:Number1:053:9:Canonical39:11:CanonicalId12:surface/Main8:identity6:Public8:Ordinary7:5:Array11:6:Number1:1103:5:Array93:9:Parameter11:6:Number1:021:11:PresentName5:value41:12:ResolvedType14:4:Type6:string7:5:False24:11:PresentName8:identity41:12:ResolvedType14:4:Type6:string7:5:False6:4:None133:10:FailureRow6:4:True7:5:Array7:5:Array7:5:Array21:18:EmptyRowExpression58:10:RowAlgebra33:8:Concrete20:9:FiniteRow7:5:Array7:5:Array137:14:RequirementRow6:4:True7:5:Array21:18:EmptyRowExpression58:10:RowAlgebra33:8:Concrete20:9:FiniteRow7:5:Array7:5:Array7:5:Array7:5:Array7:5:Array7:5:Array7:5:Array',
    )
  }),
)

it.effect('keeps malformed and unavailable header states deterministic without stale repair', () =>
  Effect.gen(function* () {
    const damaged = 'pub fn answer(value: ) -> { return 1 }'
    const first = yield* surface(damaged)
    const second = yield* surface(damaged)
    const repaired = yield* surface('pub fn answer(value: i32) -> i32 { return value }')

    assert.strictEqual(ModuleSurface.equals(first, second), true)
    assert.strictEqual(ModuleSurface.equals(first, repaired), false)
  }),
)

it.effect('distinguishes damaged applied row arguments in module surfaces', () =>
  Effect.gen(function* () {
    const withFailure = (failure: string) => `pub struct Envelope<T, !E> {}
pub fn inspect(value: Envelope<i32 ! ${failure}>) -> i32 { return 0 }`
    const left = yield* surface(withFailure('MissingA'))
    const right = yield* surface(withFailure('MissingB'))

    assert.strictEqual(ModuleSurface.equals(left, right), false)
  }),
)

it.effect('excludes conformance hook bodies from the exported semantic surface', () =>
  Effect.gen(function* () {
    const first = yield* surface(`struct Guard {}
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }`)
    const second = yield* surface(`struct Guard {}
impl Drop for Guard { fn drop(self: &mut Guard) -> () { unsafe { return () } } }`)

    assert.strictEqual(ModuleSurface.equals(first, second), true)
  }),
)
