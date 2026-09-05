import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Lifetime from '../src/Lifetime.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const codesOf = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)
// ISSUE-47: take-once mode derives from the captured type, not from a source-level `move`. A
// fresh affine temporary enters the environment by ownership and a declared `once` result keeps
// its access at the call site.
const tokenSurface = `struct Token { value: i32 }
fn addToken(value: i32, token: Token) -> i32 { return value + token.value }
`
const payloadSurface = `import silk.effect { Effect }
struct Payload { value: i32 }
`

it.effect(
  'quantifies an anonymous borrowed parameter independently of its captured environment',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSource(
        'anonymous-capture/quantified-input',
        ascii(`fn apply(use: for<'a> fn<'static>(&'a mut i32) -> i32) -> i32 {
  let mut value = 40
  return use(&mut value)
}
pub fn main() -> i32 {
  let offset = 2
  return apply(fn(value: &mut i32) -> i32 {
    value.* = value.* + offset
    return value.*
  })
}`),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const anonymous = Analysis.expressionsOf(snapshot, 'anonymous-capture/quantified-input').find(
        (expression) => expression.syntax.kind === 'AnonymousCallableExpression',
      )
      const type = anonymous?.type._tag === 'Available' ? anonymous.type.type : undefined
      assert.isTrue(type !== undefined && Type.isCallable(type))
      if (type === undefined || !Type.isCallable(type)) return
      assert.strictEqual(type.lifetimeBinders.length, 1)
      assert.deepEqual(Type.freeLifetimes(type), [Lifetime.staticLifetime])
    }),
)

it.effect('derives take-once invocation and run access from captured affine temporaries', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      [
        'once-twice',
        `${tokenSurface}pub fn main() -> i32 {
  let f = addToken(Token { value: 1 })
  return f(1) + f(2)
}`,
        ['OWN0001'],
      ],
      [
        'once-as-fn',
        `${tokenSurface}fn callShared(f: fn<'static>(i32) -> i32) -> i32 { return f(1) + f(2) }
pub fn main() -> i32 { return callShared(addToken(Token { value: 1 })) }`,
        ['SEM0076'],
      ],
      [
        'nomove-capture',
        `${tokenSurface}fn prepare(token: Token) -> once fn<'static>(i32) -> i32 { return addToken(token) }
pub fn main() -> i32 { let p = prepare(Token { value: 10 }) return p(1) }`,
        ['OWN0003'],
      ],
      [
        'run-twice',
        `${payloadSurface}fn prepare(payload: Payload) -> once Effect<'static; Payload> { return effect { return move payload } }
pub fn main() -> i32 {
  let e = prepare(Payload { value: 36 })
  let p = run e
  let q = run e
  return p.value + q.value
}`,
        ['OWN0001'],
      ],
      [
        'effect-fn-once',
        `${payloadSurface}effect fn unwrap(payload: Payload) -> Payload { return move payload }
pub fn main() -> i32 {
  let e = unwrap(Payload { value: 36 })
  let p = run e
  let q = run e
  return p.value + q.value
}`,
        ['OWN0001'],
      ],
      [
        'pass-once-nomove',
        `${payloadSurface}fn prepare(payload: Payload) -> once Effect<'static; Payload> { return effect { return move payload } }
fn take(e: once Effect<Payload>) -> i32 { return (run e).value }
pub fn main() -> i32 {
  let e = prepare(Payload { value: 36 })
  return take(e) + take(e)
}`,
        ['OWN0003', 'OWN0003'],
      ],
    ] as const) {
      const snapshot = yield* Analysis.ofSource(`anonymous-capture/${name}`, ascii(source))
      assert.deepEqual(codesOf(snapshot), expected, name)
    }
  }),
)
