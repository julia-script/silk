import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import type * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** The example from #100, verbatim: a plain `fn` calls through a function-typed parameter. */
const concrete = `fn double(value: i32) -> i32 { return value * 2 }

fn apply(transform: once fn(i32) -> i32, value: i32) -> i32 {
  return transform(value)
}

pub fn main() -> i32 { return apply(double, 21) }`

/** The generic shape #35 needs: the callable's parameter and result are both type parameters. */
const generic = `fn double(value: i32) -> i32 { return value * 2 }

fn apply<A, B>(transform: once fn(A) -> B, value: A) -> B {
  return transform(move value)
}

pub fn main() -> i32 { return apply<i32, i32>(double, 21) }`

/** A partial application rather than a named function, the shape `Effect.map(i32.add(40))` uses. */
const partial = `import silk.i32 as i32
fn apply(transform: once fn(i32) -> i32, value: i32) -> i32 {
  return transform(value)
}

pub fn main() -> i32 { return apply(i32.add(40), 2) }`

/** A deeper section binds the trailing parameter and awaits the ordered leading pair. */
const deeper = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a + b + c }

pub fn main() -> i32 { return combine(3)(19, 20) }`

/**
 * A callable parameter forwarded to a further call, so the origin is a `ParameterReference`
 * rather than a function item at the call site. Generic on both ends, so the hidden identity has
 * to survive alongside explicit type arguments through two levels.
 */
const forwarded = `fn double(value: i32) -> i32 { return value * 2 }

fn inner<A, B>(transform: once fn(A) -> B, value: A) -> B { return transform(move value) }

fn outer<A, B>(transform: once fn(A) -> B, value: A) -> B {
  return inner<A, B>(move transform, move value)
}

pub fn main() -> i32 { return outer<i32, i32>(double, 21) }`

/** A shared `fn` callable, which unlike `once` may be invoked more than once. */
const shared = `fn double(value: i32) -> i32 { return value * 2 }

fn twice(transform: fn(i32) -> i32, value: i32) -> i32 { return transform(transform(value)) }

pub fn main() -> i32 { return twice(double, 10) + 2 }`

it.effect('calls through a function-typed parameter and agrees on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    for (const [name, source] of [
      ['concrete', concrete],
      ['generic', generic],
      ['partial', partial],
      ['deeper', deeper],
      ['forwarded', forwarded],
      ['shared', shared],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `indirect-call/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        `${name}: ${Json.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        )}`,
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n, `${name} evaluator`)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42, `${name} wasm`)
    }
  }),
)

const callableTargetName = (type: Mir.Type | undefined): string | undefined =>
  type?._tag === 'CallableValue' && type.target._tag === 'DeclarationCallableTarget'
    ? type.target.declaration.name
    : undefined

it.effect('keeps the callable parameter in the lowered contract', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'indirect-call/contract',
      ascii(concrete),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const apply = Analysis.loweredMir(snapshot).functions.find((fn) => fn.id.name === 'apply')
    assert.isDefined(apply)
    if (apply === undefined) return
    // The regression in #100 was a contract whose parameter count outran its typed locals: the
    // callable slot was dropped and `value: i32` shifted into position 0.
    assert.strictEqual(apply.parameterCount, 2)
    assert.isAtLeast(apply.localTypes.length, apply.parameterCount)
    assert.strictEqual(callableTargetName(apply.localTypes.at(0)), 'double')
    assert.strictEqual(apply.localTypes.at(1)?._tag, 'i32')
  }),
)

it.effect('specializes one instance per callable argument', () =>
  Effect.gen(function* () {
    const source = `fn double(value: i32) -> i32 { return value * 2 }
fn inc(value: i32) -> i32 { return value + 1 }

fn apply(transform: once fn(i32) -> i32, value: i32) -> i32 { return transform(value) }

pub fn main() -> i32 { return apply(double, 20) + apply(inc, 1) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'indirect-call/specialization',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    // Function arguments are monomorphized like type arguments: one `apply` body per callable,
    // each naming its target statically, so no engine needs a runtime function value.
    const applied = Analysis.loweredMir(snapshot)
      .functions.filter((fn) => fn.id.name === 'apply')
      .map((fn) => callableTargetName(fn.localTypes.at(0)))
    assert.deepEqual(
      [...applied].sort((left, right) => (left ?? '').localeCompare(right ?? '')),
      ['double', 'inc'],
    )

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('still holds a `once` callable to a single call', () =>
  Effect.gen(function* () {
    const source = `fn double(value: i32) -> i32 { return value * 2 }

fn twice(transform: once fn(i32) -> i32, value: i32) -> i32 {
  return transform(transform(value))
}

pub fn main() -> i32 { return twice(double, 10) + 2 }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'indirect-call/affine',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    // Lowering an indirect call must not weaken the affinity of the callable it goes through.
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['OWN0001'],
    )
  }),
)

it.effect('still requires an explicit move to forward a `once` callable', () =>
  Effect.gen(function* () {
    const source = `fn double(value: i32) -> i32 { return value * 2 }

fn inner(transform: once fn(i32) -> i32, value: i32) -> i32 { return transform(value) }

fn outer(transform: once fn(i32) -> i32, value: i32) -> i32 {
  return inner(transform, value)
}

pub fn main() -> i32 { return outer(double, 21) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'indirect-call/forward-move',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['OWN0003'],
    )
  }),
)
