import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target?: string) =>
  Analysis.ofSourceRealized('user-services/main', encoder.encode(source), target)

const sharedSource = `import silk.effect { Effect }
service Counter { effect fn get() -> i32 ? &Counter }
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
effect fn read() -> i32 ? &Counter { return run Counter.get() }
pub fn main() -> i32 {
  let fixed = Fixed { value: 42 }
  return run Effect.provide(read(), &fixed)
}`

it.effect('lowers shared source service dispatch through native LLVM', () =>
  Effect.gen(function* () {
    const native = yield* snapshot(sharedSource, 'aarch64-apple-darwin')
    assert.deepEqual(Analysis.diagnostics(native), [])
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, 'define')
    assert.notInclude(llvm.ir, 'Counter')
  }),
)

it.effect('rejects a generic service witness bound its header never promises', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Marker<T> { fn mark(value: T) -> i32 }
interface Other<T> { fn mark(value: T) -> i32 }
service Counter<Value> { effect fn get(value: &Value) -> i32 ? &Counter<Value> }
struct Fixed<S> {}
effect fn get<S: Other>(self: &Fixed<S>, value: &S) -> i32 { return 42 }
impl<S: Marker<S>> Counter<S> for Fixed<S> { get: Fixed.get }
pub fn main() -> i32 { return 0 }`)
    const invalid = Analysis.diagnostics(self).filter((diagnostic) => diagnostic.code === 'SEM0083')
    assert.strictEqual(invalid.length, 1)
    assert.include(invalid.at(0)?.message ?? '', 'does not require')
    assert.include(invalid.at(0)?.message ?? '', 'Other')
  }),
)

it.effect('accepts failure and requirement rows promised by a generic service header', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Marker<T> { fn mark(value: T) -> i32 }

service Counter<E, ?R, Value> {
  effect fn get(value: &Value) -> i32 ! E ? R | &Counter<E, R, Value>
}

struct Fixed<S, E, ?R> {}
effect fn get<S: Marker, E, ?R>(self: &Fixed<S, E, R>, value: &S) -> i32 ! E ? R {
  return 42
}
impl<S: Marker<S>, E, ?R> Counter<E, R, S> for Fixed<S, E, R> { get: Fixed.get }

pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`),
      [],
    )
  }),
)

it.effect('keeps InsecureSeed fields private', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'insecure-seed/private',
      encoder.encode(`import silk.insecure_seed { InsecureSeed }
pub fn main() -> i32 {
  let provider = InsecureSeed.fixed(1, 2)
  return provider.seed.first
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0028'],
    )
  }),
)

it.effect('keeps ordinary Report conformance static and out of requirement rows', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub struct Problem {}
pub effect fn main() -> () ! Problem { return () }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const entry = Analysis.instancesOf(self).entry
    assert.strictEqual(entry._tag, 'Resolved')
    if (entry._tag === 'Resolved' && entry.kind === 'Effect')
      assert.deepEqual(entry.requirements, [])
  }),
)

it.effect('rejects an ordinary interface as an Effect dependency', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Clock { fn now(value: &Self) -> i32 }
effect fn read() -> i32 ? &Clock { return 42 }
pub fn main() -> i32 { return 0 }`)
    assert.isTrue(
      Analysis.diagnostics(self).some(
        (diagnostic) => diagnostic.code === 'SEM0070' && diagnostic.message.includes('Clock'),
      ),
    )
  }),
)

it.effect('allows a service to participate in an ordinary compile-time bound', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`service Clock { effect fn now() -> i32 ? &Clock }
struct Fixed {}
effect fn now(self: &Fixed) -> i32 { return 42 }
impl Clock for Fixed { now: Fixed.now }
fn preserve<T: Clock>(value: T) -> T { return move value }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
  }),
)
