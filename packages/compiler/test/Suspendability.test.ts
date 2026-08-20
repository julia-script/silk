import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Instances from '../src/Instances.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('suspendability/main', encoder.encode(source))

const key = (instance: Instances.InstanceKey): string =>
  `${instance.declaration.module}.${instance.declaration.name}<${instance.typeArguments
    .map(Type.genericArgumentKey)
    .join(',')}>`

const names = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.suspendableInstancesOf(self).map(key)

const main = (recipe: string): string => `pub fn main() -> i32 { return run ${recipe} }`

it.effect('separates lazy Effect runners from their factory and synchronous siblings', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn recipes() -> Effect<i32> {
  let synchronous = effect { return 1 }
  let suspended = delayed()
  return move suspended
}
effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 2 })
}
${main('recipes()')}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const recipes = self.instances.instances.find(
      (instance) => instance.key.declaration.name === 'recipes',
    )
    const synchronous = recipes?.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find((expression) => expression._tag === 'EffectBlock')
    const synchronousIdentity =
      recipes === undefined || synchronous?._tag !== 'EffectBlock'
        ? undefined
        : Instances.effectIdentity(recipes.key, synchronous.site)
    assert.isDefined(synchronousIdentity)
    assert.isFalse(
      synchronousIdentity === undefined
        ? true
        : Instances.isEffectSuspendable(self.instances, synchronousIdentity),
    )
    const delayed = self.instances.instances.find(
      (instance) => instance.key.declaration.name === 'delayed',
    )
    assert.isDefined(delayed?.resultEffect)
    assert.isTrue(
      delayed?.resultEffect === undefined
        ? false
        : Instances.isEffectSuspendable(self.instances, delayed.resultEffect),
    )
  }),
)

it.effect('closes direct self and mutual cycles over exact execution nodes', () =>
  Effect.gen(function* () {
    const direct = yield* snapshot(`effect fn loop(value: i32) -> i32 {
  if value == 0 { return 0 }
  return run Effect.suspend(loop(value - 1))
}
${main('loop(1)')}`)
    assert.deepEqual(Analysis.diagnostics(direct), [])
    assert.include(names(direct), 'suspendability/main.loop<>')

    const mutual = yield* snapshot(`effect fn even(value: i32) -> i32 {
  if value == 0 { return 1 }
  return run odd(value - 1)
}
effect fn odd(value: i32) -> i32 {
  if value == 0 { return 0 }
  return run Effect.suspend(even(value - 1))
}
${main('even(2)')}`)
    assert.deepEqual(Analysis.diagnostics(mutual), [])
    assert.include(names(mutual), 'suspendability/main.even<>')
    assert.include(names(mutual), 'suspendability/main.odd<>')
  }),
)

it.effect('propagates through concrete Effect.map and flatMap specializations', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`effect fn seed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
fn increment(value: i32) -> i32 { return value + 1 }
fn next(value: i32) -> Effect<i32> { return seed(value + 1) }
effect fn program() -> i32 {
  let mapped = seed(40) |> Effect.map(increment)
  return run mapped |> Effect.flatMap(next)
}
${main('program()')}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const suspendable = Analysis.suspendableInstancesOf(self)
    assert.deepEqual(
      suspendable.map(Instances.keyText),
      [...suspendable.map(Instances.keyText)].sort(),
    )
    assert.deepEqual(
      Analysis.suspendableEffectsOf(self),
      [...Analysis.suspendableEffectsOf(self)].sort(),
    )
    assert.isTrue(
      suspendable.some(
        (instance) =>
          instance.declaration.module === 'silk/effects' &&
          instance.declaration.name === 'map' &&
          instance.typeArguments.length > 0,
      ),
    )
    assert.isTrue(
      suspendable.some(
        (instance) =>
          instance.declaration.module === 'silk/effects' &&
          instance.declaration.name === 'flatMap' &&
          instance.typeArguments.length > 0,
      ),
    )
  }),
)

it.effect('propagates through applied callables but not stored callable values', () =>
  Effect.gen(function* () {
    const prelude = `fn suspendAndRecover(value: i32) -> i32 {
  let pending = Effect.suspend(effect { return value })
  return run pending
}`
    const stored = yield* snapshot(`${prelude}
pub fn main() -> i32 { let unused = suspendAndRecover return 42 }`)
    assert.deepEqual(Analysis.diagnostics(stored), [])
    assert.notInclude(names(stored), 'suspendability/main.main<>')
    assert.include(names(stored), 'suspendability/main.suspendAndRecover<>')

    const applied = yield* snapshot(`${prelude}
pub fn main() -> i32 { let callback = suspendAndRecover return callback(42) }`)
    assert.deepEqual(Analysis.diagnostics(applied), [])
    assert.include(names(applied), 'suspendability/main.main<>')
    assert.include(names(applied), 'suspendability/main.suspendAndRecover<>')
  }),
)

it.effect('keeps synchronous controls empty and ordering deterministic', () =>
  Effect.gen(function* () {
    const source = `effect fn seed(value: i32) -> i32 { return value }
fn increment(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return run seed(41) |> Effect.map(increment) }`
    const first = yield* snapshot(source)
    const second = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(first), [])
    assert.deepEqual(Analysis.suspendableInstancesOf(first), [])
    assert.deepEqual(Analysis.suspendableEffectsOf(first), [])
    assert.deepEqual(
      Analysis.suspendableInstancesOf(second),
      Analysis.suspendableInstancesOf(first),
    )
    assert.deepEqual(Analysis.suspendableEffectsOf(second), Analysis.suspendableEffectsOf(first))
  }),
)

const suspendingAllocator = `struct SuspendingAllocator {}

effect fn allocate(
  self: &mut SuspendingAllocator,
  layout: Layout
) -> Allocation ! OutOfMemoryError {
  let delayed = Effect.suspend(effect {
    return run Intrinsic.systemAllocationAcquire(move layout)
  })
  return run delayed
}

impl Allocator for SuspendingAllocator { allocate: SuspendingAllocator.allocate }`

it.effect('keeps shared non-default Allocator demands out of private coroutine storage', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${suspendingAllocator}

effect fn work() -> i32
? &Allocator@SharedAudit | &Allocator@ExclusiveAudit {
  return run Effect.suspend(effect { return 42 })
}

pub fn main() -> i32 {
  let sharedAudit = SuspendingAllocator {}
  let mut exclusiveAudit = SuspendingAllocator {}
  return run (work()
    |> Effect.provide<&Allocator@SharedAudit>(&sharedAudit)
    |> Effect.provideMut<&Allocator@ExclusiveAudit>(&mut exclusiveAudit))
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.mirOf(self)._tag, 'Available')
  }),
)

it.effect('allows ordinary allocator implementations to suspend', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${suspendingAllocator}

effect fn inert() -> i32 ? &mut Allocator { return 1 }
pub fn main() -> i32 {
  let mut custom = SuspendingAllocator {}
  let ordinary = run inert() |> Effect.provideMut(&mut custom)
  let delayed = Effect.suspend(effect { return 41 })
  return ordinary + run delayed
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.mirOf(self)._tag, 'Available')
  }),
)
