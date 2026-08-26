import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Instances from '../src/Instances.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'

const encoder = new TextEncoder()

it.effect('retains the runner of an Effect returned through a lazy forwarding wrapper', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/nested-lazy-wrapper',
      encoder.encode(`import silk.effect as Effect
effect fn nested(program: once Effect<i32>) -> i32 {
  return run program
}
fn execute(program: once Effect<i32>) -> once Effect<i32> {
  return nested(move program)
}
effect fn immediate(value: i32) -> i32 {
  return value
}
effect fn delayed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
pub fn main() -> i32 {
  let first = run execute(immediate(1))
  let second = run execute(delayed(41))
  return first + second
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const discovery = Analysis.instancesOf(self)
    const wrappers = discovery.instances.filter(
      (instance) => instance.key.declaration.name === 'execute',
    )
    assert.strictEqual(wrappers.length, 2)
    assert.strictEqual(new Set(wrappers.map((instance) => Instances.keyText(instance.key))).size, 2)
    const suspensionModes = wrappers.map((instance) =>
      instance.resultEffect === undefined
        ? []
        : Instances.effectSuspensionOf(discovery, instance.resultEffect).modes,
    )
    assert.isTrue(suspensionModes.some((modes) => modes.length === 0))
    assert.isTrue(suspensionModes.some((modes) => modes.includes('NestedTransfer')))
    const layout = Analysis.layoutOf(self)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag === 'Available') {
      const nestedEnvironments = layout.value.effectEnvironments.filter(
        (environment) =>
          environment._tag === 'EffectEnvironment' &&
          environment.instance.declaration.name === 'nested',
      )
      assert.strictEqual(nestedEnvironments.length, 2)
      assert.strictEqual(
        new Set(
          nestedEnvironments.flatMap((environment) =>
            environment._tag === 'EffectEnvironment'
              ? environment.fields.flatMap((field) =>
                  field.effectIdentity === undefined ? [] : [field.effectIdentity],
                )
              : [],
          ),
        ).size,
        2,
      )
    }
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [], MirEncoding.encode(mir))
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        evaluated,
        (_, value) => (typeof value === 'bigint' ? value.toString() : value),
        2,
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('retains two call sites for the same bound service implementation', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/two-service-call-sites',
      encoder.encode(`import silk.effect as Effect
service Counter {
  effect fn next() -> i32 ? &mut Counter
}
struct Provider {}
effect fn next(self: &mut Provider) -> i32 { return 21 }
impl Counter for Provider { next: Provider.next }
effect fn use() -> i32 ? &mut Counter {
  let left = run Counter.next()
  let right = run Counter.next()
  return left + right
}
pub fn main() -> i32 {
  let provider = Provider {}
  return run Effect.bindRequirementOwned<Counter>(use(), move provider)
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const serviceCalls = Analysis.instancesOf(self).calls.filter(
      (call) => call.owner.declaration.name === 'use' && call.target.declaration.name === 'next',
    )
    assert.strictEqual(serviceCalls.length, 2)
    assert.strictEqual(new Set(serviceCalls.map((call) => call.span.start)).size, 2)
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [], MirEncoding.encode(mir))
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        evaluated,
        (_, value) => (typeof value === 'bigint' ? value.toString() : value),
        2,
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('terminates direct recursion discovered through a bound service implementation', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/direct-service-recursion',
      encoder.encode(`import silk.effect as Effect
service Loop {
  effect fn next() -> i32 ? &mut Loop
}
struct Provider {}
effect fn next(self: &mut Provider) -> i32 ? &mut Loop {
  return run Loop.next()
}
impl Loop for Provider { next: Provider.next }
effect fn program() -> i32 ? &mut Loop {
  return run Loop.next()
}
pub fn main() -> i32 {
  let provider = Provider {}
  return run Effect.bindRequirementOwned<Loop>(program(), move provider)
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const implementations = Analysis.instancesOf(self).instances.filter(
      (instance) => instance.key.declaration.name === 'next',
    )
    assert.strictEqual(implementations.length, 1)
  }),
)

it.effect('retains a provided Effect runner transferred into an independent Execution', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/provided-independent-execution',
      encoder.encode(`import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
import silk.execution as Execution
service Value {
  effect fn get() -> i32 ? &mut Value
}
struct Provider { value: i32 }
effect fn get(self: &mut Provider) -> i32 { return self.value }
impl Value for Provider { get: Provider.get }
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
fn ready(state: &()) -> () { return () }
fn complete(owner: &mut Owner, value: i32) -> () {
  owner.result = value
  return ()
}
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
effect fn executeBody(program: once Effect<i32 ? &mut Value>) -> i32
! OutOfMemoryError {
  let provider = Provider { value: 42 }
  let bound = Effect.bindRequirementOwned<Value>(move program, move provider)
  let transferred = effect { return run move bound }
  let mut allocator = Allocator.systemAllocatorProvider()
  let execution = run Execution.make(move transferred, (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut owner = Owner { slot: Empty {}, result: 0 }
  run Execution.drive(move execution, &mut owner, complete, suspend)
  return owner.result
}
fn execute(
  program: once Effect<i32 ? &mut Value>,
) -> once Effect<i32 ! OutOfMemoryError> {
  return executeBody(move program)
}
effect fn program() -> i32 ? &mut Value {
  let value = run Value.get()
  return run Effect.suspend(effect { return value })
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 {
  return run execute(program()) |> Effect.catchAll(recover)
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [], MirEncoding.encode(mir))
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        evaluated,
        (_, value) => (typeof value === 'bigint' ? value.toString() : value),
        2,
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('retains an exact Effect argument forwarded through a service operation', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/service-operation',
      encoder.encode(`import silk.effect as Effect
service Scheduler {
  effect fn prepare<A>(program: once Effect<A>) -> A ? &mut Scheduler
}
struct Provider {}
effect fn prepare<A>(self: &mut Provider, program: once Effect<A>) -> A {
  return run program
}
impl Scheduler for Provider { prepare: Provider.prepare }
effect fn use(program: once Effect<i32>) -> i32 ? &mut Scheduler {
  return run Scheduler.prepare<i32>(move program)
}
effect fn childWork() -> i32 {
  return run Effect.suspend(effect { return 42 })
}
pub fn main() -> i32 {
  let provider = Provider {}
  return run Effect.bindRequirementOwned<Scheduler>(use(childWork()), move provider)
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [], MirEncoding.encode(mir))
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        evaluated,
        (_, value) => (typeof value === 'bigint' ? value.toString() : value),
        2,
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('retains every provider target forwarded through reachable Effect branches', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/service-branches',
      encoder.encode(`import silk.effect as Effect
service Choice {
  effect fn left() -> i32 ? &mut Choice
  effect fn right() -> i32 ? &mut Choice
}
struct Provider {}
effect fn left(self: &mut Provider) -> i32 { return 20 }
effect fn right(self: &mut Provider) -> i32 { return 22 }
impl Choice for Provider { left: Provider.left right: Provider.right }
struct First {}
struct Second {}
effect fn select(input: First | Second) -> i32 ? &mut Choice {
  return match move input {
    First {} => run Choice.left()
    Second {} => run Choice.right()
  }
}
effect fn use(input: First | Second) -> i32 ? &mut Choice {
  return run select(move input)
}
pub fn main() -> i32 {
  let provider = Provider {}
  return run Effect.bindRequirementOwned<Choice>(use(Second {}), move provider)
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const providerTargets = Analysis.instancesOf(self).calls.filter(
      (call) =>
        call.owner.declaration.name === 'select' &&
        (call.target.declaration.name === 'left' || call.target.declaration.name === 'right'),
    )
    assert.deepEqual(providerTargets.map((call) => call.target.declaration.name).sort(), [
      'left',
      'right',
    ])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [], MirEncoding.encode(mir))
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 22n)
  }),
)
