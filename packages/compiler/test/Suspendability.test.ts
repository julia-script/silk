import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ExecutableProperty from '../src/ExecutableProperty.js'
import * as ExecutionBoundary from '../src/ExecutionBoundary.js'
import * as Hir from '../src/Hir.js'
import * as Instances from '../src/Instances.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as SuspensionMode from '../src/SuspensionMode.js'
import * as Type from '../src/Type.js'
import { ordinaryStorageSource } from './support/ordinaryStorageSource.js'
import * as Projections from './support/projections.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('suspendability/main', encoder.encode(ordinaryStorageSource(source)))

const key = (instance: Instances.InstanceKey): string =>
  `${instance.declaration.module}.${instance.declaration.name}<${instance.typeArguments
    .map(Type.genericArgumentKey)
    .join(',')}>`

const names = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  self.instances.instances
    .filter((instance) =>
      SuspensionMode.has(Instances.suspensionOf(self.instances, instance.key), 'NestedTransfer'),
    )
    .map((instance) => key(instance.key))

const effectNames = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Projections.suspensionFactsOf(self).flatMap((fact) =>
    fact.subject._tag === 'Effect' && SuspensionMode.has(fact.summary, 'NestedTransfer')
      ? [fact.subject.identity]
      : [],
  )

const main = (recipe: string): string => `pub fn main() -> i32 { return run ${recipe} }`

it('normalizes direct, nested, external, open, and unavailable graph summaries deterministically', () => {
  const graph: SuspensionMode.Graph = Object.freeze({
    roots: new Map<SuspensionMode.Mode, ReadonlySet<string>>([
      ['NestedTransfer', new Set(['nested'])],
      ['ExternalPark', new Set(['park'])],
    ]),
    dependencies: new Map([
      ['both', new Set(['park', 'nested'])],
      ['owner', new Set(['both'])],
      ['nested', new Set<string>()],
      ['park', new Set<string>()],
      ['direct', new Set<string>()],
    ]),
    permitted: new Map<string, ReadonlySet<SuspensionMode.Mode>>([
      ['open', new Set(['NestedTransfer', 'ExternalPark'])],
    ]),
    unavailable: new Set(['damaged']),
  })
  const first = SuspensionMode.summarize(graph)
  const second = SuspensionMode.summarize(graph)
  assert.strictEqual(
    SuspensionMode.encode(first.get('direct') ?? SuspensionMode.direct),
    'Complete[Direct]',
  )
  assert.deepEqual(first.get('both')?.modes, ['NestedTransfer', 'ExternalPark'])
  assert.deepEqual(first.get('open')?.modes, ['NestedTransfer', 'ExternalPark'])
  assert.strictEqual(first.get('open')?.availability, 'Open')
  assert.strictEqual(first.get('damaged')?.availability, 'Unavailable')
  assert.deepEqual(
    [...first].map(([node, summary]) => `${node}=${SuspensionMode.encode(summary)}`),
    [...second].map(([node, summary]) => `${node}=${SuspensionMode.encode(summary)}`),
  )

  const external = first.get('owner') ?? SuspensionMode.direct
  const delimiter = ExecutionBoundary.delimit(external)
  assert.isTrue(SuspensionMode.has(delimiter.body, 'ExternalPark'))
  assert.strictEqual(SuspensionMode.encode(delimiter.owner), 'Complete[Direct]')
  const span = SourceSpan.fromOffsets('suspension-boundary', 4, 12)
  assert.isDefined(span)
  if (span === undefined) return
  const diagnostic = ExecutionBoundary.entryDiagnostic(external, false, span)
  assert.strictEqual(diagnostic?.code, 'SEM0140')
  assert.strictEqual(diagnostic?.reason._tag, 'MissingExplicitExecutionOwner')
  assert.strictEqual(ExecutionBoundary.entryDiagnostic(external, true, span), undefined)
  const nestedOnly = first.get('nested') ?? SuspensionMode.direct
  assert.strictEqual(ExecutableProperty.nonParkingOfSummary(nestedOnly)._tag, 'Satisfied')
  const nonParking = ExecutableProperty.nonParkingOfSummary(external)
  assert.strictEqual(nonParking._tag, 'Unsatisfied')
  assert.deepEqual(
    nonParking._tag === 'Unsatisfied' ? nonParking.causes.map((entry) => entry.path) : [],
    [['owner', 'both', 'park']],
  )
})

it.effect('separates lazy Effect runners from their factory and synchronous siblings', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect as Effect
fn recipes() -> Effect<i32> {
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
        : SuspensionMode.has(
            Instances.effectSuspensionOf(self.instances, synchronousIdentity),
            'NestedTransfer',
          ),
    )
    const delayed = self.instances.instances.find(
      (instance) => instance.key.declaration.name === 'delayed',
    )
    assert.isDefined(delayed?.resultEffect)
    assert.isTrue(
      delayed?.resultEffect === undefined
        ? false
        : SuspensionMode.has(
            Instances.effectSuspensionOf(self.instances, delayed.resultEffect),
            'NestedTransfer',
          ),
    )
  }),
)

it.effect(
  'derives Detached independently from affinity and NonParking independently from nested transfer',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot(`import silk.effect as Effect
struct Box { value: i32 }
struct HiddenResult { value: i32 }
fn borrowed(value: &Box) -> Effect<i32> { return effect { return value.value } }
fn copied(value: i32) -> Effect<i32> { return effect { return value } }
fn opaqueProducer() -> some<F: Effect<HiddenResult>> F {
  return effect { return HiddenResult { value: 42 } }
}
effect fn nested(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
pub fn main() -> i32 {
  let box = Box { value: 40 }
  let first = borrowed(&box)
  let second = copied(41)
  let opaque = opaqueProducer()
  drop first
  drop second
  drop opaque
  return run nested(42)
}`)
      const repeated = yield* snapshot(`import silk.effect as Effect
struct Box { value: i32 }
struct HiddenResult { value: i32 }
fn borrowed(value: &Box) -> Effect<i32> { return effect { return value.value } }
fn copied(value: i32) -> Effect<i32> { return effect { return value } }
fn opaqueProducer() -> some<F: Effect<HiddenResult>> F {
  return effect { return HiddenResult { value: 42 } }
}
effect fn nested(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
pub fn main() -> i32 {
  let box = Box { value: 40 }
  let first = borrowed(&box)
  let second = copied(41)
  let opaque = opaqueProducer()
  drop first
  drop second
  drop opaque
  return run nested(42)
}`)

      assert.deepEqual(Analysis.diagnostics(self), [])
      const facts = Analysis.executablePropertiesOf(self).filter(
        (fact) => fact.subject._tag === 'Effect',
      )
      assert.deepEqual(
        Analysis.executablePropertiesOf(self).map(ExecutableProperty.encode),
        Analysis.executablePropertiesOf(repeated).map(ExecutableProperty.encode),
      )
      const borrowed = facts.find(
        (fact) =>
          fact.detached._tag === 'Unsatisfied' &&
          fact.detached.causes.some((entry) => entry.reason === 'LexicalLoan'),
      )
      assert.isDefined(borrowed)
      assert.strictEqual(borrowed?.affinity._tag, 'Unrestricted')
      assert.strictEqual(borrowed?.nonParking._tag, 'Satisfied')
      const nested = facts.find((fact) => {
        if (fact.subject._tag !== 'Effect') return false
        return SuspensionMode.has(
          Instances.effectSuspensionOf(self.instances, fact.subject.identity),
          'NestedTransfer',
        )
      })
      assert.isDefined(nested)
      assert.strictEqual(nested?.nonParking._tag, 'Satisfied')
      assert.isTrue(
        facts.some(
          (fact) => fact.detached._tag === 'Satisfied' && fact.nonParking._tag === 'Satisfied',
        ),
      )
      const opaqueProducer = self.instances.effects.find(
        (effect) =>
          Type.isNominal(effect.type.success) && effect.type.success.name === 'HiddenResult',
      )
      assert.isDefined(opaqueProducer)
      assert.strictEqual(
        facts.find(
          (fact) =>
            fact.subject._tag === 'Effect' && fact.subject.identity === opaqueProducer?.identity,
        )?.detached._tag,
        'Satisfied',
      )
    }),
)

it.effect('diagnoses a failed concrete sealed-property obligation at the generic application', () =>
  Effect.gen(function* () {
    const source = `struct Box { value: i32 }
fn requireDetached<F: Effect<i32> + Intrinsic.Detached>(body: F) -> i32 {
  drop body
  return 1
}
pub fn main() -> i32 {
  let box = Box { value: 42 }
  let view = &box
  let inner = effect { return view.value }
  return requireDetached(effect { return run inner })
}`
    const self = yield* snapshot(source)
    const diagnostic = Analysis.diagnostics(self).find((candidate) => candidate.code === 'SEM0139')

    assert.deepEqual(
      Analysis.diagnostics(self).map((candidate) => candidate.code),
      ['SEM0139'],
    )
    assert.strictEqual(diagnostic?.reason._tag, 'UnsatisfiedExecutableProperty')
    assert.strictEqual(
      diagnostic?.reason._tag === 'UnsatisfiedExecutableProperty'
        ? diagnostic.reason.property
        : undefined,
      'Intrinsic.Detached',
    )
    assert.include(
      diagnostic?.reason._tag === 'UnsatisfiedExecutableProperty'
        ? diagnostic.reason.causes.join(';')
        : '',
      'Effect:',
    )
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'requireDetached(effect { return run inner })',
    )
  }),
)

it.effect('retains borrowed provider provenance through an ordinary provide wrapper', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect as Effect
service Clock { effect fn read() -> i32 ? &Clock }
struct FixedClock {}
effect fn read(self: &FixedClock) -> i32 { return 42 }
impl Clock for FixedClock { read: FixedClock.read }
effect fn program() -> i32 ? &Clock { return run Clock.read() }
fn requireDetached<F: Effect<i32> + Intrinsic.Detached>(body: F) -> i32 {
  drop body
  return 1
}
pub fn main() -> i32 {
  let clock = FixedClock {}
  let provided = program() |> Effect.provide(&clock)
  return requireDetached(move provided)
}`)

    const diagnostics = Analysis.diagnostics(self)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0139'],
    )
    assert.include(diagnostics.at(0)?.message ?? '', 'ProviderLoan')
    assert.notInclude(
      diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0071',
    )
  }),
)

it.effect('diagnoses sealed-property obligations on represented nominal fields', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Box { value: i32 }
struct Deferred<F: once Effect<i32> + Intrinsic.Detached> { operation: F }
pub fn main() -> i32 {
  let box = Box { value: 42 }
  let view = &box
  let deferred = Deferred { operation: effect { return view.value } }
  drop deferred
  return 0
}`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0139'],
    )
  }),
)

it.effect('follows represented executables nested inside captured nominals', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Box { value: i32 }
struct Deferred<F: once Effect<i32>> { operation: F }
fn requireDetached<F: once Effect<i32> + Intrinsic.Detached>(body: F) -> i32 {
  drop body
  return 1
}
pub fn main() -> i32 {
  let box = Box { value: 42 }
  let view = &box
  let deferred = Deferred { operation: effect { return view.value } }
  return requireDetached(effect { drop deferred return 1 })
}`)

    const diagnostics = Analysis.diagnostics(self)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0139'],
    )
    assert.include(diagnostics.at(0)?.message ?? '', 'LexicalLoan')
  }),
)

it.effect('closes direct self and mutual cycles over exact execution nodes', () =>
  Effect.gen(function* () {
    const direct = yield* snapshot(`import silk.effect as Effect
effect fn loop(value: i32) -> i32 {
  if value == 0 { return 0 }
  return run Effect.suspend(loop(value - 1))
}
${main('loop(1)')}`)
    assert.deepEqual(Analysis.diagnostics(direct), [])
    assert.include(names(direct), 'suspendability/main.loop<>')

    const mutual = yield* snapshot(`import silk.effect as Effect
effect fn even(value: i32) -> i32 {
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
    const self = yield* snapshot(`import silk.effect as Effect
effect fn seed(value: i32) -> i32 {
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
    const suspendable = self.instances.instances.filter((instance) =>
      SuspensionMode.has(Instances.suspensionOf(self.instances, instance.key), 'NestedTransfer'),
    )
    const suspendableKeys = suspendable.map((instance) => Instances.keyText(instance.key)).sort()
    assert.deepEqual(suspendableKeys, [...suspendableKeys].sort())
    assert.deepEqual(effectNames(self), [...effectNames(self)].sort())
    assert.isTrue(
      suspendable.some(
        (instance) =>
          instance.key.declaration.module === 'silk/effect' &&
          instance.key.declaration.name === 'map' &&
          instance.key.typeArguments.length > 0,
      ),
    )
    assert.isTrue(
      suspendable.some(
        (instance) =>
          instance.key.declaration.module === 'silk/effect' &&
          instance.key.declaration.name === 'flatMap' &&
          instance.key.typeArguments.length > 0,
      ),
    )
  }),
)

it.effect('propagates through applied callables but not stored callable values', () =>
  Effect.gen(function* () {
    const prelude = `import silk.effect as Effect
fn suspendAndRecover(value: i32) -> i32 {
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
    const source = `import silk.effect as Effect
effect fn seed(value: i32) -> i32 { return value }
fn increment(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return run seed(41) |> Effect.map(increment) }`
    const first = yield* snapshot(source)
    const second = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(first), [])
    assert.deepEqual(names(first), [])
    assert.deepEqual(effectNames(first), [])
    assert.deepEqual(names(second), names(first))
    assert.deepEqual(effectNames(second), effectNames(first))
  }),
)

it.effect(
  'grants no sealed identity, property, or suspension mode to privileged-looking names',
  () =>
    Effect.gen(function* () {
      const spellings = [
        'Execution',
        'Wake',
        'Detached',
        'NonParking',
        'Scheduler',
        'Fiber',
        'Deferred',
        'Timer',
        'Coroutine',
      ] as const
      const declarations = spellings.map((name) => `struct ${name} { value: i32 }`).join('\n')
      const self = yield* snapshot(`${declarations}
pub fn main() -> i32 { return 42 }`)

      assert.deepEqual(Analysis.diagnostics(self), [])
      for (const name of spellings) {
        const type = Type.nominal('suspendability/main', name)
        assert.isFalse(Type.isExecution(type))
        assert.strictEqual(
          self.index.modules
            .at(-1)
            ?.structs.find(
              (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === name,
            )
            ?.typeParameters.some((parameter) => parameter.staticProperties.length > 0),
          false,
        )
      }
      assert.deepEqual(names(self), [])
      assert.isTrue(
        Analysis.executablePropertiesOf(self).every((fact) => fact.nonParking._tag === 'Satisfied'),
      )
    }),
)

const suspendingAllocator = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.effect as Effect
import silk.layout { Layout }
role SharedAudit
role ExclusiveAudit
struct SuspendingAllocator {}

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
    const self = yield* snapshot(`import silk.core { Allocator }
import silk.effect as Effect
${suspendingAllocator}

effect fn work() -> i32
? &Allocator at SharedAudit | &Allocator at ExclusiveAudit {
  return run Effect.suspend(effect { return 42 })
}

pub fn main() -> i32 {
  let sharedAudit = SuspendingAllocator {}
  let mut exclusiveAudit = SuspendingAllocator {}
  return run (work()
    |> Effect.provide<Allocator at SharedAudit>(&sharedAudit)
    |> Effect.provideMut<Allocator at ExclusiveAudit>(&mut exclusiveAudit))
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.mirOf(self)._tag, 'Available')
  }),
)

it.effect('allows ordinary allocator implementations to suspend', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.core { Allocator }
import silk.effect as Effect
${suspendingAllocator}

effect fn acquire() -> Allocation ! OutOfMemoryError ? &mut Allocator {
  return run Allocator.allocate(Layout.of<i32>())
}
effect fn program() -> i32 ! OutOfMemoryError {
  let mut custom = SuspendingAllocator {}
  let pending = acquire() |> Effect.provideMut(&mut custom)
  let allocation = run pending
  drop allocation
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.mirOf(self)._tag, 'Available')
    const entry = self.instances.entry
    const summary =
      entry._tag === 'Resolved'
        ? Instances.suspensionOf(self.instances, entry.key)
        : SuspensionMode.direct
    assert.isTrue(SuspensionMode.has(summary, 'NestedTransfer'))
    assert.isTrue(
      summary.causes.some((entry) =>
        entry.path.some(
          (node) => node.includes('suspendability/main') && node.includes('\u0000allocate\u0000'),
        ),
      ),
    )
  }),
)

it.effect('ignores unused suspending operations on the selected provider witness', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect as Effect
service Work {
  effect fn direct() -> i32 ? &mut Work
  effect fn delayed() -> i32 ? &mut Work
}
struct Worker {}
effect fn direct(self: &mut Worker) -> i32 { return 42 }
effect fn delayed(self: &mut Worker) -> i32 {
  return run Effect.suspend(effect { return 0 })
}
impl Work for Worker { direct: Worker.direct delayed: Worker.delayed }
effect fn program() -> i32 ? &mut Work { return run Work.direct() }
pub fn main() -> i32 {
  let mut worker = Worker {}
  return run program() |> Effect.provideMut(&mut worker)
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const entry = self.instances.entry
    const summary =
      entry._tag === 'Resolved'
        ? Instances.suspensionOf(self.instances, entry.key)
        : SuspensionMode.direct
    assert.isFalse(SuspensionMode.has(summary, 'NestedTransfer'))
    const delayed = self.instances.instances.find(
      (instance) => instance.key.declaration.name === 'delayed',
    )
    assert.isTrue(
      delayed === undefined
        ? false
        : SuspensionMode.has(Instances.suspensionOf(self.instances, delayed.key), 'NestedTransfer'),
    )
  }),
)
