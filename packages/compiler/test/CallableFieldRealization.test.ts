import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Analysis from '../src/Analysis.js'
import * as CallableFieldRealization from '../src/CallableFieldRealization.js'
import * as Instances from '../src/Instances.js'
import * as RepresentationField from '../src/RepresentationField.js'
import * as Type from '../src/Type.js'
import { unreachable } from './support/raise.js'

/**
 * The runtime realization seam of nominal callable storage.
 *
 * `RepresentationField` (issue #187) owns stable field identity, the retained representation
 * argument, and admissibility. These tests pin the enrichment this change adds on top: one static
 * target, its concrete arguments, the ordered capture environment, the receiver access an
 * invocation demands, capture loans, liveness, and the exactly-once cleanup plan — all reached
 * through one deterministic lookup rather than re-read from initializer syntax.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const realized = Effect.fnUntraced(function* (name: string, source: string) {
  return yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
})

/** Builds the realization index the same way every downstream phase reaches it. */
const realizationsOf = (snapshot: Analysis.Snapshot): CallableFieldRealization.Index =>
  Instances.callableFieldRealizations(snapshot.instances, snapshot.index)

const soleRealization = (
  index: CallableFieldRealization.Index,
): CallableFieldRealization.CallableRealization => {
  const supported = index.entries.flatMap((entry) =>
    entry.support._tag === 'Supported' &&
    CallableFieldRealization.isCallableRealization(entry.support.realization)
      ? [entry.support.realization]
      : [],
  )
  return supported.at(0) ?? unreachable('expected one supported callable field realization')
}

const soleEffectRealization = (
  index: CallableFieldRealization.Index,
): CallableFieldRealization.EffectRealization => {
  const supported = index.entries.flatMap((entry) =>
    entry.support._tag === 'Supported' &&
    CallableFieldRealization.isEffectRealization(entry.support.realization)
      ? [entry.support.realization]
      : [],
  )
  return supported.at(0) ?? unreachable('expected one supported Effect field realization')
}

const namedCallable = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  return parser.parse(1)
}`

const capturingSection = `struct Adder<F: fn(i32) -> i32> { step: F }
pub fn main() -> i32 {
  let adder = Adder { step: i32.add(1) }
  return adder.step(2)
}`

it.effect('realizes a named callable field with a static target and no capture lanes', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized('callable-field/named', namedCallable)
    const realization = soleRealization(realizationsOf(snapshot))

    assert.deepEqual(realization.target, {
      _tag: 'Declaration',
      module: 'callable-field/named',
      name: 'decode',
    })
    assert.strictEqual(realization.environment, undefined)
    assert.deepEqual(realization.captures, [])
    assert.deepEqual(realization.loans, [])
    assert.deepEqual(realization.cleanup.lanes, [])
    assert.strictEqual(realization.cleanup.consumedByInvocation, false)
    assert.strictEqual(realization.invocation, 'Shared')
    assert.strictEqual(realization.liveness.moveOnly, true)
    assert.strictEqual(realization.liveness.ownedLanes, 0)
  }),
)

it.effect('realizes a capturing section field with one ordered inline capture lane', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized('callable-field/capturing', capturingSection)
    const realization = soleRealization(realizationsOf(snapshot))

    assert.strictEqual(realization.target._tag, 'Declaration')
    assert.strictEqual(
      realization.target._tag === 'Declaration' ? realization.target.name : undefined,
      'add',
    )
    assert.strictEqual(realization.environment?._tag, 'CallableEnvironmentIdentity')
    assert.strictEqual(realization.captures.length, 1)
    const capture = realization.captures.at(0) ?? unreachable('expected one capture lane')
    assert.strictEqual(capture.ordinal, 0)
    assert.strictEqual(capture.parameterOrdinal, 1)
    assert.strictEqual(capture.access, 'Copy')
    assert.strictEqual(Type.encode(capture.type), 'i32')
    assert.strictEqual(capture.owned, false)
    assert.strictEqual(capture.borrowed, false)
    // A Copy capture is neither loaned nor cleaned, so the aggregate carries no drop obligation.
    assert.deepEqual(realization.loans, [])
    assert.deepEqual(realization.cleanup.lanes, [])
    assert.strictEqual(realization.invocation, 'Shared')
  }),
)

it.effect(
  'records exactly-once cleanup for an unrun stored Effect without crossing its fence',
  () =>
    Effect.gen(function* () {
      const module = 'effect-field/unrun-cleanup'
      const snapshot = yield* realized(
        module,
        `struct Token { value: i32 }
struct Deferred<F: once Effect<i32>> { operation: F }
fn consume(token: Token) -> i32 { return token.value }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  let deferred = Deferred { operation: effect { return consume(move token) } }
  return 0
}`,
      )
      const realization = soleEffectRealization(realizationsOf(snapshot))

      assert.include(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        'SEM0107',
      )
      assert.strictEqual(snapshot.layoutCatalog._tag, 'Unavailable')
      assert.deepEqual(realization.runner, {
        _tag: 'CanonicalDeclarationId',
        module,
        name: 'main$effect$0',
      })
      assert.deepEqual(realization.runnerArguments, [])
      assert.strictEqual(realization.access, 'Take')
      assert.strictEqual(realization.suspendable, false)
      assert.deepEqual(realization.rows.failures, [])
      assert.deepEqual(realization.rows.requirements, [])
      assert.strictEqual(realization.environment.length, 1)
      const capture =
        realization.environment.at(0) ?? unreachable('expected one owned Effect environment slot')
      assert.strictEqual(capture.source, 'Binding')
      assert.strictEqual(capture.sourceOrdinal, 0)
      assert.strictEqual(capture.access, 'Take')
      assert.strictEqual(Type.encode(capture.type), `${module}.Token`)
      assert.strictEqual(capture.owned, true)
      assert.deepEqual(realization.cleanup.unrunLanes, [0])
      assert.strictEqual(realization.cleanup.consumedByRun, true)
    }),
)

it.effect('realizes a suspending stored runner with exact rows and no structural Effect ABI', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'effect-field/suspending',
      `struct Deferred<F: Effect<i32>> { operation: F }
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
effect fn delayed() -> i32 {
  let mut allocator = SystemAllocator.make()
  let provided = Effect.suspend(effect { return 42 }) |> Effect.provideMut(&mut allocator)
  return run Effect.catch(move provided, recover)
}
pub fn main() -> i32 {
  let deferred = Deferred { operation: effect { return run delayed() } }
  return 0
}`,
    )
    const realization = soleEffectRealization(realizationsOf(snapshot))

    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0107',
    )
    assert.strictEqual(snapshot.layoutCatalog._tag, 'Unavailable')
    assert.strictEqual(realization.suspendable, true)
    assert.strictEqual(realization.runner.module, 'effect-field/suspending')
    assert.strictEqual(realization.runner.name, 'main$effect$0')
    assert.strictEqual(realization.runnerArguments.every(Type.isConcreteGenericArgument), true)
    assert.deepEqual(realization.rows.failures, [])
    assert.deepEqual(realization.rows.requirements, [])
    assert.deepEqual(realization.environment, [])
  }),
)

it.effect('keeps the realization keyed by the shared representation field identity', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized('callable-field/lookup', namedCallable)
    const index = realizationsOf(snapshot)
    const nominals = Instances.representedNominals(snapshot.instances, snapshot.index)
    const instance = nominals.at(0) ?? unreachable('expected one represented nominal instance')
    const plan =
      RepresentationField.plansOf(snapshot.index, instance).at(0) ??
      unreachable('expected one representation field plan')

    // The same identity #187 resolves is the identity #189 realizes; no second scheme exists.
    assert.strictEqual(
      CallableFieldRealization.key(instance, plan.id),
      RepresentationField.key(instance, plan.id),
    )
    const support = CallableFieldRealization.lookup(index, instance, plan.id)
    assert.strictEqual(support?._tag, 'Supported')
    assert.deepEqual(
      CallableFieldRealization.realizationOf(index, instance, plan.id)?.field,
      plan.id,
    )
    assert.strictEqual(CallableFieldRealization.supportsInstance(index, instance), true)
  }),
)

it.effect('derives invocation admissibility from the aggregate receiver access', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized('callable-field/access', namedCallable)
    const realization = soleRealization(realizationsOf(snapshot))

    assert.deepEqual([...CallableFieldRealization.admittedModes('Shared')], ['Shared'])
    assert.deepEqual(
      [...CallableFieldRealization.admittedModes('Exclusive')],
      ['Shared', 'Exclusive'],
    )
    assert.deepEqual(
      [...CallableFieldRealization.admittedModes('Take')],
      ['Shared', 'Exclusive', 'Take'],
    )
    // A shared `fn` field is reachable through every receiver access.
    assert.strictEqual(CallableFieldRealization.admitsInvocation(realization, 'Shared'), true)
    assert.strictEqual(CallableFieldRealization.admitsInvocation(realization, 'Exclusive'), true)
    assert.strictEqual(CallableFieldRealization.admitsInvocation(realization, 'Take'), true)

    // Ownership rejects before specialization and the engines invoke after it; both decide through
    // these three functions, so the fence and the runtime cannot disagree about a receiver.
    assert.strictEqual(CallableFieldRealization.admitsMode('Shared', 'Take'), false)
    assert.strictEqual(CallableFieldRealization.admitsMode('Shared', 'Exclusive'), false)
    assert.strictEqual(CallableFieldRealization.admitsMode('Exclusive', 'Take'), false)
    assert.strictEqual(CallableFieldRealization.admitsMode('Take', 'Take'), true)
    // A borrow anywhere in a place weakens the whole place to that borrow's access.
    assert.strictEqual(CallableFieldRealization.weakerAccess('Take', 'Shared'), 'Shared')
    assert.strictEqual(CallableFieldRealization.weakerAccess('Shared', 'Take'), 'Shared')
    assert.strictEqual(CallableFieldRealization.weakerAccess('Take', 'Exclusive'), 'Exclusive')
    assert.strictEqual(CallableFieldRealization.weakerAccess('Take', 'Take'), 'Take')
  }),
)

it.effect('reports an unresolved representation field as explicitly unsupported', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized('callable-field/unsupported', namedCallable)
    const nominals = Instances.representedNominals(snapshot.instances, snapshot.index)
    const instance = nominals.at(0) ?? unreachable('expected one represented nominal instance')
    const plan =
      RepresentationField.plansOf(snapshot.index, instance).at(0) ??
      unreachable('expected one representation field plan')
    const bound = Type.callable(['i32'], 'i32')

    // An unavailable #187 resolution never becomes a realization; it keeps an explicit proof. The
    // resolution comes from #187 itself rather than a fabricated record, so this pins the real
    // shape an open representation argument produces.
    const openInstance = Type.nominal(instance.module, instance.name, [
      Type.representationParameterArgument(plan.parameter),
    ])
    const openResolution =
      RepresentationField.lookup(
        RepresentationField.resolveFields(snapshot.index, [openInstance]),
        openInstance,
        plan.id,
      ) ?? unreachable('expected an unavailable representation field resolution')
    assert.strictEqual(openResolution._tag, 'UnavailableRepresentationField')
    const open = CallableFieldRealization.realizeField(
      openResolution,
      snapshot.instances.callables,
      snapshot.instances.effects,
    )
    assert.strictEqual(open._tag, 'Unsupported')
    assert.strictEqual(
      open._tag === 'Unsupported' ? open.reason._tag : undefined,
      'UnresolvedRepresentation',
    )

    // An Effect identity without its canonical discovered runner stays explicitly unsupported.
    const effect = Type.effect('i32', [])
    const stored = CallableFieldRealization.realizeField(
      Object.freeze({
        _tag: 'ResolvedRepresentationField',
        id: plan.id,
        instance,
        argument: Type.exactRepresentationArgument(
          Type.effectIdentityArgument('callable-field/unsupported.effect'),
          effect,
        ),
        requiredBound: effect,
        admissibility: Object.freeze({ _tag: 'Admitted' }),
      }),
      snapshot.instances.callables,
      snapshot.instances.effects,
    )
    assert.strictEqual(stored._tag, 'Unsupported')
    assert.strictEqual(
      stored._tag === 'Unsupported' ? stored.reason._tag : undefined,
      'MissingEffectRunner',
    )

    // A section identity whose specialized environment was never discovered stays unsupported.
    const missing = CallableFieldRealization.realizeField(
      Object.freeze({
        _tag: 'ResolvedRepresentationField',
        id: plan.id,
        instance,
        argument: Type.exactRepresentationArgument(
          Type.callableIdentityArgument(
            'absent',
            Object.freeze({
              _tag: 'Declaration',
              module: 'callable-field/unsupported',
              name: 'decode',
            }),
            [],
            Type.callableEnvironmentIdentity(
              Type.callableEnvironmentSite(
                { module: 'callable-field/unsupported', name: 'main' },
                0,
                999,
              ),
              {
                declaration: { module: 'callable-field/unsupported', name: 'main' },
                typeArguments: [],
              },
            ),
          ),
          bound,
        ),
        requiredBound: bound,
        admissibility: Object.freeze({ _tag: 'Admitted' }),
      }),
      snapshot.instances.callables,
      snapshot.instances.effects,
    )
    assert.strictEqual(missing._tag, 'Unsupported')
    assert.strictEqual(
      missing._tag === 'Unsupported' ? missing.reason._tag : undefined,
      'MissingCallableEnvironment',
    )
  }),
)

it.effect('orders realization entries deterministically across repeated analyses', () =>
  Effect.gen(function* () {
    const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
struct Pair<F: fn(i32) -> i32> { first: Parser<F> second: Parser<F> }
fn decode(value: i32) -> i32 { return value }
fn pair<F: fn(i32) -> i32>(first: Parser<F>, second: Parser<F>) -> Pair<F> {
  return Pair<F> { first: move first, second: move second }
}
pub fn main() -> i32 {
  let first = Parser { parse: decode }
  let second = Parser { parse: decode }
  let paired = pair(move first, move second)
  return 0
}`
    const first = yield* realized('callable-field/determinism', source)
    const second = yield* realized('callable-field/determinism', source)

    const keys = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
      realizationsOf(snapshot).entries.map((entry) => entry.key)

    assert.deepEqual(keys(first), keys(second))
    assert.deepEqual([...keys(first)], [...keys(first)].sort())
  }),
)

it.effect('keeps the realization identical when source spans shift', () =>
  Effect.gen(function* () {
    const shifted = `// a leading comment that moves every span in this module
${capturingSection}`
    const baseline = soleRealization(
      realizationsOf(yield* realized('callable-field/stability', capturingSection)),
    )
    const moved = soleRealization(
      realizationsOf(yield* realized('callable-field/stability', shifted)),
    )

    // Identity is the precomputed executable site, never a source path or offset, so shifting the
    // whole module leaves the target, environment, captures, and cleanup byte-identical.
    assert.deepEqual(moved.target, baseline.target)
    assert.strictEqual(
      moved.environment !== undefined &&
        baseline.environment !== undefined &&
        Type.equalsCallableEnvironmentIdentity(moved.environment, baseline.environment),
      true,
    )
    assert.deepEqual(moved.captures, baseline.captures)
    assert.deepEqual(moved.cleanup, baseline.cleanup)
    assert.deepEqual(moved.liveness, baseline.liveness)
    assert.strictEqual(moved.invocation, baseline.invocation)
  }),
)

it.effect('selects same-site capture environments by their complete specialization', () =>
  Effect.gen(function* () {
    const source = `struct Token<T> { value: T }
struct Holder<F: once fn(i32) -> i32> { step: F }
fn consume<T>(value: i32, token: Token<T>) -> i32 { return value }
fn apply<T>(token: Token<T>, value: i32) -> i32 {
  let holder = Holder { step: consume<T>(move token) }
  return holder.step(value)
}
pub fn main() -> i32 {
  return apply<i32>(Token<i32> { value: 1 }, 20) + apply<bool>(Token<bool> { value: true }, 22)
}`
    const snapshot = yield* realized('callable-field/specializations', source)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const realizations = realizationsOf(snapshot).entries.flatMap((entry) =>
      entry.support._tag === 'Supported' &&
      CallableFieldRealization.isCallableRealization(entry.support.realization) &&
      entry.support.realization.target._tag === 'Declaration' &&
      entry.support.realization.target.name === 'consume'
        ? [entry.support.realization]
        : [],
    )

    assert.strictEqual(realizations.length, 2)
    assert.deepEqual(
      realizations
        .map((realization) => Type.encode(realization.captures.at(0)?.type ?? Type.unit))
        .sort(),
      ['callable-field/specializations.Token<bool>', 'callable-field/specializations.Token<i32>'],
    )
    assert.deepEqual(
      realizations
        .map((realization) =>
          realization.environment?.owner.typeArguments.map(Type.encodeGenericArgument).join(','),
        )
        .sort(),
      ['bool', 'i32'],
    )
  }),
)

it.effect('keeps equal same-site capture shapes distinct by owner specialization', () =>
  Effect.gen(function* () {
    const source = `struct Holder<F: fn(i32) -> i32> { step: F }
fn apply<T>(marker: T, value: i32) -> i32 {
  let holder = Holder { step: i32.add(1) }
  return holder.step(value)
}
pub fn main() -> i32 { return apply<i32>(0, 20) + apply<bool>(true, 20) }`
    const snapshot = yield* realized('callable-field/equal-specializations', source)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const realizations = realizationsOf(snapshot).entries.flatMap((entry) =>
      entry.support._tag === 'Supported' &&
      CallableFieldRealization.isCallableRealization(entry.support.realization)
        ? [entry.support.realization]
        : [],
    )

    assert.strictEqual(realizations.length, 2)
    assert.deepEqual(
      realizations
        .map((realization) =>
          realization.environment?.owner.typeArguments.map(Type.encodeGenericArgument).join(','),
        )
        .sort(),
      ['bool', 'i32'],
    )
    assert.strictEqual(
      realizations.every(
        (realization) => Type.encode(realization.captures.at(0)?.type ?? Type.unit) === 'i32',
      ),
      true,
    )
  }),
)

it.effect('mints the callable environment identity in exactly one frontend module', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const path = yield* Path.Path
    const sourceRoot = yield* path.fromFileUrl(new URL('../src/', import.meta.url))
    const sources = (yield* fileSystem.readDirectory(sourceRoot)).filter((name) =>
      name.endsWith('.ts'),
    )
    const minting: Array<string> = []
    for (const name of sources) {
      const source = yield* fileSystem.readFileString(path.join(sourceRoot, name))
      if (source.includes('Type.callableEnvironmentIdentity(')) minting.push(name)
    }

    // HIR is the only actor allowed to project an executable site into a semantic environment
    // identity. Every later phase delegates to that projection rather than parsing an encoding.
    assert.deepEqual(minting, ['Hir.ts'])
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('mints the represented Effect origin in exactly one frontend module', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const path = yield* Path.Path
    const sourceRoot = yield* path.fromFileUrl(new URL('../src/', import.meta.url))
    const sources = (yield* fileSystem.readDirectory(sourceRoot)).filter((name) =>
      name.endsWith('.ts'),
    )
    const minting: Array<string> = []
    const originMint = '`effect:' + '$' + '{executableSiteKey(self)}`'
    for (const name of sources) {
      const source = yield* fileSystem.readFileString(path.join(sourceRoot, name))
      if (source.includes(originMint)) minting.push(name)
    }

    // HIR owns the semantic origin projection. Realization and lowering both delegate to it, so a
    // second minting site would be a syntax-recovery path around the shared Effect fact.
    assert.deepEqual(minting, ['Hir.ts'])
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects extracting an owned callable field with its own diagnostic', () =>
  Effect.gen(function* () {
    const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  let taken = move parser.parse
  return 0
}`
    const snapshot = yield* realized('callable-field/extraction', source)
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'OWN0013',
    )

    // The representation-bearing field names its own rejection instead of the general partial-move
    // rule, so the reason records the aggregate, the field, and the callable contract.
    assert.strictEqual(diagnostic?.reason._tag, 'RepresentationFieldExtraction')
    assert.include(diagnostic?.message ?? '', 'cleaned with the whole aggregate')
    assert.notInclude(
      Analysis.diagnostics(snapshot).map((candidate) => candidate.code),
      'OWN0002',
    )
  }),
)

it.effect('keeps an ordinary struct field on the general partial-move rejection', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
struct Holder { token: Token }
pub fn main() -> i32 {
  let holder = Holder { token: Token { value: 1 } }
  let taken = move holder.token
  return 0
}`
    const snapshot = yield* realized('callable-field/ordinary-extraction', source)
    const codes = Analysis.diagnostics(snapshot).map((candidate) => candidate.code)

    assert.include(codes, 'OWN0002')
    assert.notInclude(codes, 'OWN0013')
  }),
)

it.effect('keeps a representation-bearing nominal move-only', () =>
  Effect.gen(function* () {
    const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
fn consume<F: fn(i32) -> i32>(parser: Parser<F>) -> i32 { return 0 }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  let first = consume(move parser)
  let second = consume(parser)
  return first + second
}`
    const snapshot = yield* realized('callable-field/move-only', source)
    const codes = Analysis.diagnostics(snapshot).map((candidate) => candidate.code)

    // A whole-value move transfers the environment; the aggregate is not implicitly copyable.
    assert.include(codes, 'OWN0001')
  }),
)
