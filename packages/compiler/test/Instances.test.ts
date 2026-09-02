import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as Instances from '../src/Instances.js'
import * as Match from '../src/Match.js'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

// Pinned, not host-resolved: the goldens record a target line, so an unpinned host target makes
// these assertions pass only on Apple Silicon.
const snapshot = (text: string): Effect.Effect<Analysis.Snapshot> =>
  Analysis.ofSourceRealized('golden/program', ascii(text), 'aarch64-apple-darwin')

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const nestedSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`

it.effect('discovers reachable call chains once and terminates recursion', () =>
  Effect.gen(function* () {
    const nested = Analysis.instancesOf(yield* snapshot(nestedSource))
    const direct = Analysis.instancesOf(yield* snapshot('pub fn main() -> i32 { return main() }'))
    const mutual = Analysis.instancesOf(
      yield* snapshot(`pub fn main() -> i32 { return other() }
pub fn other() -> i32 { return main() }`),
    )
    assert.deepEqual(
      nested.instances.map((instance) => instance.key.declaration.name),
      ['main', 'identity'],
    )
    assert.deepEqual(
      direct.instances.map((instance) => instance.key.declaration.name),
      ['main'],
    )
    assert.deepEqual(
      mutual.instances.map((instance) => instance.key.declaration.name),
      ['main', 'other'],
    )
  }),
)

it.effect('discovers calls nested in Evaluate statements deterministically', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`effect fn pulse() -> () { return () }
pub effect fn main() -> () { run pulse() return () }`)

    assert.deepEqual(Analysis.diagnostics(result), [])
    assert.deepEqual(
      Analysis.instancesOf(result).instances.map((instance) => instance.key.declaration.name),
      ['main', 'pulse'],
    )
  }),
)

it.effect('discovers concrete stored and deferred-generic callable section identities', () =>
  Effect.gen(function* () {
    const stored = Analysis.instancesOf(
      yield* snapshot(`fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn main() -> i32 { let plusTwo = add(2) return plusTwo(40) }`),
    )
    const generic = Analysis.instancesOf(
      yield* snapshot(`fn select<T>(value: T, enabled: bool) -> T { return move value }
pub fn main() -> i32 { let whenEnabled = select(true) return whenEnabled(42) }`),
    )

    assert.deepEqual(
      stored.instances.map((instance) => instance.key.declaration.name),
      ['main', 'add'],
    )
    assert.strictEqual(stored.callables.length, 1)
    assert.deepEqual(stored.callables.at(0)?.captureTypes.map(Type.encode), ['i32'])
    assert.strictEqual(stored.callables.at(0)?.mode, 'Shared')
    assert.deepEqual(
      generic.instances.map((instance) => ({
        name: instance.key.declaration.name,
        arguments: instance.key.typeArguments.map(Type.encodeGenericArgument),
      })),
      [
        { name: 'main', arguments: [] },
        { name: 'select', arguments: ['i32'] },
      ],
    )
    assert.deepEqual(generic.callables.at(0)?.typeArguments.map(Type.encodeGenericArgument), [
      'i32',
    ])
    assert.strictEqual(Type.encode(generic.callables.at(0)?.type ?? 'i32'), 'fn(i32) -> i32')
  }),
)

it.effect('resolves a provided service effect captured by a callable section', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`import silk.effect { Effect }

service Sink {
  effect fn value() -> i32 ? &mut Sink
}

struct FixedSink {}

impl Sink for FixedSink {
  effect fn value(self: &Self) -> i32 ? &mut Sink { return 42 }
}

effect fn forward<A, E, ?R>(marker: i32, protected: once Effect<A ! E ? R>) -> A ! E ? R {
  return run move protected
}

pub fn main() -> i32 {
  let mut sink = FixedSink {}
  let finish = forward(Sink.value())
  return run finish(0) |> Effect.provideMut(&mut sink)
}`)

    assert.deepEqual(Analysis.diagnostics(result), [])
    assert.deepEqual(
      Analysis.instancesOf(result).instances.map((instance) => instance.key.declaration.name),
      ['main', 'Effect.provideMut', 'forward', 'forward', 'impl@0.value'],
    )
  }),
)

it.effect('keeps inferred hidden calls scoped to the function that contains their expression', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`fn make() -> once Effect<i32> { return effect { return 42 } }
fn relay() -> once Effect<i32> { return make() }
fn forward<A, E, ?R>(protected: once Effect<A ! E ? R>) -> once Effect<A ! E ? R> {
  return move protected
}
pub fn main() -> i32 { return run forward(relay()) }`)
    const discovery = Analysis.instancesOf(result)

    assert.deepEqual(Analysis.diagnostics(result), [])
    assert.deepEqual(
      discovery.calls.map((call) => ({
        owner: call.owner.declaration.name,
        target: call.target.declaration.name,
      })),
      [
        { owner: 'main', target: 'forward' },
        { owner: 'main', target: 'relay' },
        { owner: 'relay', target: 'make' },
      ],
    )
    for (const call of discovery.calls) {
      const owner = discovery.instances.find(
        (instance) => Instances.keyText(instance.key) === Instances.keyText(call.owner),
      )
      assert.notStrictEqual(owner, undefined)
      if (owner === undefined) continue
      const ownerSpan = owner.function.declaration.syntax.span
      assert.strictEqual(call.span.sourceId, ownerSpan.sourceId)
      assert.isAtLeast(call.span.start, ownerSpan.start)
      assert.isAtMost(call.span.end, ownerSpan.end)
    }
  }),
)

it.effect('keeps executable sites distinct across generic owner specializations', () =>
  Effect.gen(function* () {
    const result = Analysis.instancesOf(
      yield* snapshot(`import silk.i32 as i32
fn section<T>(value: T) -> i32 {
  let plusOne = i32.add(1)
  return plusOne(41)
}
effect fn deferred<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let left = section<i32>(1)
  let right = section<bool>(true)
  let number = run deferred<i32>(left + right)
  let flag = run deferred<bool>(true)
  if flag { return number }
  return 0
}`),
    )
    const sections = result.callables.filter(
      (callable) => callable.owner.declaration.name === 'section',
    )
    const effects = result.instances
      .filter((instance) => instance.key.declaration.name === 'deferred')
      .flatMap((instance) => (instance.resultEffect === undefined ? [] : [instance.resultEffect]))

    assert.strictEqual(sections.length, 2)
    assert.strictEqual(new Set(sections.map(Instances.callableIdentity)).size, 2)
    assert.strictEqual(effects.length, 2)
    assert.strictEqual(new Set(effects).size, 2)
    assert.strictEqual(
      [...sections.map(Instances.callableIdentity), ...effects].some((identity) =>
        /@\d/.test(identity),
      ),
      false,
    )
  }),
)

it.effect('rejects polymorphic recursion reached through a callable section', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`fn expand<T>(seed: i32, value: T) -> i32 {
  let next = expand<[T; 1]>([move value])
  return next(seed)
}
pub fn main() -> i32 { return expand<i32>(0, 1) }`)

    assert.deepEqual(
      Analysis.diagnostics(result).map((diagnostic) => diagnostic.code),
      ['SEM0053'],
    )
    assert.strictEqual(result.instances.violations.length, 1)
    assert.deepEqual(
      result.instances.violations.at(0)?.target.typeArguments.map(Type.encodeGenericArgument),
      ['Array<i32, 1>'],
    )
  }),
)

it.effect('excludes unreachable declarations and reports unavailable entries', () =>
  Effect.gen(function* () {
    const reachable = Analysis.instancesOf(
      yield* snapshot(`pub fn unused() -> i32 { return 1 }
pub fn main() -> i32 { return 42 }`),
    )
    const missing = Analysis.instancesOf(yield* snapshot('pub fn answer() -> i32 { return 42 }'))
    const parameterized = Analysis.instancesOf(
      yield* snapshot('pub fn main(value: i32) -> i32 { return value }'),
    )
    const generic = Analysis.instancesOf(yield* snapshot('pub fn main<T>() -> i32 { return 42 }'))
    const privateEntry = Analysis.instancesOf(yield* snapshot('fn main() -> () { return () }'))
    const unitEntry = Analysis.instancesOf(yield* snapshot('pub fn main() -> () { return () }'))
    assert.deepEqual(
      reachable.instances.map((instance) => instance.key.declaration.name),
      ['main'],
    )
    assert.deepEqual(missing.entry, { _tag: 'Unavailable', reason: 'MissingEntry' })
    assert.deepEqual(generic.entry, { _tag: 'Unavailable', reason: 'GenericEntry' })
    assert.deepEqual(parameterized.entry, { _tag: 'Unavailable', reason: 'ParameterizedEntry' })
    assert.deepEqual(privateEntry.entry, { _tag: 'Unavailable', reason: 'PrivateEntry' })
    assert.strictEqual(unitEntry.entry._tag, 'Resolved')
    if (unitEntry.entry._tag === 'Resolved') {
      assert.strictEqual(unitEntry.entry.kind, 'Ordinary')
      if (unitEntry.entry.kind === 'Ordinary') assert.strictEqual(unitEntry.entry.result, 'Unit')
    }
  }),
)

it.effect('resolves closed effect entries and rejects invalid effect contracts', () =>
  Effect.gen(function* () {
    const resolved = Analysis.instancesOf(
      yield* snapshot(`struct SomeError { code: i32 }
pub effect fn main() -> () ! SomeError { fail SomeError { code: 1 } }`),
    )
    assert.strictEqual(resolved.entry._tag, 'Resolved')
    if (resolved.entry._tag === 'Resolved') {
      assert.strictEqual(resolved.entry.kind, 'Effect')
      if (resolved.entry.kind === 'Effect') {
        assert.deepEqual(resolved.entry.failures, [
          {
            type: Type.nominal('golden/program', 'SomeError'),
            identity: 'golden/program.SomeError',
          },
        ])
      }
    }

    const invalidResult = Analysis.instancesOf(
      yield* snapshot('pub effect fn main() -> i32 { return 0 }'),
    )
    const requirements = Analysis.instancesOf(
      yield* snapshot('service Clock {}\npub effect fn main() -> () ? &mut Clock { return () }'),
    )
    const ordinaryFailure = Analysis.instancesOf(
      yield* snapshot(`struct SomeError { code: i32 }
pub effect fn main() -> () ! SomeError { fail SomeError { code: 1 } }`),
    )
    assert.deepEqual(invalidResult.entry, {
      _tag: 'Unavailable',
      reason: 'InvalidEffectEntryResult',
    })
    assert.deepEqual(requirements.entry, {
      _tag: 'Unavailable',
      reason: 'EffectEntryRequirements',
      requirements: [
        {
          access: 'Exclusive',
          capability: Type.nominal('golden/program', 'Clock'),
          role: 'DefaultRole',
        },
      ],
    })
    assert.strictEqual(ordinaryFailure.entry._tag, 'Resolved')
  }),
)

it.effect('discovers cleanup hooks owned by effect-entry failures', () =>
  Effect.gen(function* () {
    const discovery = Analysis.instancesOf(
      yield* snapshot(`struct SomeError { storage: RawBuffer<i32> }
impl Drop for SomeError {
  fn drop(self: &mut SomeError) -> () { return () }
}
fn makeError() -> SomeError { return makeError() }
pub effect fn main() -> () ! SomeError {
  let error = makeError()
  fail move error
}`),
    )
    assert.strictEqual(discovery.entry._tag, 'Resolved')
    assert.deepEqual(
      discovery.instances.map((instance) => instance.key.declaration.name),
      ['main', 'makeError', 'drop@impl#0'],
    )
  }),
)

it.effect('lowers discovered instances deterministically to verifier-clean MIR', () =>
  Effect.gen(function* () {
    const first = MirEncoding.encode(Analysis.loweredMir(yield* snapshot(nestedSource)))
    const second = MirEncoding.encode(Analysis.loweredMir(yield* snapshot(nestedSource)))
    assert.strictEqual(first, golden('lowered.mir.txt'))
    assert.strictEqual(first, second)
  }),
)

it.effect('lowers callable construction and application into the structured MIR DAG', () =>
  Effect.gen(function* () {
    const stored = Analysis.loweredMir(
      yield* snapshot(`fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn main() -> i32 { let plusTwo = add(2) return plusTwo(40) }`),
    )
    const direct = Analysis.loweredMir(
      yield* snapshot(`fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn main() -> i32 { return 40 |> add(2) }`),
    )
    const storedOperations = stored.functions.at(0)
    const directOperations = direct.functions.at(0)

    assert.deepEqual(MirVerification.verify(stored), [])
    assert.deepEqual(MirVerification.verify(direct), [])
    assert.deepEqual(
      storedOperations === undefined
        ? []
        : MirVerification.operations(storedOperations).map((operation) => operation._tag),
      ['Literal', 'MakeCallable', 'Move', 'Literal', 'ApplyCallable', 'Drop'],
    )
    assert.deepEqual(
      directOperations === undefined
        ? []
        : MirVerification.operations(directOperations).map((operation) => operation._tag),
      ['Literal', 'Literal', 'ApplyCallable'],
    )
    const applied =
      directOperations === undefined
        ? undefined
        : MirVerification.operations(directOperations).find(
            (operation) => operation._tag === 'ApplyCallable',
          )
    assert.strictEqual(
      applied?._tag === 'ApplyCallable' ? applied.realization : undefined,
      'DirectErasedSection',
    )
    assert.strictEqual(
      applied?._tag === 'ApplyCallable' ? applied.evaluation : undefined,
      'LeftThenCallable',
    )
    const malformed: Mir.Module = Object.freeze({
      ...stored,
      functions: Object.freeze(
        stored.functions.map((fn, ordinal) => {
          if (ordinal !== 0) return fn
          return Object.freeze({
            ...fn,
            regions: Object.freeze(
              fn.regions.map((region) => {
                if (region._tag !== 'OperationRegion') return region
                return Object.freeze({
                  ...region,
                  operations: Object.freeze(
                    region.operations.map((operation): Mir.Operation => {
                      if (operation._tag !== 'ApplyCallable') return operation
                      return Object.freeze({ ...operation, access: 'Take' })
                    }),
                  ),
                })
              }),
            ),
          })
        }),
      ),
    })
    assert.include(
      MirVerification.verify(malformed).map((violation) => violation.rule),
      'InvalidCallableOperation',
    )
  }),
)

it.effect('ends callable capture loans before drop and transfers consuming captures once', () =>
  Effect.gen(function* () {
    const borrowed = Analysis.loweredMir(
      yield* snapshot(`fn read(value: i32, values: &mut [i32]) -> i32 { return value }
pub fn main() -> i32 {
  let mut values = [1]
  let callback = read(&mut values)
  drop callback
  values[0] = 2
  return values[0]
}`),
    )
    const consumed = Analysis.loweredMir(
      yield* snapshot(`struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let callback = consume(move token)
  return callback(40)
}`),
    )
    const borrowedMain = borrowed.functions.at(0)
    const consumedMain = consumed.functions.at(0)
    const borrowedTags =
      borrowedMain === undefined
        ? []
        : MirVerification.operations(borrowedMain).map((operation) => operation._tag)
    const consumedTags =
      consumedMain === undefined
        ? []
        : MirVerification.operations(consumedMain).map((operation) => operation._tag)

    assert.deepEqual(MirVerification.verify(borrowed), [])
    assert.deepEqual(MirVerification.verify(consumed), [])
    assert.ok(borrowedTags.indexOf('BeginLoan') < borrowedTags.indexOf('MakeCallable'))
    assert.ok(borrowedTags.indexOf('MakeCallable') < borrowedTags.indexOf('EndLoan'))
    assert.ok(borrowedTags.indexOf('EndLoan') < borrowedTags.indexOf('Drop'))
    assert.include(consumedTags, 'ApplyCallable')
    assert.strictEqual(consumedTags.filter((tag) => tag === 'ApplyCallable').length, 1)
    assert.strictEqual(consumedTags.includes('Drop'), false)
  }),
)

it.effect(
  'lowers complete ungrouped run operands before grouped post-run callable transforms',
  () =>
    Effect.gen(function* () {
      const composed = Analysis.loweredMir(
        yield* snapshot(`import silk.effect { Effect }
effect fn work() -> i32 { return 41 }
pub fn main() -> i32 { return run work() |> Effect.retry(2) }`),
      )
      const grouped = Analysis.loweredMir(
        yield* snapshot(`effect fn work() -> i32 { return 41 }
pub fn main() -> i32 { return (run work()) |> Intrinsic.i32Add(1) }`),
      )
      const composedMain = composed.functions.at(0)
      const groupedMain = grouped.functions.at(0)

      assert.deepEqual(MirVerification.verify(composed), [])
      assert.deepEqual(MirVerification.verify(grouped), [])
      assert.include(
        composedMain === undefined
          ? []
          : MirVerification.operations(composedMain).map((operation) => operation._tag),
        'RunStaticEffect',
      )
      assert.strictEqual(
        groupedMain === undefined
          ? undefined
          : MirVerification.operations(groupedMain).at(-1)?._tag,
        'ApplyCallable',
      )
    }),
)

it.effect('encodes generic, consuming, and grouped-run callable MIR deterministically', () =>
  Effect.gen(function* () {
    const sources = [
      `fn select<T>(value: T, enabled: bool) -> T { return move value }
pub fn main() -> i32 { let whenEnabled = select(true) return whenEnabled(42) }`,
      `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let callback = consume(move token)
  return callback(40)
}`,
      `effect fn work() -> i32 { return 41 }
pub fn main() -> i32 { return (run work()) |> Intrinsic.i32Add(1) }`,
    ]
    for (const source of sources) {
      const first = Analysis.loweredMir(yield* snapshot(source))
      const second = Analysis.loweredMir(yield* snapshot(source))
      assert.deepEqual(MirVerification.verify(first), [])
      assert.deepEqual(MirVerification.verify(second), [])
      const encoded = MirEncoding.encode(first)
      assert.strictEqual(encoded, MirEncoding.encode(second))
      assert.include(encoded, 'apply-callable')
      if (source === sources.at(0)) assert.strictEqual(encoded, golden('generic.mir.txt'))
    }
  }),
)

const bindingSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let value = identity(42) let extra = 1 return value }`

const nestedMatchSource = `pub struct Token { kind: i32 }
pub struct Box { token: Token }
pub fn adjust(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 {
  let boxed = Box { token: Token { kind: 41 } }
  return match move boxed {
    Box { token } => match move token {
      Token { kind: answer } => adjust(answer)
    }
  }
}`

it.effect('lowers bindings and ownership violations with generated cleanup or traps', () =>
  Effect.gen(function* () {
    const bindings = Analysis.loweredMir(yield* snapshot(bindingSource))
    const bindingFunction = bindings.functions.at(0)
    assert.deepEqual(MirVerification.verify(bindings), [])
    assert.strictEqual(MirEncoding.encode(bindings), golden('bindings.mir.txt'))
    assert.deepEqual(
      bindingFunction === undefined
        ? []
        : MirVerification.operations(bindingFunction).map((operation) => operation._tag),
      ['Literal', 'Call', 'Move', 'Literal', 'Move', 'Drop', 'Drop'],
    )

    const violated = Analysis.loweredMir(
      yield* snapshot(`pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { let value = 42 return choose(move value, value) }`),
    )
    const violatedFunction = violated.functions.at(0)
    const outcome =
      violatedFunction === undefined ? undefined : MirVerification.outcomes(violatedFunction).at(0)
    assert.strictEqual(outcome?._tag, 'Trap')
    assert.strictEqual(outcome?._tag === 'Trap' ? outcome.reason : '', 'ownership violation')
  }),
)

it.effect('lowers built-ins and unavailable bodies to explicit trapping MIR', () =>
  Effect.gen(function* () {
    const builtins = Analysis.loweredMir(
      yield* snapshot(
        'pub fn main() -> i32 { return Intrinsic.i32Subtract(Intrinsic.i32Multiply(6, 7), 0) }',
      ),
    )
    const builtinFunction = builtins.functions.at(0)
    assert.deepEqual(
      builtinFunction === undefined
        ? []
        : MirVerification.operations(builtinFunction).map((operation) => {
            if (operation._tag === 'Binary') return `Binary:${operation.operator}`
            return operation._tag
          }),
      ['Literal', 'Literal', 'Binary:Multiply', 'Literal', 'Binary:Subtract'],
    )
    const unavailable = Analysis.loweredMir(
      yield* snapshot('pub fn main() -> i32 { return missing() }'),
    )
    const unavailableFunction = unavailable.functions.at(0)
    assert.strictEqual(
      unavailableFunction === undefined
        ? undefined
        : MirVerification.outcomes(unavailableFunction).at(0)?._tag,
      'Trap',
    )
  }),
)

it.effect('discovers calls and lowers nested matches as structured acyclic operations', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(nestedMatchSource)
    assert.deepEqual(
      Analysis.diagnostics(result).map((diagnostic) => [diagnostic.code, diagnostic.message]),
      [],
    )
    assert.deepEqual(
      Analysis.instancesOf(result).instances.map((instance) => instance.key.declaration.name),
      ['main', 'adjust'],
    )
    const mir = Analysis.loweredMir(result)
    assert.deepEqual(MirVerification.verify(mir), [])
    const main = mir.functions.find((fn) => fn.id.name === 'main')
    const matches =
      main === undefined
        ? []
        : MirVerification.operations(main).filter((operation) => operation._tag === 'Match')
    assert.strictEqual(matches.length, 2)
    assert.strictEqual(matches.at(0)?.arms.at(0)?.selected.operations.at(0)?._tag, 'Match')
    const outerMember = matches.at(0)?.decisions.at(0)?.member
    const innerMember = matches.at(1)?.decisions.at(0)?.member
    const outerType = outerMember === undefined ? undefined : Match.sourceType(outerMember)
    const innerType = innerMember === undefined ? undefined : Match.sourceType(innerMember)
    assert.strictEqual(
      outerType !== undefined && Type.isNominal(outerType) ? outerType.name : undefined,
      'Box',
    )
    assert.strictEqual(
      innerType !== undefined && Type.isNominal(innerType) ? innerType.name : undefined,
      'Token',
    )
    assert.strictEqual(MirEncoding.encode(mir), golden('match.mir.txt'))
    assert.strictEqual(
      MirEncoding.encode(mir),
      MirEncoding.encode(Analysis.loweredMir(yield* snapshot(nestedMatchSource))),
    )
  }),
)

it.effect('rejects hand-built match decisions before evaluation or emission', () =>
  Effect.gen(function* () {
    const mir = Analysis.loweredMir(yield* snapshot(nestedMatchSource))
    let changed = false
    const malformed: Mir.Module = {
      ...mir,
      functions: mir.functions.map((fn) => ({
        ...fn,
        regions: fn.regions.map((region) =>
          region._tag !== 'OperationRegion'
            ? region
            : {
                ...region,
                operations: region.operations.map((operation) => {
                  if (changed || operation._tag !== 'Match') return operation
                  changed = true
                  return { ...operation, decisions: [] }
                }),
              },
        ),
      })),
    }

    assert.strictEqual(changed, true)
    assert.include(
      MirVerification.verify(malformed).map((violation) => violation.rule),
      'InvalidMatchDecision',
    )
  }),
)

const branchProgram =
  'pub fn main() -> i32 { let base = 40 if base == 40 { let bonus = 2 return base + bonus } return 0 }'

it.effect('lowers branch diamonds identically across runs', () =>
  Effect.gen(function* () {
    const first = Analysis.loweredMir(yield* snapshot(branchProgram))
    const second = Analysis.loweredMir(yield* snapshot(branchProgram))
    assert.deepEqual(MirVerification.verify(first), [])
    assert.strictEqual(MirEncoding.encode(first), golden('branch-program.mir.txt'))
    assert.strictEqual(MirEncoding.encode(first), MirEncoding.encode(second))
  }),
)

it.effect('discovers one generic Effect factory and generates its hidden runner', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`effect fn delayed<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let recipe = delayed<i32>(42)
  return run recipe
}`)

    assert.deepEqual(Analysis.diagnostics(result), [])
    const instances = Analysis.instancesOf(result).instances
    assert.deepEqual(
      instances.map((instance) => ({
        name: instance.key.declaration.name,
        arguments: instance.key.typeArguments.map(Type.encodeGenericArgument),
      })),
      [
        { name: 'main', arguments: [] },
        { name: 'delayed', arguments: ['i32'] },
      ],
    )
    const lowered = Analysis.loweredMir(result)
    const delayed = lowered.functions.find((fn) => fn.id.name === 'delayed')
    const runner = lowered.functions.find((fn) => fn.id.name.includes('delayed$effect$'))
    assert.strictEqual(delayed?.result._tag, 'EffectValue')
    assert.strictEqual(runner?.result._tag, 'EffectOutcome')
    const outcome = Analysis.evaluate(result)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      Json.stringify(
        outcome,
        (_, value) => (typeof value === 'bigint' ? value.toString() : value),
        2,
      ),
    )
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)
  }),
)

it.effect('deduplicates definitionally different open rows after concrete specialization', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`struct First {}
struct Second {}
effect fn source() -> i32 ! First | Second { return 1 }
effect fn forward<A, E>(self: once Effect<A ! E>) -> A ! E { return run self }
pub fn main() -> i32 {
  let direct = forward<i32, First | Second>(source())
  let permuted = forward<i32, Second | First>(source())
  return 0
}`)

    assert.deepEqual(Analysis.diagnostics(result), [])
    const discovered = Analysis.instancesOf(result)
    const forwards = discovered.instances.filter(
      (instance) => instance.key.declaration.name === 'forward',
    )
    assert.strictEqual(forwards.length, 1)
    for (const instance of discovered.instances) {
      const failures = instance.specialization.failureRow
      const requirements = instance.specialization.requirementRow
      if (failures !== undefined)
        assert.strictEqual(Type.isRuntimeConcreteGenericArgument(Type.failureType(failures)), true)
      if (requirements !== undefined)
        assert.strictEqual(
          Type.isRuntimeConcreteGenericArgument({
            _tag: 'RequirementRowArgument',
            row: requirements,
          }),
          true,
        )
    }
  }),
)
