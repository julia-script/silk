import * as ArtifactComposition from '../src/ArtifactComposition.js'
import * as ArtifactPlan from '../src/ArtifactPlan.js'
import * as CompilationProfile from '../src/CompilationProfile.js'
import * as ConfigurationOrigin from '../src/ConfigurationOrigin.js'
import * as NativeRequirement from '../src/NativeRequirement.js'
import * as NativeRequirementBinding from '../src/NativeRequirementBinding.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as Layout from '../src/Layout.js'
import * as Lifetime from '../src/Lifetime.js'
import * as Hir from '../src/Hir.js'
import * as Instances from '../src/Instances.js'
import * as LlvmBackend from '../src/LlvmBackend.js'
import * as Match from '../src/Match.js'
import * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Realization from '../src/Realization.js'
import * as Type from '../src/Type.js'
import { unreachable } from './support/raise.js'

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

it.effect('roots native libraries at C exports without selecting main', () =>
  Effect.gen(function* () {
    const frontend = yield* Analysis.ofSource(
      'library/Math',
      ascii(`fn helper(value: i32) -> i32 { return value + 1 }
export "C" fn increment(value: i32) -> i32 { return helper(value) }
pub fn main() -> i32 { return 0 }`),
    )
    const prepared = yield* Realization.prepare(frontend, 'aarch64-apple-darwin', {
      artifactKind: 'NativeSharedLibrary',
    })
    assert.strictEqual(prepared._tag, 'Prepared')
    if (prepared._tag !== 'Prepared') return
    assert.strictEqual(prepared.program.entry._tag, 'NoInvocation')
    assert.deepStrictEqual(
      {
        functions: prepared.program.functions.map((fn) => fn.id.name),
        exports: prepared.program.foreignExports.map((export_) => export_.symbol),
      },
      { functions: ['increment', 'helper'], exports: ['increment'] },
    )
    assert.deepStrictEqual(
      prepared.program.foreignExports.map((export_) => export_.symbol),
      ['increment'],
    )
    assert.deepStrictEqual(MirVerification.verify(prepared.program), [])
    const artifact = yield* LlvmBackend.LlvmBackend.emit(prepared.program, { mode: 'release' })
    assert.match(artifact.ir, /define hidden i32 @silk_/)
    assert.match(artifact.ir, /define i32 @increment\(/)
    assert.notMatch(artifact.ir, /define hidden i32 @increment\(/)
  }),
)

it.effect('emits an empty native library without retaining unrelated public functions', () =>
  Effect.gen(function* () {
    const frontend = yield* Analysis.ofSource(
      'library/Empty',
      ascii('pub fn helper() -> i32 { return 42 }'),
    )
    const prepared = yield* Realization.prepare(frontend, 'aarch64-apple-darwin', {
      artifactKind: 'NativeStaticLibrary',
    })
    assert.strictEqual(prepared._tag, 'Prepared')
    if (prepared._tag === 'Prepared') assert.deepEqual(prepared.program.functions, [])
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
    assert.strictEqual(
      Type.encode(generic.callables.at(0)?.type ?? 'i32'),
      "fn<'static>(i32) -> i32",
    )
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
    const result =
      yield* snapshot(`fn make() -> once Effect<'static; i32> { return effect { return 42 } }
fn relay() -> once Effect<'static; i32> { return make() }
fn forward<'env, A, E, ?R>(protected: once Effect<'env; A ! E ? R>) -> once Effect<'env; A ! E ? R> {
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
    const analyzed = yield* snapshot(`fn add(left: i32, right: i32) -> i32 { return left + right }
fn section<T>(value: T) -> i32 {
  let plusOne = add(1)
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
}`)
    assert.deepEqual(Analysis.diagnostics(analyzed), [])
    const result = Analysis.instancesOf(analyzed)
    assert.strictEqual(result.counters.residualBodies.checked, 0)
    assert.strictEqual(result.counters.residualOwnership.checked, 0)
    assert.strictEqual(result.counters.residualOwnership.cacheReused, 2)
    assert.isAbove(result.counters.residualOwnership.sourceReused, 0)
    assert.strictEqual(
      Object.values(result.counters.residualOwnership.executedWork).reduce(
        (total, count) => total + count,
        0,
      ),
      0,
    )
    assert.deepEqual(
      analyzed.report.find((phase) => phase.phase === 'instance-discovery')?.counters,
      result.counters,
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

it.effect('retains deterministic hidden anonymous targets and exact capture ordinals', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let copied = 1
  let owned = Token { value: 40 }
  let first = fn(value: i32) -> i32 { return value }
  let second = fn(value: i32) -> i32 { return value }
  let combined = fn(value: i32) -> i32 { return consume(value + copied, move owned) }
  return combined(1)
}`
    const first = yield* snapshot(source)
    const second = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(first), [])
    assert.deepEqual(Analysis.diagnostics(second), [])

    const firstHir = Analysis.rootAnalysis(first).hir
    const secondHir = Analysis.rootAnalysis(second).hir
    assert.strictEqual(Hir.encode(firstHir), Hir.encode(secondHir))
    const hiddenNames = (hir: Hir.Module) =>
      hir.functions.flatMap((fn) =>
        fn.declaration.canonical._tag === 'Canonical' &&
        fn.declaration.canonical.id.name.includes('$callable$')
          ? [fn.declaration.canonical.id.name]
          : [],
      )
    assert.deepEqual(hiddenNames(firstHir), [
      'main$callable$0',
      'main$callable$1',
      'main$callable$2',
    ])
    assert.deepEqual(hiddenNames(firstHir), hiddenNames(secondHir))

    const main = firstHir.functions.find(
      (fn) =>
        fn.declaration.canonical._tag === 'Canonical' &&
        fn.declaration.canonical.id.name === 'main',
    )
    const sections =
      main === undefined
        ? []
        : main.statements
            .flatMap(Hir.statementExpressions)
            .flatMap(Hir.expressionTree)
            .filter(
              (
                expression,
              ): expression is Extract<Hir.Expression, { readonly _tag: 'CallableSection' }> =>
                expression._tag === 'CallableSection',
            )
    assert.deepEqual(
      sections.map((section) => ({
        site: section.site.ordinal,
        target:
          section.target._tag === 'DeclarationCallableTarget'
            ? section.target.declaration.name
            : section.target.operation,
        remaining: section.remainingParameters,
        mode: section.mode,
        captures: section.captures.map((capture) => ({
          ordinal: capture.ordinal,
          parameterOrdinal: capture.parameterOrdinal,
          access: capture.access,
          expression: capture.value._tag,
        })),
      })),
      [
        {
          site: 0,
          target: 'main$callable$0',
          remaining: [0],
          mode: 'Shared',
          captures: [],
        },
        {
          site: 1,
          target: 'main$callable$1',
          remaining: [0],
          mode: 'Shared',
          captures: [],
        },
        {
          site: 2,
          target: 'main$callable$2',
          remaining: [0],
          mode: 'Take',
          captures: [
            { ordinal: 0, parameterOrdinal: 1, access: 'Copy', expression: 'BindingReference' },
            { ordinal: 1, parameterOrdinal: 2, access: 'Take', expression: 'Move' },
          ],
        },
      ],
    )

    const hidden = firstHir.functions.find(
      (fn) =>
        fn.declaration.canonical._tag === 'Canonical' &&
        fn.declaration.canonical.id.name === 'main$callable$2',
    )
    assert.deepEqual(
      hidden?.declaration.parameters.map((parameter) => ({
        ordinal: parameter.id.ordinal,
        name: parameter.name._tag === 'Present' ? parameter.name.spelling : '_',
      })),
      [
        { ordinal: 0, name: 'value' },
        { ordinal: 1, name: 'copied' },
        { ordinal: 2, name: 'owned' },
      ],
    )
    assert.strictEqual(hidden?.contract._tag, 'Contract')
    if (hidden?.contract._tag !== 'Contract') return
    assert.deepEqual(hidden.contract.parameters.map(Type.encode), [
      'i32',
      'i32',
      'golden/program.Token',
    ])
    assert.strictEqual(Type.encode(hidden.contract.result), 'i32')

    const mir = Analysis.loweredMir(first)
    assert.deepEqual(MirVerification.verify(mir), [])
    const mainMir = mir.functions.find((fn) => fn.id.name === 'main')
    assert.isDefined(mainMir)
    if (mainMir === undefined) return
    const operations = MirVerification.operations(mainMir)
    const constructions = operations.filter(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'MakeCallable' }> =>
        operation._tag === 'MakeCallable',
    )
    assert.deepEqual(
      constructions.map((operation) => ({
        target:
          operation.target._tag === 'DeclarationCallableTarget'
            ? operation.target.declaration.name
            : operation.target.operation,
        captures: operation.captures.map(({ ordinal, parameterOrdinal, access }) => ({
          ordinal,
          parameterOrdinal,
          access,
        })),
        fields:
          operation.type.environment?.fields.map(({ ordinal, parameterOrdinal, access }) => ({
            ordinal,
            parameterOrdinal,
            access,
          })) ?? [],
      })),
      [
        { target: 'main$callable$0', captures: [], fields: [] },
        { target: 'main$callable$1', captures: [], fields: [] },
        {
          target: 'main$callable$2',
          captures: [
            { ordinal: 0, parameterOrdinal: 1, access: 'Copy' },
            { ordinal: 1, parameterOrdinal: 2, access: 'Take' },
          ],
          fields: [
            { ordinal: 0, parameterOrdinal: 1, access: 'Copy' },
            { ordinal: 1, parameterOrdinal: 2, access: 'Take' },
          ],
        },
      ],
    )
    const applied = operations.find((operation) => operation._tag === 'ApplyCallable')
    assert.deepEqual(
      applied?._tag === 'ApplyCallable'
        ? { access: applied.access, realization: applied.realization, captures: applied.captures }
        : undefined,
      { access: 'Take', realization: 'Environment', captures: [] },
    )
    const callableDrops = operations.flatMap((operation) => {
      if (operation._tag !== 'Drop') return []
      const type = mainMir.localTypes.at(operation.local.ordinal)
      return type?._tag === 'CallableValue' && type.target._tag === 'DeclarationCallableTarget'
        ? [type.target.declaration.name]
        : []
    })
    assert.deepEqual(callableDrops, ['main$callable$1', 'main$callable$0'])
    assert.notInclude(callableDrops, 'main$callable$2')
  }),
)

it.effect('ends callable capture loans before drop and transfers consuming captures once', () =>
  Effect.gen(function* () {
    const borrowed = Analysis.loweredMir(
      yield* snapshot(`fn read(value: i32, values: &mut [i32]) -> i32 { return value }
pub fn main() -> i32 {
  let mut values: [i32; 1] = [1]
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
      const composedSnapshot = yield* snapshot(`effect fn work() -> i32 { return 41 }
effect<'env> fn offset<'env>(self: Effect<'env; i32>, amount: i32) -> i32 {
  let value = run self
  return value + amount
}
pub fn main() -> i32 { return run work() |> offset(1) }`)
      assert.deepEqual(Analysis.diagnostics(composedSnapshot), [])
      const composed = Analysis.loweredMir(composedSnapshot)
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

it.effect('lowers binding cleanup and rejects ownership violations during analysis', () =>
  Effect.gen(function* () {
    const bindings = Analysis.loweredMir(yield* snapshot(bindingSource))
    const bindingFunction = bindings.functions.at(0)
    assert.deepEqual(MirVerification.verify(bindings), [])
    assert.strictEqual(MirEncoding.encode(bindings), golden('bindings.mir.txt'))
    assert.deepEqual(
      bindingFunction === undefined
        ? []
        : MirVerification.operations(bindingFunction).map((operation) => operation._tag),
      ['Literal', 'Call', 'Move', 'Literal', 'Move', 'Move', 'Drop', 'Drop'],
    )

    const source = `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { let value = 42 return choose(move value, value) }`
    const violated = yield* Analysis.ofSource('ownership/violated-call', ascii(source))
    assert.deepEqual(
      Analysis.diagnostics(violated).map((diagnostic) => ({
        code: diagnostic.code,
        span: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      })),
      [{ code: 'OWN0001', span: 'value' }],
    )
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
    const selected = matches.at(0)?.arms.at(0)?.selected.execution
    assert.strictEqual(
      selected === undefined ? undefined : Mir.executionOperations(selected).at(0)?._tag,
      'Match',
    )
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

it.effect('rejects hand-built match decisions before LLVM emission', () =>
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

it.effect('discovers an uncalled native export after main and none for a Wasm target', () =>
  Effect.gen(function* () {
    const source = `export "C" fn silk_test_double_v1(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return 0 }`
    const native = Analysis.instancesOf(yield* snapshot(source))
    assert.deepEqual(
      native.instances.map((instance) => instance.key.declaration.name),
      ['main', 'silk_test_double_v1'],
    )
    assert.deepEqual(
      native.foreignExports.map((record) => [record.symbol, Instances.keyText(record.key)]),
      [
        [
          'silk_test_double_v1',
          Instances.keyText(
            native.instances.at(1)?.key ?? unreachable('expected the export instance'),
          ),
        ],
      ],
    )
    const wasm = Analysis.instancesOf(
      yield* Analysis.ofSourceRealized('golden/program', ascii(source), 'wasm32-unknown-unknown'),
    )
    assert.deepEqual(
      wasm.instances.map((instance) => instance.key.declaration.name),
      ['main'],
    )
  }),
)

it.effect('keys pointer instances by pointee and mutability without reaching the pointee', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`import silk.vector { Vector }
fn probe<T>(flag: bool) -> bool { return flag }
fn hold(value: *mut Vector<i32>) -> i32 { return 0 }
pub fn main() -> i32 {
  let first = probe<*const i32>(true)
  let second = probe<*mut i32>(true)
  let third = probe<*mut Vector<i32>>(true)
  return 0
}`)

    assert.deepEqual(Analysis.diagnostics(result), [])
    const discovery = Analysis.instancesOf(result)
    assert.deepEqual(
      discovery.instances.map((instance) => ({
        name: instance.key.declaration.name,
        arguments: instance.key.typeArguments.map(Type.encodeGenericArgument),
      })),
      [
        { name: 'main', arguments: [] },
        { name: 'probe', arguments: ['*const i32'] },
        { name: 'probe', arguments: ['*mut i32'] },
        { name: 'probe', arguments: ['*mut silk/vector.Vector<i32>'] },
      ],
    )
  }),
)

it.effect('shares runtime instances and layouts across distinct proven lifetime arguments', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(`struct View<'a> { value: &'a i32 }
struct Exclusive<'a> { value: &'a mut i32 }
impl<'a> Drop for Exclusive<'a> { fn drop(self: &mut Exclusive<'a>) -> () { return () } }
fn guard<'a>(value: &'a mut i32) -> Exclusive<'a> { return Exclusive { value: move value } }
fn wrap<'a>(value: &'a i32) -> View<'a> { return View<'a> { value: value } }
fn reborrow<'a>(value: &'a i32) -> &'a i32 { return &value.* }
effect fn read<'a>(value: &'a i32) -> i32 { return value.* }
pub fn main() -> i32 {
  let mut left = 20
  let mut right = 22
  let first = wrap(&left)
  let second = wrap(&right)
  let firstValue = run read(reborrow(first.value))
  let secondValue = run read(reborrow(second.value))
  let exclusiveLeft = guard(&mut left)
  let exclusiveRight = guard(&mut right)
  drop exclusiveLeft drop exclusiveRight
  return firstValue + secondValue
}`)
    assert.deepEqual(Analysis.diagnostics(result), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(result)), [])
    const discovery = Analysis.instancesOf(result)
    assert.strictEqual(
      discovery.instances.filter((instance) => instance.key.declaration.name === 'read').length,
      1,
    )
    assert.strictEqual(
      discovery.instances.filter((instance) => instance.key.declaration.name === 'wrap').length,
      1,
    )
    assert.strictEqual(
      discovery.instances.filter((instance) => instance.key.declaration.name === 'reborrow').length,
      1,
    )
    assert.strictEqual(
      discovery.instances.filter((instance) => instance.key.declaration.name === 'guard').length,
      1,
    )
    const plan = Analysis.layoutOf(result)
    assert.strictEqual(plan._tag, 'Available')
    if (plan._tag !== 'Available') return
    const first = Type.reference(
      'Shared',
      'i32',
      Lifetime.bound({ module: 'test', name: 'one' }, 0, "'first"),
    )
    const second = Type.reference(
      'Shared',
      'i32',
      Lifetime.bound({ module: 'test', name: 'two' }, 0, "'second"),
    )
    assert.notStrictEqual(Type.key(first), Type.key(second))
    assert.isDefined(Layout.entry(plan.value, first))
    assert.strictEqual(Layout.entry(plan.value, first), Layout.entry(plan.value, second))
    const firstView = Type.nominal('golden/program', 'View', [first.lifetime])
    const secondView = Type.nominal('golden/program', 'View', [second.lifetime])
    assert.include(
      plan.value.entries.map((entry) => Type.runtimeKey(entry.type)),
      Type.runtimeKey(firstView),
    )
    assert.strictEqual(Layout.entry(plan.value, firstView), Layout.entry(plan.value, secondView))
    const firstExclusive = Type.nominal('golden/program', 'Exclusive', [first.lifetime])
    const secondExclusive = Type.nominal('golden/program', 'Exclusive', [second.lifetime])
    assert.isDefined(Layout.entry(plan.value, firstExclusive))
    assert.strictEqual(
      Layout.entry(plan.value, firstExclusive),
      Layout.entry(plan.value, secondExclusive),
    )
    assert.strictEqual(
      Layout.callingShape(plan.value, first),
      Layout.callingShape(plan.value, second),
    )
    const fn =
      Analysis.loweredMir(result).functions.find((fn) => fn.id.name === 'read') ??
      unreachable('missing read instance')
    const key = fn.instance
    const alternate = {
      ...fn,
      instance: {
        ...key,
        typeArguments: key.typeArguments.map((argument) =>
          Lifetime.isLifetime(argument) ? Lifetime.staticLifetime : argument,
        ),
      },
    }
    assert.strictEqual(Backend.symbolFor(fn, undefined), Backend.symbolFor(alternate, undefined))
  }),
)

it.effect('activates native requirements by declaration, selected module and artifact scope', () =>
  Effect.gen(function* () {
    const source = SourceFile.make(
      'requirements',
      ascii(`
module with Intrinsic.native(kind: "startup-object", name: "module")
unsafe extern "C" fn used() -> i32 as "used"
  with Intrinsic.foreign(memory: "none")
  with Intrinsic.native(kind: "library", name: "needed", linkage: "dynamic")
unsafe extern "C" fn unused() -> i32 as "unused"
  with Intrinsic.native(kind: "library", name: "unused")
unsafe extern "C" static data: i32 with Intrinsic.native(kind: "library", name: "data")
unsafe extern "C" static unusedData: i32 with Intrinsic.native(kind: "library", name: "unusedData")
static if false {
  module with Intrinsic.native(kind: "library", name: "inactive")
  import missing
}
export "C" fn value() -> i32 { unsafe { return used() + data } }
`),
    )
    const analysis = yield* Analysis.makeRealized({
      root: source,
      configuration: {
        profile: {
          target: 'x86_64-unknown-linux-gnu',
          artifact: 'object',
          runtime: { kind: 'none' },
        },
        composition: {
          runtimes: [],
          defaults: [],
          retention: [],
          requirements: [{ kind: 'prebuilt-object', name: 'artifact' }],
        },
      },
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.deepEqual(Analysis.diagnostics(analysis), [])
    const plan = analysis.artifactPlan ?? unreachable('expected artifact plan')
    assert.deepEqual(plan.requirements.map((entry) => entry.name).sort(), [
      'artifact',
      'data',
      'module',
      'needed',
    ])
    assert.deepEqual(
      plan.requirements
        .flatMap((entry) => entry.contributions.map((fact) => fact.scope.kind))
        .sort(),
      ['artifact', 'declaration', 'declaration', 'module'],
    )
    assert.strictEqual(plan.sources.length, 1)
  }),
)

it.effect(
  'composes a selected source runtime with the application and private retention roots',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.makeRealized({
        root: SourceFile.make(
          'application',
          ascii('pub fn answer() -> i32 { return 42 } pub fn unused() -> i32 { return 9 }'),
        ),
        configuration: {
          profile: {
            target: 'aarch64-apple-darwin',
            artifact: 'static-archive',
            runtime: { kind: 'named', name: 'custom' },
            entry: { kind: 'none' },
          },
          composition: {
            defaults: [],
            runtimes: [
              { name: 'custom', module: 'runtime' },
              { name: 'unselected', module: 'missing' },
            ],
            retention: [{ module: 'capability', declaration: 'keep' }],
            requirements: [],
          },
        },
      }).pipe(
        Effect.provide(
          SourceResolver.memory(
            new Map([
              [
                'runtime',
                ascii(
                  'import Intrinsic.application as app\nexport "C" fn proxy() -> i32 as "proxy" { return app.answer() }',
                ),
              ],
              [
                'capability',
                ascii('fn keep() -> i32 { return 7 } pub fn unused() -> i32 { return 0 }'),
              ],
            ]),
          ),
        ),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(
        Analysis.modules(snapshot).map((module) => module.name),
        ['application', 'capability', 'runtime'],
      )
      const discovery = Analysis.instancesOf(snapshot)
      assert.strictEqual(discovery.entry._tag, 'None')
      assert.deepEqual(
        discovery.instances.map((instance) => instance.key.declaration.name).sort(),
        ['answer', 'keep', 'proxy'],
      )
      const program = Analysis.loweredMir(snapshot)
      assert.deepEqual(MirVerification.verify(program), [])
      assert.deepEqual(
        program.retainedRoots?.map((root) => root.declaration.name),
        ['keep'],
      )
      const artifact = yield* LlvmBackend.LlvmBackend.emit(program, { mode: 'release' })
      assert.match(artifact.ir, /@llvm.used = appending global \[1 x ptr\]/)
      assert.match(artifact.ir, /section "llvm.metadata"/)
      assert.deepEqual(
        artifact.foreignExports.map((entry) => entry.symbol),
        ['proxy'],
      )
      assert.notMatch(artifact.ir, /silk_main/)
    }),
)

it.effect('merges logical diamonds and reports all hard requirement origins', () =>
  Effect.gen(function* () {
    const profile = yield* CompilationProfile.decode({
      target: 'x86_64-unknown-linux-gnu',
      deployment: '12',
    })
    const a = yield* NativeRequirement.decode(
      {
        kind: 'library',
        name: 'dependency',
        minimumDeployment: '10',
        alternatives: ['first', 'second'],
      },
      { kind: 'module', module: 'a' },
      ConfigurationOrigin.literal('a'),
    )
    const b = yield* NativeRequirement.decode(
      {
        kind: 'library',
        name: 'dependency',
        maximumDeployment: '14',
        alternatives: ['second', 'third'],
      },
      { kind: 'module', module: 'b' },
      ConfigurationOrigin.literal('b'),
    )
    const merged = yield* NativeRequirement.merge([a, b, a], profile)
    assert.strictEqual(merged.length, 1)
    const requirement = merged[0] ?? unreachable('expected merged requirement')
    assert.deepEqual(requirement.alternatives, ['second'])
    assert.strictEqual(requirement.minimumDeployment, '10.0.0')
    assert.strictEqual(requirement.maximumDeployment, '14.0.0')
    assert.strictEqual(requirement.contributions.length, 2)
    const conflict = yield* Effect.flip(
      NativeRequirement.merge(
        [
          { ...a, linkage: 'static' },
          { ...b, linkage: 'dynamic' },
        ],
        profile,
      ),
    )
    assert.strictEqual(conflict.code, 'ConflictingBindings')
    assert.deepEqual(conflict.origins.map((origin) => origin.source).sort(), ['a', 'b'])
    const interval = yield* Effect.flip(
      NativeRequirement.merge([{ ...a, minimumDeployment: '15.0.0' }, b], profile),
    )
    assert.deepEqual(interval.origins.map((origin) => origin.source).sort(), ['a', 'b'])
    const rejected = yield* Effect.flip(
      NativeRequirementBinding.resolve(
        merged,
        [
          {
            kind: 'library',
            name: 'dependency',
            alternative: 'first',
            inputs: [{ _tag: 'Library', name: 'dependency', mode: 'Dynamic' }],
            origin: ConfigurationOrigin.literal('build'),
          },
        ],
        'loadable-module',
      ),
    )
    assert.deepEqual(rejected.origins.map((origin) => origin.source).sort(), ['a', 'b', 'build'])
    const unresolved = yield* NativeRequirementBinding.resolve(merged, [], 'object')
    assert.deepEqual(unresolved.inputs, [])
    const missing = yield* Effect.flip(
      NativeRequirementBinding.resolve(merged, [], 'loadable-module'),
    )
    assert.deepEqual(missing.origins.map((origin) => origin.source).sort(), ['a', 'b'])
  }),
)

it.effect('distinguishes runtime selection rules and loader policies without physical paths', () =>
  Effect.gen(function* () {
    const composition = yield* ArtifactComposition.decode({
      runtimes: [{ name: 'chosen', module: 'runtime' }],
      defaults: ['chosen'],
    })
    const base = yield* CompilationProfile.decode({
      target: 'x86_64-unknown-linux-gnu',
      artifact: 'object',
    })
    const defaultRoot = yield* ArtifactComposition.resolve(composition, 'app', base)
    const namedRoot = yield* ArtifactComposition.resolve(composition, 'app', {
      ...base,
      runtime: { kind: 'named', name: 'chosen' },
    })
    const noRoot = yield* ArtifactComposition.resolve(composition, 'app', {
      ...base,
      runtime: { kind: 'none' },
    })
    assert.strictEqual(defaultRoot.runtime?.module, namedRoot.runtime?.module)
    assert.strictEqual(new Set([defaultRoot.identity, namedRoot.identity, noRoot.identity]).size, 3)
    const identities = new Set<string>()
    for (const entry of [
      { kind: 'default' },
      { kind: 'none' },
      { kind: 'named', name: 'first' },
      { kind: 'named', name: 'second' },
    ] satisfies ReadonlyArray<CompilationProfile.Selection>) {
      identities.add(
        (yield* ArtifactComposition.resolve(composition, 'app', { ...base, entry })).identity,
      )
    }
    assert.strictEqual(identities.size, 4)
    const ambiguous = yield* ArtifactComposition.decode({
      runtimes: [
        { name: 'first', module: 'one' },
        { name: 'second', module: 'two' },
      ],
      defaults: ['first', 'second'],
    })
    assert.strictEqual(
      (yield* Effect.flip(ArtifactComposition.resolve(ambiguous, 'app', base))).code,
      'ConflictingBindings',
    )
    const conflicting = yield* ArtifactComposition.decode({ entry: { kind: 'none' } })
    assert.strictEqual(
      (yield* Effect.flip(
        ArtifactComposition.resolve(conflicting, 'app', {
          ...base,
          entry: { kind: 'named', name: 'entry' },
        }),
      )).code,
      'ConflictingBindings',
    )
  }),
)

it.effect(
  'publishes stable artifact identity with stage, compiler and ordered supply distinctions',
  () =>
    Effect.gen(function* () {
      const analysis = yield* Analysis.makeRealized({
        root: SourceFile.make('identity', ascii('export "C" fn value() -> i32 { return 42 }')),
        configuration: {
          profile: {
            target: 'x86_64-unknown-linux-gnu',
            artifact: 'object',
            runtime: { kind: 'none' },
          },
        },
      }).pipe(Effect.provide(SourceResolver.empty))
      const plan = analysis.artifactPlan ?? unreachable('expected logical plan')
      const remade = yield* ArtifactPlan.make(
        analysis,
        plan.profile,
        plan.composition,
        Analysis.loweredMir(analysis),
        plan.stage,
        plan.compiler,
      )
      assert.deepEqual(remade, plan)
      const object = yield* ArtifactPlan.make(
        analysis,
        plan.profile,
        plan.composition,
        Analysis.loweredMir(analysis),
        'object',
        plan.compiler,
      )
      const compiler = yield* ArtifactPlan.make(
        analysis,
        plan.profile,
        plan.composition,
        Analysis.loweredMir(analysis),
        plan.stage,
        'another compiler',
      )
      assert.strictEqual(new Set([plan.identity, object.identity, compiler.identity]).size, 3)
      const a = { _tag: 'Object', path: '/one.o' } as const
      const b = { _tag: 'Object', path: '/two.o' } as const
      assert.notStrictEqual(
        ArtifactPlan.physicalIdentity(plan, [a, b]),
        ArtifactPlan.physicalIdentity(plan, [b, a]),
      )
      assert.strictEqual(plan.identity, remade.identity)
    }),
)

it.effect(
  'rejects invalid retention roles and unsupported final loader entries before emission',
  () =>
    Effect.gen(function* () {
      for (const text of [
        'pub static fn selected() -> i32 { return 1 }',
        'pub fn selected<T>(value: T) -> T { return value }',
      ]) {
        const analysis = yield* Analysis.makeRealized({
          root: SourceFile.make('roles', ascii(text)),
          configuration: {
            profile: {
              target: 'x86_64-unknown-linux-gnu',
              artifact: 'object',
              runtime: { kind: 'none' },
            },
            composition: {
              runtimes: [],
              defaults: [],
              requirements: [],
              retention: [{ module: 'roles', declaration: 'selected' }],
            },
          },
        }).pipe(Effect.provide(SourceResolver.empty))
        const diagnostics = Analysis.diagnostics(analysis)
        assert.include(
          diagnostics.map((entry) => entry.code),
          'SEM0214',
        )
        assert.isTrue(diagnostics.every((entry) => entry.span.sourceId === 'roles'))
      }
      const analysis = yield* Analysis.makeRealized({
        root: SourceFile.make('loader', ascii('export "C" fn value() -> i32 { return 42 }')),
        configuration: {
          profile: {
            target: 'x86_64-unknown-linux-gnu',
            artifact: 'object',
            runtime: { kind: 'none' },
            entry: { kind: 'named', name: 'start' },
          },
        },
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.deepEqual(Analysis.diagnostics(analysis), [])
      const plan = analysis.artifactPlan ?? unreachable('expected intermediate plan')
      const rejected = yield* Effect.flip(
        ArtifactPlan.make(
          analysis,
          plan.profile,
          plan.composition,
          Analysis.loweredMir(analysis),
          'final',
          plan.compiler,
        ),
      )
      assert.strictEqual(rejected.code, 'UnsupportedCombination')
    }),
)
