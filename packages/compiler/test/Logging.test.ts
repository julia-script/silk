import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Instances from '../src/Instances.js'
import * as Mir from '../src/Mir.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as StandardStreams from '../src/StandardStreams.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()
const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const memorySource = String.raw`import silk.logging {
  attempts,
  length,
  levelAt,
  messageByteAt,
  messageLengthAt
}

effect fn program() -> i32 ! LogError {
  let mut logger = InMemoryLogger.memory()
  let first = run Effect.provideMut(Effect.log("first"), &mut logger)
  let second = run Effect.provideMut(Effect.logAt(LogLevel.warning(), "second\nline"), &mut logger)
  if length(&logger) != 2 { return 1 }
  if LogLevel.levelCode(levelAt(&logger, 0)) != 2 { return 2 }
  if LogLevel.levelCode(levelAt(&logger, 1)) != 3 { return 3 }
  if messageLengthAt(&logger, 0) != 5 { return 4 }
  if messageByteAt(&logger, 0, 0) != 102 { return 5 }
  if messageLengthAt(&logger, 1) != 11 { return 6 }
  if messageByteAt(&logger, 1, 6) != 10 { return 7 }
  return 42
}

effect fn recover(error: LogError) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catchAll(program(), recover)
}`

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('logging/main', encoder.encode(source), target)

it.effect('dispatches complete ordered messages through an ordinary source Logger', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(memorySource)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)

    const hir = Analysis.hirOf(self, 'silk/effects')
    assert.include(hir === undefined ? '' : Hir.encode(hir), 'service-call silk/logging.Logger.log')
    const lowered = Analysis.loweredMir(self)
    assert.strictEqual(Mir.encode(lowered), golden('logging.mir.txt'))
    assert.isFalse(
      lowered.functions.flatMap(Mir.operations).some((operation) => operation._tag.includes('Log')),
    )
  }),
)

it.effect('composes direct, piped, stored, tapped, flat-mapped, caught, and provided logs', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.logging { length }

effect fn logAndKeep(value: i32) -> i32 ! LogError ? &mut Logger {
  let logged = run Effect.logAt(LogLevel.debug(), "composed")
  return value
}

effect fn storedLog() -> i32 ! LogError ? &mut Logger {
  let logged = run Effect.log("stored")
  return 1
}

effect fn value(number: i32) -> i32 { return number }

effect fn program() -> i32 ! LogError {
  let mut logger = InMemoryLogger.memory()
  let direct = run Effect.provideMut(Effect.log("direct"), &mut logger)
  let piped = run (Effect.log("piped") |> Effect.provideMut(&mut logger))
  let stored = storedLog()
  let storedValue = run Effect.provideMut(stored, &mut logger)
  let tapped = run (value(20) |> Effect.tap(logAndKeep) |> Effect.provideMut(&mut logger))
  let flatMapped = run (value(21) |> Effect.flatMap(logAndKeep) |> Effect.provideMut(&mut logger))
  if length(&logger) != 5 { return 0 }
  return storedValue + tapped + flatMapped
}

effect fn recover(error: LogError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('accepts a user-authored Logger implementation without compiler registration', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.string { byteLength }
struct CountingLogger { calls: i32 bytes: usize }
effect fn record(
  self: &mut CountingLogger,
  level: LogLevel,
  message: string
) -> () {
  self.calls = self.calls + 1
  self.bytes = self.bytes + byteLength(message)
  return ()
}
impl Logger for CountingLogger { log: CountingLogger.record }
pub effect fn main() -> () ! LogError {
  let mut logger = CountingLogger { calls: 0, bytes: usize.add(0, 0) }
  let logged = run Effect.provideMut(Effect.log("portable"), &mut logger)
  if logger.calls != 1 { let boom = 1 / 0 }
  if logger.bytes != 8 { let boom = 1 / 0 }
  return ()
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.evaluate(self)._tag, 'Completed')
  }),
)

it.effect('keeps missing providers and invalid message inputs explicit', () =>
  Effect.gen(function* () {
    const missing = yield* snapshot(`pub effect fn main() -> () ! LogError {
  return run Effect.log("missing")
}`)
    assert.include(
      Analysis.diagnostics(missing).map((diagnostic) => diagnostic.code),
      'SEM0071',
    )

    const invalidMessage = yield* snapshot(`pub fn main() -> i32 {
  let effect = Effect.log(42)
  return 0
}`)
    assert.isAbove(Analysis.diagnostics(invalidMessage).length, 0)

    const invalidLevel = yield* snapshot(`pub fn main() -> i32 {
  let effect = Effect.logAt(42, "message")
  return 0
}`)
    assert.isAbove(Analysis.diagnostics(invalidLevel).length, 0)
  }),
)

it.effect('stops after deterministic provider failure without recording the rejected message', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.logging { attempts, length }
effect fn ignore(error: LogError) -> () { return () }
effect fn program() -> i32 ! LogError {
  let mut logger = InMemoryLogger.memoryFailAt(1)
  let first = run Effect.provideMut(Effect.log("first"), &mut logger)
  let attempted = Effect.provideMut(Effect.log("second"), &mut logger) |> Effect.catchAll(ignore)
  let second = run attempted
  let skipped = false
  if skipped { let third = run Effect.provideMut(Effect.log("third"), &mut logger) }
  if length(&logger) != 1 { return 0 }
  if attempts(&logger) != 2 { return 0 }
  return 42
}
effect fn recover(error: LogError) -> i32 { return 0 }
pub fn main() -> i32 {
  return run Effect.catchAll(program(), recover)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)

    const capacity = yield* snapshot(`import silk.logging { attempts, length }
effect fn ignore(error: LogError) -> () { return () }
pub fn main() -> i32 {
  let mut logger = InMemoryLogger.memory()
  let attempted = Effect.provideMut(
    Effect.log("12345678901234567890123456789012345678901234567890123456789012345"),
    &mut logger
  ) |> Effect.catchAll(ignore)
  let completed = run attempted
  if attempts(&logger) != 1 { return 0 }
  if length(&logger) != 0 { return 0 }
  return 42
}`)
    assert.deepEqual(Analysis.diagnostics(capacity), [])
    const capacityOutcome = Analysis.evaluate(capacity)
    assert.strictEqual(capacityOutcome._tag, 'Completed')
    if (capacityOutcome._tag === 'Completed') assert.strictEqual(capacityOutcome.result.value, 42)
  }),
)

it.effect('keeps evaluator native and direct Wasm behavior aligned', () =>
  Effect.gen(function* () {
    const native = yield* snapshot(memorySource)
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.notInclude(llvm.ir, 'Logger')

    const wasm = yield* snapshot(memorySource, 'wasm32-unknown-unknown')
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.deepEqual(artifact.hostImports, [])
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const wasmMain = instance.exports.silk_main
    if (typeof wasmMain !== 'function') throw new Error('logging Wasm lost silk_main')
    assert.strictEqual(wasmMain(), 42)
  }),
)

it.effect('adapts complete messages to stdout without making formatting semantic', () =>
  Effect.gen(function* () {
    const source = String.raw`pub effect fn main() -> () ! LogError {
  let mut logger = StdoutLogger.stdout()
  let first = run Effect.provideMut(Effect.log("one\n"), &mut logger)
  let second = run Effect.provideMut(Effect.logAt(LogLevel.error(), "two"), &mut logger)
  return ()
}`
    const self = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const memory = StandardStreams.memory()
    const outcome = Analysis.evaluate(self, { standardStreams: memory.provider })
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, undefined, 2))
    assert.deepEqual(
      memory.events().map((event) => new TextDecoder().decode(Uint8Array.from(event.bytes))),
      ['one\n', 'two'],
    )

    const failing = StandardStreams.memory({ failAt: 0 })
    const failed = Analysis.evaluate(self, { standardStreams: failing.provider })
    assert.strictEqual(failed._tag, 'UnhandledFailure')
    if (failed._tag === 'UnhandledFailure') assert.include(failed.report, 'LogError')
    assert.deepEqual(failing.events(), [])
  }),
)

it.effect('provideMut removes the Logger requirement and preserves Clock', () =>
  Effect.gen(function* () {
    const program = (bind: string) => `import silk.effects as Effect
import silk.logging { Logger, LogError }

struct Clock {}

effect fn read() -> ()
! LogError
? &mut Clock | &mut Logger {
  run Effect.log("Reading clock")
}

pub effect fn main() -> ()
! LogError {
  let mut logger = StdoutLogger.stdout()
  run ${bind}
}`

    const analyze = (body: string) =>
      Analysis.ofSource('logging/main', encoder.encode(program(body)))
    const publicWrapper = yield* analyze('Effect.provideMut(read(), &mut logger)')
    const directIntrinsic = yield* analyze('Intrinsic.bindRequirementMut(read(), &mut logger)')
    const directPipeline = yield* analyze('(read() |> Intrinsic.bindRequirementMut(&mut logger))')

    const unsatisfied = (self: Analysis.FrontendSnapshot) =>
      Analysis.rootAnalysis(self)
        .diagnostics.filter((diagnostic) => diagnostic.code === 'SEM0071')
        .map((diagnostic) => diagnostic.message)

    assert.deepEqual(unsatisfied(publicWrapper), [
      'Run leaves unsatisfied requirements: &mut logging/main.Clock',
    ])
    assert.deepEqual(unsatisfied(publicWrapper), unsatisfied(directIntrinsic))
    assert.deepEqual(unsatisfied(publicWrapper), unsatisfied(directPipeline))

    const executablePipeline = yield* snapshot(`import silk.effects as Effect
import silk.logging { Logger, LogError }
pub effect fn main() -> () ! LogError {
  let mut logger = InMemoryLogger.memory()
  return run (Effect.log("pipeline") |> Intrinsic.bindRequirementMut(&mut logger))
}`)
    assert.deepEqual(Analysis.diagnostics(executablePipeline), [])
    assert.strictEqual(Analysis.evaluate(executablePipeline)._tag, 'Completed')

    const storedIntrinsic = yield* snapshot(`import silk.effects as Effect
import silk.logging { Logger, LogError }
pub effect fn main() -> () ! LogError {
  let mut logger = InMemoryLogger.memory()
  let mut bind = Intrinsic.bindRequirementMut(&mut logger)
  return run bind(Effect.log("stored"))
}`)
    assert.deepEqual(Analysis.diagnostics(storedIntrinsic), [])
    assert.strictEqual(Analysis.evaluate(storedIntrinsic)._tag, 'Completed')
  }),
)

it.effect('forwards provider-selection evidence only from an exact enclosing constraint', () =>
  Effect.gen(function* () {
    const wrapper = (constraint: string) => `import silk.effects as Effect
import silk.logging { Logger, LogError }

effect fn bind<?S, A, P, !E, ?R>(
  self: once Effect<A ! E ? R>,
  provider: &mut P
) -> A ! E ? Without<R, S>
${constraint} {
  return run Intrinsic.bindRequirementMut<S>(move self, provider)
}

effect fn read() -> () ! LogError ? &mut Logger {
  run Effect.log("Reading")
}

pub effect fn main() -> () ! LogError {
  let mut logger = StdoutLogger.stdout()
  return run bind(read(), &mut logger)
}`

    const constrained = yield* snapshot(wrapper('where &mut P provides S from R'))
    assert.deepEqual(Analysis.diagnostics(constrained), [])
    const bind = Analysis.instancesOf(constrained).instances.find(
      (instance) => instance.key.declaration.name === 'bind',
    )
    assert.isDefined(bind)
    if (bind !== undefined) {
      assert.strictEqual(
        RowAlgebra.concretize(
          Type.requirementRowPolicy(),
          bind.specialization.requirementRow ??
            RowAlgebra.concrete(Type.requirementRowPolicy(), []),
        )._tag,
        'Concrete',
      )
      assert.isTrue(bind.specialization.evidence.length > 0)
      assert.include(
        bind.specialization.evidence.map((proof) => proof._tag),
        'RequirementSelection',
      )
    }

    const unconstrained = yield* snapshot(wrapper(''))
    assert.isAbove(Analysis.diagnostics(unconstrained).length, 0)
  }),
)

it.effect('carries a provider section through a closed generic forwarding call', () =>
  Effect.gen(function* () {
    const source = `import silk.effects as Effect
import silk.logging { Logger, LogError }

fn forward<F>(value: F) -> F { return move value }

effect fn program() -> () ! LogError {
  let mut logger = InMemoryLogger.memory()
  let mut bind = forward(Effect.provideMut<&mut Logger>(&mut logger))
  return run bind(Effect.log("forwarded"))
}

effect fn recover(error: LogError) -> () { return () }
pub fn main() -> i32 {
  let completed = run Effect.catchAll(program(), recover)
  return 42
}`
    const frontend = yield* Analysis.ofSource('logging/main', encoder.encode(source))
    assert.deepEqual(Analysis.diagnostics(frontend), [])
    const discovery = Instances.discover(
      'logging/main',
      frontend.results,
      frontend.ownership,
      frontend.index,
    )
    assert.deepEqual(discovery.specializationFailures, [])
    assert.deepEqual(discovery.violations, [])
    assert.include(
      discovery.instances.map((instance) => instance.key.declaration.name),
      'provideMut',
    )
  }),
)
