import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Analysis from '../../../dist/Analysis.js'
import * as Driver from '../../../dist/Driver.js'
import * as NodeHeapObservation from '../../../dist/NodeHeapObservation.js'
import * as SourceFile from '../../../dist/SourceFile.js'
import * as SourceResolver from '../../../dist/SourceResolver.js'

const encoder = new TextEncoder()
const clang = Effect.runSync(
  Config.string('SILK_EFFECT_STACK_CLANG').pipe(Config.withDefault('/usr/bin/clang')),
)

const argument = (name) => {
  const index = process.argv.indexOf(name)
  return index < 0 ? undefined : process.argv.at(index + 1)
}

const engine = argument('--engine')
const shape = argument('--case')
const depthText = argument('--depth')
const expected = argument('--expect')

if (engine !== 'native' && engine !== 'wasm' && engine !== 'evaluator') {
  throw new Error('--engine must be native, wasm, or evaluator')
}
if (!['scalar-non-tail', 'box-build', 'box-walk', 'box-drop'].includes(shape)) {
  throw new Error('--case must be scalar-non-tail, box-build, box-walk, or box-drop')
}
const depth = Number(depthText)
if (!Number.isSafeInteger(depth) || depth <= 0 || depth > 2_000_000_000) {
  throw new Error('--depth must be a positive i32')
}

const scalarSource = `effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }
  let inner = run count(value - 1)
  return inner + 1
}

pub fn main() -> i32 {
  let answer = run count(${depth})
  if answer == ${depth} { return 42 }
  return 1
}`

let boxAction = 'dropOnly()'
if (shape === 'box-build') boxAction = 'buildOnly()'
else if (shape === 'box-walk') boxAction = 'walkOnly()'

const boxSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.box { Box }

pub struct End {}

pub struct Link {
  next: Box<Chain>
}

pub struct Chain {
  step: End | Link
  value: i32
}

effect fn recursiveBuild(depth: i32) -> Chain ! OutOfMemoryError ? &mut Allocator {
  if depth == 0 { return Chain { step: End {}, value: 0 } }
  let inner = run recursiveBuild(depth - 1)
  let boxed = run Box.make<Chain>(move inner)
  return Chain { step: Link { next: move boxed }, value: 1 }
}

effect fn iterativeBuild(depth: i32) -> Chain ! OutOfMemoryError ? &mut Allocator {
  let mut current = Chain { step: End {}, value: 0 }
  let mut index = 0
  while index < depth {
    let boxed = run Box.make<Chain>(move current)
    current = Chain { step: Link { next: move boxed }, value: 1 }
    index = index + 1
  }
  return move current
}

// Characterization-only teardown: consume one link at a time so build and walk measurements do
// not accidentally exercise recursive Box.drop. Every allocation still releases normally.
fn iterativeDrop(chain: Chain) -> () {
  let mut current = move chain
  let mut complete = false
  while complete == false {
    complete = consumeStep(&mut current)
  }
  return ()
}

fn consumeStep(current: &mut Chain) -> bool {
  let step = Intrinsic.replace(current.step, End {})
  return match move step {
    End nothing => true
    Link { next } => continueWith(move current, move next)
  }
}

fn continueWith(current: &mut Chain, next: Box<Chain>) -> bool {
  let mut replacement = Box.into<Chain>(move next)
  let replacementStep = Intrinsic.replace(replacement.step, End {})
  let previousStep = Intrinsic.replace(current.step, move replacementStep)
  drop previousStep
  current.value = replacement.value
  drop replacement
  return false
}

fn walk(self: &Chain) -> i32 {
  return self.value + match &self.step {
    End nothing => 0
    Link { next } => walkBox(Box.get<Chain>(&next))
  }
}

fn walkBox(view: &[Chain]) -> i32 {
  return match &view[usize.ZERO] {
    Chain { step, value } => value + match &step {
      End nothing => 0
      Link { next } => walkBox(Box.get<Chain>(&next))
    }
  }
}

effect fn buildOnly() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let built = run recursiveBuild(${depth}) |> Effect.provideMut(&mut allocator)
  iterativeDrop(move built)
  return 42
}

effect fn walkOnly() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let built = run iterativeBuild(${depth}) |> Effect.provideMut(&mut allocator)
  let answer = walk(&built)
  iterativeDrop(move built)
  if answer == ${depth} { return 42 }
  return 1
}

effect fn dropOnly() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let built = run iterativeBuild(${depth}) |> Effect.provideMut(&mut allocator)
  drop built
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 2 }

pub fn main() -> i32 {
  return run Effect.catchAll(${boxAction}, recover)
}`

const source = shape === 'scalar-non-tail' ? scalarSource : boxSource
const sourceId = `characterization/effect-suspend-stack/${shape}-${engine}-${depth}`
const temporary = mkdtempSync(join(tmpdir(), 'silk-effect-suspend-stack-'))

const classifyWasm = (cause) => {
  if (cause instanceof RangeError) return 'host-stack-exhaustion'
  if (cause instanceof WebAssembly.RuntimeError) return 'wasm-trap'
  return 'host-error'
}

const classifyNative = (run) => {
  if (run.status === 42) return 'completed'
  if (run.signal === 'SIGSEGV') return 'host-stack-exhaustion'
  if (run.error?.code === 'ETIMEDOUT') return 'timeout'
  if (run.signal === null) return 'unexpected-exit'
  return 'host-signal'
}

try {
  const startedAt = performance.now()
  let outcome
  if (engine === 'evaluator') {
    const snapshot = await Effect.runPromise(
      Analysis.ofSourceRealized(sourceId, encoder.encode(source), 'aarch64-apple-darwin'),
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    if (diagnostics.length > 0) {
      throw new Error(`Silk diagnostics: ${JSON.stringify(diagnostics)}`)
    }
    const evaluated = Analysis.evaluate(snapshot, {
      maxSteps: Math.max(1_000_000, depth * 10_000),
      maxCallDepth: Math.max(1_024, depth + 16),
    })
    if (evaluated._tag === 'Completed') {
      outcome = Object.freeze({ kind: 'completed', result: evaluated.result.value })
    } else {
      const kind = evaluated._tag === 'Blocked' ? 'evaluation-blocked' : 'evaluation-failure'
      outcome = Object.freeze({ kind, outcome: evaluated })
    }
  } else if (engine === 'wasm') {
    const snapshot = await Effect.runPromise(
      Analysis.ofSourceRealized(sourceId, encoder.encode(source), 'wasm32-unknown-unknown'),
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    if (diagnostics.length > 0) {
      throw new Error(`Silk diagnostics: ${JSON.stringify(diagnostics)}`)
    }
    const artifact = await Effect.runPromise(Analysis.codegenWasm(snapshot, { mode: 'release' }))
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new Error('missing Wasm silk_main export')
    try {
      const result = main()
      outcome = Object.freeze({ kind: 'completed', result })
    } catch (cause) {
      outcome = Object.freeze({
        kind: classifyWasm(cause),
        error: cause instanceof Error ? `${cause.name}: ${cause.message}` : String(cause),
      })
    }
  } else {
    const destination = join(temporary, `${shape}-${depth}`)
    const compiled = await Effect.runPromise(
      Driver.compile({
        compilation: { root: SourceFile.make(sourceId, encoder.encode(source)) },
        toolchain: Object.freeze({
          _tag: 'Toolchain',
          clang,
          llvmAr: 'llvm-ar',
        }),
        profile: 'release',
        artifactKind: 'NativeExecutable',
        destination,
      }).pipe(Effect.provide(Layer.mergeAll(SourceResolver.empty, NodeHeapObservation.layer))),
    )
    if (compiled._tag !== 'Compiled') {
      throw new Error(`native compilation failed: ${JSON.stringify(compiled)}`)
    }
    const run = spawnSync(compiled.path, [], { encoding: 'utf8', timeout: 60_000 })
    outcome = Object.freeze({
      kind: classifyNative(run),
      status: run.status,
      signal: run.signal,
      stderr: run.stderr,
    })
  }

  const report = Object.freeze({
    schema: 1,
    engine,
    case: shape,
    depth,
    outcome,
    elapsedMs: Math.round(performance.now() - startedAt),
    host: Object.freeze({ platform: process.platform, arch: process.arch, node: process.version }),
  })
  process.stdout.write(`${JSON.stringify(report)}\n`)
  if (expected !== undefined && outcome.kind !== expected) {
    process.stderr.write(`expected ${expected}, received ${outcome.kind}\n`)
    process.exitCode = 1
  }
} finally {
  rmSync(temporary, { recursive: true, force: true })
}
