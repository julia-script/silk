import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../../dist/Analysis.js'
import * as Driver from '../../../dist/Driver.js'
import * as SourceFile from '../../../dist/SourceFile.js'
import * as SourceResolver from '../../../dist/SourceResolver.js'

const encoder = new TextEncoder()

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

const boxSource = `import silk.box {
  Box,
  make as boxMake,
  get as boxGet,
  into as boxInto
}

pub struct End {}

pub struct Link {
  next: Box<Chain>
}

pub struct Chain {
  step: End | Link
  value: i32
}

effect fn recursiveBuild(depth: i32) -> Chain ! OutOfMemory ? &mut Allocator {
  if depth == 0 { return Chain { step: End {}, value: 0 } }
  let inner = run recursiveBuild(depth - 1)
  let boxed = run boxMake<Chain>(move inner)
  return Chain { step: Link { next: move boxed }, value: 1 }
}

effect fn iterativeBuild(depth: i32) -> Chain ! OutOfMemory ? &mut Allocator {
  let mut current = Chain { step: End {}, value: 0 }
  let mut index = 0
  while index < depth {
    let boxed = run boxMake<Chain>(move current)
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
  let mut replacement = boxInto<Chain>(move next)
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
    Link { next } => walkBox(boxGet<Chain>(&next))
  }
}

fn walkBox(view: &[Chain]) -> i32 {
  return match &view[usize.ZERO] {
    Chain { step, value } => value + match &step {
      End nothing => 0
      Link { next } => walkBox(boxGet<Chain>(&next))
    }
  }
}

effect fn buildOnly() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let built = run recursiveBuild(${depth}) |> Effect.provideMut(&mut allocator)
  iterativeDrop(move built)
  return 42
}

effect fn walkOnly() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let built = run iterativeBuild(${depth}) |> Effect.provideMut(&mut allocator)
  let answer = walk(&built)
  iterativeDrop(move built)
  if answer == ${depth} { return 42 }
  return 1
}

effect fn dropOnly() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let built = run iterativeBuild(${depth}) |> Effect.provideMut(&mut allocator)
  drop built
  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 2 }

pub fn main() -> i32 {
  return run Effect.catch(${shape === 'box-build' ? 'buildOnly()' : shape === 'box-walk' ? 'walkOnly()' : 'dropOnly()'}, recover)
}`

const source = shape === 'scalar-non-tail' ? scalarSource : boxSource
const sourceId = `characterization/effect-suspend-stack/${shape}-${engine}-${depth}`
const temporary = mkdtempSync(join(tmpdir(), 'silk-effect-suspend-stack-'))

const classifyWasm = (cause) => {
  if (cause instanceof RangeError) return 'host-stack-exhaustion'
  if (cause instanceof WebAssembly.RuntimeError) return 'wasm-trap'
  return 'host-error'
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
    outcome =
      evaluated._tag === 'Completed'
        ? Object.freeze({ kind: 'completed', result: evaluated.result.value })
        : Object.freeze({
            kind: evaluated._tag === 'Blocked' ? 'evaluation-blocked' : 'evaluation-failure',
            outcome: evaluated,
          })
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
          clang: process.env.SILK_EFFECT_STACK_CLANG ?? '/usr/bin/clang',
        }),
        profile: 'release',
        destination,
      }).pipe(Effect.provide(SourceResolver.empty)),
    )
    if (compiled._tag !== 'Compiled') {
      throw new Error(`native compilation failed: ${JSON.stringify(compiled)}`)
    }
    const run = spawnSync(compiled.path, [], { encoding: 'utf8', timeout: 60_000 })
    outcome = Object.freeze({
      kind:
        run.status === 42
          ? 'completed'
          : run.signal === 'SIGSEGV'
            ? 'host-stack-exhaustion'
            : run.error?.code === 'ETIMEDOUT'
              ? 'timeout'
              : run.signal === null
                ? 'unexpected-exit'
                : 'host-signal',
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
