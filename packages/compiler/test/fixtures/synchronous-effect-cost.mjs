import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Mir from '../../dist/Mir.js'

const encoder = new TextEncoder()
const hash = (value) => createHash('sha256').update(value).digest('hex')
const occurrences = (text, pattern) => [...text.matchAll(pattern)].length
const normalize = (text) =>
  text
    .replaceAll(/cost\/[a-z0-9-]+\/(?:native|wasm)/g, 'cost/<case>/<target>')
    .replaceAll(/\[\d+,\s*\d+\)/g, '[span)')
    .replaceAll(/\/var\/folders\/[^\s"']+/g, '<temporary>')

const sources = Object.freeze([
  {
    id: 'pure-imperative',
    pair: 'pure-pipe',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'pure-piped',
    pair: 'pure-pipe',
    kind: 'effect',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return 41 |> addOne }`,
  },
  {
    id: 'map-imperative',
    pair: 'map',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'map-effect',
    pair: 'map',
    kind: 'effect',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(41) |> Effect.map(addOne) }`,
  },
  {
    id: 'map-both-success-imperative',
    pair: 'map-both-success',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'map-both-success-effect',
    pair: 'map-both-success',
    kind: 'effect',
    expected: 42,
    source: `pub struct Problem { code: i32 }
fn addOne(value: i32) -> i32 { return value + 1 }
fn recover(problem: Problem) -> Problem { return move problem }
effect fn succeed() -> i32 ! Problem { return 41 }
effect fn handle(problem: Problem) -> i32 { return problem.code }
pub fn main() -> i32 {
  return run succeed() |> Effect.mapBoth(addOne, recover) |> Effect.catch(handle)
}`,
  },
  {
    id: 'map-both-failure-imperative',
    pair: 'map-both-failure',
    kind: 'baseline',
    expected: 42,
    source: `struct Problem { code: i32 }
fn recover(problem: Problem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 { return recover(Problem { code: 41 }) }`,
  },
  {
    id: 'map-both-failure-effect',
    pair: 'map-both-failure',
    kind: 'effect',
    expected: 42,
    source: `struct Problem { code: i32 }
struct OtherProblem { code: i32 }
fn keep(value: i32) -> i32 { return value }
fn translate(problem: Problem) -> OtherProblem { return OtherProblem { code: problem.code } }
effect fn failValue() -> i32 ! Problem { fail Problem { code: 41 } }
effect fn recover(problem: OtherProblem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 {
  return run failValue()
    |> Effect.mapBoth(keep, translate)
    |> Effect.catch(recover)
}`,
  },
  {
    id: 'flat-map-imperative',
    pair: 'flat-map',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'flat-map-effect',
    pair: 'flat-map',
    kind: 'effect',
    expected: 42,
    source: `effect fn succeed(value: i32) -> i32 { return value }
effect fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return run succeed(41) |> Effect.flatMap(addOne) }`,
  },
  {
    id: 'provide-imperative',
    pair: 'provide',
    kind: 'baseline',
    expected: 42,
    source: `struct Clock { value: i32 }
fn read(clock: &Clock) -> i32 { return clock.value }
pub fn main() -> i32 { let clock = Clock { value: 42 } return read(&clock) }`,
  },
  {
    id: 'provide-effect',
    pair: 'provide',
    kind: 'effect',
    expected: 42,
    source: `struct Clock { value: i32 }
effect fn read() -> i32 ? &Clock { return 42 }
fn adapt<A, !E, ?R>(self: once Effect<A ! E ? R>) -> once Effect<A ! E ? R> {
  return move self
}
pub fn main() -> i32 {
  let clock = Clock { value: 42 }
  return run adapt(read()) |> Effect.provide(&clock)
}`,
  },
  {
    id: 'stored-imperative',
    pair: 'stored',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return double(addOne(20)) }`,
  },
  {
    id: 'stored-effect',
    pair: 'stored',
    kind: 'effect',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
fn double(value: i32) -> i32 { return value * 2 }
effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let first = succeed(20) |> Effect.map(addOne)
  let second = move first |> Effect.map(double)
  return run second
}`,
  },
  {
    id: 'affine-imperative',
    pair: 'affine',
    kind: 'baseline',
    expected: 42,
    source: `struct Payload { storage: Allocation value: i32 }
impl Drop for Payload { fn drop(self: &mut Payload) -> () { return () } }
fn consume(payload: Payload) -> i32 { return payload.value }
effect fn produce() -> Payload ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
    |> Effect.bindRequirement(&mut allocator)
  return Payload { storage: move storage, value: 42 }
}
effect fn program() -> i32 ! OutOfMemory {
  let payload = run produce()
  return consume(move payload)
}
effect fn recover(error: OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 { return run program() |> Effect.catch(recover) }`,
  },
  {
    id: 'affine-effect',
    pair: 'affine',
    kind: 'effect',
    expected: 42,
    source: `struct Payload { storage: Allocation value: i32 }
impl Drop for Payload { fn drop(self: &mut Payload) -> () { return () } }
fn consume(payload: Payload) -> i32 { return payload.value }
effect fn produce() -> Payload ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
    |> Effect.bindRequirement(&mut allocator)
  return Payload { storage: move storage, value: 42 }
}
effect fn program() -> i32 ! OutOfMemory {
  return run produce() |> Effect.map(consume)
}
effect fn recover(error: OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 { return run program() |> Effect.catch(recover) }`,
  },
  {
    id: 'trap-imperative',
    pair: 'trap',
    kind: 'baseline',
    expected: 'trap',
    source: `fn divide(value: i32) -> i32 { return 1 / value }
pub fn main() -> i32 { return divide(0) }`,
  },
  {
    id: 'trap-effect',
    pair: 'trap',
    kind: 'effect',
    expected: 'trap',
    source: `fn divide(value: i32) -> i32 { return 1 / value }
effect fn zero() -> i32 { return 0 }
pub fn main() -> i32 { return run zero() |> Effect.map(divide) }`,
  },
])

const temporary = mkdtempSync(join(tmpdir(), 'silk-effect-cost-'))
const clang = process.env.SILK_EFFECT_COST_CLANG ?? 'clang'
const artifactDirectory = process.env.SILK_EFFECT_COST_ARTIFACT_DIR

const clangText = (bitcode, id, arguments_) => {
  const bitcodePath = join(temporary, `${id}.bc`)
  writeFileSync(bitcodePath, bitcode)
  const result = spawnSync(clang, [...arguments_, '-x', 'ir', bitcodePath, '-o', '-'], {
    encoding: 'utf8',
  })
  if (result.status !== 0) {
    throw new Error(`${clang} ${arguments_.join(' ')} failed: ${result.stderr}`)
  }
  return normalize(result.stdout)
}

const summarize = (text, includeHash = true) => {
  const suspensionMatches = [
    ...text.matchAll(
      /\bsuspended=true\b|\bsuspend(?:ed)?[_ .-]?(?:effect|operation|step)\b|\b(?:scheduler|fiber|effect[_ .-]?continuation|runtime[_ .-]?continuation)\b/gi,
    ),
  ].map((match) =>
    text.slice(Math.max(0, (match.index ?? 0) - 48), (match.index ?? 0) + match[0].length + 48),
  )
  return Object.freeze({
    bytes: Buffer.byteLength(text),
    ...(includeHash ? { hash: hash(text) } : {}),
    calls: occurrences(text, /\bcall\b/g),
    branches: occurrences(text, /\b(?:br|if|switch)\b/g),
    allocations: occurrences(text, /\b(?:malloc|calloc|realloc|free|memory\.grow)\b/gi),
    indirectCalls: occurrences(text, /\bcall\b[^\n@]*%[A-Za-z0-9_.]+/g),
    suspensionTerms: suspensionMatches.length,
    suspensionMatches,
    effectTerms: occurrences(text, /\bEffect\b|silk\/effects|effects\./g),
  })
}

const summarizeAssembly = (text) =>
  Object.freeze({
    ...summarize(text, false),
    calls: occurrences(text, /^\s*bl\s+/gm),
    branches: occurrences(text, /^\s*(?:b(?:\.[a-z]+)?|cbnz|cbz|tbnz|tbz)\s+/gm),
  })

const llvmEntry = (text) => {
  const marker = '@silk_main('
  const markerIndex = text.indexOf(marker)
  if (markerIndex < 0) return ''
  const start = text.lastIndexOf('define ', markerIndex)
  const body = text.indexOf('{', markerIndex)
  if (start < 0 || body < 0) return ''
  const end = text.indexOf('\n}', body)
  return end < 0 ? '' : text.slice(start, end + 2)
}

const assemblyEntry = (text) => {
  const match = /^_?silk_main:.*\n[\s\S]*?^\s*; -- End function/m.exec(text)
  return match?.[0] ?? ''
}

try {
  const cases = []
  for (const sample of sources) {
    try {
      const sourceId = `cost/${sample.id}`
      const native = await Effect.runPromise(
        Analysis.ofSourceRealized(
          `${sourceId}/native`,
          encoder.encode(sample.source),
          'aarch64-apple-darwin',
        ),
      )
      const wasm = await Effect.runPromise(
        Analysis.ofSourceRealized(
          `${sourceId}/wasm`,
          encoder.encode(sample.source),
          'wasm32-unknown-unknown',
        ),
      )
      const diagnostics = Analysis.diagnostics(native)
      const wasmDiagnostics = Analysis.diagnostics(wasm)
      if (diagnostics.length > 0 || wasmDiagnostics.length > 0) {
        throw new Error(
          `${sample.id} diagnostics: ${JSON.stringify({ native: diagnostics, wasm: wasmDiagnostics })}`,
        )
      }

      const evaluated = Analysis.evaluate(native)
      const evaluatorBehavior =
        evaluated._tag === 'Completed'
          ? evaluated.result.value
          : evaluated.reason._tag === 'Trap'
            ? 'trap'
            : evaluated._tag
      if (evaluatorBehavior !== sample.expected) {
        throw new Error(
          `${sample.id} did not complete: ${JSON.stringify(evaluated, (_, value) =>
            typeof value === 'bigint' ? value.toString() : value,
          )}`,
        )
      }
      const debug = await Effect.runPromise(Analysis.codegen(native, { mode: 'debug' }))
      const release = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
      const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
      const wasmInstance = new WebAssembly.Instance(
        new WebAssembly.Module(wasmArtifact.bytes.slice()),
        {},
      )
      const wasmMain = wasmInstance.exports.silk_main
      if (typeof wasmMain !== 'function') throw new Error(`${sample.id} has no Wasm silk_main`)
      let wasmBehavior
      try {
        wasmBehavior = wasmMain()
      } catch (cause) {
        if (!(cause instanceof WebAssembly.RuntimeError)) throw cause
        wasmBehavior = 'trap'
      }
      const hir = normalize(Hir.encode(Analysis.rootAnalysis(native).hir))
      const mir = normalize(Mir.encode(Analysis.loweredMir(native)))
      const debugLlvm = normalize(debug.ir)
      const releaseLlvm = normalize(release.ir)
      const optimizedLlvm = clangText(release.bitcode, `${sample.id}-optimized`, [
        '-O2',
        '-S',
        '-emit-llvm',
      ])
      const assembly = clangText(release.bitcode, `${sample.id}-assembly`, ['-O2', '-S'])
      const wat = normalize(wasmArtifact.wat)
      if (artifactDirectory !== undefined) {
        const destination = join(artifactDirectory, sample.id)
        mkdirSync(destination, { recursive: true })
        for (const [name, text] of [
          ['hir.txt', hir],
          ['mir.txt', mir],
          ['llvm-debug.ll', debugLlvm],
          ['llvm-release.ll', releaseLlvm],
          ['llvm-optimized.ll', optimizedLlvm],
          ['native.s', assembly],
          ['direct.wat', wat],
        ]) {
          writeFileSync(join(destination, name), text)
        }
        writeFileSync(join(destination, 'native.bc'), release.bitcode)
        writeFileSync(join(destination, 'direct.wasm'), wasmArtifact.bytes)
      }
      const dropCalls = evaluated.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl#'),
      ).length

      cases.push(
        Object.freeze({
          id: sample.id,
          pair: sample.pair,
          kind: sample.kind,
          expected: sample.expected,
          behavior: Object.freeze({
            evaluator: evaluatorBehavior,
            wasm: wasmBehavior,
            dropCalls,
          }),
          pipeTokens: Object.freeze({
            hir: occurrences(hir, /\|>/g),
            mir: occurrences(mir, /\|>/g),
          }),
          hir: summarize(hir),
          mir: summarize(mir),
          debugLlvm: summarize(debugLlvm),
          releaseLlvm: summarize(releaseLlvm),
          optimizedLlvm: summarize(optimizedLlvm, false),
          optimizedLlvmEntry: summarize(llvmEntry(optimizedLlvm), false),
          assembly: summarizeAssembly(assembly),
          assemblyEntry: summarizeAssembly(assemblyEntry(assembly)),
          wasm: Object.freeze({ ...summarize(wat), binaryBytes: wasmArtifact.bytes.length }),
          symbols: Object.freeze({
            native: release.symbols.map((entry) => entry.declaration.name),
            wasm: wasmArtifact.symbols.map((entry) => entry.declaration.name),
          }),
        }),
      )
    } catch (cause) {
      throw new Error(`${sample.id}: ${cause instanceof Error ? cause.stack : String(cause)}`)
    }
  }

  const version = spawnSync(clang, ['--version'], { encoding: 'utf8' })
  if (version.status !== 0) throw new Error(`${clang} --version failed: ${version.stderr}`)
  process.stdout.write(
    JSON.stringify({
      schema: 1,
      clang: version.stdout.split('\n').at(0),
      node: process.version,
      cases,
    }),
  )
} finally {
  rmSync(temporary, { recursive: true, force: true })
}
