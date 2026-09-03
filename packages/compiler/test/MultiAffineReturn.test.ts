import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-multi-affine-return-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const allocated = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.usize as usize
import silk.vector { Vector }

struct Step { pc: usize opcode: u8 depth: usize top: i32 }
struct VmDiagnostic { pc: usize code: usize }

struct Returned {
  first: Vector<Step>
  second: Vector<VmDiagnostic>
  result: i32
  fingerprint: i32
}

effect fn pushStep(
  values: &mut Vector<Step>,
  step: Step
) -> () ! OutOfMemoryError ? &mut Allocator {
  return run Vector.append<Step>(move values, move step)
}

effect fn pushDiagnostic(
  values: &mut Vector<VmDiagnostic>,
  diagnostic: VmDiagnostic
) -> () ! OutOfMemoryError ? &mut Allocator {
  return run Vector.append<VmDiagnostic>(move values, move diagnostic)
}

fn finish(
  first: Vector<Step>,
  second: Vector<VmDiagnostic>,
  result: i32,
  fingerprint: i32
) -> Returned {
  return Returned {
    first: move first,
    second: move second,
    result: result,
    fingerprint: fingerprint
  }
}

effect fn build() -> Returned ! OutOfMemoryError ? &mut Allocator {
  let stepLayout = Layout.of<Step>()
  let diagnosticLayout = Layout.of<VmDiagnostic>()
  let mut first = Vector.make<Step>()
  let mut index = usize.add(0, 0)
  while index < 6 {
    let step = Step { pc: index, opcode: 1, depth: index + 1, top: usize.toI32(index) }
    let firstAdded = run pushStep(&mut first, move step)
    index = index + 1
  }
  let mut second = Vector.make<VmDiagnostic>()
  if index == 99 {
    let diagnostic = VmDiagnostic { pc: index, code: index }
    let diagnosticAdded = run pushDiagnostic(&mut second, move diagnostic)
  }
  if index == 6 {
    return finish(move first, move second, 5, 7)
  }
  return finish(move first, move second, 1, 1)
}

fn observeValues(
  first: Vector<Step>,
  second: Vector<VmDiagnostic>,
  result: i32,
  fingerprint: i32
) -> i32 {
  if Vector.length<Step>(&first) == 6 {} else { return 1 }
  if Vector.length<VmDiagnostic>(&second) == 0 {} else { return 2 }
  return result + fingerprint + 30
}

fn observe(returned: Returned) -> i32 {
  return match move returned {
    Returned { first, second, result, fingerprint } =>
      observeValues(move first, move second, result, fingerprint)
  }
}

effect fn execute() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let pending = build() |> Effect.provideMut(&mut allocator)
  let returned = run pending
  return observe(move returned)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(execute(), recover) }`

const stackVmWithSeparateVectors = readFileSync(
  new URL('../../../examples/language-pressure/stack-vm/main.silk', import.meta.url),
  'utf8',
)
  .replace(
    'events: Vector<Step | VmDiagnostic>\n  result:',
    'steps: Vector<Step>\n  diagnostics: Vector<VmDiagnostic>\n  result:',
  )
  .replace(
    'events: &mut Vector<Step | VmDiagnostic>,\n  pc: usize,\n  opcode:',
    'steps: &mut Vector<Step>,\n  pc: usize,\n  opcode:',
  )
  .replace(
    'let added = run Vector.append<Step | VmDiagnostic>(move events, move step)',
    'let added = run Vector.append<Step>(move steps, move step)',
  )
  .replace(
    'events: &mut Vector<Step | VmDiagnostic>,\n  pc: usize,\n  code:',
    'diagnostics: &mut Vector<VmDiagnostic>,\n  pc: usize,\n  code:',
  )
  .replace(
    'let added = run Vector.append<Step | VmDiagnostic>(move events, move diagnostic)',
    'let added = run Vector.append<VmDiagnostic>(move diagnostics, move diagnostic)',
  )
  .replace(
    `fn finish(
  result: i32,
  events: Vector<Step | VmDiagnostic>,
  fingerprint: i32
) -> Executed {`,
    `fn finish(
  result: i32,
  steps: Vector<Step>,
  diagnostics: Vector<VmDiagnostic>,
  fingerprint: i32
) -> Executed {`,
  )
  .replace('    events: move events,', '    steps: move steps,\n    diagnostics: move diagnostics,')
  .replace(
    '  let mut events = Vector.make<Step | VmDiagnostic>()',
    '  let mut steps = Vector.make<Step>()\n  let mut diagnostics = Vector.make<VmDiagnostic>()',
  )
  .replaceAll('pushStep(&mut events', 'pushStep(&mut steps')
  .replaceAll('pushDiagnostic(&mut events', 'pushDiagnostic(&mut diagnostics')
  .replaceAll(
    'finish(currentTop, move events, fingerprint)',
    'finish(currentTop, move steps, move diagnostics, fingerprint)',
  )
  .replace(
    /fn fingerprintEvents\([\s\S]*?\n}\n\nfn fingerprint\([\s\S]*?\n}\n/,
    `fn fingerprint(executed: Executed) -> i32 {
  return executed.fingerprint
}
`,
  )
  .replace('if value != 0', 'if value != 184')

it.effect('keeps ordinary multi-affine Effect transport in evaluator and Wasm parity', () =>
  Effect.gen(function* () {
    const wasmSnapshot = yield* Analysis.ofSourceRealized(
      'multi-affine-return/allocated',
      ascii(allocated),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    const evaluated = Analysis.evaluate(wasmSnapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'preserves the stack VM diagnostic root when its exclusive branch is untaken',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'multi-affine-return/stack-vm',
        ascii(stackVmWithSeparateVectors),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.isAbove(acquires.length, 0)
      assert.strictEqual(releases.length, acquires.length)

      const wasmSnapshot = yield* Analysis.ofSourceRealized(
        'multi-affine-return/stack-vm',
        ascii(stackVmWithSeparateVectors),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
      const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 0)

      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('multi-affine-return/stack-vm', ascii(stackVmWithSeparateVectors)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
        profile: 'release',
        artifactKind: 'NativeExecutable',
        destination: join(destinationRoot, 'stack-vm'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(
        run.status,
        0,
        Json.stringify({
          stderr: run.stderr,
          signal: run.signal,
          error: run.error?.message,
        }),
      )
    }),
  120_000,
)
