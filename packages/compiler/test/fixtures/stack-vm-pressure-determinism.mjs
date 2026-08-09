import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as Ownership from '../../dist/Ownership.js'

const pressureSource = readFileSync(
  new URL('../../../../examples/language-pressure/stack-vm/main.silk', import.meta.url),
  'utf8',
)
const source = pressureSource.replace(
  '  if value != 0 { let mismatch = 1 / 0 }',
  '  if value != 184 { let mismatch = 1 / 0 }',
)
const separateSource = pressureSource
  .replace(
    'events: Vector<Step | VmDiagnostic>\n  result:',
    'steps: Vector<Step>\n  diagnostics: Vector<VmDiagnostic>\n  result:',
  )
  .replace(
    'events: &mut Vector<Step | VmDiagnostic>,\n  pc: usize,\n  opcode:',
    'steps: &mut Vector<Step>,\n  pc: usize,\n  opcode:',
  )
  .replace(
    'let added = run append<Step | VmDiagnostic>(move events, move step)',
    'let added = run append<Step>(move steps, move step)',
  )
  .replace(
    'events: &mut Vector<Step | VmDiagnostic>,\n  pc: usize,\n  code:',
    'diagnostics: &mut Vector<VmDiagnostic>,\n  pc: usize,\n  code:',
  )
  .replace(
    'let added = run append<Step | VmDiagnostic>(move events, move diagnostic)',
    'let added = run append<VmDiagnostic>(move diagnostics, move diagnostic)',
  )
  .replace(
    'fn finish(result: i32, events: Vector<Step | VmDiagnostic>, fingerprint: i32) -> Executed {',
    'fn finish(result: i32, steps: Vector<Step>, diagnostics: Vector<VmDiagnostic>, fingerprint: i32) -> Executed {',
  )
  .replace('    events: move events,', '    steps: move steps,\n    diagnostics: move diagnostics,')
  .replace(
    '  let mut events = make<Step | VmDiagnostic>()',
    '  let mut steps = make<Step>()\n  let mut diagnostics = make<VmDiagnostic>()',
  )
  .replaceAll('pushStep(&mut events', 'pushStep(&mut steps')
  .replaceAll('pushDiagnostic(&mut events', 'pushDiagnostic(&mut diagnostics')
  .replaceAll(
    'finish(currentTop, move events, fingerprint)',
    'finish(currentTop, move steps, move diagnostics, fingerprint)',
  )
  .replace('if value != 0', 'if value != 184')

const snapshot = (input, target) =>
  Effect.runPromise(
    Analysis.ofSource('stack-vm-pressure/determinism', new TextEncoder().encode(input), target),
  )
const hash = (value) => createHash('sha256').update(value).digest('hex')
const json = (value) =>
  JSON.stringify(value, (_key, candidate) =>
    typeof candidate === 'bigint' ? candidate.toString() : candidate,
  )

const native = await snapshot(source, 'aarch64-apple-darwin')
const wasm = await snapshot(source, 'wasm32-unknown-unknown')
const separateNative = await snapshot(separateSource, 'aarch64-apple-darwin')
const separateWasm = await snapshot(separateSource, 'wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const separateNativeArtifact = await Effect.runPromise(
  Analysis.codegen(separateNative, { mode: 'release' }),
)
const separateWasmArtifact = await Effect.runPromise(
  Analysis.codegenWasm(separateWasm, { mode: 'release' }),
)

const encodeSnapshot = (self) => {
  const evaluated = Analysis.evaluate(self)
  return {
    diagnostics: Analysis.diagnostics(self),
    modules: Analysis.modules(self).map((module) => module.name),
    hir: hash(
      Analysis.modules(self)
        .map((module) => Hir.encode(Analysis.hirOf(self, module.name)))
        .join('\n'),
    ),
    ownership: hash(
      Analysis.modules(self)
        .map((module) => {
          const value = Analysis.ownershipOf(self, module.name)
          return value === undefined ? '' : Ownership.encode(value)
        })
        .join('\n'),
    ),
    layout: hash(Layout.encode(Analysis.layoutOf(self).value)),
    mir: hash(Mir.encode(Analysis.loweredMir(self))),
    evaluation: hash(json(Analysis.traceOf(evaluated))),
    outcome: evaluated._tag,
    allocations: Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag),
  }
}

process.stdout.write(
  json({
    native: encodeSnapshot(native),
    wasm: encodeSnapshot(wasm),
    nativeSymbols: nativeArtifact.symbols,
    wasmSymbols: wasmArtifact.symbols,
    nativeText: hash(nativeArtifact.ir),
    wasmText: hash(wasmArtifact.wat),
    nativeBytes: hash(nativeArtifact.bitcode),
    wasmBytes: hash(wasmArtifact.bytes),
    separate: {
      native: encodeSnapshot(separateNative),
      wasm: encodeSnapshot(separateWasm),
      nativeText: hash(separateNativeArtifact.ir),
      wasmText: hash(separateWasmArtifact.wat),
      nativeBytes: hash(separateNativeArtifact.bitcode),
      wasmBytes: hash(separateWasmArtifact.bytes),
    },
  }),
)
