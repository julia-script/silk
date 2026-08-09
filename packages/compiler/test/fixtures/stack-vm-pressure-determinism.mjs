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

const snapshot = (target) =>
  Effect.runPromise(
    Analysis.ofSourceRealized(
      'stack-vm-pressure/determinism',
      new TextEncoder().encode(source),
      target,
    ),
  )
const hash = (value) => createHash('sha256').update(value).digest('hex')
const json = (value) =>
  JSON.stringify(value, (_key, candidate) =>
    typeof candidate === 'bigint' ? candidate.toString() : candidate,
  )

const native = await snapshot('aarch64-apple-darwin')
const wasm = await snapshot('wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))

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
  }),
)
