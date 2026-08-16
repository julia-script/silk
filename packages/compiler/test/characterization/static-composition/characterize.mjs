import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../../dist/Analysis.js'
import * as FormattedDocument from '../../../dist/FormattedDocument.js'
import * as Formatter from '../../../dist/Formatter.js'
import * as Lexer from '../../../dist/Lexer.js'
import * as Mir from '../../../dist/Mir.js'
import * as Parser from '../../../dist/Parser.js'
import * as SourceFile from '../../../dist/SourceFile.js'

const encoder = new TextEncoder()
const sizes = [1, 8, 32, 64, 128]
const shapes = ['left', 'balanced']

const sha256 = (value) => createHash('sha256').update(value).digest('hex')

const leaf = (index) =>
  Object.freeze({
    type: `Leaf${index}`,
    value: `Leaf${index} { value: ${index} }`,
  })

const tree = (shape, indexes, nodes) => {
  if (indexes.length === 1) return leaf(indexes[0])
  const middle = shape === 'left' ? indexes.length - 1 : Math.floor(indexes.length / 2)
  const left = tree(shape, indexes.slice(0, middle), nodes)
  const right = tree(shape, indexes.slice(middle), nodes)
  const type = `Node${nodes.length}`
  nodes.push(`struct ${type} { left: ${left.type} right: ${right.type} }`)
  return Object.freeze({
    type,
    value: `${type} { left: ${left.value}, right: ${right.value} }`,
  })
}

const sourceOf = (shape, size) => {
  const indexes = Array.from({ length: size }, (_, index) => index)
  const nodes = []
  const commandTree = tree(shape, indexes, nodes)
  const leaves = indexes.map((index) => `struct Leaf${index} { value: i32 }`).join('\n')
  return `${leaves}
${nodes.join('\n')}
pub fn main() -> i32 {
  let commandTree = ${commandTree.value}
  return 42
}`
}

const canonical = async (id, source) => {
  const document = await Effect.runPromise(
    Formatter.format(Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))),
  )
  return Uint8Array.from(FormattedDocument.toUint8Array(document))
}

const snapshotOf = async (id, bytes, target) => {
  const snapshot = await Effect.runPromise(Analysis.ofSourceRealized(id, bytes, target))
  const diagnostics = Analysis.diagnostics(snapshot)
  if (diagnostics.length > 0)
    throw new Error(
      `${id} diagnostics: ${JSON.stringify(diagnostics.map(({ code, message }) => ({ code, message })))}`,
    )
  if (snapshot.layout._tag !== 'Available' || snapshot.mir._tag !== 'Available')
    throw new Error(`${id} target-dependent phases unavailable`)
  return snapshot
}

const startedAt = performance.now()
const reports = []
for (const shape of shapes) {
  for (const size of sizes) {
    const id = `characterization/static-composition/${shape}-${size}`
    const canonicalBytes = await canonical(id, sourceOf(shape, size))
    const heapBefore = process.memoryUsage().heapUsed
    const wasmSnapshot = await snapshotOf(id, canonicalBytes, 'wasm32-unknown-unknown')
    const wasm = await Effect.runPromise(Analysis.codegenWasm(wasmSnapshot, { mode: 'release' }))
    const nativeSnapshot = await snapshotOf(id, canonicalBytes, 'aarch64-apple-darwin')
    const native = await Effect.runPromise(Analysis.codegen(nativeSnapshot, { mode: 'release' }))
    const mir = wasmSnapshot.mir.value
    reports.push(
      Object.freeze({
        shape,
        size,
        canonicalBytes: canonicalBytes.byteLength,
        canonicalSha256: sha256(canonicalBytes),
        semanticOccurrences: [...wasmSnapshot.semanticOccurrences.modules.values()].reduce(
          (total, module) => total + module.occurrences.length,
          0,
        ),
        representations:
          wasmSnapshot.instances.callables.length + wasmSnapshot.instances.effects.length,
        instances: wasmSnapshot.instances.instances.length,
        layouts: wasmSnapshot.layout.value.entries.length,
        mirFunctions: mir.functions.length,
        mirOperations: mir.functions.flatMap(Mir.operations).length,
        phaseMs: Object.fromEntries(
          wasmSnapshot.report.map((phase) => [phase.phase, Number(phase.elapsedMs.toFixed(3))]),
        ),
        peakHeapBytes: Math.max(heapBefore, process.memoryUsage().heapUsed),
        nativeBytes: native.bitcode.byteLength,
        nativeSha256: sha256(native.bitcode),
        wasmBytes: wasm.bytes.byteLength,
        wasmSha256: sha256(wasm.bytes),
      }),
    )
  }
}

process.stdout.write(
  `${JSON.stringify({
    schema: 1,
    environment: {
      platform: process.platform,
      arch: process.arch,
      node: process.version,
    },
    elapsedMs: Number((performance.now() - startedAt).toFixed(3)),
    reports,
  })}\n`,
)
