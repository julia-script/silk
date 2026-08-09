import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'

const source = 'pub fn main() -> i32 { return 2 + 3 * 4 |> i32.add(1) }'
const encoder = new TextEncoder()

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/operator.${name}`, import.meta.url), 'utf8')

it.effect('lowers negation to generated zero plus source-authored trapping subtraction', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'golden/negation',
      encoder.encode('pub fn main() -> i32 { let value = 42 return -value }'),
      'aarch64-apple-darwin',
    )
    const fn = Analysis.loweredMir(snapshot).functions.at(0)
    const operations = fn === undefined ? [] : Mir.operations(fn)
    const zero = operations.find(
      (operation) => operation._tag === 'Literal' && operation.value === 0n,
    )
    const subtraction = operations.find(
      (operation) => operation._tag === 'Binary' && operation.operator === 'Subtract',
    )

    assert.strictEqual(zero?._tag, 'Literal')
    assert.strictEqual(zero?.provenance.generated, true)
    assert.strictEqual(subtraction?._tag, 'Binary')
    assert.strictEqual(subtraction?.provenance.generated, false)
  }),
)

it.effect('pins one operator pipeline through canonical HIR, MIR, LLVM, and WebAssembly', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSource(
      'golden/operator',
      encoder.encode(source),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSource(
      'golden/operator',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    )
    const llvmArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })

    assert.strictEqual(Hir.encode(Analysis.rootAnalysis(native).hir), golden('hir.txt'))
    assert.strictEqual(Mir.encode(Analysis.loweredMir(native)), golden('mir.txt'))
    assert.strictEqual(llvmArtifact.ir, golden('ll.txt'))
    assert.strictEqual(wasmArtifact.wat, golden('wat.txt'))
    assert.strictEqual(
      `${createHash('sha256').update(llvmArtifact.bitcode).digest('hex')}\n`,
      golden('bc.sha256'),
    )
    assert.strictEqual(
      `${createHash('sha256').update(wasmArtifact.bytes).digest('hex')}\n`,
      golden('wasm.sha256'),
    )
  }),
)
