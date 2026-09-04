import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
const encoder = new TextEncoder()

const pipelineSource =
  'import silk.i32 as i32\npub fn main() -> i32 { return 2 + 3 * 4 |> i32.add(1) }'

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/operator.${name}`, import.meta.url), 'utf8')

it.effect('lowers negation to generated zero plus source-authored trapping subtraction', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'golden/negation',
      encoder.encode('pub fn main() -> i32 { let value = 42 return -value }'),
      'aarch64-apple-darwin',
    )
    const fn = Analysis.loweredMir(snapshot).functions.at(0)
    const operations = fn === undefined ? [] : MirVerification.operations(fn)
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

it.effect('pins one operator pipeline through canonical HIR, MIR, and LLVM', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'golden/operator',
      encoder.encode(pipelineSource),
      'aarch64-apple-darwin',
    )
    const artifact = yield* Analysis.codegen(snapshot, { mode: 'release' })

    assert.strictEqual(Hir.encode(Analysis.rootAnalysis(snapshot).hir), golden('hir.txt'))
    assert.strictEqual(MirEncoding.encode(Analysis.loweredMir(snapshot)), golden('mir.txt'))
    assert.strictEqual(artifact.ir, golden('ll.txt'))
    assert.strictEqual(
      `${createHash('sha256').update(artifact.bitcode).digest('hex')}\n`,
      golden('bc.sha256'),
    )
  }),
)
