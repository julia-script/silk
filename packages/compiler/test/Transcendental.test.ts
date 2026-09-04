import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Schema from 'effect/Schema'
import * as Analysis from '../src/Analysis.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Transcendental from '../src/Transcendental.js'

const Vector = Schema.Struct({
  width: Schema.Literals([32, 64]),
  inputBits: Schema.String,
  operation: Schema.Literals(['Sin', 'Cos']),
  referenceBits: Schema.String,
  acceptedBits: Schema.optional(Schema.String),
})

const Fixture = Schema.Struct({
  generator: Schema.String,
  vectors: Schema.Array(Vector),
})

const fixture = Schema.decodeUnknownSync(Fixture)(
  JSON.parse(
    readFileSync(
      fileURLToPath(new URL('./fixtures/transcendental-vectors.json', import.meta.url)),
      'utf8',
    ),
  ),
)

const ulpDistance = (width: 32 | 64, left: bigint, right: bigint): bigint => {
  const leftKey = FloatingPoint.totalOrderKey({ width, bits: left })
  const rightKey = FloatingPoint.totalOrderKey({ width, bits: right })
  return leftKey < rightKey ? rightKey - leftKey : leftKey - rightKey
}

it('stays within four ulp of independently generated high-precision vectors', () => {
  assert.include(fixture.generator, 'bc -l at scale 1200')
  for (const vector of fixture.vectors) {
    const actual = Transcendental.evaluate(vector.operation, {
      width: vector.width,
      bits: BigInt(vector.inputBits),
    })
    assert.isAtMost(
      Number(ulpDistance(vector.width, actual.bits, BigInt(vector.referenceBits))),
      4,
      `${vector.width} ${vector.operation}(${vector.inputBits})`,
    )
  }
})

it.effect('rejects a mismatched transcendental MIR result before execution', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'transcendental/malformed',
      new TextEncoder().encode(
        'import silk.f64 as f64\npub fn main() -> i32 { return f64.toI32(f64.cos(1.0)) }',
      ),
      'aarch64-apple-darwin',
    )
    const mir = Analysis.loweredMir(snapshot)
    const transcendental = mir.functions
      .flatMap(MirVerification.operations)
      .find((operation) => operation._tag === 'FloatTranscendental')
    assert.strictEqual(transcendental?._tag, 'FloatTranscendental')
    if (transcendental?._tag !== 'FloatTranscendental') return
    const malformed: Mir.Module = {
      ...mir,
      functions: mir.functions.map((fn): Mir.MirFunction => {
        if (!MirVerification.operations(fn).includes(transcendental)) return fn
        return {
          ...fn,
          localTypes: fn.localTypes.map((type, ordinal) => {
            if (ordinal === transcendental.destination.ordinal) return { _tag: 'f32' }
            return type
          }),
        }
      }),
    }
    assert.include(
      MirVerification.verify(malformed).map((violation) => violation.rule),
      'InvalidIntegerOperation',
    )
  }),
)
