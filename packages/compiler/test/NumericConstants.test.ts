import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as Scalar from '../src/Scalar.js'
import * as Stdlib from '../src/Stdlib.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const decoder = new TextDecoder()

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-numeric-constants-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/**
 * The eight fixed-width integer modules, taken from the catalog rather than spelled out, so a
 * later integer type inherits the same demands. `usize` and `isize` carry a pointer width, so no
 * one literal can be correct for every target; their constants wait on a target-dependent constant
 * mechanism and are deliberately absent here.
 */
const fixedWidthIntegers = Scalar.integers().filter((scalar) => scalar.width._tag === 'FixedWidth')

const floatSpellings = ['f32', 'f64'] as const

const sourceText = (spelling: string): string => {
  const bytes = Stdlib.sources.get(`silk/${spelling}`)
  assert.isDefined(bytes, `silk/${spelling}`)
  return decoder.decode(bytes)
}

/**
 * Reads one declaration back as its declared type and its literal spelling. The pattern admits a
 * single literal only, so a constant that is missing, that carries no explicit type, or that is
 * written as an expression fails to match.
 */
const declaration = (spelling: string, name: string): { type: string; literal: string } => {
  const matched = sourceText(spelling).match(
    new RegExp(`^pub const ${name}: (\\S+) = (\\S+)$`, 'm'),
  )
  assert.isNotNull(matched, `silk/${spelling} declares no explicitly typed single-literal ${name}`)
  return { type: matched?.[1] ?? '', literal: matched?.[2] ?? '' }
}

/**
 * Requirements 1, 2, 5 and 6. Every bound is compared against `Scalar.range`, which is the same
 * table the checked intrinsics test against in `Backend`, so a constant cannot drift away from the
 * bound the checked path actually enforces.
 */
it('declares every fixed-width integer bound at the value the checked intrinsics enforce', () => {
  assert.strictEqual(fixedWidthIntegers.length, 8)
  for (const scalar of fixedWidthIntegers) {
    const spelling = scalar.spelling
    const range = Scalar.range(scalar, 64)

    const max = declaration(spelling, 'MAX')
    assert.strictEqual(max.type, spelling, `${spelling}.MAX is not typed ${spelling}`)
    assert.strictEqual(BigInt(max.literal), range.maximum, `${spelling}.MAX`)

    const min = declaration(spelling, 'MIN')
    assert.strictEqual(min.type, spelling, `${spelling}.MIN is not typed ${spelling}`)
    assert.strictEqual(BigInt(min.literal), range.minimum, `${spelling}.MIN`)

    const bits = declaration(spelling, 'BITS')
    assert.strictEqual(bits.type, 'u32', `${spelling}.BITS is not typed u32`)
    assert.strictEqual(Number(bits.literal), Scalar.bits(scalar, 64), `${spelling}.BITS`)
  }
})

/** The IEEE 754 encoding each float constant must carry. */
const floatBits = {
  f64: {
    MAX: 0x7fefffffffffffffn,
    MIN: 0xffefffffffffffffn,
    EPSILON: 0x3cb0000000000000n,
    INFINITY: 0x7ff0000000000000n,
    PI: 0x400921fb54442d18n,
    E: 0x4005bf0a8b145769n,
  },
  f32: {
    MAX: 0x7f7fffffn,
    MIN: 0xff7fffffn,
    EPSILON: 0x34000000n,
    INFINITY: 0x7f800000n,
    PI: 0x40490fdbn,
    E: 0x402df854n,
  },
} as const

/**
 * Requirements 3, 4 and 5, plus the exact-bits demand on `INFINITY`. `INFINITY` has no literal
 * spelling of its own: it is written as a magnitude that overflows the type, and `fromDecimal`
 * rounds that overflow to the infinity encoding. Pinning the whole bit pattern here — rather than
 * only that the value is infinite — is what stops a change to that rounding from passing silently.
 */
it('encodes every float constant as the declared literal rounds', () => {
  for (const spelling of floatSpellings) {
    const width = spelling === 'f32' ? 32 : 64
    for (const [name, expected] of Object.entries(floatBits[spelling])) {
      const declared = declaration(spelling, name)
      assert.strictEqual(declared.type, spelling, `${spelling}.${name} is not typed ${spelling}`)
      const encoded = FloatingPoint.fromDecimal(declared.literal, width)
      assert.isDefined(encoded, `${spelling}.${name} spelling ${declared.literal} does not encode`)
      assert.strictEqual(
        encoded === undefined ? undefined : BigInt(encoded.bits),
        expected,
        `${spelling}.${name}`,
      )
    }
  }
})

/**
 * An all-ones exponent over a nonzero fraction is unreachable from any decimal spelling, so `NAN`
 * ships with the target-dependent constants instead of here. Naming the absence keeps a later
 * reader from reading it as an oversight.
 */
it('declares no NAN, which no literal spelling can produce', () => {
  for (const spelling of floatSpellings)
    assert.notMatch(sourceText(spelling), /^pub const NAN:/m, `silk/${spelling}`)
})

/**
 * Requirement 8 and the last acceptance criterion. `SEM0086` rejects any constant initializer that
 * is not one literal, and realizing a program pulls the whole standard library through elaboration,
 * so a clean diagnostic set is the evidence that every new declaration obeys the one-literal rule.
 */
it.effect('reports no invalid-constant diagnostic for any stdlib declaration', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'numeric-constants/clean',
      ascii('pub fn main() -> i32 { return 42 }'),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.code === 'SEM0086'),
      [],
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)
