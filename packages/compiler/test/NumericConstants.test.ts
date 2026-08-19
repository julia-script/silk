import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as Scalar from '../src/Scalar.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
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

const integerSpellings = fixedWidthIntegers.map((scalar) => scalar.spelling)

const floatSpellings = ['f32', 'f64'] as const

/** The conversion that types a one for a given width, since a bare literal keeps the i32 default
 * even in an argument position. */
const typedOne = (spelling: string): string =>
  `i32.to${spelling[0]?.toUpperCase() ?? ''}${spelling.slice(1)}(1)`

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

const widthOf = (spelling: string): number => {
  const scalar = fixedWidthIntegers.find((candidate) => candidate.spelling === spelling)
  assert.isDefined(scalar, spelling)
  return scalar === undefined ? 0 : Scalar.bits(scalar, 64)
}

/**
 * Requirements 1, 2, 5 and 6. Every bound is compared against `Scalar.range`, which is the same
 * table the checked intrinsics test against in both `Backend` and `BootstrapEvaluation`, so a
 * constant cannot drift away from the bound the checked path actually enforces.
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

/**
 * One checked-overflow probe per integer type. A step past `MAX` and a step below `MIN` must both
 * refuse, and the same step taken from inside the bound must succeed — so the probes fail both on a
 * bound that is too small and on one that is too large.
 */
const checkedCases = integerSpellings.flatMap((spelling, ordinal) => [
  {
    name: `pastMax${ordinal}`,
    body: `match move ${spelling}.checkedAdd(${spelling}.MAX, ${typedOne(spelling)}) { Some<${spelling}> { value: result } => 0 None nothing => 42 }`,
  },
  {
    name: `belowMin${ordinal}`,
    body: `match move ${spelling}.checkedSubtract(${spelling}.MIN, ${typedOne(spelling)}) { Some<${spelling}> { value: result } => 0 None nothing => 42 }`,
  },
  {
    name: `insideBound${ordinal}`,
    body: `match move ${spelling}.checkedAdd(${spelling}.MIN, ${typedOne(spelling)}) { Some<${spelling}> { value: result } => 42 None nothing => 0 }`,
  },
])

const acceptanceCases = [
  ...checkedCases.map((probe) => `fn ${probe.name}() -> i32 { return ${probe.body} }`),
  ...integerSpellings.map(
    (spelling, ordinal) =>
      `fn width${ordinal}() -> i32 { if ${spelling}.BITS == ${widthOf(spelling)} { return 42 } return 0 }`,
  ),
  ...floatSpellings.flatMap((spelling) =>
    Object.entries(floatBits[spelling]).map(
      ([name, expected]) =>
        `fn bits${spelling}${name}() -> i32 { if ${spelling}.toBits(${spelling}.${name}) == ${expected.toString()} { return 42 } return 0 }`,
    ),
  ),
]

const probeNames = acceptanceCases.map((declared) => declared.slice(3, declared.indexOf('(')))

const acceptance = `import silk.option { Some, None }

${acceptanceCases.join('\n')}

fn verify(value: i32) -> () { if value != 42 { let boom = 1 / 0 } }

pub fn main() -> i32 {
${probeNames.map((name) => `  let checked${name} = verify(${name}())`).join('\n')}
  return 42
}`

/**
 * The remaining acceptance criteria: `i32.MAX` plus one must refuse in the checked path, and every
 * constant must carry one value across the evaluator, the Wasm backend and the native LLVM backend.
 * Each probe answers 42 and `verify` divides by zero on any other answer, so a single disagreeing
 * constant fails the whole program on whichever engine disagrees.
 */
it.effect(
  'gives every numeric constant the same value on all three engines',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'numeric-constants/acceptance',
        ascii(acceptance),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('numeric-constants/acceptance', ascii(acceptance)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'acceptance'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
  60_000,
)
