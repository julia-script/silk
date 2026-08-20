import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as Scalar from '../src/Scalar.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Target from '../src/Target.js'
import * as TargetConstant from '../src/TargetConstant.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const decoder = new TextDecoder()

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-target-constants-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/** One 32-bit and one 64-bit target, which is the whole reason a single literal cannot serve. */
const wasm32 = 'wasm32-unknown-unknown'
const native64 = 'x86_64-unknown-linux-gnu'

const sourceText = (spelling: string): string => {
  const bytes = Stdlib.sources.get(`silk/${spelling}`)
  assert.isDefined(bytes, `silk/${spelling}`)
  return decoder.decode(bytes)
}

/**
 * Reads one declaration back as its declared type and its initializer. The pattern admits a single
 * token on the right of the `=`, so a constant written as an expression fails to match — the
 * one-initializer shape `SEM0086` enforces is unchanged by the target-dependent form.
 */
const declaration = (spelling: string, name: string): { type: string; initializer: string } => {
  const matched = sourceText(spelling).match(
    new RegExp(`^pub const ${name}: (\\S+) = (\\S+)$`, 'm'),
  )
  assert.isNotNull(matched, `silk/${spelling} declares no explicitly typed single-term ${name}`)
  return { type: matched?.[1] ?? '', initializer: matched?.[2] ?? '' }
}

const integer = (spelling: 'usize' | 'isize'): Scalar.IntegerScalar => {
  const scalar = Scalar.find(spelling)
  assert.isTrue(scalar?.category === 'Integer', spelling)
  if (scalar?.category !== 'Integer') throw new Error(spelling)
  return scalar
}

/**
 * The first acceptance criterion, read off the source. `usize.MIN` is the one bound that stays a
 * literal: an unsigned floor is zero at every pointer width, so nothing is selected for it.
 */
it('declares every pointer-width bound as a target fact and every fixed bound as a literal', () => {
  assert.deepEqual(declaration('usize', 'MAX'), {
    type: 'usize',
    initializer: 'Target.usizeMax',
  })
  assert.deepEqual(declaration('usize', 'MIN'), { type: 'usize', initializer: '0' })
  assert.deepEqual(declaration('usize', 'BITS'), {
    type: 'u32',
    initializer: 'Target.pointerBits',
  })
  assert.deepEqual(declaration('isize', 'MAX'), {
    type: 'isize',
    initializer: 'Target.isizeMax',
  })
  assert.deepEqual(declaration('isize', 'MIN'), {
    type: 'isize',
    initializer: 'Target.isizeMin',
  })
  assert.deepEqual(declaration('isize', 'BITS'), {
    type: 'u32',
    initializer: 'Target.pointerBits',
  })
})

/**
 * Nothing is evaluated here: each selector reads the same `Scalar.range` table the checked
 * intrinsics enforce, at the pointer width the target supplies. The final assertion states the
 * premise of the whole issue — the two widths disagree on every bound but the unsigned floor.
 */
it('selects each bound from the table the checked intrinsics enforce', () => {
  for (const bits of [32, 64] as const) {
    assert.strictEqual(
      TargetConstant.value('usizeMax', bits),
      Scalar.range(integer('usize'), bits).maximum,
    )
    assert.strictEqual(
      TargetConstant.value('isizeMax', bits),
      Scalar.range(integer('isize'), bits).maximum,
    )
    assert.strictEqual(
      TargetConstant.value('isizeMin', bits),
      Scalar.range(integer('isize'), bits).minimum,
    )
    assert.strictEqual(TargetConstant.value('pointerBits', bits), BigInt(bits))
  }
  for (const selector of TargetConstant.all) {
    assert.notStrictEqual(
      TargetConstant.value(selector, 32),
      TargetConstant.value(selector, 64),
      `${selector} would be spellable as one literal`,
    )
  }
})

/** Every canonical target words its pointers at one of the two widths the selectors cover. */
it('covers the pointer width of every canonical target', () => {
  const widths = new Set(Target.all.map((target) => TargetConstant.pointerBits(target)))
  assert.deepEqual([...widths].sort(), [32, 64])
  assert.strictEqual(TargetConstant.pointerBits(Target.wasm32UnknownUnknown), 32)
  for (const target of Target.native) assert.strictEqual(TargetConstant.pointerBits(target), 64)
})

const mirProgram = `import silk.isize as isize
import silk.usize as usize
fn largestUsize() -> usize { return usize.MAX }
fn largestIsize() -> isize { return isize.MAX }
fn smallestIsize() -> isize { return isize.MIN }
fn pointerBits() -> u32 { return usize.BITS }

pub fn main() -> i32 {
  let max = largestUsize()
  let top = largestIsize()
  let bottom = smallestIsize()
  let width = pointerBits()
  return 42
}`

const mirLiterals = (snapshot: Analysis.Snapshot): ReadonlyArray<bigint> =>
  Analysis.loweredMir(snapshot)
    .functions.flatMap((fn) => Mir.operations(fn))
    .flatMap((operation) => (operation._tag === 'Literal' ? [BigInt(operation.value)] : []))

/**
 * Lowering is where the selection happens, and every engine reads the MIR it produces, so this is
 * the evidence that the value narrowed rather than that three engines agreed on a wrong one. The
 * absence assertion is the important half: a 64-bit bound reaching a 32-bit program is exactly the
 * silent failure the target-independent value would cause if lowering ever stopped replacing it.
 */
it.effect('lowers each pointer-width bound to the selected target and to nothing wider', () =>
  Effect.gen(function* () {
    for (const [target, bits] of [
      [wasm32, 32],
      [native64, 64],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        'target-constants/mir',
        ascii(mirProgram),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], target)
      const literals = new Set(mirLiterals(snapshot))
      const other = bits === 32 ? 64 : 32
      for (const selector of TargetConstant.all) {
        assert.isTrue(
          literals.has(TargetConstant.value(selector, bits)),
          `${target} lowers no literal for ${selector}`,
        )
        assert.isFalse(
          literals.has(TargetConstant.value(selector, other)),
          `${target} lowers the ${other}-bit value of ${selector}`,
        )
      }
    }
  }),
)

/**
 * `SEM0086` still admits one initializer only; `Target.<fact>` joins the literals as an accepted
 * one, and nothing else does. The wrong-type case matters because a selector names a value and its
 * type together — `usize.MAX` at `isize` would silently be the wrong bound.
 */
it.effect('rejects an unknown target fact and a target fact at the wrong type', () =>
  Effect.gen(function* () {
    const rejected = [
      { name: 'unknown', source: 'pub const WIDTH: u32 = Target.pointerBytes' },
      { name: 'mistyped', source: 'pub const WIDTH: i32 = Target.pointerBits' },
      { name: 'bound-mistyped', source: 'pub const BOUND: isize = Target.usizeMax' },
      { name: 'computed', source: 'pub const WIDTH: u32 = Target.pointerBits + 1' },
    ]
    for (const probe of rejected) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `target-constants/${probe.name}`,
        ascii(`${probe.source}\n\npub fn main() -> i32 { return 42 }\n`),
      )
      const codes = Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)
      assert.include(codes, 'SEM0086', probe.name)
    }
  }),
)

/**
 * `Target` is recognized on syntax alone, in one position. A field projection was never an accepted
 * constant initializer, so no program that analyzes today changes meaning; and outside an
 * initializer the spelling stays exactly as unresolvable as it was.
 */
it.effect('leaves the target root unresolvable outside a constant initializer', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'target-constants/body',
      ascii('pub fn main() -> i32 { return Target.pointerBits }\n'),
    )
    const codes = Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)
    assert.isNotEmpty(codes)
    assert.notInclude(codes, 'SEM0086')
  }),
)

/**
 * Realizing any program pulls the whole standard library through elaboration and through the
 * target-aware `usize` range check, so a clean diagnostic set on both widths is the evidence that
 * the new declarations are accepted everywhere — including on the 32-bit target, where the widest
 * bound would otherwise be reported out of range as `LAY0001`.
 */
it.effect('reports no diagnostic for the new stdlib declarations on either width', () =>
  Effect.gen(function* () {
    for (const target of [wasm32, native64]) {
      const snapshot = yield* Analysis.ofSourceRealized(
        'target-constants/clean',
        ascii('pub fn main() -> i32 { return 42 }'),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], target)
    }
  }),
)

/**
 * Every probe is an identity that holds at any pointer width but pins the exact bound at the width
 * actually selected: `MAX` is all ones, one past it wraps to zero and refuses the checked step,
 * `isize.MAX` is the complement of `isize.MIN`, and shifting `MAX` right by `BITS - 1` leaves one,
 * which ties `BITS` to the bound rather than letting the two drift apart.
 */
const probes = [
  'fn usizeMaxIsAllOnes() -> i32 { if usize.MAX == usize.bitNot(usize.ZERO) { return 42 } return 0 }',
  'fn usizeMaxWrapsToZero() -> i32 { if usize.wrappingAdd(usize.MAX, usize.ONE) == usize.ZERO { return 42 } return 0 }',
  'fn usizeMaxRefusesStep() -> i32 { return match move usize.checkedAdd(usize.MAX, usize.ONE) { Some<usize> { value: result } => 0 None nothing => 42 } }',
  'fn usizeMinIsZero() -> i32 { if usize.MIN == usize.ZERO { return 42 } return 0 }',
  'fn usizeMinRefusesStep() -> i32 { return match move usize.checkedSubtract(usize.MIN, usize.ONE) { Some<usize> { value: result } => 0 None nothing => 42 } }',
  'fn usizeBitsPinsMax() -> i32 { if usize.shiftRight(usize.MAX, usize.subtract(u32.toUsize(usize.BITS), usize.ONE)) == usize.ONE { return 42 } return 0 }',
  'fn isizeMaxComplementsMin() -> i32 { if isize.MAX == isize.bitNot(isize.MIN) { return 42 } return 0 }',
  'fn isizeMaxWrapsToMin() -> i32 { if isize.wrappingAdd(isize.MAX, i32.toIsize(1)) == isize.MIN { return 42 } return 0 }',
  'fn isizeMaxRefusesStep() -> i32 { return match move isize.checkedAdd(isize.MAX, i32.toIsize(1)) { Some<isize> { value: result } => 0 None nothing => 42 } }',
  'fn isizeMinRefusesStep() -> i32 { return match move isize.checkedSubtract(isize.MIN, i32.toIsize(1)) { Some<isize> { value: result } => 0 None nothing => 42 } }',
  'fn isizeBitsMatchUsize() -> i32 { if isize.BITS == usize.BITS { return 42 } return 0 }',
]

const probeNames = probes.map((declared) => declared.slice(3, declared.indexOf('(')))

/**
 * The program answers the selected target's pointer width, so the same source is a different
 * observation on a 32-bit target than on a 64-bit one — which is the acceptance criterion #38 had
 * to drop. Every identity above must hold first: `verify` divides by zero on any other answer, so a
 * single disagreeing bound fails the whole program on whichever engine disagrees.
 */
const acceptance = `import silk.i32 as i32
import silk.isize as isize
import silk.u32 as u32
import silk.usize as usize
import silk.option { Some, None }

${probes.join('\n')}

fn verify(value: i32) -> () { if value != 42 { let boom = 1 / 0 } }

pub fn main() -> i32 {
${probeNames.map((name) => `  let checked${name} = verify(${name}())`).join('\n')}
  return u32.toI32(usize.BITS)
}`

/**
 * The last acceptance criterion, on both widths. The 32-bit target runs on the evaluator and the
 * Wasm backend; the 64-bit target runs on the evaluator and the native LLVM backend. Each engine
 * answers the pointer width of the target it compiled for.
 */
it.effect(
  'answers the selected pointer width on every engine that can reach it',
  () =>
    Effect.gen(function* () {
      const thirtyTwo = yield* Analysis.ofSourceRealized(
        'target-constants/acceptance32',
        ascii(acceptance),
        wasm32,
      )
      assert.deepEqual(Analysis.diagnostics(thirtyTwo), [])

      const evaluated32 = Analysis.evaluate(thirtyTwo)
      assert.strictEqual(evaluated32._tag, 'Completed')
      if (evaluated32._tag !== 'Completed') return
      assert.strictEqual(evaluated32.result.value, 32n)

      const wasm = yield* Analysis.codegenWasm(thirtyTwo, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 32)

      const sixtyFour = yield* Analysis.ofSourceRealized(
        'target-constants/acceptance64',
        ascii(acceptance),
        native64,
      )
      assert.deepEqual(Analysis.diagnostics(sixtyFour), [])

      const evaluated64 = Analysis.evaluate(sixtyFour)
      assert.strictEqual(evaluated64._tag, 'Completed')
      if (evaluated64._tag !== 'Completed') return
      assert.strictEqual(evaluated64.result.value, 64n)

      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('target-constants/acceptance', ascii(acceptance)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'acceptance'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 64, run.stderr)
    }),
  60_000,
)
