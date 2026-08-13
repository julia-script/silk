import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const decoder = new TextDecoder()

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-typed-count-acceptance-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/** The five modules that used to copy a private `counted` identity to type their own literals. */
const previousHolders = [
  'silk/vector',
  'silk/bytes',
  'silk/string',
  'silk/filesystem',
  'silk/os_filesystem',
] as const

const sourceText = (module: string): string => {
  const bytes = Stdlib.sources.get(module)
  assert.isDefined(bytes, module)
  return decoder.decode(bytes)
}

/**
 * `silk/usize` owns the shared typed zero and one, so no module has to reintroduce an identity
 * call to keep a bare count off the `i32` default.
 */
it('declares the shared usize counts once and ships no private counted identity', () => {
  for (const entry of Stdlib.manifest)
    assert.notMatch(
      sourceText(entry.module),
      /\bfn\s+counted\b/,
      `${entry.module} still declares a counted identity`,
    )

  const usize = sourceText('silk/usize')
  assert.include(usize, 'pub const ZERO: usize = 0')
  assert.include(usize, 'pub const ONE: usize = 1')

  for (const module of previousHolders)
    assert.match(
      sourceText(module),
      /\busize\.(ZERO|ONE)\b/,
      `${module} names no shared typed count`,
    )
})

/**
 * The vector program that exercised the identity call the hardest. Its observable result and its
 * lowered counts are the evidence that replacing `counted(0)` with `usize.ZERO` moved no value:
 * the two struct fields still receive the same `usize` zero, now as a direct typed immediate.
 */
const growth = `import silk.vector { Vector, make, append, get, length, capacity }

effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<i32>()
  let pending0 = append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<i32>(&mut values, 32) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  if length<i32>(&values) == 2 {} else { return 0 }
  if capacity<i32>(&values) == 4 {} else { return 1 }
  return get<i32>(&values, 0) + get<i32>(&values, 1)
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

it.effect(
  'lowers a vector program to the same typed counts on all three engines with no identity call',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'typed-count-acceptance/growth',
        ascii(growth),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const mir = Analysis.loweredMir(snapshot)
      // No identity survives, neither as a lowered function nor as a call at any use site.
      assert.deepEqual(
        mir.functions.filter((fn) => fn.id.name === 'counted').map((fn) => fn.id.module),
        [],
      )
      assert.isFalse(
        mir.functions.some((fn) =>
          Mir.operations(fn).some(
            (operation) => operation._tag === 'Call' && operation.target.name === 'counted',
          ),
        ),
      )

      // `make` still builds the same value: an empty storage union plus two `usize` zeroes. The
      // identity call that produced each zero is gone; the typed immediate it returned is not.
      const made = mir.functions.find(
        (fn) => fn.id.module === 'silk/vector' && fn.id.name === 'make',
      )
      assert.isDefined(made)
      if (made === undefined) return
      assert.deepEqual(
        Mir.operations(made).map((operation) => operation._tag),
        ['ConstructArray', 'Construct', 'ConvertUnion', 'Literal', 'Literal', 'Construct'],
      )
      assert.deepEqual(
        Mir.operations(made).flatMap((operation) =>
          operation._tag === 'Literal'
            ? [`${operation.value.toString()} : ${operation.type._tag}`]
            : [],
        ),
        ['0 : usize', '0 : usize'],
      )

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('typed-count-acceptance/growth', ascii(growth)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'growth'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
  60_000,
)

/**
 * A UTF-8 walk reaches the counts the shared constants do not name — the two, three, and four byte
 * widths. Every one of them still carries `usize`, so the removal left no literal on the `i32`
 * default.
 */
const scalars = `import silk.string {
  ScalarCursor,
  ScalarStep,
  copy,
  view,
  scalarCursor,
  nextScalar,
  scalarValue,
  nextCursor,
  fromUtf8,
  utf8Bytes
}
import silk.option { Some, None }

fn scalarSum(value: string, cursor: ScalarCursor) -> u32 {
  return match move nextScalar(value, move cursor) {
    Some<ScalarStep> { value: step } => continueSum(value, move step)
    None nothing => u32.toU32(0)
  }
}

fn continueSum(value: string, step: ScalarStep) -> u32 {
  let scalar = scalarValue(&step)
  let cursor = nextCursor(move step)
  return scalar + scalarSum(value, move cursor)
}

effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let copying = copy("A\\u{a2}\\u{20ac}\\u{10348}") |> Effect.provideMut(&mut allocator)
  let mut owned = run copying
  let borrowed = view(&owned)
  let decoded = fromUtf8(utf8Bytes(borrowed))
  drop decoded
  if scalarSum(borrowed, scalarCursor()) == u32.toU32(74967) {} else { return 1 }
  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

it.effect('types every reachable standard-library count as usize, never as the i32 default', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'typed-count-acceptance/scalars',
      ascii(scalars),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const plan = Analysis.layoutOf(snapshot)
    assert.strictEqual(plan._tag, 'Available')
    if (plan._tag !== 'Available') return
    const verdicts = plan.value.literalVerdicts
    assert.isAbove(verdicts.length, 0)
    assert.deepEqual(
      verdicts.filter((verdict) => verdict._tag !== 'AvailableUsizeLiteral'),
      [],
    )

    const counts = (module: string): ReadonlyArray<string> =>
      [
        ...new Set(
          verdicts.flatMap((verdict) =>
            verdict.span.sourceId === module ? [verdict.value.toString()] : [],
          ),
        ),
      ].sort()

    // The shared constants carry the zero and one, and `MAX` carries the largest usize of the
    // target this snapshot selected — the 32-bit one, so the bound ranges at 4294967295 rather
    // than at the wider value a 64-bit target would give it.
    assert.deepEqual(counts('silk/usize'), ['0', '1', '4294967295'])
    // Every width the constants do not name is still a usize literal inside silk/string.
    assert.deepEqual(counts('silk/string'), ['2', '3', '4'])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
