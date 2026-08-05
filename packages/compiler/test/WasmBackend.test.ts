import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import type * as Mir from '../src/Mir.js'
import * as WasmBackend from '../src/WasmBackend.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshotOf = (text: string) =>
  Analysis.ofSource('wasm://program.silk', ascii(text), 'wasm32-unknown-unknown')

const emit = (text: string) => Analysis.codegenWasm(snapshotOf(text), { mode: 'release' })

/**
 * Instantiates the emitted module and calls its entry export, reporting a wasm trap as the
 * `'trap'` marker so trapping and value-producing programs compare uniformly.
 */
const run = Effect.fnUntraced(function* (text: string) {
  // `Uint8Array.slice` re-backs the bytes with a plain `ArrayBuffer`, which is what the
  // WebAssembly types accept; the artifact's own array is generic over `ArrayBufferLike`.
  const bytes = (yield* emit(text)).bitcode.slice()
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes), {})
  const main = instance.exports.silk_main as () => number
  try {
    return main()
  } catch {
    return 'trap'
  }
})

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

/** The bootstrap interpreter is the oracle every emission is differentially checked against. */
const interpret = (text: string): number | 'trap' => {
  const outcome = Analysis.evaluate(snapshotOf(text))
  return outcome._tag === 'Completed' ? outcome.result.value : 'trap'
}

it.effect('emits an instantiable module whose entry is exported as silk_main', () =>
  Effect.gen(function* () {
    const artifact = yield* emit('pub fn main() -> I32 { return 42 }')

    assert.strictEqual(artifact.module, 'wasm://program.silk')
    assert.deepEqual(
      artifact.symbols.map((entry) => entry.symbol),
      ['silk_main'],
    )
    // The wasm binary preamble: "\0asm" followed by version 1.
    assert.deepEqual(Array.from(artifact.bitcode.slice(0, 8)), [0, 97, 115, 109, 1, 0, 0, 0])
    assert.match(artifact.ir, /\(export "silk_main"/)
  }),
)

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const branchSource = 'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'

it.effect('matches the WAT golden and the binary digest golden', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(nestedSource)

    assert.strictEqual(artifact.ir, golden('program.wat.txt'))
    assert.strictEqual(
      `${createHash('sha256').update(artifact.bitcode).digest('hex')}\n`,
      golden('program.wasm.sha256'),
    )
  }),
)

it.effect('matches the branch WAT golden and stays deterministic', () =>
  Effect.gen(function* () {
    const first = yield* emit(branchSource)
    const second = yield* emit(branchSource)

    assert.strictEqual(first.ir, golden('branch.wat.txt'))
    assert.deepEqual(first.bitcode, second.bitcode)
  }),
)

it.effect('keeps the deterministic symbol naming the LLVM backend uses', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`)

    assert.deepEqual(
      artifact.symbols.map((entry) => entry.symbol),
      ['silk_main', 'silk_1_identity'],
    )
  }),
)

it.effect('recovers branch diamonds as structured if/else', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }',
    )

    assert.match(artifact.ir, /\bif\b/)
    assert.match(artifact.ir, /\belse\b/)
    // The structure is taken from MIR rather than rebuilt, so no dispatch scaffolding appears.
    assert.notMatch(artifact.ir, /br_table/)
    assert.notMatch(artifact.ir, /\bloop\b/)
  }),
)

it.effect('emits a bare if when only one arm exists and the join falls through', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> I32 { let x = 1 if I32.equals(x, 1) { let a = 5 } return x }',
    )

    assert.match(artifact.ir, /\bif\b/)
    assert.notMatch(artifact.ir, /\belse\b/)
  }),
)

it.effect('nests an if inside an arm for nested source conditionals', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> I32 { let x = 1 if I32.equals(x, 1) { if I32.equals(x, 1) { return 42 } return 1 } return 0 }',
    )

    // Two conditionals in the source produce two `if` constructs, one inside the other.
    assert.strictEqual(artifact.ir.match(/^\s*if$/gm)?.length, 2)
  }),
)

it.effect('rejects a CFG with a back-edge instead of emitting wrong control flow', () =>
  Effect.gen(function* () {
    const snapshot = snapshotOf('pub fn main() -> I32 { return 42 }')
    const program = Analysis.loweredMir(snapshot)
    const main = program.functions[0]
    const entry = main?.blocks[0]
    if (main === undefined || entry === undefined) {
      throw new Error('sample program lost its entry block')
    }
    // A self-jump is the smallest back-edge; structured emission cannot model it.
    const looping: Mir.Module = {
      ...program,
      functions: [
        {
          ...main,
          blocks: [
            {
              ...entry,
              terminator: {
                _tag: 'Jump',
                target: { _tag: 'Block', ordinal: 0 },
                provenance: entry.terminator.provenance,
              },
            },
          ],
        },
      ],
    }

    const failure = yield* Effect.flip(
      Backend.emit(WasmBackend.WasmBackend, looping, { mode: 'release' }),
    )
    assert.strictEqual(failure.reason._tag, 'UnsupportedMir')
    assert.include(failure.message, 'forward-only CFG')
  }),
)

it.effect('rejects native-target MIR before constructing a WebAssembly module', () =>
  Effect.gen(function* () {
    const snapshot = Analysis.ofSource(
      'wasm://native-plan.silk',
      ascii('pub fn main() -> I32 { return 42 }'),
      'aarch64-apple-darwin',
    )
    const failure = yield* Effect.flip(
      Backend.emit(WasmBackend.WasmBackend, Analysis.loweredMir(snapshot), { mode: 'release' }),
    )

    assert.strictEqual(failure.reason._tag, 'UnsupportedTarget')
    if (failure.reason._tag !== 'UnsupportedTarget') return
    assert.strictEqual(failure.reason.target, 'aarch64-apple-darwin')
  }),
)

/**
 * The `name` custom section is WebAssembly's counterpart to the LLVM backend's native debug
 * metadata, so `mode` gates it the same way that backend's `strip` flag gates DWARF.
 */
it.effect('emits the name section only for debug builds', () =>
  Effect.gen(function* () {
    const source = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`
    const debug = yield* Analysis.codegenWasm(snapshotOf(source), { mode: 'debug' })
    const release = yield* Analysis.codegenWasm(snapshotOf(source), { mode: 'release' })

    const decoder = new TextDecoder('utf8', { fatal: false })
    assert.include(decoder.decode(debug.bitcode), 'name')
    assert.match(debug.ir, /\$silk_main/)
    assert.match(debug.ir, /\(local \$scratch/)

    // Release keeps the exports — the module stays callable — but drops every internal name.
    assert.notMatch(release.ir, /\$silk_main/)
    assert.notMatch(release.ir, /\$scratch/)
    assert.match(release.ir, /\(export "silk_main"/)
    assert.isBelow(release.bitcode.length, debug.bitcode.length)
  }),
)

it.effect('runs identically whether or not names were stripped', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> I32 { return I32.add(40, 2) }'
    const instantiate = Effect.fnUntraced(function* (mode: 'debug' | 'release') {
      const bytes = (yield* Analysis.codegenWasm(snapshotOf(source), { mode })).bitcode.slice()
      const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes), {})
      return (instance.exports.silk_main as () => number)()
    })

    assert.strictEqual(yield* instantiate('debug'), 42)
    assert.strictEqual(yield* instantiate('release'), 42)
  }),
)

it.effect('maps divisions onto wasm operators that already trap, with no guard expansion', () =>
  Effect.gen(function* () {
    const artifact = yield* emit('pub fn main() -> I32 { return I32.divide(84, 2) }')

    assert.match(artifact.ir, /i32\.div_s/)
  }),
)

const programs: ReadonlyArray<readonly [string, string]> = [
  ['literal', 'pub fn main() -> I32 { return 42 }'],
  [
    'direct call',
    `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer() }`,
  ],
  [
    'nested calls',
    `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  ],
  [
    'multiple parameters',
    `pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(1, 42) }`,
  ],
  ['addition', 'pub fn main() -> I32 { return I32.add(40, 2) }'],
  ['subtraction', 'pub fn main() -> I32 { return I32.subtract(50, 8) }'],
  ['multiplication', 'pub fn main() -> I32 { return I32.multiply(6, 7) }'],
  ['division', 'pub fn main() -> I32 { return I32.divide(84, 2) }'],
  ['remainder', 'pub fn main() -> I32 { return I32.remainder(85, 43) }'],
  ['chained arithmetic', 'pub fn main() -> I32 { return I32.divide(I32.add(40, 2), 1) }'],
  ['negative results', 'pub fn main() -> I32 { return I32.subtract(0, 42) }'],
  ['branch taken', 'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'],
  ['branch not taken', 'pub fn main() -> I32 { if I32.equals(1, 2) { return 0 } return 42 }'],
  ['ordered comparison', 'pub fn main() -> I32 { if I32.lessThan(1, 2) { return 42 } return 0 }'],
  [
    'let bindings across a branch',
    'pub fn main() -> I32 { let base = 40 if I32.equals(base, 40) { let bonus = 2 return I32.add(base, bonus) } return 0 }',
  ],
  ['division by zero traps', 'pub fn main() -> I32 { return I32.divide(1, 0) }'],
  ['remainder by zero traps', 'pub fn main() -> I32 { return I32.remainder(1, 0) }'],
  ['addition overflow traps', 'pub fn main() -> I32 { return I32.add(2147483647, 1) }'],
  ['subtraction overflow traps', 'pub fn main() -> I32 { return I32.subtract(-2147483648, 1) }'],
  ['multiplication overflow traps', 'pub fn main() -> I32 { return I32.multiply(2147483647, 2) }'],
  ['unresolved call traps', 'pub fn main() -> I32 { return missing() }'],
]

for (const [name, source] of programs) {
  it.effect(`executes ${name} exactly as the bootstrap interpreter does`, () =>
    Effect.gen(function* () {
      assert.strictEqual(yield* run(source), interpret(source))
    }),
  )
}

/**
 * MIR specifies trapping signed arithmetic, but wasm's `i32.add`, `i32.sub`, and `i32.mul` wrap,
 * so the backend emits inline overflow checks. This sweeps the sign and magnitude boundaries
 * those checks turn on against exact arithmetic, which JavaScript numbers compute without loss
 * across the whole `i32` range.
 */
it.effect('traps signed overflow exactly at the i32 boundaries', () =>
  Effect.gen(function* () {
    const minimum = -2147483648
    const maximum = 2147483647
    const operands = [
      0,
      1,
      -1,
      2,
      -2,
      3,
      -3,
      46340,
      -46340,
      46341,
      -46341,
      65536,
      -65536,
      1073741824,
      -1073741824,
      maximum,
      minimum,
      maximum - 1,
      minimum + 1,
    ]
    const references: ReadonlyArray<
      readonly [string, (a: number, b: number) => number | undefined]
    > = [
      ['add', (a, b) => a + b],
      ['subtract', (a, b) => a - b],
      ['multiply', (a, b) => a * b],
      ['divide', (a, b) => (b === 0 ? undefined : Math.trunc(a / b))],
      ['remainder', (a, b) => (b === 0 ? undefined : a % b)],
    ]

    const mismatches: Array<string> = []
    for (const [operator, reference] of references) {
      for (const left of operands) {
        for (const right of operands) {
          const exact = reference(left, right)
          const traps = exact === undefined || exact > maximum || exact < minimum
          const actual = yield* run(
            `pub fn main() -> I32 { return I32.${operator}(${left}, ${right}) }`,
          )
          if (actual !== (traps ? 'trap' : exact)) {
            mismatches.push(
              `I32.${operator}(${left}, ${right}) expected ${traps ? 'trap' : exact}, got ${actual}`,
            )
          }
        }
      }
    }

    assert.deepEqual(mismatches, [])
  }),
)
