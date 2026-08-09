import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import type * as Mir from '../src/Mir.js'
import * as WasmBackend from '../src/WasmBackend.js'
import { corpus } from './support/corpus.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshotOf = (text: string) =>
  Analysis.ofSource('wasm/program', ascii(text), 'wasm32-unknown-unknown')

const emit = (text: string) =>
  Effect.flatMap(snapshotOf(text), (snapshot) =>
    Analysis.codegenWasm(snapshot, { mode: 'release' }),
  )

/**
 * Instantiates the emitted module and calls its entry export, reporting a wasm trap as the
 * `'trap'` marker so trapping and value-producing programs compare uniformly.
 */
const run = Effect.fnUntraced(function* (text: string) {
  // `Uint8Array.slice` re-backs the bytes with a plain `ArrayBuffer`, which is what the
  // WebAssembly types accept; the artifact's own array is generic over `ArrayBufferLike`.
  const bytes = (yield* emit(text)).bytes.slice()
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
const interpret = Effect.fnUntraced(function* (text: string) {
  const outcome = Analysis.evaluate(yield* snapshotOf(text))
  return outcome._tag === 'Completed' ? outcome.result.value : 'trap'
})

it.effect('emits an instantiable module whose entry is exported as silk_main', () =>
  Effect.gen(function* () {
    const artifact = yield* emit('pub fn main() -> I32 { return 42 }')

    assert.strictEqual(artifact.module, 'wasm/program')
    assert.deepEqual(
      artifact.symbols.map((entry) => entry.symbol),
      ['silk_main'],
    )
    // The wasm binary preamble: "\0asm" followed by version 1.
    assert.strictEqual(artifact._tag, 'WebAssemblyModuleArtifact')
    assert.strictEqual(artifact.backend, 'wasm')
    assert.deepEqual(Array.from(artifact.bytes.slice(0, 8)), [0, 97, 115, 109, 1, 0, 0, 0])
    assert.match(artifact.wat, /\(export "silk_main"/)
  }),
)

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const branchSource = 'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'

const matchSource = `pub struct Left { value: I32 }
pub struct Right { value: I32 }
pub fn inspect(input: Left | Right) -> I32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => I32.add(answer, 1)
    Right { value } => value
  }
}
pub fn main() -> I32 { return inspect(Left { value: 41 }) }`

it.effect('matches the WAT golden and the binary digest golden', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(nestedSource)

    assert.strictEqual(artifact.wat, golden('program.wat.txt'))
    assert.strictEqual(
      `${createHash('sha256').update(artifact.bytes).digest('hex')}\n`,
      golden('program.wasm.sha256'),
    )
  }),
)

it.effect('matches the branch WAT golden and stays deterministic', () =>
  Effect.gen(function* () {
    const first = yield* emit(branchSource)
    const second = yield* emit(branchSource)

    assert.strictEqual(first.wat, golden('branch.wat.txt'))
    assert.deepEqual(first.bytes, second.bytes)
  }),
)

it.effect('keeps the deterministic symbol naming the LLVM backend uses', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`)

    assert.deepEqual(
      artifact.symbols.map((entry) => entry.symbol),
      [
        'silk_main',
        'silk_wasm_program_identity__12_7761736d2f70726f6772616d_8_6964656e74697479_11_6275696c74696e3a493332_18_726573756c743a6275696c74696e3a493332',
      ],
    )
  }),
)

it.effect('emits source conditionals directly as structured if', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }',
    )

    assert.match(artifact.wat, /\bif\b/)
    // The structure is taken from MIR rather than rebuilt, so no dispatch scaffolding appears.
    assert.notMatch(artifact.wat, /br_table/)
    assert.notMatch(artifact.wat, /\bloop\b/)
  }),
)

it.effect('emits a bare if when only one arm exists and the join falls through', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> I32 { let x = 1 if I32.equals(x, 1) { let a = 5 } return x }',
    )

    assert.match(artifact.wat, /\bif\b/)
    assert.notMatch(artifact.wat, /\belse\b/)
  }),
)

it.effect('nests an if inside an arm for nested source conditionals', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> I32 { let x = 1 if I32.equals(x, 1) { if I32.equals(x, 1) { return 42 } return 1 } return 0 }',
    )

    // Two conditionals in the source produce two `if` constructs, one inside the other.
    assert.strictEqual(artifact.wat.match(/^\s*if$/gm)?.length, 2)
  }),
)

it.effect('emits structured match dispatch and agrees with logical evaluation', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(matchSource)
    const interpreted = yield* interpret(matchSource)
    const executed = yield* run(matchSource)

    assert.strictEqual(interpreted, 42)
    assert.strictEqual(executed, interpreted)
    assert.match(artifact.wat, /i32\.eq/)
    assert.match(artifact.wat, /\bif\b/)
    assert.notMatch(artifact.wat, /br_table/)
  }),
)

it.effect('keeps every valid match corpus case in three-engine agreement', () =>
  Effect.gen(function* () {
    for (const program of corpus.filter((candidate) => candidate.name.startsWith('match-'))) {
      assert.strictEqual(yield* run(program.source), yield* interpret(program.source), program.name)
    }
  }),
)

it.effect('rejects a structural MIR cycle before structured emission', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('pub fn main() -> I32 { return 42 }')
    const program = Analysis.loweredMir(snapshot)
    const main = program.functions[0]
    const entry = main?.regions.at(0)
    if (main === undefined || entry === undefined) {
      throw new Error('sample program lost its entry region')
    }
    const provenance =
      entry._tag === 'OperationRegion' || entry._tag === 'CleanupRegion'
        ? entry.outcome.provenance
        : entry.provenance
    const looping: Mir.Module = {
      ...program,
      functions: [
        {
          ...main,
          regions: [
            {
              _tag: 'OperationRegion',
              id: { _tag: 'Region', ordinal: 0 },
              operations: [],
              outcome: {
                _tag: 'Forward',
                target: { _tag: 'Region', ordinal: 0 },
                provenance,
              },
            },
          ],
        },
      ],
    }

    const failure = yield* Effect.flip(
      Backend.emit(WasmBackend.WasmBackend, looping, { mode: 'release' }),
    )
    assert.strictEqual(failure.reason._tag, 'InvalidMir')
    if (failure.reason._tag !== 'InvalidMir') return
    assert.strictEqual(failure.reason.violations.at(0)?.rule, 'StructuralCycle')
  }),
)

it.effect('rejects native-target MIR before constructing a WebAssembly module', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'wasm/native-plan',
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
    const debug = yield* Analysis.codegenWasm(yield* snapshotOf(source), { mode: 'debug' })
    const release = yield* Analysis.codegenWasm(yield* snapshotOf(source), { mode: 'release' })

    const decoder = new TextDecoder('utf8', { fatal: false })
    assert.include(decoder.decode(debug.bytes), 'name')
    assert.match(debug.wat, /\$silk_main/)
    assert.match(debug.wat, /\(local \$scratch/)

    // Release keeps the exports — the module stays callable — but drops every internal name.
    assert.notMatch(release.wat, /\$silk_main/)
    assert.notMatch(release.wat, /\$scratch/)
    assert.match(release.wat, /\(export "silk_main"/)
    assert.isBelow(release.bytes.length, debug.bytes.length)
  }),
)

it.effect('runs identically whether or not names were stripped', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> I32 { return I32.add(40, 2) }'
    const instantiate = Effect.fnUntraced(function* (mode: 'debug' | 'release') {
      const bytes = (yield* Analysis.codegenWasm(yield* snapshotOf(source), {
        mode,
      })).bytes.slice()
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

    assert.match(artifact.wat, /i32\.div_s/)
  }),
)

const programs: ReadonlyArray<readonly [string, string]> = [
  [
    'nested loops',
    `pub fn main() -> I32 {
  let mut outer = 0
  let mut total = 0
  while outer < 6 {
    let mut inner = 0
    while inner < 7 { total = total + 1 inner = inner + 1 }
    outer = outer + 1
  }
  return total
}`,
  ],
  [
    'mutable struct loop',
    `struct Pair { left: I32 right: I32 }
pub fn main() -> I32 {
  let mut pair = Pair { left: 0, right: 40 }
  while pair.left < 2 { pair.left = pair.left + 1 }
  return pair.left + pair.right
}`,
  ],
  [
    'mutable scalar loop',
    `pub fn main() -> I32 {
  let mut count = 0
  while count < 42 { count = count + 1 }
  return count
}`,
  ],
  [
    'mutable array loop',
    `pub fn main() -> I32 {
  let mut values = [40, 0]
  let mut index = 0
  while index < 2 {
    values[index] = values[index] + 1
    index = index + 1
  }
  return values[0] + values[1]
}`,
  ],
  [
    'loop continue and break',
    `pub fn main() -> I32 {
  let mut index = 0
  while index < 50 {
    index = index + 1
    if index == 2 { continue }
    if index == 42 { break }
  }
  return index
}`,
  ],
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
  ['operator precedence', 'pub fn main() -> I32 { return 2 + 3 * 4 }'],
  ['operator pipeline', 'pub fn main() -> I32 { return 2 |> I32.add(3) |> I32.multiply(4) }'],
  ['operator negation', 'pub fn main() -> I32 { return -(40 + 2) }'],
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
  ['inferred fixed array', 'pub fn main() -> I32 { let values = [10, 42] return values[1] }'],
  [
    'dynamic fixed array index',
    `fn choose(values: [I32; 3], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([10, 42, 90], 1) }`,
  ],
  [
    'nested fixed array index',
    `fn choose(values: [[I32; 2]; 2], outer: I32, inner: I32) -> I32 { return values[outer][inner] }
pub fn main() -> I32 { return choose([[10, 11], [42, 43]], 1, 0) }`,
  ],
  [
    'indexed struct array field',
    `struct Pair { left: I32 right: I32 }
fn choose(values: [Pair; 2], index: I32) -> I32 { return values[index].left }
pub fn main() -> I32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
  ],
  [
    'negative array index traps',
    `fn choose(values: [I32; 2], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([10, 42], -1) }`,
  ],
  [
    'upper array index traps',
    `fn choose(values: [I32; 2], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([10, 42], 2) }`,
  ],
  [
    'zero-length array index traps',
    `fn choose(values: [I32; 0], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([], 0) }`,
  ],
  ['division by zero traps', 'pub fn main() -> I32 { return I32.divide(1, 0) }'],
  ['remainder by zero traps', 'pub fn main() -> I32 { return I32.remainder(1, 0) }'],
  ['addition overflow traps', 'pub fn main() -> I32 { return I32.add(2147483647, 1) }'],
  ['subtraction overflow traps', 'pub fn main() -> I32 { return I32.subtract(-2147483648, 1) }'],
  ['multiplication overflow traps', 'pub fn main() -> I32 { return I32.multiply(2147483647, 2) }'],
]

for (const [name, source] of programs) {
  it.effect(`executes ${name} exactly as the bootstrap interpreter does`, () =>
    Effect.gen(function* () {
      assert.strictEqual(yield* run(source), yield* interpret(source))
    }),
  )
}

/**
 * MIR specifies trapping signed arithmetic, but wasm's `i32.add`, `i32.sub`, and `i32.mul` wrap,
 * so the backend emits inline overflow checks. This sweeps the sign and magnitude boundaries
 * those checks turn on against exact arithmetic, which JavaScript numbers compute without loss
 * across the whole `i32` range.
 */
it.effect(
  'traps signed overflow exactly at the i32 boundaries',
  () =>
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
  15_000,
)
