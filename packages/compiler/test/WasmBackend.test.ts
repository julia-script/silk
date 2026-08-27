import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as WasmBackend from '../src/WasmBackend.js'
import { corpus, scalarEnumLaneAcceptance } from './support/corpus.js'
import * as WasmMain from './support/WasmMain.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshotOf = (text: string) =>
  Analysis.ofSourceRealized('wasm/program', ascii(text), 'wasm32-unknown-unknown')

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

const binaryOperation = Effect.fnUntraced(function* (operator: string) {
  const artifact = yield* emit(`import silk.i32 as i32
pub fn calculate(left: i32, right: i32) -> i32 {
  return i32.${operator}(left, right)
}
pub fn main() -> i32 { return calculate(0, 1) }`)
  const symbol = artifact.symbols.find((entry) => entry.declaration.name === 'calculate')?.symbol
  if (symbol === undefined) throw new Error(`Wasm artifact omitted i32.${operator}`)
  const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
  const calculate = instance.exports[symbol]
  if (typeof calculate !== 'function') throw new Error(`Wasm export ${symbol} is not callable`)
  return (left: number, right: number): number => calculate(left, right)
})

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

/** The bootstrap interpreter is the oracle every emission is differentially checked against. */
const interpret = Effect.fnUntraced(function* (text: string) {
  const outcome = Analysis.evaluate(yield* snapshotOf(text))
  return outcome._tag === 'Completed' ? Number(outcome.result.value) : 'trap'
})

it.effect('emits an instantiable module whose entry is exported as silk_main', () =>
  Effect.gen(function* () {
    const artifact = yield* emit('pub fn main() -> i32 { return 42 }')

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

it.effect('lowers scalar enums through verified i32 and i64 lanes and exact narrow storage', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf(scalarEnumLaneAcceptance)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.strictEqual(yield* WasmMain.invoke(artifact.bytes, 'WasmBackend.scalarEnums'), 42)
    assert.match(artifact.wat, /i32\.load8_s/)
    assert.match(artifact.wat, /i32\.load8_u/)
    assert.match(artifact.wat, /i32\.store8/)
    assert.match(artifact.wat, /i64\.const 4294967297/)
    assert.match(artifact.wat, /i64\.eq/)

    const wordSnapshot = yield* snapshotOf(scalarEnumWordStorage)
    assert.deepEqual(Analysis.diagnostics(wordSnapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(wordSnapshot)), [])
    const wordArtifact = yield* Analysis.codegenWasm(wordSnapshot, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(wordArtifact.bytes, 'WasmBackend.scalarEnumWords'),
      42,
    )
    assert.match(wordArtifact.wat, /i32\.load16_s/)
    assert.match(wordArtifact.wat, /i32\.load16_u/)
    assert.match(wordArtifact.wat, /i32\.store16/)
  }),
)

it.effect('does not transfer borrow roots through a plain aggregate call result', () =>
  Effect.gen(function* () {
    const source = `struct Cell { value: i32 }
struct Token { value: i32 }
fn touch(cell: &mut Cell) -> Token {
  cell.value = 2
  return Token { value: 0 }
}
fn consume(token: Token) -> i32 { return token.value }
pub fn main() -> i32 {
  let mut cell = Cell { value: 1 }
  let token = touch(&mut cell)
  cell.value = 5
  let ignored = consume(move token)
  return cell.value + ignored
}`
    const snapshot = yield* snapshotOf(source)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', evaluated._tag)
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 5n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.strictEqual(yield* WasmMain.invoke(wasm.bytes, 'WasmBackend.invokePlainAggregate'), 5)
  }),
)

it.effect('preserves borrow roots carried by returned Effect environment lanes', () =>
  Effect.gen(function* () {
    const source = `import silk.effect as Effect
struct Token { value: i32 }
service Counter { effect fn read(token: &mut Token) -> i32 ? &mut Counter }
struct Cell { value: i32 }
effect fn read(self: &mut Cell, token: &mut Token) -> i32 {
  self.value = self.value + 1
  return self.value + token.value
}
impl Counter for Cell { read: Cell.read }
effect fn request(token: &mut Token) -> i32 ? &mut Counter {
  return run Counter.read(move token)
}
pub fn main() -> i32 {
  let mut token = Token { value: 0 }
  let mut cell = Cell { value: 41 }
  let pending = Effect.provideMut(request(&mut token), &mut cell)
  return run move pending
}`
    const snapshot = yield* snapshotOf(source)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const module = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(module), [])
    assert.isTrue(
      module.functions.some((fn) =>
        MirVerification.operations(fn).some(
          (operation) =>
            operation._tag === 'WritePlace' &&
            fn.localTypes.at(operation.root.ordinal)?._tag === 'Reference',
        ),
      ),
    )
    assert.isTrue(
      module.functions.some((fn) =>
        fn.localTypes.some(
          (type) =>
            type._tag === 'EffectValue' &&
            type.environment.fields.some(
              (field) => typeof field.type !== 'string' && field.type._tag === 'ReferenceType',
            ),
        ),
      ),
    )

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', evaluated._tag)
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.strictEqual(yield* WasmMain.invoke(wasm.bytes, 'WasmBackend.invokeBorrowedEffect'), 42)
  }),
)

const nestedSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`

const branchSource =
  'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }'

const scalarEnumWordStorage = `enum(i16) SignedWord { Negative = -300, Positive = 300 }
enum(u16) UnsignedWord { Low = 2, High = 60000 }
struct StoredWords { signed: SignedWord unsigned: UnsignedWord }
fn inspect(words: &StoredWords) -> i32 {
  if words.signed != SignedWord.Negative { return 1 }
  if words.unsigned != UnsignedWord.High { return 2 }
  return 42
}
pub fn main() -> i32 {
  let words = StoredWords { signed: SignedWord.Negative, unsigned: UnsignedWord.High }
  return inspect(&words)
}`

const matchSource = `import silk.i32 as i32
pub struct Left { value: i32 }
pub struct Right { value: i32 }
pub fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => i32.add(answer, 1)
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 41 }) }`

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
    const artifact = yield* emit(`pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`)

    assert.deepEqual(
      artifact.symbols.map((entry) => entry.symbol),
      [
        'silk_main',
        'silk_wasm_program_identity__12_7761736d2f70726f6772616d_8_6964656e74697479_11_6275696c74696e3a693332_18_726573756c743a6275696c74696e3a693332',
      ],
    )
  }),
)

it.effect('emits source conditionals directly as structured if', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }',
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
      'import silk.i32 as i32\npub fn main() -> i32 { let x = 1 if i32.equals(x, 1) { let a = 5 } return x }',
    )

    assert.match(artifact.wat, /\bif\b/)
    assert.notMatch(artifact.wat, /\belse\b/)
  }),
)

it.effect('nests an if inside an arm for nested source conditionals', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'import silk.i32 as i32\npub fn main() -> i32 { let x = 1 if i32.equals(x, 1) { if i32.equals(x, 1) { return 42 } return 1 } return 0 }',
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

it.effect('keeps arithmetic convergence corpus cases in engine agreement', () =>
  Effect.gen(function* () {
    // Remainder MIN/-1 traps, checked remainder answers None, rotate counts wrap, and float
    // remainder is exact fmod — identically on wasm and the interpreter (the native acceptance
    // differential covers the same programs through nativeCorpus).
    const programs = corpus.filter(
      (candidate) =>
        candidate.name.startsWith('arith-convergence-') ||
        candidate.name === 'finite-effect-join-capture-arity',
    )
    assert.isAbove(programs.length, 0)
    for (const program of programs) {
      const executed = yield* run(program.source)
      assert.strictEqual(executed, yield* interpret(program.source), program.name)
      assert.strictEqual(
        executed,
        program.expected._tag === 'Completes' ? program.expected.result : 'trap',
        program.name,
      )
    }
  }),
)

it.effect('agrees with the interpreter on partially annotated generic calls', () =>
  Effect.gen(function* () {
    // A substitution seeded from an explicit prefix specializes the same way an inferred one
    // does, so emission must not be able to tell how the call was written.
    const program = corpus.find((candidate) => candidate.name === 'generic-partial-type-arguments')
    assert.notStrictEqual(program, undefined)
    if (program === undefined) return
    assert.strictEqual(yield* run(program.source), yield* interpret(program.source), program.name)
  }),
)

it.effect('rejects a structural MIR cycle before structured emission', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('pub fn main() -> i32 { return 42 }')
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
    const snapshot = yield* Analysis.ofSourceRealized(
      'wasm/native-plan',
      ascii('pub fn main() -> i32 { return 42 }'),
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
    const source = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`
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
    const source = 'import silk.i32 as i32\npub fn main() -> i32 { return i32.add(40, 2) }'
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
    const artifact = yield* emit(
      'import silk.i32 as i32\npub fn main() -> i32 { return i32.divide(84, 2) }',
    )

    assert.match(artifact.wat, /i32\.div_s/)
  }),
)

const programs: ReadonlyArray<readonly [string, string]> = [
  [
    'nested loops',
    `pub fn main() -> i32 {
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
    `struct Pair { left: i32 right: i32 }
pub fn main() -> i32 {
  let mut pair = Pair { left: 0, right: 40 }
  while pair.left < 2 { pair.left = pair.left + 1 }
  return pair.left + pair.right
}`,
  ],
  [
    'mutable scalar loop',
    `pub fn main() -> i32 {
  let mut count = 0
  while count < 42 { count = count + 1 }
  return count
}`,
  ],
  [
    'mutable array loop',
    `import silk.usize as usize
pub fn main() -> i32 {
  let mut values = [40, 0]
  let mut index = usize.add(0, 0)
  while index < 2 {
    values[index] = values[index] + 1
    index = index + 1
  }
  return values[0] + values[1]
}`,
  ],
  [
    'loop continue and break',
    `import silk.usize as usize
pub fn main() -> i32 {
  let mut index = usize.add(0, 0)
  while index < 50 {
    index = index + 1
    if index == 2 { continue }
    if index == 42 { break }
  }
  return usize.toI32(index)
}`,
  ],
  ['literal', 'pub fn main() -> i32 { return 42 }'],
  [
    'direct call',
    `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer() }`,
  ],
  [
    'nested calls',
    `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`,
  ],
  [
    'multiple parameters',
    `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(1, 42) }`,
  ],
  ['addition', 'import silk.i32 as i32\npub fn main() -> i32 { return i32.add(40, 2) }'],
  ['operator precedence', 'pub fn main() -> i32 { return 2 + 3 * 4 }'],
  [
    'operator pipeline',
    'import silk.i32 as i32\npub fn main() -> i32 { return 2 |> i32.add(3) |> i32.multiply(4) }',
  ],
  ['operator negation', 'pub fn main() -> i32 { return -(40 + 2) }'],
  ['subtraction', 'import silk.i32 as i32\npub fn main() -> i32 { return i32.subtract(50, 8) }'],
  ['multiplication', 'import silk.i32 as i32\npub fn main() -> i32 { return i32.multiply(6, 7) }'],
  ['division', 'import silk.i32 as i32\npub fn main() -> i32 { return i32.divide(84, 2) }'],
  ['remainder', 'import silk.i32 as i32\npub fn main() -> i32 { return i32.remainder(85, 43) }'],
  [
    'chained arithmetic',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.divide(i32.add(40, 2), 1) }',
  ],
  [
    'negative results',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.subtract(0, 42) }',
  ],
  [
    'branch taken',
    'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }',
  ],
  [
    'branch not taken',
    'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 2) { return 0 } return 42 }',
  ],
  [
    'ordered comparison',
    'import silk.i32 as i32\npub fn main() -> i32 { if i32.lessThan(1, 2) { return 42 } return 0 }',
  ],
  [
    'let bindings across a branch',
    'import silk.i32 as i32\npub fn main() -> i32 { let base = 40 if i32.equals(base, 40) { let bonus = 2 return i32.add(base, bonus) } return 0 }',
  ],
  ['inferred fixed array', 'pub fn main() -> i32 { let values = [10, 42] return values[1] }'],
  [
    'dynamic fixed array index',
    `fn choose(values: [i32; 3], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42, 90], 1) }`,
  ],
  [
    'nested fixed array index',
    `fn choose(values: [[i32; 2]; 2], outer: usize, inner: usize) -> i32 { return values[outer][inner] }
pub fn main() -> i32 { return choose([[10, 11], [42, 43]], 1, 0) }`,
  ],
  [
    'indexed struct array field',
    `struct Pair { left: i32 right: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
  ],
  [
    'upper array index traps',
    `fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42], 2) }`,
  ],
  [
    'zero-length array index traps',
    `fn choose(values: [i32; 0], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([], 0) }`,
  ],
  [
    'division by zero traps',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.divide(1, 0) }',
  ],
  [
    'remainder by zero traps',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.remainder(1, 0) }',
  ],
  [
    'addition overflow traps',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.add(2147483647, 1) }',
  ],
  [
    'subtraction overflow traps',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.subtract(-2147483648, 1) }',
  ],
  [
    'multiplication overflow traps',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.multiply(2147483647, 2) }',
  ],
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
        // MIN % -1 traps like the overflowing division it implies, even though wasm rem_s
        // itself would answer 0.
        ['remainder', (a, b) => (b === 0 || (a === minimum && b === -1) ? undefined : a % b)],
      ]

      const mismatches: Array<string> = []
      for (const [operator, reference] of references) {
        const calculate = yield* binaryOperation(operator)
        for (const left of operands) {
          for (const right of operands) {
            const exact = reference(left, right)
            const traps = exact === undefined || exact > maximum || exact < minimum
            let actual: number | 'trap'
            try {
              actual = calculate(left, right)
            } catch {
              actual = 'trap'
            }
            if (actual !== (traps ? 'trap' : exact)) {
              mismatches.push(
                `i32.${operator}(${left}, ${right}) expected ${traps ? 'trap' : exact}, got ${actual}`,
              )
            }
          }
        }
      }

      assert.deepEqual(mismatches, [])
    }),
  15_000,
)
