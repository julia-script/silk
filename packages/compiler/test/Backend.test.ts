import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Backend from '../src/Backend.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nestedSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`

const emit = Effect.fnUntraced(function* (text: string, request: Backend.CodegenRequest) {
  const snapshot = yield* Analysis.ofSource('golden/program', ascii(text), 'aarch64-apple-darwin')
  return yield* Analysis.codegen(snapshot, request)
})

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it.effect('emits one artifact per program with deterministic symbols', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(nestedSource, { mode: 'release' })

    assert.strictEqual(artifact.module, 'golden/program')
    assert.deepEqual(
      artifact.symbols.map((entry) => entry.symbol),
      [
        'silk_main',
        'silk_golden_program_identity__14_676f6c64656e2f70726f6772616d_8_6964656e74697479_11_6275696c74696e3a693332_18_726573756c743a6275696c74696e3a693332',
      ],
    )
    assert.strictEqual(artifact.symbols.at(0)?.declaration.name, 'main')
  }),
)

it.effect('matches the IR golden and the bitcode digest golden', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(nestedSource, { mode: 'release' })

    assert.strictEqual(artifact.ir, golden('program.ll.txt'))
    assert.strictEqual(
      `${createHash('sha256').update(artifact.bitcode).digest('hex')}\n`,
      golden('program.bc.sha256'),
    )
  }),
)

it.effect('emits byte-identical bitcode across repeated fresh runs', () =>
  Effect.gen(function* () {
    const first = yield* emit(nestedSource, { mode: 'release' })
    const second = yield* emit(nestedSource, { mode: 'release' })

    assert.deepEqual(first.bitcode, second.bitcode)
    assert.strictEqual(first.ir, second.ir)
  }),
)

it.effect('emits target-correct LLVM bitcode for wasm32 while retaining silk_main', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'golden/llvm-wasm',
      ascii('pub fn main() -> i32 { return 42 }'),
      'wasm32-unknown-unknown',
    )
    const artifact = yield* Analysis.codegen(snapshot, { mode: 'release' })
    assert.strictEqual(artifact._tag, 'LlvmBitcodeArtifact')
    assert.strictEqual(artifact.backend, 'llvm')
    assert.strictEqual(artifact.target.id, 'wasm32-unknown-unknown')
    assert.include(artifact.ir, 'target triple = "wasm32-unknown-unknown"')
    assert.strictEqual(artifact.symbols.at(0)?.symbol, 'silk_main')
  }),
)

it.effect('realizes stored declaration and builtin callable sections in LLVM and Wasm', () =>
  Effect.gen(function* () {
    const programs = [
      `fn add(value: i32, adjustment: i32) -> i32 { return value + adjustment }
pub fn main() -> i32 { let plusTwo = add(2) return plusTwo(40) }`,
      'pub fn main() -> i32 { let plusTwo = i32.add(2) return plusTwo(40) }',
    ]
    for (const [ordinal, source] of programs.entries()) {
      const native = yield* Analysis.ofSource(
        `callable/native-${ordinal}`,
        ascii(source),
        'aarch64-apple-darwin',
      )
      const wasm = yield* Analysis.ofSource(
        `callable/wasm-${ordinal}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
      const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      const instance = new WebAssembly.Instance(
        new WebAssembly.Module(wasmArtifact.bytes.slice()),
        {},
      )
      const main = instance.exports.silk_main
      assert.isFunction(main)
      if (typeof main !== 'function') return
      assert.strictEqual(main(), 42)
      assert.strictEqual(Analysis.evaluate(native)._tag, 'Completed')
      assert.include(nativeArtifact.ir, ordinal === 0 ? 'callable' : 'callable_arith')
    }
  }),
)

it.effect(
  'keeps reusable, exclusive, take-once, generic, and dropped callables in backend parity',
  () =>
    Effect.gen(function* () {
      const programs = [
        `fn write(value: i32, values: &mut [i32]) -> i32 { values[0] = value return values[0] }
pub fn main() -> i32 {
  let mut values = [0]
  let mut callback = write(&mut values)
  let first = callback(40)
  let second = callback(first + 2)
  drop callback
  return second
}`,
        `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let callback = consume(move token)
  return callback(40)
}`,
        `fn choose<T>(value: T, fallback: T) -> T { return move value }
pub fn main() -> i32 { let chosen = choose<i32>(0) return chosen(42) }`,
        `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let callback = consume(move token)
  drop callback
  return 42
}`,
      ]
      for (const [ordinal, source] of programs.entries()) {
        const native = yield* Analysis.ofSource(
          `callable-modes/native-${ordinal}`,
          ascii(source),
          'aarch64-apple-darwin',
        )
        const wasm = yield* Analysis.ofSource(
          `callable-modes/wasm-${ordinal}`,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        const llvm = yield* Analysis.codegen(native, { mode: 'release' })
        const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
        const instance = new WebAssembly.Instance(
          new WebAssembly.Module(artifact.bytes.slice()),
          {},
        )
        const main = instance.exports.silk_main
        assert.isFunction(main)
        if (typeof main !== 'function') return
        assert.strictEqual(main(), 42)
        const evaluated = Analysis.evaluate(native)
        assert.strictEqual(evaluated._tag, 'Completed')
        assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42)
        assert.include(llvm.ir, 'define i32 @silk_main')
      }
    }),
)

it.effect('emits deterministic callable layouts, MIR, LLVM, and Wasm artifacts', () =>
  Effect.gen(function* () {
    const source = `fn add<T>(value: T, fallback: T) -> T { return move value }
pub fn main() -> i32 { let callback = add<i32>(2) return callback(42) }`
    const nativeFirst = yield* Analysis.ofSource(
      'callable-determinism/native',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const nativeSecond = yield* Analysis.ofSource(
      'callable-determinism/native',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const wasmFirst = yield* Analysis.ofSource(
      'callable-determinism/wasm',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    const wasmSecond = yield* Analysis.ofSource(
      'callable-determinism/wasm',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    const llvmFirst = yield* Analysis.codegen(nativeFirst, { mode: 'release' })
    const llvmSecond = yield* Analysis.codegen(nativeSecond, { mode: 'release' })
    const wasmArtifactFirst = yield* Analysis.codegenWasm(wasmFirst, { mode: 'release' })
    const wasmArtifactSecond = yield* Analysis.codegenWasm(wasmSecond, { mode: 'release' })

    assert.strictEqual(llvmFirst.ir, llvmSecond.ir)
    assert.deepEqual(llvmFirst.bitcode, llvmSecond.bitcode)
    assert.deepEqual(wasmArtifactFirst.bytes, wasmArtifactSecond.bytes)
    assert.deepEqual(nativeFirst.layout, nativeSecond.layout)
    assert.deepEqual(Analysis.loweredMir(nativeFirst), Analysis.loweredMir(nativeSecond))
  }),
)

it.effect('refuses diagnosed trap bodies before backend emission', () =>
  Effect.gen(function* () {
    const result = yield* Effect.result(
      emit('pub fn main() -> i32 { return missing() }', { mode: 'release' }),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag === 'Failure') assert.strictEqual(result.failure._tag, 'CodegenUnavailable')
  }),
)

it.effect('emits native debug metadata only for debug requests', () =>
  Effect.gen(function* () {
    const debug = yield* emit(nestedSource, {
      mode: 'debug',
      sources: new Map([['golden/program', ascii(nestedSource)]]),
    })
    const release = yield* emit(nestedSource, { mode: 'release' })

    assert.include(debug.ir, '!DICompileUnit(')
    assert.include(debug.ir, '!DISubprogram(')
    assert.include(debug.ir, '!dbg')
    assert.notInclude(release.ir, 'DICompileUnit')
    assert.notInclude(release.ir, '!dbg')
  }),
)

const arithmeticSource = 'pub fn main() -> i32 { return i32.subtract(i32.multiply(6, 7), 0) }'

const matchSource = `pub struct Left { value: i32 }
pub struct Right { value: i32 }
pub fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => i32.add(answer, 1)
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 41 }) }`

it.effect('emits checked arithmetic through overflow intrinsics and guarded division', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(arithmeticSource, { mode: 'release' })
    const division = yield* emit('pub fn main() -> i32 { return i32.divide(1, 0) }', {
      mode: 'release',
    })

    assert.include(artifact.ir, 'llvm.smul.with.overflow')
    assert.include(artifact.ir, 'llvm.ssub.with.overflow')
    assert.include(artifact.ir, 'arith_trap')
    assert.include(division.ir, 'sdiv')
    assert.include(division.ir, 'icmp eq')
    assert.include(division.ir, '@llvm.trap()')
  }),
)

it.effect('matches the arithmetic IR golden and stays deterministic', () =>
  Effect.gen(function* () {
    const first = yield* emit(arithmeticSource, { mode: 'release' })
    const second = yield* emit(arithmeticSource, { mode: 'release' })

    assert.strictEqual(first.ir, golden('arithmetic.ll.txt'))
    assert.deepEqual(first.bitcode, second.bitcode)
  }),
)

it.effect('emits comparisons as icmp with zero-extension and branches natively', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> i32 { if i32.lessThan(1, 2) { return 42 } return 0 }',
      {
        mode: 'release',
      },
    )

    assert.include(artifact.ir, 'icmp slt')
    assert.include(artifact.ir, 'zext')
    assert.include(artifact.ir, 'br i1')
  }),
)

it.effect('privately flattens structured match regions with deterministic member branches', () =>
  Effect.gen(function* () {
    const first = yield* emit(matchSource, { mode: 'release' })
    const second = yield* emit(matchSource, { mode: 'release' })

    assert.include(first.ir, 'match')
    assert.include(first.ir, 'icmp eq')
    assert.include(first.ir, 'br i1')
    assert.strictEqual(first.ir, second.ir)
    assert.deepEqual(first.bitcode, second.bitcode)
  }),
)

it.effect('realizes fixed arrays and checked mixed place reads from compiler-owned lanes', () =>
  Effect.gen(function* () {
    const source = `struct Pair { left: i32 right: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`
    const first = yield* emit(source, { mode: 'release' })
    const second = yield* emit(source, { mode: 'release' })

    assert.include(first.ir, 'icmp ult')
    assert.include(first.ir, 'select i1')
    assert.include(first.ir, '@llvm.trap()')
    assert.deepEqual(first.bitcode, second.bitcode)
    assert.strictEqual(first.ir, second.ir)
  }),
)

it.effect('orders checked array reads before private match blocks', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      `struct A {}
struct B {}
fn decode(code: i32) -> A | B { if code == 1 { return A {} } return B {} }
pub fn main() -> i32 {
  let codes = [1, 2]
  let index = usize.add(0, 0)
  let candidate = decode(codes[index])
  return match move candidate { A {} => 42 B {} => 0 }
}`,
      { mode: 'release' },
    )

    assert.match(artifact.ir, /index\d+_0_ok/)
    assert.include(artifact.ir, 'match')
  }),
)

it.effect('publishes native branch provenance back to canonical loop regions', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> i32 { let mut value = 0 while value < 2 { value = value + 1 } return value }',
      { mode: 'release' },
    )
    assert.strictEqual(
      artifact.control.every((entry) => entry.backend === 'LLVM'),
      true,
    )
    assert.strictEqual(
      artifact.control.some((entry) => entry.construct === 'LlvmBranch'),
      true,
    )
    assert.strictEqual(
      artifact.control.some(
        (entry) =>
          entry.construct === 'LlvmJump' &&
          entry.targets.some((target) => target.ordinal <= entry.region.ordinal),
      ),
      true,
    )
  }),
)
