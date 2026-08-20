import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Driver from '../src/Driver.js'
import * as Layout from '../src/Layout.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = (text: string, target: string) =>
  Analysis.ofSourceRealized('usize/program', ascii(text), target)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-usize-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const nativeExact = `fn increment(value: usize) -> usize { return usize.add(value, 1) }
pub fn main() -> i32 {
  if increment(9007199254740993) == 9007199254740994 { return 42 }
  return 0
}`

const sharedUnsigned = `fn maximum() -> usize { return 4294967293 |> usize.add(2) }
pub fn main() -> i32 {
  if maximum() > 2147483647 { return 42 }
  return 0
}`

it.effect('retains exact contextual magnitudes and target-owned usize layout facts', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(nativeExact, 'aarch64-apple-darwin')
    assert.deepEqual(snapshot.diagnostics, [])

    const integers = Analysis.expressionsOf(snapshot, 'usize/program').flatMap((expression) =>
      expression._tag === 'Integer' && expression.integer._tag === 'Available'
        ? [expression.integer]
        : [],
    )
    assert.include(
      integers.map((integer) => integer.value),
      9007199254740993n,
    )
    const plan = Analysis.layoutOf(snapshot)
    assert.strictEqual(plan._tag, 'Available')
    if (plan._tag !== 'Available') return
    const usize = Layout.entry(plan.value, 'usize')
    assert.deepEqual(usize?.representation, { _tag: 'UnsignedInteger', bits: 64 })
    // The program's own three usize literals. Reached library modules contribute their own
    // verdicts — silk/usize's shared ZERO and ONE among them — so the count names this module.
    assert.strictEqual(
      plan.value.literalVerdicts.filter((verdict) => verdict.span.sourceId === 'usize/program')
        .length,
      3,
    )
    assert.strictEqual(
      plan.value.literalVerdicts.every((verdict) => verdict._tag === 'AvailableUsizeLiteral'),
      true,
    )
  }),
)

it.effect(
  'uses the selected pointer width on every target and evaluates native maximum exactly',
  () =>
    Effect.gen(function* () {
      for (const target of [
        'aarch64-apple-darwin',
        'x86_64-unknown-linux-gnu',
        'aarch64-unknown-linux-gnu',
      ]) {
        const snapshot = yield* source(nativeExact, target)
        const layout = Analysis.layoutOf(snapshot)
        assert.strictEqual(layout._tag, 'Available')
        if (layout._tag === 'Available') {
          const usize = Layout.entry(layout.value, 'usize')
          assert.deepEqual(
            [usize?.size, usize?.alignment, usize?.representation],
            [8, 8, { _tag: 'UnsignedInteger', bits: 64 }],
          )
        }
      }

      const maximumProgram = `fn maximum() -> usize { return 18446744073709551615 }
pub fn main() -> i32 {
  if maximum() == 18446744073709551615 { return 42 }
  return 0
}`
      const snapshot = yield* source(maximumProgram, 'aarch64-apple-darwin')
      const outcome = Analysis.evaluate(snapshot)
      assert.strictEqual(outcome._tag, 'Completed')
      const returned = outcome.trace.find(
        (event) => event._tag === 'Return' && event.function.name === 'maximum',
      )
      assert.strictEqual(returned?._tag, 'Return')
      if (returned?._tag === 'Return' && returned.value._tag === 'IntegerValue') {
        assert.strictEqual(returned.value.value.toString(), '18446744073709551615')
      } else assert.fail('expected an exact usize return trace')
    }),
)

it.effect('rejects an exact usize magnitude outside the selected target before MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(
      'fn wide() -> usize { return 4294967296 } pub fn main() -> i32 { if wide() == 0 { return 1 } return 0 }',
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      ['LAY0001'],
    )
    const diagnostic = snapshot.diagnostics.at(0)
    assert.strictEqual(diagnostic?.reason._tag, 'UsizeTargetOutOfRange')
    if (diagnostic?.reason._tag === 'UsizeTargetOutOfRange') {
      assert.strictEqual(diagnostic.reason.spelling, '4294967296')
      assert.strictEqual(diagnostic.reason.bits, 32)
      assert.strictEqual(diagnostic.reason.target, 'wasm32-unknown-unknown')
    }
    assert.strictEqual(Analysis.mirOf(snapshot)._tag, 'Unavailable')
  }),
)

it.effect('evaluates exact native usize arithmetic without host-number precision loss', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(nativeExact, 'aarch64-apple-darwin')
    const outcome = Analysis.evaluate(snapshot)

    assert.strictEqual(outcome._tag, 'Completed')
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)
    const wideBinding = outcome.trace.find(
      (event): event is BootstrapEvaluation.BindingTraceEvent =>
        event._tag === 'Binding' && event.target.name === 'increment',
    )
    assert.strictEqual(wideBinding?.value._tag, 'IntegerValue')
    if (wideBinding?.value._tag === 'IntegerValue') {
      assert.strictEqual(wideBinding.value.value, 9007199254740993n)
    }
  }),
)

it.effect('lowers native usize lanes and operations as unsigned i64', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(nativeExact, 'aarch64-apple-darwin')
    const artifact = yield* Analysis.codegen(snapshot, { mode: 'release' })

    assert.include(artifact.ir, 'i64 9007199254740993')
    assert.include(artifact.ir, 'llvm.uadd.with.overflow.i64')
    assert.include(artifact.ir, 'icmp eq i64')
  }),
)

it.effect('executes an exact native i64 call and rejects the same source on Wasm', () =>
  Effect.gen(function* () {
    const native = yield* Driver.compile({
      compilation: { root: SourceFile.make('usize/program', ascii(nativeExact)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'native-exact'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(native._tag, 'Compiled')
    if (native._tag !== 'Compiled') return
    const executed = spawnSync(native.path, [], { encoding: 'utf8' })
    assert.strictEqual(executed.status, 42, executed.stderr)

    const wasm = yield* source(nativeExact, 'wasm32-unknown-unknown')
    assert.include(
      wasm.diagnostics.map((diagnostic) => diagnostic.code),
      'LAY0001',
    )
    assert.strictEqual(Analysis.mirOf(wasm)._tag, 'Unavailable')
  }),
)

it.effect('executes Wasm usize comparisons with unsigned i32 semantics', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(sharedUnsigned, 'wasm32-unknown-unknown')
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.include(artifact.wat, 'i32.gt_u')
    assert.strictEqual(main(), 42)
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)
  }),
)

it.effect('preserves usize through generic aggregates and heterogeneous calling lanes', () =>
  Effect.gen(function* () {
    const program = `struct Pair<T> { first: T second: i32 }
pub fn main() -> i32 {
  let pair = Pair<usize> { first: 4294967295, second: 7 }
  if pair.first == 4294967295 { return 42 }
  return 0
}`
    const snapshot = yield* source(program, 'wasm32-unknown-unknown')
    assert.deepEqual(snapshot.diagnostics, [])
    const layout = Analysis.layoutOf(snapshot)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag !== 'Available') return
    const pair = layout.value.callingShapes.find(
      (shape) => typeof shape.type !== 'string' && shape.type._tag === 'NominalType',
    )
    assert.deepEqual(
      pair?.lanes.map((lane) => lane.type),
      ['usize', 'i32'],
    )

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('preserves usize through fixed arrays, calls, and indexed returns', () =>
  Effect.gen(function* () {
    const program = `fn second(values: [usize; 2]) -> usize { return values[1] }
pub fn main() -> i32 {
  if second([7, 4294967295]) == 4294967295 { return 42 }
  return 0
}`
    const native = yield* source(program, 'aarch64-apple-darwin')
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.match(llvm.ir, /i64.*i64/)
    const outcome = Analysis.evaluate(native)
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)

    const wasm = yield* source(program, 'wasm32-unknown-unknown')
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('realizes mixed usize and i32 union payload lanes in both backends', () =>
  Effect.gen(function* () {
    const program = `struct Wide { value: usize }
struct Narrow { value: i32 }
fn decode(flag: bool) -> Wide | Narrow {
  if flag { return Wide { value: 4294967295 } }
  return Narrow { value: 7 }
}
fn inspect(input: Wide | Narrow) -> i32 {
  return match &input {
    Wide { value } if value > 2147483647 => 42
    Wide { value } => 0
    Narrow { value } => value
  }
}
pub fn main() -> i32 { return inspect(decode(true)) }`
    const native = yield* source(program, 'aarch64-apple-darwin')
    assert.deepEqual(native.diagnostics, [])
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.match(llvm.ir, /\{ i32, i64 \}/)
    assert.include(llvm.ir, 'trunc i64')

    const wasm = yield* source(program, 'wasm32-unknown-unknown')
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    const logical = Analysis.evaluate(wasm)
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42n)
  }),
)

it.effect('discovers one generic usize instance independently of literal magnitude', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(
      `fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  if identity<usize>(1) + identity<usize>(4294967294) == 4294967295 { return 42 }
  return 0
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(snapshot.diagnostics, [])
    const identities = Analysis.instancesOf(snapshot).instances.filter(
      (instance) => instance.key.declaration.name === 'identity',
    )
    assert.strictEqual(identities.length, 1)
    assert.deepEqual(identities.at(0)?.key.typeArguments, ['usize'])
  }),
)

it.effect('traps unsigned underflow and rejects unary minus without compiler crashes', () =>
  Effect.gen(function* () {
    const underflow = yield* source(
      'fn subtract() -> usize { return 0 - 1 } pub fn main() -> i32 { if subtract() == 0 { return 1 } return 0 }',
      'wasm32-unknown-unknown',
    )
    const outcome = Analysis.evaluate(underflow)
    assert.strictEqual(outcome._tag, 'Trap')
    assert.strictEqual(outcome._tag === 'Trap' ? outcome.reason : undefined, 'arithmetic underflow')

    const negative = yield* source(
      'fn invalid() -> usize { return -1 } pub fn main() -> i32 { return 0 }',
      'aarch64-apple-darwin',
    )
    assert.include(
      negative.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0060',
    )

    const mixed = yield* source(
      'fn invalid(wide: usize, narrow: i32) -> bool { return wide == narrow } pub fn main() -> i32 { return 0 }',
      'aarch64-apple-darwin',
    )
    assert.include(
      mixed.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0012',
    )
  }),
)

it.effect('traps usize overflow and division by zero in both logical and Wasm execution', () =>
  Effect.gen(function* () {
    for (const expression of ['4294967295 + 1', '1 / 0']) {
      const program = `fn invalid() -> usize { return ${expression} }
pub fn main() -> i32 { if invalid() == 0 { return 1 } return 0 }`
      const snapshot = yield* source(program, 'wasm32-unknown-unknown')
      const logical = Analysis.evaluate(snapshot)
      assert.strictEqual(logical._tag, 'Trap', expression)
      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.throws(() => (instance.exports.silk_main as () => number)(), WebAssembly.RuntimeError)
    }
  }),
)

it.effect('rejects malformed usize target verdicts and MIR literals as verifier data', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(sharedUnsigned, 'wasm32-unknown-unknown')
    const layout = Analysis.layoutOf(snapshot)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag !== 'Available') return
    const first = layout.value.literalVerdicts.at(0)
    if (first === undefined) return assert.fail('expected a usize literal verdict')
    const malformedLayout: Layout.Plan = {
      ...layout.value,
      literalVerdicts: [{ ...first, bits: 64 }],
    }
    assert.include(
      Layout.verify(malformedLayout).map((violation) => violation.rule),
      'InvalidLiteralVerdict',
    )

    const program = Analysis.loweredMir(snapshot)
    const malformed: Mir.Module = {
      ...program,
      functions: program.functions.map((fn) => ({
        ...fn,
        regions: fn.regions.map((region) =>
          region._tag === 'OperationRegion'
            ? {
                ...region,
                operations: region.operations.map((operation) =>
                  operation._tag === 'Literal' && operation.type._tag === 'usize'
                    ? { ...operation, value: 4294967296n }
                    : operation,
                ),
              }
            : region,
        ),
      })),
    }
    assert.include(
      Mir.verify(malformed).map((violation) => violation.rule),
      'InvalidIntegerOperation',
    )
  }),
)
