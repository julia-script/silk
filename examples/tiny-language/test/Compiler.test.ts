import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Compiler from '../src/Compiler.js'

const compile = Effect.fnUntraced(function* (source: string) {
  return (yield* Compiler.compile(source)).ir
})

it.effect('lowers fn main() = 42 to a complete LLVM function', () =>
  Effect.gen(function* () {
    const ir = yield* compile('fn main() = 42')
    assert.include(ir, 'define i32 @main()')
    assert.include(ir, 'entry:')
    assert.include(ir, 'ret i32 42')
  }),
)

it.effect('returns the completed frontend artifacts with textual IR', () =>
  Effect.gen(function* () {
    const compilation = yield* Compiler.compile('fn main() = 42')
    assert.strictEqual(compilation.source, 'fn main() = 42')
    assert.strictEqual(compilation.tokens.at(-1)?.kind, 'Eof')
    assert.strictEqual(compilation.program.functions.at(0)?.name, 'main')
    assert.include(compilation.ir, 'ret i32 42')
    assert.ok(Object.isFrozen(compilation))
  }),
)

it.effect('encodes deterministic LLVM bitcode from the same committed module', () =>
  Effect.gen(function* () {
    const first = yield* Compiler.compile('fn main() = 42')
    const second = yield* Compiler.compile('fn main() = 42')
    assert.deepStrictEqual(first.bitcode, second.bitcode)
    assert.deepStrictEqual(Array.from(first.bitcode.slice(0, 4)), [0x42, 0x43, 0xc0, 0xde])
  }),
)

it.effect('requires a zero-parameter main entry point', () =>
  Effect.gen(function* () {
    const error = yield* Effect.flip(compile('fn main(x) = x'))
    assert.strictEqual(error._tag, 'ResolutionError')
    if (error._tag !== 'ResolutionError') return assert.fail('expected ResolutionError')
    assert.strictEqual(error.reason._tag, 'InvalidMain')
  }),
)

it.effect('declares every function before lowering forward calls', () =>
  Effect.gen(function* () {
    const ir = yield* compile('fn main() = double(21) fn double(x) = x * 2')
    assert.match(ir, /%called_0 = call i32 @double\(i32 21\)/)
    assert.match(ir, /define i32 @double\(i32 %v0\)/)
    assert.match(ir, /%multiplied_0 = mul i32 %v0, 2/)
  }),
)

it.effect('allows a function body to call its own declared handle', () =>
  Effect.gen(function* () {
    const ir = yield* compile('fn recur(n) = recur(n) fn main() = 1')
    assert.match(ir, /call i32 @recur\(i32 %v0\)/)
  }),
)

it.effect('lowers recursive factorial after declaring its self-call target', () =>
  Effect.gen(function* () {
    const source =
      'fn factorial(n) = if n < 2 then 1 else n * factorial(n - 1) fn main() = factorial(5)'
    const ir = yield* compile(source)
    assert.match(ir, /%called_4 = call i32 @factorial\(i32 %subtracted_3\)/)
    assert.match(ir, /%if_result_6 = phi i32 \[ 1, %if_true_0 \], \[ %multiplied_5, %if_false_0 \]/)
    assert.match(ir, /%called_0 = call i32 @factorial\(i32 5\)/)
  }),
)

it.effect('lowers abs with one incoming value from each predecessor', () =>
  Effect.gen(function* () {
    const ir = yield* compile('fn abs(x) = if x < 0 then -x else x fn main() = abs(-3)')
    assert.match(ir, /br i1 %condition_2, label %if_true_0, label %if_false_0/)
    assert.match(ir, /%if_result_4 = phi i32 \[ %negated_3, %if_true_0 \], \[ %v0, %if_false_0 \]/)
  }),
)

it.effect('uses the actual nested merge block as an outer phi predecessor', () =>
  Effect.gen(function* () {
    const ir = yield* compile('fn main() = if 1 then if 0 then 2 else 3 else 4')
    assert.include(ir, 'if_true_0:')
    assert.include(ir, 'if_true_1:')
    assert.match(ir, /%if_result_2 = phi i32 \[ 2, %if_true_1 \], \[ 3, %if_false_1 \]/)
    assert.match(ir, /%if_result_3 = phi i32 \[ %if_result_2, %if_merge_1 \], \[ 4, %if_false_0 \]/)
  }),
)

it.effect('reports duplicate, unknown, and wrong-arity names as compiler diagnostics', () =>
  Effect.gen(function* () {
    const duplicate = yield* Effect.flip(compile('fn main() = 1 fn main() = 2'))
    if (duplicate._tag !== 'ResolutionError') return assert.fail('expected ResolutionError')
    assert.strictEqual(duplicate.reason._tag, 'DuplicateFunction')

    const unknownName = yield* Effect.flip(compile('fn main() = missing'))
    if (unknownName._tag !== 'ResolutionError') return assert.fail('expected ResolutionError')
    assert.strictEqual(unknownName.reason._tag, 'UnknownName')

    const unknownFunction = yield* Effect.flip(compile('fn main() = missing()'))
    if (unknownFunction._tag !== 'ResolutionError') return assert.fail('expected ResolutionError')
    assert.strictEqual(unknownFunction.reason._tag, 'UnknownFunction')

    const wrongArity = yield* Effect.flip(compile('fn id(x) = x fn main() = id()'))
    if (wrongArity._tag !== 'ResolutionError') return assert.fail('expected ResolutionError')
    assert.strictEqual(wrongArity.reason._tag, 'WrongArity')
  }),
)

it.effect('lowers arithmetic in AST dependency order', () =>
  Effect.gen(function* () {
    const ir = yield* compile('fn main() = 1 + 2 * 3')
    assert.match(ir, /%multiplied_0 = mul i32 2, 3/)
    assert.match(ir, /%added_1 = add i32 1, %multiplied_0/)
    assert.include(ir, 'ret i32 %added_1')
  }),
)

it.effect('uses signed division after unary negation', () =>
  Effect.gen(function* () {
    const ir = yield* compile('fn main() = -20 / 3')
    assert.match(ir, /%negated_0 = sub i32 0, 20/)
    assert.match(ir, /%divided_1 = sdiv i32 %negated_0, 3/)
  }),
)

it.effect('normalizes signed comparisons from i1 to Tiny i32', () =>
  Effect.gen(function* () {
    const less = yield* compile('fn main() = -1 < 0')
    assert.match(less, /%comparison_1 = icmp slt i32 %negated_0, 0/)
    assert.match(less, /%comparison_i32_2 = zext i1 %comparison_1 to i32/)
    assert.include(less, 'ret i32 %comparison_i32_2')

    const greater = yield* compile('fn main() = 2 > 3')
    assert.match(greater, /icmp sgt i32 2, 3/)
  }),
)
