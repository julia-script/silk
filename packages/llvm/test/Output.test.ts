import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Alias from '../src/Alias.js'
import * as Attribute from '../src/Attribute.js'
import * as Bitcode from '../src/Bitcode.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FunctionActor from '../src/Function.js'
import * as Global from '../src/Global.js'
import * as IrText from '../src/IrText.js'
import * as Type from '../src/Type.js'
import * as Variable from '../src/Variable.js'

const hexadecimal = (bytes: Uint8Array): string =>
  Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('')

it.effect('renders deterministic LLVM IR headers and module assembly', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      moduleName: 'test',
      sourceFilename: 'test.ll',
      targetTriple: 'aarch64-apple-darwin',
    })
    yield* Builder.appendModuleAssembly(builder, 'nop')

    assert.strictEqual(
      yield* IrText.render(builder),
      '; ModuleID = \'test\'\nsource_filename = "test.ll"\ntarget triple = "aarch64-apple-darwin"\n\nmodule asm "nop"\n',
    )
  }),
)

it.effect('renders and directly encodes representative declarations deterministically', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      sourceFilename: 'decl.ll',
      dataLayout: 'e-p:64:64-i32:32',
      targetTriple: 'aarch64-unknown-linux',
    })
    const i32 = yield* Type.integer(builder, 32)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const answer = yield* Variable.make(builder, 'answer', i32, {
      initializer: one,
      constant: true,
      linkage: 'internal',
    })
    const answerPointer = yield* Constant.fromGlobal(
      builder,
      yield* Variable.global(builder, answer),
    )
    yield* Alias.make(builder, 'answer_alias', i32, answerPointer)
    const functionType = yield* Type.functionType(builder, i32, [i32])
    const nounwind = yield* Attribute.flag(builder, 'nounwind')
    const attributes = yield* Attribute.functionSet(builder, {
      functionAttributes: yield* Attribute.set(builder, [nounwind]),
    })
    yield* FunctionActor.declare(builder, 'compute', functionType, { attributes })

    assert.strictEqual(
      yield* IrText.render(builder),
      'source_filename = "decl.ll"\ntarget datalayout = "e-p:64:64-i32:32"\ntarget triple = "aarch64-unknown-linux"\n\n@answer = internal constant i32 1\n@answer_alias = alias i32, ptr @answer\ndeclare i32 @compute(i32) nounwind\n',
    )
    const first = yield* Bitcode.encode(builder)
    const second = yield* Bitcode.encode(builder)
    assert.deepEqual(first, second)
    assert.deepEqual(first.slice(0, 4), Uint8Array.of(0x42, 0x43, 0xc0, 0xde))
  }),
)

it.effect('emits the pinned minimal LLVM bitcode fixture exactly', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      moduleName: 'test',
      sourceFilename: 'test.ll',
      targetTriple: 'aarch64-apple-darwin',
    })
    yield* Builder.appendModuleAssembly(builder, 'nop')
    const bytes = yield* Bitcode.encode(builder)

    assert.strictEqual(
      hexadecimal(bytes),
      '4243c0de350c0000070000001a034c904401328ce6d2d8d65acacccccac6e840605c605c600a0000210c00000b00000012030a68103141ac50616172636836342d6170706c652d64617277696e4539e8cae6e85cd8d84a83db1b1c005d0c0000020000001203940000000000',
    )
  }),
)

it.effect('keeps bitcode actor order and global references after rebinding and removal', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make({ sourceFilename: 'global-order.ll' })
    const i32 = yield* Type.integer(builder, 32)
    const functionType = yield* Type.functionType(builder, i32, [])
    const reboundVariable = yield* FunctionActor.global(
      builder,
      yield* FunctionActor.declare(builder, 'late_variable', functionType),
    )
    const reboundFunction = yield* Variable.global(
      builder,
      yield* Variable.make(builder, 'late_function', i32),
    )
    const reboundAlias = yield* Variable.global(
      builder,
      yield* Variable.make(builder, 'late_alias', i32),
    )
    yield* FunctionActor.declare(builder, 'first_function', functionType)
    const firstVariable = yield* Variable.global(
      builder,
      yield* Variable.make(builder, 'first_variable', i32),
    )
    const removed = yield* Variable.global(builder, yield* Variable.make(builder, 'removed', i32))
    const replaced = yield* Variable.global(builder, yield* Variable.make(builder, 'replaced', i32))
    const pointer = yield* Constant.fromGlobal(builder, firstVariable)
    yield* Alias.make(builder, 'first_alias', i32, pointer)
    yield* Global.remove(builder, removed)
    yield* Global.replace(builder, replaced, firstVariable)
    yield* Variable.fromGlobal(builder, reboundVariable, i32)
    yield* FunctionActor.fromGlobal(builder, reboundFunction, functionType)
    yield* Alias.fromGlobal(builder, reboundAlias, i32, pointer)

    // Rebound actors retain their original global indices, but append to their new actor
    // tables. The golden also checks actual global indices after inactive entries are omitted.
    assert.strictEqual(
      hexadecimal(yield* Bitcode.encode(builder)),
      '4243c0de350c0000070000001a034c904401328ce6d2d8d65acacccccac6e840605c605c600a0000211000003f0000002206149041c404490c0f0806824184e8404688188241840c1112440408204268884030100c22444910423088211844041011404480006444080ec23a100c048308c1818c1011226488104185f29cb1bd8985b1b5bcc99195c9b9b0b1450401001100000022060814128888681040c403e1414809092642480a092642c6091324e302c120424606824184881944c4d42062224ca090b2428208134103000000070200000fa00100000600e000800000000000000000600e000d00080000000000000000b701e0001000010000000000000000002e05a001200002000000000000000000a00dc0020000000000000083001400000000000000000000005d0c000016000000120394ab0000000066697273745f7661726961626c656c6174655f7661726961626c6566697273745f66756e6374696f6e6c6174655f66756e6374696f6e66697273745f616c6961736c6174655f616c6961730000000000',
    )
  }),
)
