import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Alias from '../src/Alias.js'
import * as Attribute from '../src/Attribute.js'
import * as Bitcode from '../src/Bitcode.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FunctionActor from '../src/Function.js'
import * as IrText from '../src/IrText.js'
import * as Type from '../src/Type.js'
import * as Variable from '../src/Variable.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

const hexadecimal = (bytes: Uint8Array): string =>
  Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('')

test(
  'renders deterministic LLVM IR headers and module assembly',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make({
      moduleName: 'test',
      sourceFilename: 'test.ll',
      targetTriple: 'aarch64-apple-darwin',
    })
    yield* Builder.appendModuleAssembly(builder, 'nop')

    expect(yield* IrText.render(builder)).toBe(
      '; ModuleID = \'test\'\nsource_filename = "test.ll"\ntarget triple = "aarch64-apple-darwin"\n\nmodule asm "nop"\n',
    )
  }, TestRuntime.runPromise),
)

test(
  'renders and directly encodes representative declarations deterministically',
  Effect.fnUntraced(function* () {
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

    expect(yield* IrText.render(builder)).toBe(
      'source_filename = "decl.ll"\ntarget datalayout = "e-p:64:64-i32:32"\ntarget triple = "aarch64-unknown-linux"\n\n@answer = internal constant i32 1\n@answer_alias = alias i32, ptr @answer\ndeclare i32 @compute(i32) nounwind\n',
    )
    const first = yield* Bitcode.encode(builder)
    const second = yield* Bitcode.encode(builder)
    expect(first).toEqual(second)
    expect(first.slice(0, 4)).toEqual(Uint8Array.of(0x42, 0x43, 0xc0, 0xde))
  }, TestRuntime.runPromise),
)

test(
  'emits the pinned minimal LLVM bitcode fixture exactly',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make({
      moduleName: 'test',
      sourceFilename: 'test.ll',
      targetTriple: 'aarch64-apple-darwin',
    })
    yield* Builder.appendModuleAssembly(builder, 'nop')
    const bytes = yield* Bitcode.encode(builder)

    expect(hexadecimal(bytes)).toBe(
      '4243c0de350c0000070000001a034c904401328ce6d2d8d65acacccccac6e840605c605c600a0000210c00000b00000012030a68103141ac50616172636836342d6170706c652d64617277696e4539e8cae6e85cd8d84a83db1b1c005d0c0000020000001203940000000000',
    )
  }, TestRuntime.runPromise),
)
