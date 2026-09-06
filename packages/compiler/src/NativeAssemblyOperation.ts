import type * as Builder from '@silklang/llvm/Builder'
import * as LlvmBlock from '@silklang/llvm/Block'
import type * as Target from './Target.js'
import * as Attribute from '@silklang/llvm/Attribute'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as LlvmType from '@silklang/llvm/Type'
import * as Effect from 'effect/Effect'
import * as Mir from './Mir.js'
import * as NativeAssembly from './NativeAssembly.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as Type from './Type.js'

/** Lowers validated assembly using existing LLVM constants and checked call construction. */
export const emit = Effect.fnUntraced(function* (
  context: Context,
  operation: Extract<Mir.Operation, { readonly _tag: 'NativeAssembly' }>,
) {
  const { builder, body, storage, program } = context
  const word = yield* LlvmType.integer(builder, 64)
  const pointer = context.pointer
  const result = Mir.semanticType(operation.type)
  let resultType = Type.isPointer(result) ? pointer : word
  if (Type.equals(result, Type.unit)) resultType = yield* LlvmType.voidType(builder)
  const parameters = operation.arguments.map((argument) => {
    const type = storage.fn.localTypes[argument.ordinal]
    if (type === undefined) throw new RangeError('Assembly argument lost its type')
    return Type.isPointer(Mir.semanticType(type)) ? pointer : word
  })
  const memory = { none: 0, read: 1, write: 2, readwrite: 3 }[operation.assembly.memory]
  // Observable machine effects use LLVM's inaccessible-memory lane, even with no data access.
  const effects = operation.assembly.sideEffects ? (memory * 21) | 12 : memory * 21
  const properties = [yield* Attribute.integer(builder, 'memory', effects)]
  if (operation.assembly.noReturn) properties.push(yield* Attribute.flag(builder, 'noreturn'))
  const value = yield* FunctionBody.callAssembly(
    body,
    yield* LlvmType.functionType(builder, resultType, parameters),
    operation.assembly.template,
    NativeAssembly.llvmConstraints(operation.assembly, program.layout.target),
    operation.arguments.map((argument) => NativeStorage.readScalar(storage, argument)),
    Type.equals(result, Type.unit) ? undefined : `assembly${operation.destination.ordinal}`,
    { sideEffect: operation.assembly.sideEffects, alignStack: false, canThrow: false },
    {
      attributes: yield* Attribute.functionSet(builder, {
        functionAttributes: yield* Attribute.set(builder, properties),
      }),
    },
  )
  if (!operation.assembly.noReturn && operation.assembly.memory !== 'none')
    for (const root of [...storage.addressRoots].sort((a, b) => a - b))
      yield* NativeStorage.reloadAddressRoot(storage, root)
  if (!Type.equals(result, Type.unit) && value === undefined)
    throw new RangeError('Assembly result was not emitted')
  storage.locals.set(operation.destination.ordinal, value === undefined ? [] : [value])
})

/** Emits the admitted naked body in one LLVM block, with no compiler-created control edges. */
export const emitNaked = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  body: FunctionBody.FunctionBody,
  fn: Mir.MirFunction,
  target: Target.Target,
) {
  const operation = fn.regions.flatMap(Mir.operationsOf)[0]
  if (operation?._tag !== 'NativeAssembly')
    throw new RangeError('Naked MIR lost its assembly operation')
  yield* LlvmBlock.make(body, 'entry')
  yield* FunctionBody.callAssembly(
    body,
    yield* LlvmType.functionType(builder, yield* LlvmType.voidType(builder), []),
    operation.assembly.template,
    NativeAssembly.llvmConstraints(operation.assembly, target),
    [],
    undefined,
    { sideEffect: true, alignStack: false, canThrow: false },
    {
      attributes: yield* Attribute.functionSet(builder, {
        functionAttributes: yield* Attribute.set(builder, [
          yield* Attribute.flag(builder, 'noreturn'),
        ]),
      }),
    },
  )
  yield* FunctionBody.unreachable(body)
})
