# How to declare globals, aliases, and functions

This guide shows you how to populate one module-level symbol table with a constant global, an
alias, and an external function declaration. It assumes you already know LLVM's distinction
between a global value and its pointer.

```typescript
import * as Effect from 'effect/Effect'
import * as Alias from '@silklang/llvm/Alias'
import * as Attribute from '@silklang/llvm/Attribute'
import * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as IrText from '@silklang/llvm/IrText'
import * as Type from '@silklang/llvm/Type'
import * as Variable from '@silklang/llvm/Variable'

const program = Effect.gen(function* () {
  const builder = yield* Builder.make({ sourceFilename: 'declarations.ll' })
  const i32 = yield* Type.integer(builder, 32)

  // Define an internal constant global: @answer = 42.
  const initializer = yield* Constant.integerUnsigned(builder, i32, 42)
  const answer = yield* Variable.make(builder, 'answer', i32, {
    constant: true,
    initializer,
    linkage: 'internal',
  })

  // Reuse the global's address as the aliasee.
  const answerPointer = yield* Constant.fromGlobal(builder, yield* Variable.global(builder, answer))
  yield* Alias.make(builder, 'answer_alias', i32, answerPointer)

  // Declare an external function with a nounwind function attribute.
  const signature = yield* Type.functionType(builder, i32, [i32])
  const nounwind = yield* Attribute.flag(builder, 'nounwind')
  const attributes = yield* Attribute.functionSet(builder, {
    functionAttributes: yield* Attribute.set(builder, [nounwind]),
  })
  yield* FunctionActor.declare(builder, 'transform', signature, { attributes })

  return yield* IrText.render(builder)
})

const text = await Effect.runPromise(program)
```

## What the example does

1. The builder owns the module, and `Type.integer(builder, 32)` interns the `i32` type inside it.
2. `Constant.integerUnsigned` creates the initializer value `42`. `Variable.make` uses it to
   declare an internal constant named `answer`.
3. An LLVM alias points at a global address rather than at the variable declaration wrapper.
   `Variable.global` obtains the shared global identity, and `Constant.fromGlobal` turns that
   identity into the pointer constant accepted by `Alias.make`.
4. The function declaration is assembled from two independent pieces: its `i32 (i32)` signature
   and a function attribute set containing `nounwind`.
5. `IrText.render` snapshots the builder after all three names have entered the shared global
   symbol table.

`text` contains all three symbols:

```llvm
@answer = internal constant i32 42
@answer_alias = alias i32, ptr @answer
declare i32 @transform(i32) nounwind
```

Declaration names share one ordered namespace. Repeating a compatible function declaration
returns its existing identity. An incompatible redeclaration or a collision with a variable or
alias fails with `LlvmError` without inserting another symbol.

For the complete module-level actor list, refer to the
[actor reference](../reference/actors.md#module-level-declarations).
