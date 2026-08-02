# `@silk-effect/llvm`

Effect-native LLVM IR construction and deterministic text and bitcode emission, ported from Zig's
standard-library LLVM builder.

The runtime has no dependency on Zig, native LLVM libraries, filesystem access, or external
executables. It returns LLVM assembly as a `string` and bitcode as a `Uint8Array`.

```typescript
import * as Effect from 'effect/Effect'
import * as Bitcode from '@silk-effect/llvm/Bitcode'
import * as Block from '@silk-effect/llvm/Block'
import * as Builder from '@silk-effect/llvm/Builder'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as IrText from '@silk-effect/llvm/IrText'
import * as Type from '@silk-effect/llvm/Type'
import * as Value from '@silk-effect/llvm/Value'

const program = Effect.gen(function* () {
  const builder = yield* Builder.make({ sourceFilename: 'add.ll' })

  // Declare add(i32, i32) -> i32.
  const i32 = yield* Type.integer(builder, 32)
  const signature = yield* Type.functionType(builder, i32, [i32, i32])
  const add = yield* FunctionActor.declare(builder, 'add', signature)

  yield* FunctionActor.buildBody(
    builder,
    add,
    Effect.fn('Readme.addBody')(function* (body) {
      // Read the parameters, add them, and return the instruction result.
      yield* Block.make(body, 'entry')
      const left = yield* Value.argument(body, 0)
      const right = yield* Value.argument(body, 1)
      const sum = yield* FunctionBody.binary(body, 'add', left, right, 'sum')
      yield* FunctionBody.returnValue(body, sum)
    }),
  )

  return {
    text: yield* IrText.render(builder),
    bytes: yield* Bitcode.encode(builder),
  }
})

const output = await Effect.runPromise(program)
console.log(output.text)
```

The example moves through the package's main lifecycle:

1. Create one module-owning `Builder`.
2. Build and intern the function's `i32 (i32, i32)` type.
3. Declare `add`, then construct its body inside the scoped `Function.buildBody` transaction.
4. Read the two arguments, emit an `add` instruction, and terminate the block with the result.
5. Render the committed module as text and encode the same state as bitcode bytes.

## Documentation

- [Build a tiny expression compiler](./docs/tutorials/tiny-expression-compiler.md)
- [How-to guides](./docs/README.md#solve-a-task)
- [Actor reference](./docs/reference/actors.md)
- [Behavior and guarantees](./docs/reference/behavior.md)
- [Design explanations](./docs/README.md#understand-the-design)

The package is organized as actor modules with explicit subpath exports. Prefer imports such as
`@silk-effect/llvm/Builder` and `@silk-effect/llvm/FunctionBody` over a growing import from the root
barrel.

## Scope and compatibility

The package builds LLVM module state and emits `.ll` text or `.bc` bytes. It does not provide a
JIT, optimizer pipeline, object-code backend, linker, filesystem integration, or process runner.

The authoritative compatibility baseline is Zig `6db520a4cd1ce2391c79d0d55b2b2d5297e133a3`, LLVM
`22.1.8`, and Node.js `>=22.13.0`. See [UPSTREAM.md](./UPSTREAM.md) for provenance and pinned
sources, and [the parity report](./parity/REPORT.md) for the supported upstream surface.

## Development verification

```sh
pnpm --filter @silk-effect/llvm fixtures:verify
pnpm --filter @silk-effect/llvm parity:validate
```

Fixture generation uses pinned Zig and LLVM tools, but runtime APIs never invoke them.
