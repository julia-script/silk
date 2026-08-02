# How to add debug metadata

This guide shows you how to preserve a compile unit, a function subprogram, and an instruction
source location. It assumes you already have a function body to annotate.

Create the builder with `strip: false`; debug constructors otherwise return `undefined` and retain
no debug state.

```typescript
import * as Effect from 'effect/Effect'
import * as Block from '@silk-effect/llvm/Block'
import * as Builder from '@silk-effect/llvm/Builder'
import * as DISPFlags from '@silk-effect/llvm/DISPFlags'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Metadata from '@silk-effect/llvm/Metadata'
import { SilkError } from '@silk-effect/llvm/SilkError'
import * as Type from '@silk-effect/llvm/Type'

const program = Effect.gen(function* () {
  // Debug constructors only retain nodes when stripping is disabled.
  const builder = yield* Builder.make({ strip: false, sourceFilename: 'main.tiny' })

  // Describe the source file that owns the compile unit.
  const filename = yield* Metadata.string(builder, 'main.tiny')
  const file = yield* Metadata.file(builder, filename)
  if (file === undefined) {
    return yield* Effect.fail(
      new SilkError({ operation: 'Guide.debug', message: 'debug metadata was stripped', cause: file }),
    )
  }

  const producer = yield* Metadata.string(builder, '@silk-effect/llvm guide')
  const unit = yield* Metadata.compileUnit(builder, file, producer)
  if (unit === undefined) {
    return yield* Effect.fail(
      new SilkError({ operation: 'Guide.debug', message: 'compile unit was stripped', cause: unit }),
    )
  }
  // Named metadata makes the compile unit reachable from module output.
  yield* Metadata.named(builder, 'llvm.dbg.cu', [unit])

  // Describe the source-level function and attach it to the LLVM declaration.
  const name = yield* Metadata.string(builder, 'main')
  const subprogram = yield* Metadata.subprogram(builder, file, name, {
    line: 1,
    scopeLine: 1,
    spFlags: DISPFlags.make({ definition: true }),
    compileUnit: unit,
  })
  if (subprogram === undefined) {
    return yield* Effect.fail(
      new SilkError({ operation: 'Guide.debug', message: 'subprogram was stripped', cause: name }),
    )
  }

  const voidType = yield* Type.voidType(builder)
  const signature = yield* Type.functionType(builder, voidType, [])
  const fn = yield* FunctionActor.declare(builder, 'main', signature)
  yield* FunctionActor.setSubprogram(builder, fn, subprogram)

  // Attach one source location to the function's return instruction.
  yield* FunctionActor.buildBody(
    builder,
    fn,
    Effect.fn('Guide.debugBody')(function* (body) {
      yield* Block.make(body, 'entry')
      const returned = yield* FunctionBody.returnVoid(body)
      const location = yield* Metadata.location(builder, 2, 1, subprogram)
      yield* FunctionBody.setDebugLocation(body, returned, location)
    }),
  )
})

await Effect.runPromise(program)
```

## Follow the metadata chain

Debug information is a graph of handles, so the order reflects their dependencies:

1. `Builder.make({ strip: false })` enables retention. The APIs still return optional values because
   the same functions also support stripped builders; the checks narrow those values before use.
2. `Metadata.string` creates the filename bytes, and `Metadata.file` turns them into a source-file
   node.
3. `Metadata.compileUnit` describes the translation unit. Adding it to `llvm.dbg.cu` makes the unit
   reachable from module output.
4. `Metadata.subprogram` describes the source-level `main` function. `Function.setSubprogram`
   attaches that description to the LLVM function declaration.
5. `Metadata.location` identifies line 2, column 1 inside the subprogram. The final call attaches
   that location to the `ret void` instruction, so rendered IR includes a `!dbg` reference there.

Attach the compile unit to `llvm.dbg.cu`, attach the subprogram to the function, and attach locations
to individual instructions. Metadata values are builder-owned handles and cannot be reused in
another builder.

For recursive type graphs, create a typed placeholder with `Metadata.forward(builder, 'type')` and
resolve it exactly once with `Metadata.resolveForward`. An unresolved placeholder fails during text
or bitcode output only when it is reachable from named metadata or an attachment.
