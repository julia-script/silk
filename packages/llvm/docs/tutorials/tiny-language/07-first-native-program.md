# Build Tiny, a compiled language: Run the first native program

**Lesson 7 of 13** · [Previous: Parse complete programs](./06-parse-programs.md) ·
[Next: Lower expressions into SSA values](./08-ssa-expressions.md)

In this lesson, we will lower `fn main() = 42` into textual LLVM IR, ask Clang to compile it, and
run a function authored in Tiny. This is the smallest end-to-end success; later lessons will
generalize its literal body without changing the surrounding ownership model.

## Add a compile diagnostic

Add `CompileError` to `src/Diagnostic.ts`. For now it needs the operation, message, source span, and
an `UnsupportedProgram` reason. The lesson compiler will accept exactly one zero-parameter `main`
whose body is an integer. Rejecting every other AST explicitly is safer than silently generating
wrong IR for syntax we have not lowered yet.

LLVM operations already return `LlvmError` through Effect. Do not catch or replace those errors in
the compiler; the public error channel should retain both `CompileError` and `LlvmError`.

## Build the module one LLVM concept at a time

Create `src/Compiler.ts` and import the package's public actor subpaths:

```typescript
import * as Block from '@silk-effect/llvm/Block'
import * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as IrText from '@silk-effect/llvm/IrText'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import * as Type from '@silk-effect/llvm/Type'
```

After validating the AST shape, create the module and signature:

```typescript
const builder = yield* Builder.make({
  moduleName: 'tiny-language',
  sourceFilename: 'answer.tiny',
})
const i32 = yield* Type.integer(builder, 32)
const signature = yield* Type.functionType(builder, i32, [])
const main = yield* FunctionActor.declare(builder, 'main', signature)
```

These operations establish four things introduced in Lesson 2:

| Builder operation | LLVM structure |
| --- | --- |
| `Builder.make` | One module and one owner for every handle that follows |
| `Type.integer(builder, 32)` | The `i32` Tiny value type |
| `Type.functionType(builder, i32, [])` | A function returning `i32` with no parameters |
| `Function.declare(builder, 'main', signature)` | The module-global symbol `@main` |

Now construct the body transaction:

```typescript
yield* FunctionActor.buildBody(
  builder,
  main,
  Effect.fnUntraced(function* (body) {
    yield* Block.make(body, 'entry')
    const value = yield* Constant.integerSigned(builder, i32, literal.value)
    yield* FunctionBody.returnValue(body, value)
  }),
)
```

`Block.make` creates `entry` and makes it the insertion block. `Constant.integerSigned` creates
the typed operand `i32 42`. `returnValue` terminates the block with that operand. A basic block
must end in exactly one terminator; `Function.buildBody` validates that invariant before it commits
the body to the module.

Every type, constant, function, and body handle belongs to its builder or body transaction. Mixing
a type from another builder or an instruction from another function fails with `LlvmError` rather
than corrupting the module.

Finally, render the teaching artifact:

```typescript
return yield* IrText.render(builder)
```

`Compiler.compile` should be a named `Effect.fn` returning textual IR and preserving
`CompileError | LlvmError` in its error channel.

## Connect the existing stages

Update `src/Cli.ts` to run the three stages for the lesson source:

```typescript
const program = Effect.gen(function* () {
  const tokens = yield* Lexer.tokenize('fn main() = 42')
  const syntax = yield* Parser.parse(tokens)
  return yield* Compiler.compile(syntax)
})

console.log(await Effect.runPromise(program))
```

`Effect.runPromise` belongs here at the application edge. Lexer, parser, and compiler operations
remain composable Effects.

Run the smoke script. The significant output is:

```llvm
; ModuleID = 'tiny-language'
source_filename = "answer.tiny"

define i32 @main() {
entry:
  ret i32 42
}
```

The code and IR now line up directly: the function type produces `i32 @main()`, the block produces
`entry:`, and the typed constant plus terminator produce `ret i32 42`.

## Compile and run outside the compiler

Clang is a separate application step. On POSIX shells:

```sh
mkdir -p build
pnpm --silent smoke > build/answer.ll
/opt/homebrew/opt/llvm/bin/clang build/answer.ll -o build/answer
./build/answer
echo $?
```

Use the path printed by your LLVM 22 installation if it differs. On systems where `clang` already
selects version 22, the shorter `clang build/answer.ll -o build/answer` is enough. A warning that
Clang is overriding a missing module target triple is expected in this tutorial.

The final line should be:

```text
42
```

In PowerShell:

```powershell
New-Item -ItemType Directory -Force build | Out-Null
pnpm --silent smoke | Out-File -Encoding utf8 build/answer.ll
clang build/answer.ll -o build/answer.exe
./build/answer.exe
$LASTEXITCODE
```

The operating system exposes the low byte of `main`'s integer return as the process exit status.
We deliberately keep tutorial results between `0` and `255`.

## Test the checkpoint

Add `test/Compiler.test.ts`. Parse the lesson source, compile it, and assert the IR contains:

```text
define i32 @main()
ret i32 42
```

Also keep the complete known-good output in `fixtures/answer.ll`, then run typecheck and tests
against the packed local package. There should now be thirteen consumer tests.

If LLVM reports a return type mismatch, verify that both the function return type and constant use
the same builder-owned `i32`. If `Function.buildBody` reports an unterminated block, make sure
`returnValue` is the final body operation. If an error mentions a foreign handle or owner, trace
the handle back to its builder; do not cache LLVM handles across module compilations.

You have now run your own Tiny function as native code. Next, we will replace the one literal with
a recursive expression lowering function and see how LLVM's SSA form names intermediate results.

[Previous: Parse complete programs](./06-parse-programs.md) ·
[Next: Lower expressions into SSA values](./08-ssa-expressions.md)
