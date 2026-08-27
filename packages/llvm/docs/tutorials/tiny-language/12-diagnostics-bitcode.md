# Build Tiny, a compiled language: Diagnose failures and emit bitcode

**Lesson 12 of 13** · [Previous: Compile the complete program](./11-complete-compiler.md) ·
[Next: Extend Tiny with remainder](./13-extend-remainder.md)

In this lesson, we will deliberately feed each compiler phase invalid input, inspect the phase that
owns the failure, and restore the successful checkpoint after every experiment. We will also
encode LLVM bitcode from the same committed module and distinguish it from bytecode, textual IR,
object files, and executables.

## Keep phase ownership in the error channel

Tiny now exposes these expected failure families:

| Tag | Owner | Representative failure |
| --- | --- | --- |
| `LexError` | Lexer | Unsupported source character |
| `ParseError` | Parser | Missing `else` or trailing syntax |
| `ResolutionError` | Tiny resolution | Unknown name, duplicate function, or wrong arity |
| `CompileError` | Tiny-to-LLVM lowering invariant | A Tiny `i32` call unexpectedly produces no value |
| `LlvmError` | `@silk-lang/llvm` | Invalid type, ownership, body, or serialization state |

`LexError`, `ParseError`, and `ResolutionError` carry `[start, end)` source offsets. `LlvmError`
keeps its package operation and discriminated reason unchanged. Do not catch these inside
`Compiler.compile` and flatten them into one string; callers may want different recovery for each
phase.

Update the CLI's one private renderer so source-owned failures include the tag, message, and span,
while LLVM failures include the exact operation:

```typescript
if (
  error instanceof Diagnostic.LexError ||
  error instanceof Diagnostic.ParseError ||
  error instanceof Diagnostic.ResolutionError ||
  error instanceof Diagnostic.CompileError
) {
  return `${error._tag}: ${error.message} [${error.start}, ${error.end})`
}
if (error instanceof LlvmError.LlvmError) {
  return `LlvmError: ${error.operation}: ${error.message}`
}
```

Formatting belongs at the application edge. The structured error remains available to tests and a
future playground.

## Break and restore one phase at a time

Use separate fixture files; do not replace `score.tiny`.

First run `examples/invalid-character.tiny`:

```text
fn main() = @
```

Expected stderr:

```text
LexError: Unexpected character "@" [12, 13)
```

Restore boundary: leave the fixture in place, but switch the CLI argument back to `score.tiny` and
confirm it still emits IR.

Next run `examples/missing-else.tiny`:

```text
fn main() = if 1 then 2
```

Expected stderr:

```text
ParseError: Expected 'else', found Eof [24, 24)
```

Restore boundary: run the parser tests, then return to `score.tiny`.

Now run `examples/unknown-function.tiny` and `examples/wrong-arity.tiny`. Their first stderr lines
should be:

```text
ResolutionError: Unknown function "missing" [12, 21)
ResolutionError: id expects 1 arguments, received 0 [25, 29)
```

Restore boundary: run all consumer tests and compile `score.tiny` again. These source failures are
values returned by separate compilations; they cannot mutate the canonical source file or reuse a
module builder from a successful compilation.

## Observe transactional body failure

Create a dedicated test builder, declare `answer`, and deliberately omit its terminator:

```typescript
const failure = yield* Effect.flip(
  FunctionActor.buildBody(
    builder,
    answer,
    Effect.fnUntraced(function* (body) {
      yield* Block.make(body, 'entry')
      // Deliberate break: no return terminator.
    }),
  ),
)
```

`Function.buildBody` validates the draft before commit. The Effect fails with `LlvmError`, and
rendering the builder still shows `declare i32 @answer()` rather than a partial definition.

Now restore the body:

```typescript
yield* FunctionActor.buildBody(
  builder,
  answer,
  Effect.fnUntraced(function* (body) {
    yield* Block.make(body, 'entry')
    const value = yield* Constant.integerSigned(builder, i32, 42)
    yield* FunctionBody.returnValue(body, value)
  }),
)
```

The retry succeeds and renders `define i32 @answer()` with `ret i32 42`. The failed draft released
its reservation and never leaked its block into module state. This transaction is why the compiler
can trust that a committed function body is structurally complete.

## Encode the same module as bitcode

Import `Bitcode` and extend the successful compilation artifact:

```typescript
export interface Compilation {
  readonly source: string
  readonly tokens: ReadonlyArray<Token.Token>
  readonly program: Program.Program
  readonly ir: string
  readonly bitcode: Uint8Array
}
```

After `buildProgram` returns one committed builder, serialize both formats:

```typescript
const builder = yield* buildProgram(program)
const ir = yield* IrText.render(builder)
const bitcode = yield* Bitcode.encode(builder)
return Object.freeze({ source, tokens, program, ir, bitcode })
```

Both serializers snapshot the same module. `IrText.render` produces the readable `.ll` used
throughout the tutorial. `Bitcode.encode` produces deterministic binary LLVM IR bytes in memory;
it does not invoke Clang, LLVM tools, a server, or the filesystem.

## Use the right name for each artifact

| Artifact | Typical extension | What it contains | Human-readable? |
| --- | --- | --- | --- |
| Tiny source | `.tiny` | Tiny grammar and functions | yes |
| Textual LLVM IR | `.ll` | Typed LLVM module as text | yes |
| LLVM bitcode | `.bc` | Binary serialization of LLVM IR | no |
| VM bytecode | varies | Instructions for a particular virtual machine | usually no |
| Object file | `.o` / `.obj` | Relocatable target machine code and metadata | no |
| Executable | platform-specific | Linked native program | no |

LLVM bitcode is not “Tiny bytecode.” Tiny does not define a virtual instruction set or bytecode
interpreter. Bitcode and `.ll` are two representations of the LLVM module produced after Tiny has
already been lowered.

Clang can consume either `.ll` or `.bc`, but `.ll` remains the primary tutorial artifact because
you can connect each line to the builder operations you wrote.

## Check deterministic bytes

Compile the same source twice and compare the `Uint8Array` values. Then inspect the first four
bytes:

```typescript
assert.deepStrictEqual(first.bitcode, second.bitcode)
assert.deepStrictEqual(Array.from(first.bitcode.slice(0, 4)), [0x42, 0x43, 0xc0, 0xde])
```

In hexadecimal, the LLVM bitcode magic header is:

```text
42 43 C0 DE
```

Run typecheck and tests. You should have twenty-five passing tests across lexer, parser, compiler,
bitcode, and transactional LLVM failure behavior. Finally, run `score.tiny` once more and confirm
its native exit status remains `20`.

If a source failure prints IR to stdout, make sure compilation completes before the stdout sink
runs. If an LLVM error lacks its operation, add the `LlvmError` renderer branch before the generic
fallback. If bitcode differs between identical compilations, compare the module options and source
order; the test must compile identical inputs with the same producer settings.

The compiler is complete and its failure boundaries are visible. The final lesson will ask you to
extend the language with `%` using the same lexer, parser, lowering, IR, and native validation
skills—without giving you the finished patch in the lesson itself.

[Previous: Compile the complete program](./11-complete-compiler.md) ·
[Next: Extend Tiny with remainder](./13-extend-remainder.md)
