# Build Tiny, a compiled language: Compile the complete program

**Lesson 11 of 13** · [Previous: Lower conditionals with PHI nodes](./10-conditionals-phi.md) ·
[Next: Diagnose failures and emit bitcode](./12-diagnostics-bitcode.md)

In this lesson, we will compose the compiler's phases behind one source-oriented operation, finish
the file-based CLI, and run the three-function program promised in Lesson 1. The written path ends
with native exit status `20` and does not depend on a browser playground.

## Make source the compiler's public input

The compiler currently lowers an already-parsed `Program`. Rename that private operation to
`lowerProgram`, then define the successful result exposed to consumers:

```typescript
export interface Compilation {
  readonly source: string
  readonly tokens: ReadonlyArray<Token.Token>
  readonly program: Program.Program
  readonly ir: string
}
```

Expose one named pipeline:

```typescript
export const compile = Effect.fn('Compiler.compile')(function* (
  source: string,
): Effect.fn.Return<
  Compilation,
  | Diagnostic.LexError
  | Diagnostic.ParseError
  | Diagnostic.ResolutionError
  | Diagnostic.CompileError
  | LlvmError.LlvmError
> {
  const tokens = yield* Lexer.tokenize(source)
  const program = yield* Parser.parse(tokens)
  const ir = yield* lowerProgram(program)
  return Object.freeze({ source, tokens, program, ir })
})
```

The operation completes lexing, parsing, Tiny validation and resolution, two-pass LLVM lowering,
body validation, and textual rendering. Each phase keeps its own typed error. The returned
artifacts let tests inspect a token or AST without rerunning earlier work, and a future compile-only
playground can display the same values. The CLI only needs `ir`.

## Add Node's Effect services at the application edge

Install the Node platform layer matching Effect:

```sh
pnpm add @effect/platform-node@4.0.0-beta.102
```

Use `effect/FileSystem` and `effect/Stdio` inside the CLI, then provide `NodeServices.layer` once at
the edge. This keeps file and stream failures in Effect instead of allowing a throwing Node API to
cross into the program.

The core of `src/Cli.ts` is:

```typescript
const program = Effect.gen(function* () {
  const stdio = yield* Stdio.Stdio
  const fileSystem = yield* FileSystem.FileSystem
  const args = yield* stdio.args
  const sourcePath = args.at(0)
  if (sourcePath === undefined) {
    return yield* new Diagnostic.CliError({ message: 'Usage: tiny <source.tiny>' })
  }

  const source = yield* fileSystem.readFileString(sourcePath)
  const compilation = yield* Compiler.compile(source)
  yield* Stream.make(compilation.ir).pipe(Stream.run(stdio.stdout()))
})
```

On failure, render one concise diagnostic to `stdio.stderr()` and retain the failure so
`NodeRuntime.runMain` sets a nonzero exit code. Disable its additional automatic error report to
avoid duplicate output. Provide `NodeServices.layer`, then run the Effect with
`NodeRuntime.runMain`.

The stream boundary gives the CLI a strict contract:

- success writes only valid LLVM IR to stdout;
- failure writes no IR to stdout and one diagnostic to stderr; and
- Clang is never invoked inside the compiler or `@silk-effect/llvm`.

## Compile the central program

Use `examples/score.tiny`:

```text
fn abs(x) = if x < 0 then -x else x
fn score(x, y) = abs(x - y) * 3 + 2
fn main() = score(4, 10)
```

Build the CLI and redirect its quiet stdout:

```sh
mkdir -p build
pnpm build
node build-js/Cli.js examples/score.tiny > build/score.ll
```

The direct `node` command has no package-manager status text to pollute the file. If you use the
example's combined smoke script instead, use `pnpm --silent smoke > build/score.ll`.

Inspect the module:

```sh
rg '^define i32 @' build/score.ll
```

The checkpoint is three definitions:

```text
define i32 @abs(i32 %v0) {
define i32 @score(i32 %v0, i32 %v1) {
define i32 @main() {
```

The `score` body should subtract its parameters, call `abs`, multiply the result by `3`, add `2`,
and return that SSA value. `main` should call `score(i32 4, i32 10)`.

## Compile with Clang and run

On POSIX:

```sh
/opt/homebrew/opt/llvm/bin/clang build/score.ll -o build/score
./build/score
result_code=$?
echo "$result_code"
```

Replace the Clang path with your LLVM 22 executable when needed. Capture `$?` immediately: every
later command replaces it. The result must be:

```text
20
```

On PowerShell:

```powershell
clang build/score.ll -o build/score.exe
./build/score.exe
$resultCode = $LASTEXITCODE
$resultCode
```

Again, copy `$LASTEXITCODE` before running another command.

The computation is now entirely Tiny-authored: `main` calls `score`; `score` calls `abs`; `abs`
chooses its result through LLVM control flow; the native process returns `20`.

## Verify the clean failure boundary

Create an invalid file containing `fn main() = missing()` and run the built CLI while redirecting
stdout and stderr separately. The process should exit nonzero, stdout should be empty, and stderr
should contain a line like:

```text
ResolutionError: Unknown function "missing" [12, 21)
```

If Clang says `expected top-level entity` at line 1, inspect the start of `score.ll`; a package
manager, logger, or debug statement probably wrote status text to stdout. Run the CLI directly or
use `pnpm --silent`. If the shell prints `0` after a program that should return `20`, capture the
status immediately after the executable. If Clang links fail with a missing `main`, inspect the IR
definitions and confirm the compiler accepted only a zero-parameter Tiny `main`.

## Confirm the tutorial stands alone

From a clean temporary consumer containing the packed local `@silk-effect/llvm` tarball, run:

```sh
pnpm typecheck
pnpm test
pnpm build
node build-js/Cli.js examples/score.tiny > build/score.ll
clang build/score.ll -o build/score
./build/score
```

The tests pass and the executable returns `20` without a browser, server, JIT, or repository-source
import. A playground can later call `Compiler.compile(source)` and show `tokens`, `program`, or
`ir`, but it is an optional interface over the same portable compiler.

You have built and run a small compiled language from source to native execution. Lesson 12 will
make failures more helpful and compare the readable `.ll` artifact with LLVM bitcode.

[Previous: Lower conditionals with PHI nodes](./10-conditionals-phi.md) ·
[Next: Diagnose failures and emit bitcode](./12-diagnostics-bitcode.md)
