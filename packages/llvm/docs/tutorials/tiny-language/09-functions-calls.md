# Build Tiny, a compiled language: Resolve functions and calls

**Lesson 9 of 13** · [Previous: Lower expressions into SSA values](./08-ssa-expressions.md) ·
[Next: Lower conditionals with PHI nodes](./10-conditionals-phi.md)

In this lesson, we will grow from one `main` body to a module of user-defined Tiny functions. A
declaration pass will collect every function before a body pass resolves parameters and calls.
That small two-pass design enables forward references and recursion without a JIT.

## Separate Tiny's two scopes

Tiny has two kinds of names:

- function names live in the whole module; and
- parameter names live only in one function body.

Add a compiler-owned function table entry:

```typescript
interface FunctionEntry {
  readonly handle: FunctionActor.Function
  readonly arity: number
  readonly definition: Program.FunctionDefinition
}
```

The LLVM handle is an implementation detail of lowering, not part of the AST. Arity supports a
Tiny-specific diagnostic before LLVM sees a malformed call. The source definition supplies spans
and the body for the second pass.

Extend `LoweringContext` with the module function table and a per-body parameter map:

```typescript
readonly functions: ReadonlyMap<string, FunctionEntry>
readonly parameters: ReadonlyMap<string, Value.Value>
```

## Validate Tiny names before creating LLVM state

At the beginning of `Compiler.compile`, scan definitions into a `Set<string>`. Yield a
`DuplicateFunction` diagnostic at the second definition instead of relying on LLVM's global-name
collision error.

Then find `main`. Tiny programs require exactly this entry shape:

```text
fn main() = expression
```

A missing `main` or a parameterized `main` yields `InvalidMain`. Other functions may have any
number of parameters, but all parameters and return values are still `i32`.

## Pass one: declare every signature

Create the builder and `i32` once. Then walk the source definitions without building bodies:

```typescript
for (const definition of program.functions) {
  const parameterTypes = Array.from({ length: definition.parameters.length }, () => i32)
  const signature = yield* Type.functionType(builder, i32, parameterTypes)
  const handle = yield* FunctionActor.declare(builder, definition.name, signature)
  functions.set(
    definition.name,
    Object.freeze({ handle, arity: definition.parameters.length, definition }),
  )
}
```

For this deliberately reordered source:

```text
fn main() = double(21)
fn double(x) = x * 2
```

the checkpoint after pass one is:

| Tiny name | Arity | LLVM symbol | Body committed? |
| --- | ---: | --- | --- |
| `main` | 0 | `@main` | no |
| `double` | 1 | `@double` | no |

`main` can therefore refer to `@double` even though `double` appears later in the file. The table
contains the identity before either body is lowered.

## Pass two: build bodies and bind parameters

Walk the definitions again. Look up each declared entry and call `Function.buildBody`. Immediately
after creating `entry`, resolve every positional LLVM argument once:

```typescript
const parameters = new Map<string, Value.Value>()
for (const [index, parameter] of definition.parameters.entries()) {
  parameters.set(parameter.name, yield* Value.argument(body, index))
}
```

The parser already rejected duplicate parameters, so the map is one-to-one. A `Name` expression
now looks up its value here. If it is absent, yield `UnknownName` at that expression's span.

Pass the parameter map, shared function table, builder, body, and `i32` into recursive expression
lowering. Return the lowered body value exactly as in Lesson 8.

## Lower direct calls

For a `Call`, first find the function entry. An absent entry yields `UnknownFunction`. Compare the
source argument count to `entry.arity` before lowering the arguments; a mismatch yields
`WrongArity` with expected and actual counts.

Lower each argument from left to right, then call the declared handle:

```typescript
const result = yield* FunctionBody.callDirect(
  context.body,
  target.handle,
  arguments_,
  'called',
)
```

`callDirect` inherits the target's declared signature and checks the argument types. Its result is
optional because LLVM also supports `void` functions. Tiny does not. If the result is `undefined`,
yield a `MissingCallResult` compiler diagnostic instead of using a non-null assertion.

The forward-call program now renders:

```llvm
define i32 @main() {
entry:
  %called = call i32 @double(i32 21)
  ret i32 %called
}

define i32 @double(i32 %v0) {
entry:
  %multiplied = mul i32 %v0, 2
  ret i32 %multiplied
}
```

Compile it with LLVM 22 Clang and run it. The exit status should be `42`.

## See why recursion does not imply a JIT

Because pass one declares a function before pass two builds its body, that body can call its own
handle:

```text
fn recur(n) = recur(n)
fn main() = 1
```

The first body contains an ordinary static IR call:

```llvm
%called = call i32 @recur(i32 %v0)
```

No source is compiled at runtime. Clang resolves the emitted symbol when it compiles and links the
module, and the native machine-code function calls itself when executed. That is recursion in an
ahead-of-time compiled program, not JIT compilation.

Add `examples/factorial.tiny` as the useful recursive checkpoint:

```text
fn factorial(n) = if n < 2 then 1 else n * factorial(n - 1)
fn main() = factorial(5)
```

The function name and recursive call now resolve, but the compiler should still reject its `If`
expression. That temporary failure is intentional. Lesson 10 will add the control-flow lowering
needed to compile and execute the fixture, whose result will be `120`.

## Verify resolution failures

Add tests for the reordered forward call, a self-call, duplicate function names, an unknown
parameter, an unknown function, and wrong arity. Run:

```sh
pnpm typecheck
pnpm test
```

There should now be nineteen passing tests. If a forward call is unknown, confirm declarations and
bodies are separate loops. If a parameter is unknown, inspect the order used by `Value.argument`
and the source parameter list. If LLVM reports call type mismatch instead of Tiny reporting wrong
arity, perform the arity check before `callDirect`.

We now have all the names and calls needed by the final program. Next, we will lower `if` into
basic-block branches and merge its two `i32` results with a PHI node; that will also make the
factorial fixture executable.

[Previous: Lower expressions into SSA values](./08-ssa-expressions.md) ·
[Next: Lower conditionals with PHI nodes](./10-conditionals-phi.md)
