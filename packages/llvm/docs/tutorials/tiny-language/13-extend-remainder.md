# Build Tiny, a compiled language: Extend it yourself

**Lesson 13 of 13** · [Previous: Diagnose failures and emit bitcode](./12-diagnostics-bitcode.md)

You have followed one program from characters to native execution. In this final lesson, you will
add signed remainder, `%`, without a finished implementation patch in the lesson. The objective is
to prove that you can locate the affected compiler stages, preserve precedence, choose the correct
LLVM operation, and validate the result independently.

## Define the feature before changing code

Tiny remainder has this contract:

- `%` is a binary operator over signed `i32` values;
- it has the same precedence as `*` and `/`;
- operators at that precedence remain left-associative;
- LLVM lowering uses signed `srem`, not unsigned `urem`; and
- no function, name-resolution, or control-flow semantics change.

For this expression:

```text
10 + 7 % 4 * 2
```

the required AST rendering is:

```text
(+ 10 (* (% 7 4) 2))
```

Remainder and multiplication share a precedence level and group from the left, so the calculation
is `10 + ((7 % 4) * 2)`, which returns `16`.

## Identify every affected stage

Before editing, write down why each checked stage changes:

- [ ] Token vocabulary can represent `%` distinctly.
- [ ] The lexer recognizes one `%` character and preserves its source span.
- [ ] The AST's binary-operator type can store `%`.
- [ ] The parser assigns `%` multiplication-level precedence and left associativity.
- [ ] LLVM lowering maps `%` to signed `srem` and returns the resulting `i32` SSA value.
- [ ] Tests cover tokenization, AST grouping, IR, and native behavior.

Do not add a general operator framework, user-defined operators, or a second numeric type. This is
one small language feature using the architecture already present.

## Start from tests

Open `test/Remainder.test.ts`. Make the public checks pass in this order:

1. Tokenizing `7 % 4` produces `Integer`, the new remainder token, `Integer`, and `Eof`.
2. Parsing `10 + 7 % 4 * 2` renders `(+ 10 (* (% 7 4) 2))`.
3. Compiling `fn isOdd(n) = n % 2 fn main() = isOdd(7)` emits an `srem` instruction and a direct
   call to `isOdd`.

Run only this test while iterating, then run the complete suite:

```sh
pnpm test -- Remainder.test.ts
pnpm typecheck
pnpm test
```

Do not accept a native result as the only evidence. An incorrectly grouped expression can
occasionally produce the same small result; the AST checkpoint pins the language semantics before
LLVM enters the picture.

## Check native behavior

After the public tests pass, compile these two programs through the same CLI and LLVM 22 Clang
workflow used in Lesson 11.

Mixed precedence:

```text
fn main() = 10 + 7 % 4 * 2
```

Expected IR evidence and native result:

```text
srem i32 7, 4
exit status 16
```

Function call:

```text
fn isOdd(n) = n % 2
fn main() = isOdd(7)
```

Expected evidence:

```text
srem i32 %v0, 2
exit status 1
```

The non-published validation fixture records these same expectations so the exercise and reference
checks cannot drift.

## Use progressively specific hints

Try the exercise before opening a hint.

<details>
<summary>Hint 1: find the path</summary>

Trace `/` from its token kind through lexer punctuation, `BinaryOperator`, the parser operator
table, and the binary-expression lowering switch. Remainder crosses the same boundaries.

</details>

<details>
<summary>Hint 2: fix grouping</summary>

The parser's relative precedence number should match both multiplication and division. Do not
change the precedence-climbing algorithm: its recursive right-hand minimum already makes
equal-precedence operators left-associative.

</details>

<details>
<summary>Hint 3: fix LLVM semantics</summary>

Tiny's `/` uses the signed LLVM operation because all values are signed `i32`. Choose the signed
remainder operation from the same `FunctionBody.binary` API and give its SSA result a fresh name.

</details>

<details>
<summary>Hint 4: a lexer error remains</summary>

If `%` still produces `LexError`, the token type alone is insufficient. The lexer's single-character
punctuation table must map the source character to that token kind.

</details>

## Explain what did not change

Answer these questions in your own words before checking the reference validation:

1. Why does the function declaration table not change when `%` is added?
2. Why do parameter and function-name resolution not change?
3. Why does conditional block and PHI lowering not change?
4. Why is the parser test necessary even when the native result is correct?
5. Why would `urem` contradict the semantics already established for `/` and negative values?

The short architectural answer is that `%` creates no names, scopes, functions, or control-flow
edges. It is another expression node that consumes two already-resolved `i32` operands and
produces one `i32` value. Only stages that recognize, represent, group, or lower that operator
need modification.

## Completion criteria

The extension is complete when all of these are true:

- `%` has an exact source-spanned token;
- the mixed expression renders `(+ 10 (* (% 7 4) 2))`;
- generated IR contains `srem`, never `urem`;
- the mixed program exits `16` and `isOdd(7)` exits `1`;
- all earlier 25 tests still pass alongside the three remainder tests;
- `score.tiny` still exits `20`; and
- repository and package release checks remain green.

If an earlier compiler test changes unexpectedly, revert the unrelated edit and compare the `%`
path with `/` again. This feature should be additive and narrow.

## Where to go next

You now have a compact but genuine compiled language: a lexer, precedence-aware parser, immutable
AST, two-pass function resolver, recursive calls, signed arithmetic, conditionals, SSA values,
PHI nodes, structured diagnostics, textual IR, and bitcode.

Useful next experiments include:

- change the factorial fixture while keeping results under the exit-code limit;
- add equality or comments, beginning with a semantic contract and affected-stage checklist;
- add a compile-only browser playground with one source editor and tabs for tokens, AST, and IR;
  call `Compiler.compile(source)` in the browser and display its structured errors;
- keep that playground optional—do not add a server or make any written lesson depend on it; or
- replace exit statuses with an explicit host-facing printing function in a separate language
  extension.

The important result is not the size of Tiny. It is that you can now explain and extend the path
from source syntax to native behavior, one owned compiler phase at a time.

[Previous: Diagnose failures and emit bitcode](./12-diagnostics-bitcode.md)
