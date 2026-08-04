# Build Tiny, a compiled language: Parse complete programs

**Lesson 6 of 13** · [Previous: Build the expression tree](./05-precedence-ast.md) ·
[Next: Run the first native program](./07-first-native-program.md)

In this lesson, we will turn the arithmetic parser into the complete Tiny frontend. By the end,
one call to `Parser.parse` will produce an immutable program containing functions, parameters,
calls, comparisons, and expression-valued conditionals.

The parser still will not know anything about LLVM. Its job is to describe what the source says;
later lessons will decide how that description becomes LLVM IR.

## Write down the language

Before extending the parser, record Tiny's grammar. This EBNF is the contract between the lexer
and parser:

```ebnf
program             = functionDefinition*, EOF ;
functionDefinition  = "fn", Identifier, "(", parameterList?, ")", "=", expression ;
parameterList       = Identifier, { ",", Identifier } ;

expression          = ifExpression | comparison ;
ifExpression        = "if", expression, "then", expression, "else", expression ;
comparison          = additive, { ("<" | ">"), additive } ;
additive            = multiplicative, { ("+" | "-"), multiplicative } ;
multiplicative      = unary, { ("*" | "/"), unary } ;
unary               = "-", unary | primary ;
primary             = Integer
                    | Identifier, ("(", argumentList?, ")")?
                    | "(", expression, ")" ;
argumentList        = expression, { ",", expression } ;
```

There are no semicolons. A function body ends when its expression is complete; the next `fn` or
EOF begins the next top-level step. A bare identifier is a parameter reference. An identifier
immediately followed by `(` is a direct function call.

Comparisons have precedence `5`, below addition at `10` and multiplication at `20`. An `if` has
the lowest precedence because its condition and both branches accept complete expressions.

## Model a program separately from an expression

Create `src/Program.ts`. Add immutable `Parameter`, `FunctionDefinition`, and `Program` interfaces:

```typescript
export interface FunctionDefinition {
  readonly name: string
  readonly parameters: ReadonlyArray<Parameter>
  readonly body: Expression.Expression
  readonly start: number
  readonly end: number
}

export interface Program {
  readonly functions: ReadonlyArray<FunctionDefinition>
  readonly start: number
  readonly end: number
}
```

Add sibling constructors that freeze the value and its arrays. Also add `Program.render`, using the
same small S-expression notation as `Expression.render`. This formatter is a learning and testing
tool, not part of Tiny's syntax.

This extra actor gives later phases a useful boundary: expressions describe computations;
`Program` describes the definitions that own them.

## Parse names and calls

In `parsePrimary`, keep the existing identifier token after consuming it. Look at the next token:

- if it is not `LeftParen`, return `Expression.name`;
- if it is `LeftParen`, parse zero or more comma-separated expressions, require `RightParen`, and
  return `Expression.call`.

Arguments use the full expression parser, so calls such as `f(1, g(2 + 3))` work without a second
argument grammar.

Change the parenthesized-expression branch to parse a full expression too. Without that change,
an `if` inside parentheses would fail even though the grammar permits it.

## Put comparisons below arithmetic

Extend the binary operator table:

```typescript
const binaryOperators: ReadonlyMap<Token.Kind, BinaryInfo> = new Map([
  ['Less', { operator: '<', precedence: 5 }],
  ['Greater', { operator: '>', precedence: 5 }],
  ['Plus', { operator: '+', precedence: 10 }],
  ['Minus', { operator: '-', precedence: 10 }],
  ['Star', { operator: '*', precedence: 20 }],
  ['Slash', { operator: '/', precedence: 20 }],
])
```

Now `x - y < 0` becomes `(< (- x y) 0)`. The AST records both the subtraction and the fact that it
happens before the comparison.

## Parse conditional expressions

Add a private `parseIf`. It consumes `If`, parses the condition, requires `Then`, parses the true
branch, requires `Else`, and parses the false branch. Each recursive step accepts a complete
expression.

Then make the private expression entry point choose between an `if` and binary parsing:

```typescript
const parseExpressionInternal = Effect.fnUntraced(function* (
  state: State,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  return current(state).kind === 'If' ? yield* parseIf(state) : yield* parseBinary(state, 0)
})
```

For nested conditionals, the recursive inner call consumes its own `else` before returning. Thus:

```text
if x then if y then 1 else 2 else 3
```

has this unambiguous shape:

```text
(if x (if y 1 2) 3)
```

## Parse definitions and the program

Add a private `parseFunction` for:

```text
fn name(parameter, parameter) = expression
```

While reading the parameters, keep a local `Set<string>`. If a name is already present, yield a
`ParseError` at the duplicate token with `expected` set to `a unique parameter name`. This is a
local syntactic invariant, so the parser owns it. Unknown names and wrong call arities need the
complete function table and will belong to lowering.

Expose the program operation:

```typescript
export const parse = Effect.fn('Parser.parse')(function* (
  tokens: ReadonlyArray<Token.Token>,
): Effect.fn.Return<Program.Program, Diagnostic.ParseError> {
  const state: State = { tokens, index: 0 }
  const functions: Array<Program.FunctionDefinition> = []
  while (current(state).kind === 'Fn') functions.push(yield* parseFunction(state))
  const eof = yield* expect(
    state,
    'Eof',
    "a function definition beginning with 'fn' or end of input",
  )
  return Program.make(functions, functions.at(0)?.start ?? eof.start, eof.end)
})
```

The explicit EOF check is important. `fn main() = 1 nope` must fail at `nope`; accepting the valid
prefix would hide a source error.

## Check the complete AST

Create `examples/score.tiny`:

```text
fn abs(x) = if x < 0 then -x else x
fn score(x, y) = abs(x - y) * 3 + 2
fn main() = score(4, 10)
```

Tokenize it, parse it, and render the result. The checkpoint is:

```text
(program
  (fn abs (x) (if (< x 0) (- x) x))
  (fn score (x y) (+ (* (call abs (- x y)) 3) 2))
  (fn main () (call score 4 10)))
```

`Program.render` emits this on one line; it is wrapped above only for readability. Notice what is
absent: the tree contains no LLVM types, blocks, instructions, values, or handles. Parsing has
fixed Tiny's structure without choosing a lowering strategy.

Add focused tests for a nested call, the nested conditional above, a missing `else`, duplicate
parameters, and trailing syntax. Then run:

```sh
pnpm typecheck
pnpm test
```

You should now have eleven passing lexer and parser tests.

If a call fails at its comma, confirm each argument uses `parseExpressionInternal`. If a missing
`else` is reported at the start of the true branch, the true branch probably did not finish before
`expect('Else')`. If trailing text is accepted, ensure the public program parser explicitly
requires EOF after its function loop.

You now have a complete frontend contract: source becomes tokens, then one immutable AST or a
source-spanned typed diagnostic. In Lesson 7, we will cross the LLVM boundary for the first time
and run a Tiny-authored `main` function.

[Previous: Build the expression tree](./05-precedence-ast.md) ·
[Next: Run the first native program](./07-first-native-program.md)
