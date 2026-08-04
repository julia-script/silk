# Build Tiny, a compiled language: Build the expression tree

**Lesson 5 of 13** · [Previous: Tokenize source](./04-tokenize-source.md) ·
[Next: Parse complete programs](./06-parse-programs.md)

In this lesson, we will turn arithmetic tokens into an abstract syntax tree. The tree—not LLVM—will
decide that multiplication happens before addition and that subtraction groups from the left.

Our first checkpoint is this transformation:

```text
1 + 2 * 3  →  (+ 1 (* 2 3))
```

## Model expressions as immutable data

Create `src/Expression.ts`. Define a discriminated union with these variants:

```typescript
export type Expression = Integer | Name | Unary | Binary | Call | If
```

Every variant has `start` and `end` offsets. `Integer` stores a `bigint`; `Name` stores an
identifier; `Unary` owns one operand; and `Binary` owns its left and right expressions. Add `Call`
and `If` now as destination shapes—we will construct them in Lesson 6.

Keep the data free of parser or LLVM methods. Add sibling constructors such as:

```typescript
export const binary = (
  operator: BinaryOperator,
  left: Expression,
  right: Expression,
  start: number,
  end: number,
): Binary => Object.freeze({ _tag: 'Binary', operator, left, right, start, end })
```

Finally, add `Expression.render`, a small recursive formatter used only to make tree shapes obvious
in tests. It renders a binary node as `(<operator> <left> <right>)`.

## Give operators explicit precedence

Create `src/Parser.ts` and define the arithmetic table:

```typescript
const binaryOperators: ReadonlyMap<Token.Kind, BinaryInfo> = new Map([
  ['Plus', { operator: '+', precedence: 10 }],
  ['Minus', { operator: '-', precedence: 10 }],
  ['Star', { operator: '*', precedence: 20 }],
  ['Slash', { operator: '/', precedence: 20 }],
])
```

The numbers matter only relative to one another. Multiplication and division bind more tightly
because `20` is greater than `10`.

The parser keeps a private token array and cursor. `parsePrimary` accepts integers, identifiers,
and parenthesized expressions. `parseUnary` consumes `-` and recursively parses its operand before
binary parsing begins.

## Climb precedence

Implement the binary loop:

```typescript
const parseBinary = Effect.fnUntraced(function* (
  state: State,
  minimumPrecedence: number,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  let left = yield* parseUnary(state)

  while (true) {
    const info = binaryOperators.get(current(state).kind)
    if (info === undefined || info.precedence < minimumPrecedence) return left

    advance(state)
    const right = yield* parseBinary(state, info.precedence + 1)
    left = Expression.binary(info.operator, left, right, left.start, right.end)
  }
})
```

For `1 + 2 * 3`, the cursor and minimum precedence move like this:

| Step | Remaining input | Minimum | Result being built |
| --- | --- | ---: | --- |
| Parse left | `+ 2 * 3` | 0 | `1` |
| Consume `+` | `2 * 3` | 0 | parse right at 11 |
| See `*` on right | `* 3` | 11 | `*` has 20, so keep climbing |
| Finish right | end | 11 | `(* 2 3)` |
| Finish outer | end | 0 | `(+ 1 (* 2 3))` |

The table is also a text description of the tree:

```text
Binary +
├── Integer 1
└── Binary *
    ├── Integer 2
    └── Integer 3
```

Passing `info.precedence + 1` while parsing the right side makes equal-precedence operators group
from the left. `10 - 3 - 2` therefore becomes `(- (- 10 3) 2)`.

## Expose one parsing operation

Add a typed `ParseError` to `Diagnostic.ts`, then expose:

```typescript
export const parseExpression = Effect.fn('Parser.parseExpression')(function* (
  tokens: ReadonlyArray<Token.Token>,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  const state: State = { tokens, index: 0 }
  const expression = yield* parseBinary(state, 0)
  yield* expect(state, 'Eof', 'end of input')
  return expression
})
```

Requiring EOF prevents the parser from silently accepting only the first part of an expression.

## Verify calculation order

Add `test/Parser.test.ts` and assert these rendered trees:

```text
1 + 2 * 3    → (+ 1 (* 2 3))
(1 + 2) * 3  → (* (+ 1 2) 3)
10 - 3 - 2   → (- (- 10 3) 2)
-2 * 3       → (* (- 2) 3)
```

Run:

```sh
pnpm typecheck
pnpm test
```

All parser and lexer tests should pass. At this point, changing LLVM code could not alter the
calculation order: the AST already records it.

If multiplication appears outside addition, inspect the comparison between `info.precedence` and
`minimumPrecedence`. If subtraction groups from the right, confirm the recursive right-hand call
uses `info.precedence + 1`. If the cursor stops at `)`, make sure `parsePrimary` recursively parses
inside the parentheses and then explicitly consumes `RightParen`.

Next, we will place these expressions inside function definitions and add calls, comparisons, and
`if/then/else`.

[Previous: Tokenize source](./04-tokenize-source.md) ·
[Next: Parse complete programs](./06-parse-programs.md)
