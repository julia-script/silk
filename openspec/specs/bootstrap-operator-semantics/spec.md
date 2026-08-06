# bootstrap-operator-semantics Specification

## Purpose

Define one closed, deterministic bootstrap expression-operator model that preserves qualified
data-first behavior while giving source programs conventional precedence and pipeline syntax.

## Requirements

### Requirement: The bootstrap operator surface is closed and ordered

The language SHALL recognize prefix `-` and `!`; multiplicative `*`, `/`, and `%`; additive `+`
and `-`; relational `<`, `<=`, `>`, and `>=`; equality `==` and `!=`; and pipeline `|>`. Grouping
parentheses SHALL override precedence. Primary and grouped expressions SHALL bind most tightly,
followed by right-associative prefix operators, left-associative multiplicative operators,
left-associative additive operators, non-associative relational operators, non-associative equality
operators, and left-associative pipelines. Chaining a non-associative comparison without explicit
grouping SHALL be a parser error rather than an implicit multi-way comparison.

#### Scenario: Apply arithmetic precedence

- **WHEN** a body returns `1 + 2 * 3`
- **THEN** the expression groups as `1 + (2 * 3)` and evaluates to `7`

#### Scenario: Apply left associativity

- **WHEN** a body returns `20 / 5 / 2`
- **THEN** the expression groups as `(20 / 5) / 2` and evaluates to `2`

#### Scenario: Override precedence with grouping

- **WHEN** a body returns `(1 + 2) * 3`
- **THEN** the grouped addition is the multiplication's left operand and the result is `9`

#### Scenario: Reject a comparison chain

- **WHEN** a body spells `1 < 2 < 3`
- **THEN** the second relational operator is retained as recovered syntax with a parser diagnostic rather than defining a chained comparison

### Requirement: Operators resolve to compiler-known actor operations

Arithmetic operators and prefix negation SHALL resolve to `I32` actor operations; relational
operators SHALL resolve to the matching two-argument `I32` comparison; `!` SHALL resolve to
`Bool.not`; and equality SHALL resolve to `equals` or `notEquals` for two operands of the same
available scalar type, currently `I32` or `Bool`. Operator resolution SHALL use the same closed
operation identities, contracts, exact arithmetic, and `SEM0012` operand-type diagnostic as their
qualified actor-call forms. It MUST NOT search source declarations, imports, methods, conformances,
or overload candidates, and it MUST NOT perform truthiness, implicit numeric conversion, or
operand reordering. A directly negated decimal literal SHALL retain the signed-literal behavior
that admits exact `-2147483648`; any other prefix negation SHALL be the trapping `I32.negate`
operation.

#### Scenario: Resolve arithmetic syntax

- **WHEN** a body returns `40 + 2`
- **THEN** the operator resolves to the same `I32.add` operation and `I32` result as `I32.add(40, 2)`

#### Scenario: Resolve boolean equality

- **WHEN** a body returns `true == false`
- **THEN** equality resolves to the compiler-known `Bool.equals` operation and produces `Bool`

#### Scenario: Reject a mistyped operand

- **WHEN** a body returns `true + 1`
- **THEN** the left operand receives `SEM0012`, the operator expression remains unavailable, and no alternate operation is searched

#### Scenario: Trap prefix-negation overflow

- **WHEN** runtime evaluation negates an `I32` value equal to `-2147483648`
- **THEN** the operation traps for arithmetic overflow exactly like subtracting it from zero

### Requirement: Pipelines insert one explicit first argument

A pipeline SHALL take the completed value on its left and insert it as argument zero of the
qualified actor or module operation on its right. The right side SHALL be a qualified path either
without parentheses, supplying no later arguments, or with an argument list supplying arguments
one onward. Pipelines SHALL associate left-to-right, so each completed call becomes the next
pipeline's first argument. Resolution, visibility, arity, type checking, evaluation order, and
diagnostics SHALL be those of the resulting ordinary qualified call. A pipeline MUST NOT create
method lookup, an implicit import, a runtime pipe value, a distinct HIR call kind, or a backend
operation.

#### Scenario: Pipe into a binary actor operation

- **WHEN** a body returns `2 |> I32.add(3)`
- **THEN** it resolves and evaluates exactly as `I32.add(2, 3)`

#### Scenario: Pipe into a unary actor operation

- **WHEN** a body returns `true |> Bool.not`
- **THEN** it resolves and evaluates exactly as `Bool.not(true)` without requiring an empty argument list

#### Scenario: Chain pipelines left-to-right

- **WHEN** a body returns `2 |> I32.add(3) |> I32.multiply(4)`
- **THEN** the first call's result becomes argument zero of the second call and the result is `20`

#### Scenario: Pipe into an imported public operation

- **WHEN** a module imports `math.Transform as Transform` and returns `value |> Transform.apply(extra)`
- **THEN** the pipeline resolves through the existing namespace and visibility rules to the canonical imported declaration

### Requirement: Operator-authored programs reuse the backend-neutral pipeline

Elaboration SHALL erase surface operator and pipeline sugar into ordinary canonical HIR builtin
calls or declaration calls while retaining the complete source expression span. Lowering SHALL
reuse the existing MIR binary, call, literal, and generated-operation vocabulary; interpretation,
LLVM emission, and WebAssembly emission SHALL consume that MIR without a surface-operator-specific
path. Equivalent operator and qualified-call programs SHALL have the same result or trap behavior,
and repeated compilation SHALL produce deterministic facts and encodings.

#### Scenario: Preserve arithmetic traps across execution paths

- **WHEN** an operator-authored program overflows or divides by zero
- **THEN** interpreter, native, and WebAssembly execution all trap at the operator expression's provenance

#### Scenario: Keep MIR backend-neutral

- **WHEN** `40 + 2` is lowered for native and WebAssembly targets
- **THEN** both targets consume the same canonical `Add` MIR operation and the snapshot's target-aware layout

#### Scenario: Repeat operator compilation

- **WHEN** equivalent operator programs are compiled repeatedly in fresh processes
- **THEN** syntax, semantic facts, HIR, MIR, diagnostics, symbols, and emitted artifacts are deterministic
