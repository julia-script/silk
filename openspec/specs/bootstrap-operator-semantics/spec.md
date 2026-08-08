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

### Requirement: Pipelines apply one unary callable

A pipeline SHALL evaluate its completed left expression exactly once, then evaluate the callable
expression on its right and invoke that callable with the left value as its sole argument. The right
side MAY be a named function, automatic leading-argument section, binding, grouped expression, or
other expression with compatible unary callable type. Pipelines SHALL associate left-to-right and
MUST NOT create method lookup, implicit imports, or runtime namespace objects.

#### Scenario: Pipe into an actor section

- **WHEN** a body returns `2 |> I32.add(3)`
- **THEN** `I32.add(3)` first denotes a unary callable and the pipeline invokes it with `2`, producing `5`

#### Scenario: Pipe into a callable binding

- **WHEN** `increment` holds `I32.add(1)` and a body returns `2 |> increment`
- **THEN** the pipeline invokes the stored callable and produces `3`

#### Scenario: Chain applications left-to-right

- **WHEN** a body returns `2 |> I32.add(3) |> I32.multiply(4)`
- **THEN** the first application produces `5` and the second produces `20`

### Requirement: Operator-authored programs reuse the backend-neutral pipeline

Elaboration SHALL erase surface operator sugar into ordinary canonical HIR builtin calls and SHALL
erase pipeline syntax into canonical unary callable application while retaining the complete source
expression span. Lowering SHALL reuse the existing MIR operation and callable-application
vocabulary; interpretation, LLVM emission, and WebAssembly emission SHALL consume that MIR without
a surface-operator-specific or surface-pipeline-specific path. Equivalent programs SHALL have the
same result or trap behavior, and repeated compilation SHALL produce deterministic facts and
encodings.

#### Scenario: Preserve arithmetic traps across execution paths

- **WHEN** an operator-authored program overflows or divides by zero
- **THEN** interpreter, native, and WebAssembly execution all trap at the operator expression's provenance

#### Scenario: Keep MIR backend-neutral

- **WHEN** `40 + 2` and `40 |> I32.add(2)` are lowered for native and WebAssembly targets
- **THEN** both targets consume the same canonical arithmetic and callable plans with target-aware layout

#### Scenario: Repeat operator compilation

- **WHEN** equivalent operator and callable-pipeline programs are compiled repeatedly in fresh processes
- **THEN** syntax, semantic facts, HIR, MIR, diagnostics, symbols, and emitted artifacts are deterministic

### Requirement: Operators resolve homogeneously for Usize

The established prefix, arithmetic, equality, and ordering pipeline SHALL resolve binary `Usize`
operators only when both operands are `Usize`. It SHALL select unsigned checked semantics and a
`Usize` arithmetic or `Bool` comparison result without introducing overload lookup, implicit
conversion, or backend-specific operator identity. Unary minus on `Usize` SHALL be rejected.

#### Scenario: Resolve checked multiplication

- **WHEN** both operands of `*` have canonical type `Usize`
- **THEN** operator facts select checked unsigned multiplication returning `Usize`

#### Scenario: Reject unary minus

- **WHEN** unary `-` is applied to a `Usize` expression
- **THEN** operator analysis reports that the prefix operation is unavailable for that type
