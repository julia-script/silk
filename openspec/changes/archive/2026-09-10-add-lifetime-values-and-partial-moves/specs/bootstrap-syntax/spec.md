## MODIFIED Requirements

### Requirement: Match expressions are lossless in every expression position

Every expression position SHALL accept `match` followed by an optional `move`, `&`, `&mut`, or contextual `place` mode,
one scrutinee expression, and a braced source-ordered arm list. Each arm SHALL contain a nominal or
universal pattern, an optional `if` guard expression, `=>`, and one result expression. Newlines and
trivia MAY separate arms without a comma. The concrete tree SHALL retain every token, pattern,
guard, arm boundary, trivia item, and exact span without deciding coverage or types.

#### Scenario: Parse a consuming match initializer

- **WHEN** a binding initializes from `match move event { Token { kind, .. } => kind End {} => 0 }`
- **THEN** the concrete tree retains one match expression with its mode, scrutinee, two ordered arms, patterns, results, and punctuation

#### Scenario: Parse a guarded shared match

- **WHEN** a return expression matches `&event` with a guarded nominal arm followed by `_`
- **THEN** the tree retains the ampersand, guard expression, both fat arrows, and universal identifier in source order

#### Scenario: Parse owned-place refinement

- **WHEN** source contains match place value with ordinary variant patterns and guards
- **THEN** the tree retains the place marker separately from the scrutinee and existing consuming and borrowed match forms

### Requirement: Type parameter and application syntax is contextual and recoverable

The parser SHALL represent angle-bracket type parameter lists after struct and function declaration
names, generic applications in type positions, and explicit specialization after a recognized
callee. Generic brackets MUST NOT consume comparison operators, and reserved JSX-like template
starts SHALL remain reserved only at primary-expression boundaries. Missing names, commas, closing
brackets, and type arguments SHALL remain explicit local syntax nodes and diagnostics.

#### Scenario: Parse a generic declaration and call

- **WHEN** source contains `pub fn identity<T>(value: T) -> T` and `identity<i32>(1)`
- **THEN** syntax records the declaration parameter and call specialization losslessly

#### Scenario: Preserve a comparison

- **WHEN** source contains `left < right`
- **THEN** the expression remains a comparison rather than a damaged generic application

#### Scenario: Keep a reserved template start distinct

- **WHEN** `<Panel />` appears where a primary expression begins
- **THEN** the parser preserves the reserved template start rather than treating `Panel` as a type argument

#### Scenario: Parse lifetime binders and bounds

- **WHEN** source declares Holder<'a: 'b, 'b, T: 'a> and uses Holder<'static, 'data, i32>
- **THEN** the tree retains distinct lifetime and type arguments, outlives punctuation, and exact source spans without treating lifetime names as ordinary types

### Requirement: Lexical slice syntax is lossless and recoverable

The parser SHALL recognize shared `&[T]` and exclusive `&mut [T]` type branches plus explicit `&'a [T]` and `&'a mut [T]` variants and prefix `&` and
`&mut` borrow-expression branches. It SHALL retain every ampersand, keyword, bracket, nested element
type, trivia token, recovery element, and exact source-owned span without deciding whether the
operand is borrowable or the type is permitted at that source position.

#### Scenario: Parse a shared slice parameter and borrow argument

- **WHEN** source spells `fn fold(values: &[i32]) -> i32 { return use(&values) }`
- **THEN** the tree retains one shared slice type and one shared borrow expression with their punctuation and provenance in source order

#### Scenario: Parse an exclusive slice parameter and borrow argument

- **WHEN** source spells `fn edit(values: &mut [i32]) -> i32 { return use(&mut values) }`
- **THEN** the tree retains both `mut` keywords under distinct exclusive slice-type and borrow-expression branches

#### Scenario: Recover a damaged slice type

- **WHEN** a parameter starts a slice type but omits its element or closing bracket before the parameter boundary
- **THEN** the parser inserts explicit missing syntax, preserves following parameters and the function body, and emits deterministic parser diagnostics

## ADDED Requirements

### Requirement: Lifetime syntax has one explicit surface

The grammar SHALL represent lifetime names as an apostrophe followed by an identifier, reserve 'static, and accept lifetime and ordinary parameters in one declaration-ordered list, including <'a, T> and <T, 'a>, with distinct argument namespaces. Bounds SHALL use inline <'a: 'b, T: 'a>; references SHALL use &'a T or &'a mut T, strings string<'a>, and Effect environment bounds Effect<'env; A ! E ? R> with ordinary run-mode prefixes outside Effect. One outer callable lifetime binder SHALL use for<'a> fn(...) -> ... (and existing mut/once invocation modes). Its independent environment annotation SHALL use fn<'env>(...) -> ..., combinable as for<'a> fn<'env>(...) -> .... A lifetime annotation SHALL NOT appear on borrow expressions. Missing or damaged lifetime syntax SHALL recover within its containing declaration or type.

#### Scenario: Parse explicit semantic forms

- **WHEN** a declaration uses &'data mut T, string<'text>, for<'call> fn(&'call T) -> &'call T, and Effect<'env; A ! E ? R>
- **THEN** the lossless tree retains lifetime positions and punctuation separately from value borrows and Effect channels

#### Scenario: Reject a lifetime on a value borrow

- **WHEN** an expression attempts &'a value
- **THEN** the parser reports invalid borrow-expression syntax without treating the lifetime name as a value

## ADDED Requirements

### Requirement: Effect declarations may name their retained environment

An effect-function or Effect operation declaration SHALL accept `effect<'env> fn name<...>(...) -> ...` and resolve its environment in the complete declaration lifetime scope. Omission SHALL retain deterministic environment elaboration. Naming an environment SHALL preserve all obligations from retained parameter contents and SHALL NOT grant additional borrowing authority. Syntax and formatting SHALL preserve the annotation losslessly.

#### Scenario: Name a generic retained environment

- **WHEN** an Effect function declares `effect<'env> fn retain<T: 'env, 'env>(value: T) -> i32`
- **THEN** its constructed Effect uses `'env` and checking retains `T: 'env` as the environment-validity requirement

#### Scenario: Expand a unique inferred environment exactly

- **WHEN** Make lifetimes explicit expands a declaration with an inferred retained environment and several incomparable ambient bounds
- **THEN** it emits the exact inferred environment binder and declaration annotation rather than selecting a different ambient region or withholding a complete expansion
