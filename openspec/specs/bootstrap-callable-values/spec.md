# bootstrap-callable-values Specification

## Purpose

Define first-class callable values, automatic data-first sections, invocation modes, and
ownership-aware capture behavior for ordinary and higher-order Silk programs.
## Requirements
### Requirement: Integer actor callables use lowercase identities

Compiler-known integer actors SHALL use canonical lowercase source names such as `i32.add` and `u8.wrappingAdd`. Sections SHALL preserve the selected type and operation mode; uppercase actor names MUST NOT resolve as aliases.

#### Scenario: Construct a primitive section

- **WHEN** `i32.add(2)` appears where `fn(i32) -> i32` is required
- **THEN** it constructs the ordinary leading-argument section

### Requirement: Named functions are first-class callable values

Every resolved named function SHALL be usable as a callable value without invoking it. A function
with two or more parameters SHALL also support one automatic leading-argument section: supplying
exactly parameters one through the last SHALL construct a unary callable awaiting parameter zero.
Silk MUST NOT require a `dual` marker and MUST NOT interpret fewer trailing arguments as arbitrary
currying. A unary function SHALL be referenced by name rather than by an empty call.

#### Scenario: Construct a binary section

- **WHEN** `i32.add(2)` refers to `i32.add(left: i32, right: i32)`
- **THEN** it produces a unary callable equivalent to applying `i32.add` with `2` as `right`

#### Scenario: Reject deeper under-application

- **WHEN** a three-parameter function is supplied only its final parameter
- **THEN** analysis reports an arity error rather than constructing a callable awaiting two parameters

#### Scenario: Reference a unary function

- **WHEN** `bool.not` appears where `fn(bool) -> bool` is expected
- **THEN** the function item satisfies that callable contract without `bool.not()` or a dual declaration

### Requirement: Callable contracts expose invocation mode

Callable types SHALL distinguish shared reusable `fn(A) -> B`, exclusive reusable
`mut fn(A) -> B`, and consuming `once fn(A) -> B` invocation. A shared reusable callable SHALL be
accepted where an exclusive or consuming callable is required, and an exclusive reusable callable
SHALL be accepted where a consuming callable is required. The reverse substitutions MUST be
rejected. Invocation mode SHALL describe access to the callable environment, independently of the
ownership mode of each newly supplied argument.

#### Scenario: Accept a reusable function once

- **WHEN** a plain named function is passed to a parameter of type `once fn(i32) -> i32`
- **THEN** the call is valid because invoking a reusable callable once satisfies the weaker contract

#### Scenario: Reject a consuming callback as reusable

- **WHEN** a callable that consumes one captured owner is passed where `fn(i32) -> i32` is required
- **THEN** analysis rejects it and identifies the incompatible consuming invocation mode

### Requirement: Sections capture every ownership mode

Section construction SHALL capture Copy values by snapshot, shared borrows by shared loan,
exclusive borrows by exclusive loan, and moved affine values by ownership transfer. Shared loans
SHALL constrain callable lifetime, exclusive loans SHALL additionally require exclusive invocation,
and consuming a captured affine value SHALL make the callable take-once. Dropping an uninvoked
callable SHALL clean every owned capture exactly once, while successful consuming invocation SHALL
transfer each consumed capture exactly once.

#### Scenario: Reuse a Copy section

- **WHEN** `i32.add(2)` captures the Copy value `2` and is invoked twice
- **THEN** both invocations succeed with the same snapshotted captured value

#### Scenario: Hold an exclusive capture

- **WHEN** a section captures `&mut counter`
- **THEN** the counter remains exclusively borrowed until the section is invoked and released or dropped

#### Scenario: Drop an uncalled owned section

- **WHEN** a section captures `move file` and leaves its region without invocation
- **THEN** the generated callable environment drops the captured file exactly once

### Requirement: Callable application is ordinary and ordered

Any expression with compatible callable type SHALL be invokable. Application SHALL evaluate the
callable and every supplied argument exactly once in the language's defined call order, enforce the
callable invocation mode, and produce the declared result. A pipeline `value |> operation` SHALL
evaluate `value` exactly once before evaluating `operation`, then invoke the resulting unary
callable with that value. Pipelines SHALL associate left-to-right.

#### Scenario: Pipe through a stored section

- **WHEN** `increment` holds `i32.add(1)` and source evaluates `2 |> increment`
- **THEN** the stored callable is invoked once with `2` and produces `3`

#### Scenario: Preserve pipeline evaluation order

- **WHEN** both the left expression and the callable-producing right expression have observable eager work
- **THEN** the left expression completes once before the callable-producing expression begins

### Requirement: Higher-order calls preserve callable guarantees

Function parameters, local bindings, generic substitutions, and Effect combinators SHALL preserve
callable parameter and result types, invocation modes, capture dependencies, and ownership. An
operation that stores or transfers a callback in its compiler-known environment SHALL transfer its
obligations; an operation that may invoke it repeatedly MUST require a compatible reusable mode or
derive a correspondingly restricted result. Bootstrap MUST NOT erase distinct capture-environment
identities merely because their public callable signatures match.

#### Scenario: Derive a one-shot mapped Effect

- **WHEN** `Effect.map` captures a `once fn(A) -> B` mapper in a reusable input Effect
- **THEN** the mapped Effect becomes take-once rather than promising an invalid second mapper invocation

#### Scenario: Reject repeated use in a generic combinator

- **WHEN** a generic function promises to call its callback repeatedly but receives a `once fn`
- **THEN** its callable contract rejects the argument before lowering

### Requirement: Floating actor operations are callable values

`f32` and `f64` actor operations SHALL support ordinary named references and leading-argument sections while preserving width and operation identity.

#### Scenario: Construct an f64 section

- **WHEN** `f64.add(2.0)` appears where `fn(f64) -> f64` is expected
- **THEN** it constructs a width-preserving callable section
