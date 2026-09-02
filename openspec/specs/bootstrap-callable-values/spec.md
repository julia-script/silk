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
with `N` parameters SHALL form a section whenever a call supplies a non-empty trailing suffix of
`K` arguments with `0 < K < N`. The section SHALL await the remaining ordered leading prefix, and
successive direct stages SHALL bind another non-empty trailing suffix without holes, reordering, or
repeated evaluation. Silk MUST NOT require a `dual` marker. Supplying zero or more than the
remaining arity SHALL use the ordinary arity diagnostic; the retired unary-only `SEM0079`
diagnostic SHALL NOT be emitted. A unary function SHALL be referenced by name rather than by an
empty call.

#### Scenario: Construct a binary section

- **WHEN** `i32.add(2)` refers to `i32.add(left: i32, right: i32)`
- **THEN** it produces a unary callable equivalent to applying `i32.add` with `2` as `right`

#### Scenario: Construct a deeper section

- **WHEN** `combine(a, b, c)` is referenced as `combine(3)`
- **THEN** it produces `fn(A, B) -> C` with parameter `c` captured once

#### Scenario: Apply in stages

- **WHEN** source evaluates `combine(3)(2)(1)`
- **THEN** capture evaluation follows source order and invocation calls `combine(1, 2, 3)`

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
transfer each consumed capture exactly once. Each staged argument SHALL be captured exactly once
when its stage is constructed, and its original parameter position SHALL remain explicit
independently of capture evaluation order.

#### Scenario: Reuse a Copy section

- **WHEN** `i32.add(2)` captures the Copy value `2` and is invoked twice
- **THEN** both invocations succeed with the same snapshotted captured value

#### Scenario: Hold an exclusive capture

- **WHEN** a section captures `&mut counter`
- **THEN** the counter remains exclusively borrowed until the section is invoked and released or dropped

#### Scenario: Drop an uncalled owned section

- **WHEN** a section captures `move file` and leaves its region without invocation
- **THEN** the generated callable environment drops the captured file exactly once

#### Scenario: Preserve staged capture positions

- **WHEN** successive stages capture parameters `c` and then `b`
- **THEN** both captures evaluate once in that order while final invocation supplies them as `b, c`

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

#### Scenario: Pipe an explicit borrowed view

- **WHEN** the left expression forms `&value` or `&mut value` and the unary callable expects that exact borrowed-view type
- **THEN** ordinary argument compatibility accepts the application and ownership retains the same loan as the equivalent direct call

#### Scenario: Retain a returned borrow from an exact section source

- **WHEN** an exact function item or section returns a borrowed view backed by a supplied argument or one known trailing capture
- **THEN** callable application records that exact source and keeps its loan active through the result's last use

#### Scenario: Keep opaque callable provenance unknown

- **WHEN** application knows only a structural callable contract whose result is a borrowed view
- **THEN** it does not infer a returned-borrow source from an arbitrary argument or hidden environment

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

#### Scenario: Call through a function-typed parameter of an ordinary function

- **WHEN** a non-effect `fn` declares a `once fn(i32) -> i32` parameter and calls it, and `main`
  passes a named function
- **THEN** the call lowers and the evaluator, LLVM, and Wasm agree on its result

#### Scenario: Call through a function-typed parameter of a generic function

- **WHEN** `fn apply<A, B>(transform: once fn(A) -> B, value: A) -> B` calls `transform`
- **THEN** the callable's hidden identity accompanies the explicit type arguments in the target's
  instance key, and the call lowers for named functions and leading-argument sections alike

### Requirement: Callable arguments monomorphize their target

A call passing a callable value SHALL specialize its target on that callable's hidden concrete
identity, exactly as a call passing an Effect value does. Structural callable and Effect contracts
have no standalone target layout. A concrete callable representation stored through a complete
representation-dependent nominal MAY contribute its environment to that enclosing nominal's inline
layout while retaining static monomorphization. Discovery SHALL route every open callable parameter
off the executable path and require one finite concrete instance before layout or MIR.

#### Scenario: Distinguish two callables behind one signature

- **WHEN** one function taking a `once fn(i32) -> i32` is called with two different named functions
- **THEN** each call reaches its own specialized instance naming its target statically, and neither
  instance drops the callable parameter from its lowered contract

#### Scenario: Keep a structural callable unlayoutable

- **WHEN** layout receives only `fn(i32) -> i32` without a concrete representation argument
- **THEN** it reports the existing unavailable layout rather than choosing a uniform closure shape

#### Scenario: Layout a represented callable field

- **WHEN** a complete nominal specialization identifies one callable target and capture environment
- **THEN** layout includes that environment inline without giving the structural callable contract a standalone ABI

### Requirement: Floating actor operations are callable values

`f32` and `f64` actor operations SHALL support ordinary named references and leading-argument sections while preserving width and operation identity.

#### Scenario: Construct an f64 section

- **WHEN** `f64.add(2.0)` appears where `fn(f64) -> f64` is expected
- **THEN** it constructs a width-preserving callable section

### Requirement: Effect values cross ordinary higher-order boundaries

Closed Effect values SHALL be valid ordinary parameter, result, local-binding, capture, generic-
argument, and concretely represented nominal-field values without exposing or erasing their hidden
construction-site identity. Passing, capturing, or storing an Effect SHALL preserve its success,
failure, requirement, and run-access contracts and the ownership of every hidden environment field.
A structural Effect contract has no standalone target layout; a concrete represented environment MAY
contribute inline lanes only through its complete enclosing nominal realization.

#### Scenario: Implement map as an ordinary function

- **WHEN** a generic source function accepts one Effect and one unary callable and returns an Effect that runs the input later
- **THEN** its returned Effect retains both hidden environments and derives the strongest required shared, exclusive, or consuming run access

#### Scenario: Preserve a take-once input

- **WHEN** a source combinator captures an Effect that owns an affine value consumed during execution
- **THEN** the composition remains take-once and ownership rejects a second run without requiring compiler knowledge of the combinator's name

#### Scenario: Store a concrete Effect realization

- **WHEN** a complete nominal specialization stores one Effect representation in a field
- **THEN** its runner and environment remain lazy, inline, statically targeted, and unavailable through the structural contract alone

### Requirement: Effectful channel callbacks are ordinary callables

Generic source combinators SHALL accept ordinary shared, exclusive, or consuming callbacks that
return values or Effects. Specialization and ownership SHALL derive callback invocation count,
captured state, failure and requirement rows, and cleanup from the callable contract and function
body rather than an Effect-specific callback category.

#### Scenario: Compose an effectful failure callback

- **WHEN** source-defined recovery invokes an ordinary callback returning `Effect<A ! F ? S>` on the failure branch
- **THEN** the composition retains the untouched success path and exposes normalized failure `F` and requirements `R | S`

### Requirement: Source callables may carry an unsafe caller contract

Ordinary and effectful function declarations MAY be marked `unsafe`. Calling such a declaration
SHALL require one lexical unsafe acknowledgement at the call site while preserving every ordinary
type, Effect, ownership, requirement, target, and cleanup check.

#### Scenario: Call an unsafe source wrapper

- **WHEN** source calls an `unsafe fn` inside an unsafe acknowledgement
- **THEN** the call is accepted if all ordinary checks succeed

#### Scenario: Reject an unacknowledged call

- **WHEN** safe source directly calls an unsafe function
- **THEN** analysis reports the missing acknowledgement at the call

### Requirement: Unsafe qualification survives callable composition

Callable values, generic substitution, partial application, storage, returns, and interface
operation contracts SHALL preserve unsafe qualification. A safe implementation MAY satisfy an
unsafe operation contract, but an unsafe implementation SHALL NOT satisfy a safe contract.

#### Scenario: Partially apply an unsafe function

- **WHEN** source supplies a leading argument to an unsafe multi-parameter function
- **THEN** the resulting callable remains unsafe and its later invocation requires acknowledgement

#### Scenario: Keep checks active inside unsafe code

- **WHEN** an acknowledged unsafe call also violates borrowing or Effect requirements
- **THEN** analysis reports those ordinary violations rather than treating unsafe as a checking bypass

### Requirement: Associated members are first-class callable values

A resolved associated member SHALL be usable as a callable value under the same rules as a named
function: `Owner.member` is a function item whose contract lists the receiver as parameter zero
when present, `Owner.member(trailing)` with a non-empty trailing suffix forms an ordinary section
awaiting the leading prefix, and `value |> Owner.member(trailing)` applies that section. Sections
of associated members SHALL capture ownership modes, preserve evaluation order, and monomorphize
exactly as sections of root functions do. No method-specific currying or partial-application form
SHALL exist.

#### Scenario: Reference a receiver method as a function item

- **WHEN** `Option.map` is passed where `fn(Option<i32>, once fn(i32) -> i32) -> Option<i32>` is expected
- **THEN** the function item satisfies that contract with the receiver as its first parameter

#### Scenario: Pipe through an associated-member section

- **WHEN** source evaluates `Option.some(2) |> Option.map(addOne)`
- **THEN** `Option.map(addOne)` captures `transform` and the pipeline supplies the receiver, resolving to the same member as `Option.map(Option.some(2), addOne)`

#### Scenario: Section an associated function without a receiver

- **WHEN** `Pair.make(a, b)` is referenced as `Pair.make(2)`
- **THEN** it produces a unary section over `a` with `b` captured once

### Requirement: Bound method values are receiver sections

`value.member` naming an inherent receiver method SHALL construct a section whose single capture is
parameter zero and whose remaining parameters are the member's parameters one onward, in declared
order. Every rule of "Sections capture every ownership mode" SHALL apply to that capture: a shared
loan constrains the callable's lifetime, an exclusive loan additionally requires exclusive
invocation, and a moved affine receiver makes the callable take-once and is dropped exactly once if
the callable is never invoked. Application SHALL order operands by parameter ordinal: captures at
their declared ordinal, supplied arguments filling the remaining ordinals in order. Bound method
values SHALL monomorphize, lower, and execute exactly as trailing sections of the same member do on
the evaluator and on every compiled backend.

#### Scenario: Apply a bound method

- **WHEN** `let plusForty = counter.add` is applied as `plusForty(2)` with `fn add(self: &Self, adjustment: i32) -> i32`
- **THEN** the invocation calls `Counter.add(&counter, 2)` on the evaluator, LLVM, and Wasm, with the supplied argument placed after the captured receiver

#### Scenario: Bind a receiver-only method

- **WHEN** `let reader = counter.read` is applied as `reader()` with `fn read(self: &Self) -> i32`
- **THEN** the application supplies the captured loan as the only operand and produces the same result as `counter.read()`

#### Scenario: Drop an uninvoked bound receiver

- **WHEN** `let taker = token.take` moves an affine `token` and `taker` leaves its region uninvoked
- **THEN** the callable environment drops the captured token exactly once
