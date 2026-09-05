## MODIFIED Requirements

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

#### Scenario: Apply a structural lifetime contract

- **WHEN** application knows a structural callable contract with explicit input/output lifetime relationships
- **THEN** it instantiates those declared relationships and preserves the callable environment bound without requiring an exact body or guessing hidden sources

#### Scenario: Keep opaque callable provenance unknown

- **WHEN** a structural callable result has no declared or deterministically elided input or environment lifetime relationship
- **THEN** analysis does not guess a source from arbitrary arguments or hidden captures and rejects an unavailable contract

### Requirement: Higher-order calls preserve callable guarantees

Function parameters, local bindings, generic substitutions, and Effect combinators SHALL preserve
callable parameter and result types including lifetime binders, invocation modes, environment validity bounds, capture dependencies, and ownership. An
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
- **THEN** the call lowers and the LLVM native and WebAssembly artifacts agree on its result

#### Scenario: Call through a function-typed parameter of a generic function

- **WHEN** `fn apply<A, B>(transform: once fn(A) -> B, value: A) -> B` calls `transform`
- **THEN** the callable's hidden identity accompanies the explicit type arguments in the target's
  instance key, and the call lowers for named functions and leading-argument sections alike

### Requirement: Effect values cross ordinary higher-order boundaries

Closed Effect values SHALL be valid ordinary parameter, result, local-binding, capture, generic-
argument, and concretely represented nominal-field values without exposing or erasing their hidden
construction-site identity. Passing, capturing, or storing an Effect SHALL preserve its success,
failure, requirement, environment-lifetime, and run-access contracts and the ownership of every hidden environment field.
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

#### Scenario: Retain a hidden short environment

- **WHEN** an Effect capturing a shared borrowed holder or string view passes through a generic identity and nominal field
- **THEN** its environment bound remains short and escape or Detached admission cannot erase the nested data dependency

## ADDED Requirements

### Requirement: Expected callable contracts admit one finite outer lifetime binder

Compatibility SHALL support one outer for<'a, ...> lifetime binder on an expected callable or operation contract, including references to already bound surrounding lifetimes. Checking SHALL introduce scoped rigid placeholders, apply ordinary variance and finite outlives obligations, and reject placeholder escape. The quantified signature SHALL NOT contain nested quantified callable contracts. The checker SHALL reject arbitrary unconstrained higher-rank inference. Compatibility SHALL validate the already selected operation rather than choose implementation or provider candidates.

#### Scenario: Compare a universally borrowed callback

- **WHEN** an expected for<'call> fn(&'call T) -> &'call T contract is supplied a compatible generic identity
- **THEN** the offered signature satisfies the scoped rigid lifetime for every invocation without a runtime lifetime argument

#### Scenario: Reject a nested quantified signature

- **WHEN** a quantified callable signature contains a second quantified callable type in a parameter or result
- **THEN** analysis reports the unsupported form without searching alternate quantifier arrangements

#### Scenario: Reject placeholder escape

- **WHEN** checking attempts to store a quantified invocation's rigid reference into surrounding longer-lived storage
- **THEN** the escape is rejected at the assignment or retaining boundary
