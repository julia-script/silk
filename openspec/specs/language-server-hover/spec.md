# language-server-hover Specification

## Purpose

Defines token-specific semantic hover for Silk source, including source-like declarations,
intrinsic language operations, recovered programs, and anonymous expression type fallback.

## Requirements

### Requirement: Concrete provider hover lists proved contracts

Hover for a concrete nominal value, produced function result, inferred binding, or nominal type
occurrence SHALL append an `Implements` section containing every distinct valid, proved, and
endpoint-visible service or interface conformance. Contract names SHALL use the requesting module's
shortest unambiguous presentation and deterministic order. Existing signature and documentation
content SHALL remain unchanged, and a subject with no applicable conformance MUST NOT receive an
empty section. The language server SHALL format compiler-owned facts without repeating conformance
proof or recognizing a standard-library declaration by spelling.

#### Scenario: Hover a standard-stream provider

- **WHEN** hover selects `Core.native()`, its inferred binding, or `NativeStandardStreams`
- **THEN** it lists `Core.StandardStreams` in an appended `Implements` section

#### Scenario: Omit an unproved conditional conformance

- **WHEN** a concrete generic provider does not satisfy a conditional conformance's requirements
- **THEN** hover does not advertise that contract

### Requirement: Hover renders canonical integer types

Hover SHALL render lowercase integer spellings, `bool`, `()`, and `never`, never removed uppercase or backend lane names.

#### Scenario: Hover an inferred literal

- **WHEN** an unconstrained integer literal defaults successfully
- **THEN** hover reports `i32`

### Requirement: The server advertises semantic hover support

The language server SHALL advertise hover support and SHALL interpret hover positions using the
negotiated position encoding. A hover selected from a semantic token SHALL identify that token's
exact source range rather than the range of an enclosing expression.

#### Scenario: Client initializes hover support

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise hover support and the selected position encoding

#### Scenario: Hover after non-ASCII source

- **WHEN** a hover request uses a UTF-16 position after non-ASCII source text
- **THEN** the server selects the semantic occurrence at the corresponding Silk source byte span

### Requirement: Symbol hover uses source-like declarations

Hovering a declaration or resolved reference SHALL render the selected symbol's source-level form,
including its name, parameter names, generic parameters, mutability, source-visible type spellings,
and function kind where those properties apply. An `effect fn` SHALL remain an `effect fn` whose
declared result follows `->`; it MUST NOT be presented only as an anonymous callable whose result
is a lowered `Effect<...>` value.

#### Scenario: Hover an inferred mutable binding

- **WHEN** the cursor hovers `allocator` in `let mut allocator = SystemAllocator.make()`
- **THEN** hover identifies a mutable binding named `allocator` with inferred type `SystemAllocator`

#### Scenario: Hover an effect function reference

- **WHEN** the cursor hovers a resolved use of `recover` declared as `effect fn recover(error: OutOfMemoryError) -> i32`
- **THEN** hover renders `effect fn recover(error: OutOfMemoryError) -> i32` rather than `fn(OutOfMemoryError) -> Effect<i32>`

#### Scenario: Hover a function declaration name

- **WHEN** the cursor hovers the declared name of a function
- **THEN** hover renders the same source-like signature used for resolved references to that function

### Requirement: Qualified tokens have distinct hover identities

Each identifier in a qualified expression or type application SHALL be hoverable according to its
own semantic role. Actor or namespace qualifiers, operation names, type arguments, fields, and the
resulting expression MUST NOT inherit one undifferentiated enclosing-expression hover merely
because their source ranges overlap.

#### Scenario: Hover an intrinsic allocator call

- **WHEN** the cursor separately hovers `SystemAllocator` and `make` in `SystemAllocator.make()`
- **THEN** the qualifier is presented as the `SystemAllocator` type or actor and `make` is presented with its operation signature

#### Scenario: Hover an effect handler application

- **WHEN** the cursor separately hovers `Effect`, `catch`, and `Problem` in `Effect.catch<Problem>(recover)`
- **THEN** hover presents the intrinsic actor, the generic catch operation, and the `Problem` type as three distinct semantic entities

### Requirement: Intrinsic hover uses one authoritative signature catalog

Built-in actors, namespaces, and operations without source declarations SHALL still expose stable,
source-like hover presentations. Their names, generic parameters, value parameters, result forms,
and unsafe or effect distinctions SHALL agree with the same intrinsic definitions used by semantic
analysis and completion.

#### Scenario: Hover a source-less intrinsic operation

- **WHEN** the cursor hovers a recognized intrinsic operation that has no Silk declaration file
- **THEN** hover returns its authoritative Silk signature without fabricating a source location

### Requirement: Expression hover is a semantic fallback

When a selected position has no identifier-like semantic occurrence but lies within an available
typed anonymous expression, hover SHALL report the smallest containing expression's type. Trivia,
comments, unavailable expressions, and punctuation without a meaningful expression type SHALL
produce no hover. Damage elsewhere in the module MUST NOT suppress an available hover.

#### Scenario: Hover an integer literal

- **WHEN** the cursor hovers an available integer literal with inferred type `i32`
- **THEN** hover reports `i32` for that literal expression

#### Scenario: Hover trivia

- **WHEN** the cursor hovers whitespace or a comment outside a semantic occurrence
- **THEN** the server returns no hover

#### Scenario: Hover beside recovered syntax

- **WHEN** one declaration is damaged and the cursor hovers an available symbol in another declaration
- **THEN** hover still reports the available symbol presentation

### Requirement: Symbol hover includes complete authored documentation

When a declaration or resolved reference has attached documentation, hover SHALL render the
compiler-derived source-like signature followed by the complete parsed Markdown document, including
its examples. Definition and reference hover for the same declaration SHALL show equivalent
documentation. Hover SHALL resolve intra-document links best-effort and render unresolved links as
inline code without a diagnostic. Symbols without documentation SHALL retain signature-only hover.

#### Scenario: Hover a documented function definition

- **WHEN** the cursor hovers the declared name of a function preceded by `///` prose and an `Examples` section
- **THEN** hover contains the function signature, prose, and fenced example as Markdown

#### Scenario: Hover a documented function reference

- **WHEN** the cursor hovers a resolved reference to that function
- **THEN** hover contains documentation equivalent to the declaration hover

#### Scenario: Hover an undocumented symbol

- **WHEN** the cursor hovers a semantic symbol with no attached documentation
- **THEN** hover continues to contain its existing source-like presentation without an empty documentation section

### Requirement: Hover renders exact float width

Hover SHALL render `f32` or `f64` from semantic facts and never substitute a backend lane or generic number label.

#### Scenario: Hover a default float literal

- **WHEN** an unconstrained float literal is accepted
- **THEN** hover reports `f64`

### Requirement: Hover distinguishes Intrinsic from source APIs

Hover over a qualified `Intrinsic` member SHALL show its authoritative concrete signature, safety
classification, and source-less intrinsic identity. Hover over a standard-library wrapper,
interface, service, or provider operation SHALL show its authored source declaration and
documentation without presenting it as a compiler intrinsic.

#### Scenario: Hover concrete and generic addition

- **WHEN** a document contains both a generic integer addition call and `Intrinsic.i32Add`
- **THEN** hover presents the navigable generic source contract for the first and the concrete intrinsic contract for the second

### Requirement: Pattern-local hover uses source-like declarations

Hover over a shared pattern declaration or reference SHALL use its compiler-published identity and
render the source-like local binding name and exact narrowed type. Hover MUST NOT infer a type by
reparsing the pattern or inspecting runtime tags.

#### Scenario: Hover a pattern local

- **WHEN** hover selects a reference introduced by `let Point { x, .. } = point`
- **THEN** it renders `let x: i32` from the semantic pattern fact

### Requirement: A called member hovers with its receiver-bound contract

Hovering the member name in `receiver.member(args)` SHALL present the member's contract with
parameter zero bound to the receiver and owner binders substituted from the receiver type, while
hovering `Owner.member` SHALL present the complete explicit-receiver contract. Both hovers SHALL
identify the same declaration and include its authored documentation.

#### Scenario: Hover a called method

- **WHEN** the cursor hovers `map` in `option.map(addOne)` with `option: Option<i32>`
- **THEN** hover shows `fn<U>(transform: once fn(i32) -> U) -> Option<U>` and the member's documentation

#### Scenario: Hover the explicit form

- **WHEN** the cursor hovers `map` in `Option.map(move option, addOne)`
- **THEN** hover shows the complete declaration, `pub fn map<T, U>(self: Option<T>, transform: once fn(T) -> U) -> Option<U>`, with the return type spelled as the declaration's module spells it
