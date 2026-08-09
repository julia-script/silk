## Purpose

Defines token-specific semantic hover for Silk source, including source-like declarations,
intrinsic language operations, recovered programs, and anonymous expression type fallback.

## ADDED Requirements

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

- **WHEN** the cursor hovers a resolved use of `recover` declared as `effect fn recover(error: OutOfMemory) -> I32`
- **THEN** hover renders `effect fn recover(error: OutOfMemory) -> I32` rather than `fn(OutOfMemory) -> Effect<I32>`

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

- **WHEN** the cursor hovers an available integer literal with inferred type `I32`
- **THEN** hover reports `I32` for that literal expression

#### Scenario: Hover trivia

- **WHEN** the cursor hovers whitespace or a comment outside a semantic occurrence
- **THEN** the server returns no hover

#### Scenario: Hover beside recovered syntax

- **WHEN** one declaration is damaged and the cursor hovers an available symbol in another declaration
- **THEN** hover still reports the available symbol presentation
