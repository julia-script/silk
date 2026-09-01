# silk-standard-library-documentation Specification

## Purpose

Defines how the compiler-shipped Silk standard library teaches its public API and how that source
documentation becomes a complete, navigable, and continuously verified reference.

## Requirements

### Requirement: Shipped modules expose complete public documentation

Every compiler-shipped standard-library module SHALL carry leading module documentation, and every
intended public root declaration SHALL carry a non-empty standalone summary. Every public field and
every service or interface operation SHALL carry documentation that explains its public semantic
role rather than merely restating its type.

Parameter and type-parameter documentation SHALL attach directly to the parameter it describes and
SHALL be present when ownership, units, interpretation, bounds, mutation, defaults, selection, or
another non-obvious contract would otherwise be unclear. Obvious names and types SHALL NOT receive
mechanical restatements solely to increase a coverage count.

#### Scenario: Inspect a shipped module

- **WHEN** a documentation consumer opens any module in the standard-library manifest
- **THEN** the module has a module document and each intended public root declaration has a useful standalone summary
- **AND** its public fields and service or interface operations have locally owned documentation

#### Scenario: Explain a semantically significant parameter

- **WHEN** a public operation accepts a parameter whose ownership, units, interpretation, bounds, mutation, default, or selection behavior is not obvious from its name and type
- **THEN** a `///` block immediately above that parameter explains the relevant behavior

#### Scenario: Keep an obvious operand concise

- **WHEN** a parameter's name and compiler-derived type already communicate its entire public role
- **THEN** the documentation remains complete without adding a comment that only repeats those facts

### Requirement: Documentation follows one Silk-native teaching structure

Each declaration document SHALL begin with exactly one concise summary paragraph. When their
content is useful, subsequent CommonMark sections SHALL occur in this order: `When to use`,
`Details`, `Gotchas`, `Examples`, and `See also`. Empty, speculative, redundant, and ad hoc sections
SHALL be omitted.

Documentation SHALL use the language's direct declaration-owned comments and ordinary CommonMark,
not `@param`, `@returns`, `@fails`, `@requires`, `@examples`, or equivalent contract directives.
Return types, failure rows, requirement rows, and other signature facts SHALL remain derived from
compiler semantics. Relationships to other APIs SHALL use resolvable ``[`Symbol`]`` links when
navigation helps a reader understand or choose the API.

#### Scenario: Render a richly documented declaration

- **WHEN** a declaration needs selection guidance, explanatory behavior, a concrete caveat, and an example
- **THEN** its document presents those sections in the defined order after the summary
- **AND** no section duplicates compiler-derived signature facts as documentation metadata

#### Scenario: Render a simple declaration

- **WHEN** a declaration is fully taught by one concise summary
- **THEN** its document contains the summary without empty template sections or filler

### Requirement: Documentation teaches verified public behavior

Documentation claims SHALL be grounded in the declaration and implementation, relevant call sites,
behavioral tests, related public APIs, and maintained standard-library design prose. Documentation
SHALL describe the public concept before incidental implementation mechanics and SHALL distinguish
stable caller-visible behavior from details that may change without affecting the contract.

#### Scenario: Document a boundary condition

- **WHEN** implementation branches or behavioral tests expose a caller-visible failure, ownership rule, lifecycle constraint, boundary value, or portability limitation
- **THEN** the relevant declaration documents that fact in `Details` or `Gotchas`
- **AND** does not generalize an incidental backend or test representation into a public guarantee

### Requirement: Examples are selective, titled, and compilable

An example SHALL be included only when it teaches meaningful behavior, composition, inference,
failure handling, lifecycle, ownership, or another contract not obvious from the signature and
prose. Examples SHALL live below an `Examples` heading, each example SHALL have a distinct
scenario-oriented title, and each ordinary `silk` fence SHALL contain one complete module that the
standard-library doctest can compile without hidden setup.

An intentionally non-compilable illustration SHALL use the repository's explicit `silk,ignore`
marker, remain readable, and be reported as skipped rather than silently treated as executable.

#### Scenario: Verify an executable example

- **WHEN** a documented declaration contains a fenced `silk` example below its `Examples` heading
- **THEN** the doctest compiles the complete fenced module exactly as authored
- **AND** reports a failure with source provenance when the example no longer compiles

#### Scenario: Keep a conceptual illustration

- **WHEN** a useful example cannot be a complete compilable module without obscuring the concept
- **THEN** the fence uses `silk,ignore`
- **AND** verification reports the example as intentionally skipped

### Requirement: Generated reference preserves the intended documentation hierarchy

The generated standard-library reference SHALL render every item included by the public
documentation model in deterministic source order. It SHALL preserve module prose, root
declarations, documented type parameters, public fields, documented parameters, service and
interface operations, implementations, and implementation operations as a navigable hierarchy.

Generated counts SHALL count the public items they claim to represent, private declarations SHALL
not appear in the public reference, source-authored heading structure SHALL be nested correctly
under generated page and declaration headings, and semantic symbol links SHALL navigate to the
rendered target when that target is included.

#### Scenario: Generate a module with nested public items

- **WHEN** a documented public service contains documented operations and parameters
- **THEN** its module page renders the service, operations, parameters, signatures, and prose in source order
- **AND** no documented child is discarded by the formatter

#### Scenario: Count public declarations

- **WHEN** the generated index reports a declaration count for a module that also contains private helpers
- **THEN** the count equals the public declarations represented by the public reference
- **AND** excludes the private helpers

#### Scenario: Nest source headings

- **WHEN** declaration documentation contains its own CommonMark section headings
- **THEN** the generated page rebases them below the declaration heading without changing their order or example membership

### Requirement: Standard-library reference is organized by module

The main documentation SHALL provide one generated standard-library index and one generated page
for each shipped module. The index SHALL list every manifest module with its import namespace,
accurate public declaration count, summary, and link to the module page. Module page names and
ordering SHALL be deterministic so equivalent standard-library sources generate byte-identical
documentation trees.

#### Scenario: Navigate from the standard-library index

- **WHEN** a reader opens the standard-library reference
- **THEN** the index lists every module from the manifest in canonical order
- **AND** each entry links to a page containing that module's complete public reference

#### Scenario: Regenerate unchanged documentation

- **WHEN** the same standard-library sources are generated twice
- **THEN** the resulting index and module pages have identical paths and bytes

### Requirement: Documentation quality is part of normal verification

Repository verification SHALL fail when required standard-library documentation is missing, the
teaching structure violates the policy, an executable example does not compile, or generated
reference output is stale. The documentation policy check SHALL be separate from ordinary Silk
compilation so malformed or incomplete prose does not change whether a Silk program compiles.

#### Scenario: Detect missing required coverage

- **WHEN** a shipped module, public root declaration, public field, or service or interface operation lacks its required documentation
- **THEN** the documentation policy check fails with the declaration identity and source location

#### Scenario: Detect a stale example

- **WHEN** an executable standard-library example stops compiling after an API change
- **THEN** normal repository verification fails and identifies the authored example location

#### Scenario: Compile without requesting documentation

- **WHEN** ordinary compiler analysis builds a Silk program without running documentation verification
- **THEN** documentation policy and Markdown quality do not affect compilation

### Requirement: Effect documentation teaches exact contract-row transformation

Generated and authored standard-library documentation SHALL explain failure rows as nominal sets,
requirement rows as capability-role keyed access-labelled rows, union access joining, exact
membership/difference, and forward-only `Without<R, S>` including set-to-set removal. It SHALL
distinguish provider compatibility from stored-member identity and explain why a stronger provider
still subtracts the exact stored access.

Effect reference pages SHALL document selected-row-first generic calls, shared/exclusive/owned
binding, conformance-based provider matches, ambiguity diagnostics, partial application, acquisition
cleanup, singleton `catch`, and executable whole-row `catchAll`. Examples SHALL compile or carry an
explicit diagnostic expectation for an intentionally invalid call.

#### Scenario: Document the Logger and Clock regression

- **WHEN** readers view `Effect.provideMut`
- **THEN** an example shows `StdoutLogger` removing `&mut Logger` from `&mut Clock | &mut Logger` while preserving `Clock`

#### Scenario: Document set-to-set difference

- **WHEN** readers view contract-row algebra
- **THEN** examples cover singleton, absent-member no-op, exact access mismatch, and set-to-set `Without`

#### Scenario: Document executable selective catch honestly

- **WHEN** readers view selective `Effect.catch`
- **THEN** the reference explains its singleton typing contract, executable evaluator/WebAssembly/native behavior, and use of `catchAll` for whole-row recovery
