# inspector-views Delta Specification

## Purpose

A shared library of compiler-phase view projections: the registry of inspector views and the
per-phase row builders that turn an analysis snapshot into serializable row models, consumed by
both the docs workbench and the language server.

## ADDED Requirements

### Requirement: Views project snapshots into serializable rows

The inspector-views package SHALL define a registry of phase views (syntax, semantics, modules,
backend, and pipeline-wide views) where each view projects an analysis context into a result of
row models plus optional header meta, fact strip, and unavailability message. Row models SHALL be
pure data — strings, numbers, tones, and spans — with no functions or host objects, so a projected
result survives structured serialization unchanged.

#### Scenario: Project a view

- **WHEN** a consumer projects a registered view for a snapshot
- **THEN** it receives rows, optional meta and facts, and the result round-trips through JSON serialization without loss

#### Scenario: Phase unavailable

- **WHEN** a projected phase produced nothing (for example MIR without a resolved target)
- **THEN** the result carries an unavailability message naming which phase broke and why, instead of empty rows

### Requirement: Rows carry module-qualified spans

Every row that refers to a source construct SHALL carry the module name together with the byte
span, so a consumer can navigate to the construct even when it lives in a different module than
the view's root. Cursor tinting SHALL compare module-qualified spans: a cursor tints a row only
when the modules match and the row's span equals or contains the cursor span.

#### Scenario: Row in another module

- **WHEN** a module-closure view lists a declaration from an imported module
- **THEN** the row's span names the imported module, and activating it can navigate there

#### Scenario: Cursor does not tint across modules

- **WHEN** the cursor sits in one module and a row's span covers the same byte range in a different module
- **THEN** the row is not tinted

### Requirement: Projections consume the compiler through the facade

View projections SHALL value-import only the compiler's analysis facade and its data-model types,
never phase modules directly, preserving the existing facade boundary when the projections move
out of the docs app.

#### Scenario: Facade-only imports

- **WHEN** the package's imports are checked
- **THEN** no projection value-imports a phase module (lexer, parser, closure loading, elaboration, evaluation)

### Requirement: The docs workbench consumes the shared projections

The docs `/labs` workbench SHALL consume the shared package for its view registry and row
projections, keeping its observable behavior (views offered, rows rendered, meta, facts,
cursor behavior, saved layouts and URLs) unchanged.

#### Scenario: Workbench behavior preserved

- **WHEN** the workbench renders any view after the extraction
- **THEN** the rows, meta, facts, and span-cursor behavior match what the in-app projections produced before
