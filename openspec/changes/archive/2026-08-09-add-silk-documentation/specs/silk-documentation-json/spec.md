## Purpose

Defines the deterministic experimental JSON documentation intermediate representation consumed by
independent Silk documentation formatters and publishing tools.

## ADDED Requirements

### Requirement: Generation emits a formatter-neutral JSON IR

Documentation generation SHALL emit a structured JSON value containing source-ordered modules,
declarations, compiler-derived signatures, parsed documentation nodes, semantic link targets,
examples, visibility, and module-relative source provenance. The output MUST NOT serialize private
compiler data structures, third-party Markdown AST types, absolute filesystem paths, HTML, or a
site-generator-specific format.

#### Scenario: Generate one documented module

- **WHEN** generation analyzes a module with module prose and one documented public function
- **THEN** the JSON contains the module document, function identity, structured signature, parsed function document, visibility, and relative provenance

#### Scenario: Keep output formatter-neutral

- **WHEN** a formatter reads generated JSON
- **THEN** it can distinguish documentation blocks, inline nodes, code examples, and semantic links without importing compiler internals or reparsing source comments

### Requirement: JSON generation is deterministic

Equivalent source closures, compiler targets, and visibility options SHALL produce byte-identical
JSON across fresh processes. Modules and declarations SHALL use canonical source order, object field
order SHALL be fixed by the emitter, and output SHALL end with one newline.

#### Scenario: Repeat generation

- **WHEN** the same project is documented twice in fresh processes with the same options
- **THEN** both JSON byte sequences are identical

### Requirement: Public documentation is the default

JSON generation SHALL include public declarations by default and SHALL omit private declarations
and their private children. An explicit include-private option SHALL include both public and private
declarations while retaining their visibility in the output. Documentation queries used by editor
hover SHALL remain able to access private declarations independently of generation visibility.

#### Scenario: Generate default public documentation

- **WHEN** a module contains documented public and private functions and generation uses default options
- **THEN** the JSON contains the public function and omits the private function

#### Scenario: Include private documentation explicitly

- **WHEN** generation enables the include-private option
- **THEN** both functions appear with their respective visibility values

### Requirement: The bootstrap JSON schema is experimental

The generated JSON SHALL identify itself as Silk documentation IR but SHALL make no compatibility
promise during bootstrap. The project MAY change the shape without migration or backward-
compatibility behavior until formatter support is deliberately published with a versioned schema.

#### Scenario: Inspect bootstrap format metadata

- **WHEN** a consumer opens bootstrap documentation JSON
- **THEN** it can identify the artifact as Silk documentation IR and as experimental without inferring a stable schema contract
