# compilation-profiles Specification

## Purpose

Define immutable logical compilation profiles so multiple artifacts and configurations on one target remain deterministic across compiler and tooling requests.

## Requirements

### Requirement: Profiles separate logical domains

A profile SHALL contain versioned target facts, CPU model/features, deployment, libc, artifact form, entry policy, link policy, code and relocation models, optimization, safety, threading, sanitizers, unwind, runtime-selection requests and resolved package parameters as distinct typed domains. Artifact form SHALL distinguish executable image, loadable module, static archive and relocatable object from emission stages. Runtime-selection requests SHALL distinguish default, named logical composition and none. Physical supplies, output paths and runtime-discovered facts MUST NOT be logical profile fields.

#### Scenario: Distinguish artifacts on one target

- **WHEN** two requests select one canonical target but different logical runtime, artifact or safety settings
- **THEN** their normalized profiles retain those distinct typed choices and have different canonical identities

#### Scenario: Exclude machine-local supplies

- **WHEN** equivalent logical inputs are supplied from two physical SDK or checkout locations
- **THEN** their semantic profile identity is equal and no physical path is exposed through static profile queries

### Requirement: Machine descriptions are versioned and verified

The compiler SHALL supply descriptions for aarch64-apple-darwin, x86_64-unknown-linux-gnu, aarch64-unknown-linux-gnu and wasm32-unknown-unknown. Each SHALL identify architecture, OS, ABI, object format, endianness, admitted primitive widths/alignment, address spaces, stack alignment, supported CPU features and toolchain class. Missing, inconsistent or unsupported required facts SHALL fail before use. Admitted primitive facts SHALL be verified against pinned LLVM and independent C/object fixtures. Aggregate layout and complete ABI conformance are outside this fact verification contract.

#### Scenario: Reject an inconsistent description

- **WHEN** a description disagrees with its required primitive facts or claims an unsupported CPU feature
- **THEN** normalization returns a structured diagnostic before static use or backend emission

#### Scenario: Verify every initial target

- **WHEN** the target-description fixture suite runs
- **THEN** it compares the admitted primitive facts for all four targets with pinned LLVM and independent header-free C/object evidence without using the host target as a fallback

### Requirement: Canonical identity covers resolved logical values

Canonical encoding SHALL be versioned and independent of input order, object allocation and physical paths. It SHALL include the target-description revision and every normalized selection/codegen-affecting choice. Set-valued fields SHALL be sorted and deduplicated; arrays SHALL retain order; records and parameter identities SHALL use deterministic field/identity order. Typed integer, enum and optional encodings SHALL be unambiguous; strings SHALL retain their exact contents. Provenance SHALL accompany values but MUST NOT alter logical identity. Cache entries MUST NOT reuse another request's diagnostic origins.

#### Scenario: Equal inputs constructed independently

- **WHEN** equivalent logical values arrive with reordered record fields, features or binding entries
- **THEN** canonical encoding and identity are equal and diagnostics follow deterministic ordering with the current request's origins

#### Scenario: Change a package parameter

- **WHEN** otherwise equal same-target profiles bind one declared boolean to false and true
- **THEN** their canonical encodings and identities differ and both remain usable in one process

### Requirement: Tooling selects complete profiles at the application edge

Compilation, static evaluation, backend identity and tooling SHALL consume the same canonical profile. Tooling SHALL accept a complete explicit override, a named project profile, a selected project default, or target-triple shorthand. Mutually exclusive explicit selection modes SHALL be diagnosed. When no explicit or project selection exists, host facts SHALL be supplied at the application edge. Compiler normalization MUST NOT inspect ambient host state. Logical requests for later runtime/root/link facilities SHALL remain distinguishable without claiming those facilities have been resolved.

#### Scenario: Select the project profile in the language server

- **WHEN** a project chooses a named profile and the language server has no explicit override
- **THEN** analysis observes the same resolved parameters and profile identity as compilation with that project selection

#### Scenario: Cross-compile explicitly

- **WHEN** tooling selects a target triple or full profile that differs from its host
- **THEN** normalization uses only that selection and never fills logical fields from ambient host facts

### Requirement: Ordinal target APIs are removed

The source Profile enum, Intrinsic.targetProfile operation and ordinal dispatch SHALL be deleted together with their consumers and generated documentation. Individual static fact queries through sealed Intrinsic SHALL replace the ordinal operation; ordinary Silk SHALL own typed wrappers. Canonical target IDs SHALL remain valid selectors and diagnostic identities. Requirements about architecture, OS or primitive width SHALL use those narrower facts.

#### Scenario: Select by pointer width

- **WHEN** source needs a 32-bit pointer case
- **THEN** it queries the primitive width through its ordinary source wrapper without comparing a whole-target ordinal
