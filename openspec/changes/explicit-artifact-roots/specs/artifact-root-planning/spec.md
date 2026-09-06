## Purpose

Define selected source composition, explicit retention, independent artifact stages and loader policy, and scoped logical native requirements.

## ADDED Requirements

### Requirement: Resolve explicit artifact compositions

Every artifact SHALL have an application module and SHALL resolve its default/named/none runtime request through explicit build descriptors to exactly one source root or none. A runtime descriptor MAY declare an invocation root independently of loader policy. The compiler SHALL NOT choose a standard-library runtime by spelling or recover an invalid/none request with hosted defaults. Build-provided hosted defaults remain the existing startup policy until its separate migration. Selected runtime source SHALL import the exact application module through `Intrinsic.application` with ordinary import/visibility semantics. Missing, ambiguous or invalid roots SHALL diagnose with relevant origins.

#### Scenario: Replace runtime source

- **WHEN** a named runtime imports the application and declares a foreign export calling its public function
- **THEN** only that runtime and its selected dependencies enter analysis and its export reaches the application without a conventional application main

#### Scenario: Select no runtime

- **WHEN** a relocatable object, archive or loadable module requests no runtime
- **THEN** no conventional main or hosted invocation is required and no unselected runtime contributes source, roots, symbols or requirements

### Requirement: Retain only explicit native roots

Active foreign exports and explicit monomorphic runtime retention selectors SHALL seed native instance discovery. A retention selector MAY name a private function. Public visibility alone SHALL NOT seed discovery. Missing, ambiguous, generic, static or otherwise invalid root roles SHALL diagnose. Duplicate active source exports SHALL diagnose before emission. Empty no-runtime compositions SHALL be valid. Explicit retention SHALL survive native optimization and object lowering through llvm.used; archive retention SHALL NOT imply unrelated downstream archive extraction.

#### Scenario: Keep a private capability

- **WHEN** an object retains one private function and declares an unrelated public function
- **THEN** the private definition survives optimized object emission and the unrelated public definition is absent

### Requirement: Keep form, stage and loader entry independent

Semantic native forms SHALL be executable image, loadable module, static archive and relocatable object. IR, bitcode, assembly and intermediate object SHALL be independent emission stages. Loader-entry policy SHALL be default, absent or named symbol and SHALL NOT imply a Silk application call or foreign export. Named entries MAY be supplied by declared physical inputs. Unsupported combinations and conflicting requests SHALL diagnose. LTO SHALL remain rejected.

#### Scenario: Inspect a non-final object request

- **WHEN** an executable logical artifact requests LLVM IR with a named loader entry
- **THEN** the plan retains executable form, IR stage and the named loader request independently

### Requirement: Collect typed requirements by activation scope

Foreign-declaration `with Intrinsic.native(...)` clauses SHALL activate only for reachable foreign declarations. Standalone module clauses SHALL activate with their selected module, including modules with no reachable function. Artifact configuration requirements SHALL always activate for the artifact. Inactive branches and unselected modules SHALL contribute none. Requirements SHALL contain logical kind/name identities and admitted linkage, deployment and alternative constraints, never source filesystem paths or raw commands. No foreign-symbol or library-actor spelling SHALL imply a requirement.

#### Scenario: Distinguish three owners

- **WHEN** a selected module contains an unreachable foreign declaration requirement, a module requirement and the build adds an artifact requirement
- **THEN** only the module and artifact requirements enter the active logical plan

### Requirement: Merge hard constraints without losing origins

Exact logical duplicates SHALL collapse while preserving every origin. Compatible linkage, deployment and alternative constraints SHALL merge deterministically. Contradictory hard constraints SHALL diagnose with all contributing origins. Artifact choices SHALL select only admitted alternatives and SHALL NOT silently override a hard constraint. Physical resolution SHALL remain separate and ordered physical inputs SHALL retain duplicates and order. A final link without an explicit binding for an active unresolved requirement SHALL diagnose rather than infer a supply path.

#### Scenario: Merge a diamond dependency

- **WHEN** selected dependency paths contribute compatible requirements for the same logical identity
- **THEN** one merged requirement retains all scopes/origins and satisfies their intersection

#### Scenario: Reject incompatible linkage

- **WHEN** source and artifact configuration require contradictory hard linkage modes
- **THEN** the plan rejects the contradiction and reports every contributing source/configuration origin

### Requirement: Publish deterministic inspectable artifact identity

The logical plan SHALL expose normalized profile, form/stage, runtime request and resolution, application/runtime/retention roots, loader policy and named symbol, exports, selected closure/content, requirement facts/constraints/origins, codegen settings and compiler identity. Canonical logical identity SHALL exclude machine-local paths and incidental construction order, and SHALL distinguish changes to each semantic domain including default/named/none requests resolving to the same root. Physical link identity SHALL preserve ordered inputs separately.

#### Scenario: Compare equivalent requests

- **WHEN** independently constructed requests differ only in logical set ordering or source storage location
- **THEN** their normalized logical plans and identity agree

#### Scenario: Change selection rule

- **WHEN** default and named runtime requests resolve to the same module, or default and named loader policies currently resolve to the same symbol
- **THEN** the respective request identities remain distinct

### Requirement: Verify real native artifact retention

The designated conformance lanes SHALL use pinned LLVM, ABI and target supplies to compile/link/inspect no-runtime object/archive/module and custom-runtime fixtures on Darwin ARM64 and GNU/Linux ARM64/x86-64 in debug and optimized modes. A small independently compiled C consumer SHALL verify the distinguishing exported behavior. Missing supplies SHALL fail, not skip. Structural tests SHALL prove retention and analysis tests SHALL prove activation/conflict/identity.

#### Scenario: Retain through optimization

- **WHEN** native conformance runs for GNU/Linux ARM64
- **THEN** actual ARM64 objects, archives and linked exports are inspected and the independent consumer executes; target labels alone do not satisfy the gate
