## Purpose

Resolve compatible physical platform components into immutable, inspectable native supplies and
ordered final link plans without influencing logical compiler semantics.

## ADDED Requirements

### Requirement: Supply requests select exactly one provider

Supply requests SHALL distinguish explicit, native discovery, automatic, and unsupported managed
selection. A direct request SHALL override artifact then project pins; automatic SHALL honor the
winning pin before native discovery. Discovery SHALL require declared host compatibility with the
canonical target. Explicit failure SHALL never fall back. Failures SHALL identify provider, input
origin, missing capability or incompatibility, and corrective configuration.

#### Scenario: An invalid pin is final

- **WHEN** automatic selection finds an explicit artifact pin whose SDK is missing
- **THEN** resolution fails for that pin without trying the project or native provider

#### Scenario: Cross-target automatic selection

- **WHEN** an unpinned automatic request targets a different host ABI
- **THEN** resolution requests an explicit compatible supply and does not discover host libraries

#### Scenario: Managed supply is deferred

- **WHEN** a managed request is selected
- **THEN** a typed unsupported-provider failure is returned without installing or downloading

### Requirement: Platform capabilities are validated independently

Darwin supplies SHALL distinguish SDK metadata, libSystem, frameworks, headers, and linker;
GNU supplies SHALL distinguish glibc libraries/scripts, CRT, support objects, interpreter, headers,
and tools. Resolution SHALL validate target architecture/ABI/libc and deployment without silently
raising the requested baseline. Explicit component combinations SHALL carry compatible provenance;
accidental installations outside the selected platform/support roots SHALL fail before linking.

#### Scenario: Darwin deployment exceeds SDK support

- **WHEN** the requested deployment or architecture is unsupported by SDK metadata
- **THEN** resolution fails before final linking and names the SDK and requested facts

#### Scenario: Missing Darwin capability

- **WHEN** a selected SDK lacks a compatible libSystem or requested framework
- **THEN** the failure distinguishes that capability from a missing linker or SDK

#### Scenario: GNU incomplete or mixed supply

- **WHEN** a GNU plan selects missing or incompatible CRT, libc/script, support object, or loader
- **THEN** resolution names the component and provenance conflict before final linking

#### Scenario: No libc profile

- **WHEN** an artifact selects no libc and an explicit loader entry
- **THEN** its plan introduces no platform CRT, libc, or dynamic loader implicitly

### Requirement: Resolution freezes every consulted physical input

The resolved supply SHALL expose immutable relevant environment values, exact query commands and
results, full tool versions and executable content digests, selected paths, versions and provenance.
Subsequent stages SHALL consume the snapshot and reject changed selected contents rather than
rediscover ambient values. Headers SHALL affect only operations that consume them.

#### Scenario: Environment changes after resolution

- **WHEN** SDKROOT or PATH changes after a successful resolution
- **THEN** compilation and linking use the recorded selection until a new resolution is requested

#### Scenario: A consumed header changes

- **WHEN** a C translation unit consumes a changed header
- **THEN** its newly resolved C object identity changes while header-free semantic/object identities do not

### Requirement: Final plans account for the complete ordered physical closure

A final plan SHALL consume logical requirements, roots, profile and loader-entry policy and expose
selected tools, concrete objects/archives, libraries, scripts and recursive references, stubs,
frameworks, runtime objects and interpreter. Named searches SHALL finish before complete identity
publication. Archive/group/as-needed/weak/import semantics SHALL be retained; unsupported input
forms SHALL diagnose. Symbol failures SHALL use the full actual linker input set and preserve origins.

#### Scenario: A named static library resolves

- **WHEN** a pinned search root and static library name select an archive
- **THEN** inspection shows the concrete archive, bytes digest and ordered origin before final linking

#### Scenario: Nested linker script

- **WHEN** a selected script references another script and an archive within a group
- **THEN** all referenced inputs enter the closure and execution preserves group resolution

#### Scenario: Weak and archive symbol semantics

- **WHEN** an unused archive member has undefined symbols or a weak symbol is superseded
- **THEN** validation follows actual archive extraction and weak resolution instead of rejecting every undefined member symbol

#### Scenario: Duplicate or missing definition

- **WHEN** the complete link fails symbol resolution
- **THEN** the typed failure retains the selected linker output and input origins

### Requirement: Physical identities respect stage boundaries

Semantic and object identities SHALL remain independent of equivalent physical supply locations.
Final identity SHALL hash actual ordered selected contents including linker bytes, CRT, libraries,
scripts/references, stubs/frameworks and runtime objects. Paths SHALL distinguish final identity only
when emitted contents depend on them. Complete accounting SHALL feed the existing permanent final
cache admission rule; unresolved native plans SHALL remain ineligible.

#### Scenario: Supply relocation

- **WHEN** byte-identical supplies move without changing any emitted path identity
- **THEN** semantic, object and final identities remain unchanged

#### Scenario: Physical content changes

- **WHEN** a selected linker, CRT, nested script, stub, framework or runtime object changes
- **THEN** final identity changes even when tool paths and first version lines remain the same

#### Scenario: Interpreter path is emitted

- **WHEN** the runtime interpreter path changes with identical loader bytes
- **THEN** final identity changes because the executable embeds that path

### Requirement: Required native lanes validate real supplies

Required CI SHALL pin Darwin ARM64 and GNU/Linux x86-64 and ARM64 tools/platform baselines, compile,
link and inspect real artifacts and separately compiled C fixtures, execute designated Darwin and
x86-64 cases and ARM64 cases where a runner exists, and fail missing or skipped designated cases.
Debug and optimized boundaries SHALL be exercised; unverified LTO SHALL be rejected explicitly.
Inspection SHALL publish exact component digests and tool/SDK/glibc/deployment identities.

#### Scenario: A required supply is missing

- **WHEN** a required native lane cannot locate its pinned tools or components
- **THEN** the lane fails instead of reporting a skipped conformance success

#### Scenario: Actual final artifacts

- **WHEN** a required lane succeeds
- **THEN** retained evidence includes real link, object/interpreter or Mach-O inspection, C fixture execution and resolved supply identities
