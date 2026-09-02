# bootstrap-module-closure Specification

## Purpose

Loading the complete source-module closure of one compilation request: syntactic import
following, canonical module identities and deterministic ordering, explicit import-resolution
facts, and module-cycle facts, so every later phase operates over a known, reproducible set of
`SyntaxFile` artifacts.

## Requirements

### Requirement: Load the reachable closure of one compilation request

A compilation request SHALL name one canonical root module, provide that root's exact source bytes,
and resolve imported sources by canonical logical module identity through the source-resolution
capability. Loading SHALL seed the closure with the explicit root, parse each loaded module into its
`SyntaxFile`, convert every contextual segment of each syntactic dotted import path to a
slash-separated canonical identity without filtering by retained token kind, and follow resolved
imports transitively until every reachable resolution has been attempted. Each reachable canonical
module SHALL be resolved and parsed at most once, and modules not reachable from the root SHALL NOT
be resolved or included. A request whose explicit root identity is not canonical SHALL be rejected
as a caller error rather than producing a source diagnostic.

#### Scenario: Load a diamond closure

- **WHEN** the root imports two modules that both import one shared module
- **THEN** the closure contains all four modules and the shared module is resolved and parsed exactly once

#### Scenario: Follow a multi-segment import

- **WHEN** a root source imports `compiler.Syntax` and the resolver provides canonical module `compiler/Syntax`
- **THEN** loading follows that exact canonical module and retains the dotted spelling in the import's source provenance

#### Scenario: Follow a reserved path segment

- **WHEN** a root imports `silk.effect as Effect` and the resolver provides canonical module `silk/effect`
- **THEN** loading follows that exact module using the retained bytes of both path segments

#### Scenario: Exclude unreachable modules

- **WHEN** the resolver could provide a module that no reachable module imports
- **THEN** the closure does not request or contain it and no diagnostics mention it

#### Scenario: Use the explicit root without resolving it

- **WHEN** a request provides canonical root `app/Main` and its exact bytes
- **THEN** loading parses those bytes as `app/Main` without requesting the root from the resolver

#### Scenario: Reject a noncanonical root identity

- **WHEN** the explicit root identity is an absolute path, includes a source extension, or contains an empty, `.` or `..` segment
- **THEN** loading rejects the request as a caller error before parsing the root bytes or resolving imports

### Requirement: Canonical module identity orders the closure

Every loaded module SHALL carry one case-sensitive, extensionless, slash-separated logical path
relative to the compilation source root as its canonical identity. Dotted import segments SHALL map
one-for-one to that path. Source text SHALL NOT declare or override module identity, and the same
source bytes supplied under a moved, renamed, or differently cased canonical path SHALL identify a
different module and different declarations. The closure SHALL present modules in canonical
identity order, and its content, order, and diagnostics MUST NOT depend on filesystem traversal,
source supply order, import traversal order, symlink aliases, or case-folding behavior of the host
filesystem.

#### Scenario: Reorder the supplied sources

- **WHEN** the same canonical module set is supplied in two different orders and loaded from the same root
- **THEN** both closures present identical modules, module order, import facts, and diagnostics

#### Scenario: Order modules canonically

- **WHEN** the root imports modules whose identities sort before the root's own identity
- **THEN** the closure lists all modules in canonical identity order rather than traversal order

#### Scenario: Preserve exact case

- **WHEN** source imports `compiler.Syntax` but only canonical identity `compiler/syntax` is supplied
- **THEN** resolution reports the exact requested identity as unknown and does not case-fold to the supplied module

#### Scenario: Rename a module identity

- **WHEN** identical source bytes are supplied first as `compiler/Syntax` and later as `compiler/Tree`
- **THEN** the two closures assign different canonical module and declaration identities

### Requirement: Import resolution stays explicit

Every parsed import SHALL produce one import fact retaining its ordered path syntax, dotted source
spelling, derived canonical target identity, and exact source provenance. An import whose exact
target resolves to source bytes SHALL resolve to that canonical module; an import naming the
containing module SHALL be rejected as redundant with a stable module-phase diagnostic; and an
import whose exact target is absent SHALL produce a stable module-phase diagnostic at the path's
span. An operationally failed resolution SHALL remain a distinct failed import fact carrying its
typed failure and SHALL NOT produce an unknown-module diagnostic. Unknown and self-import facts
SHALL retain the identity of the diagnostic that originated them. An import whose path is
unavailable after parser recovery SHALL remain unavailable and SHALL NOT request the resolver or
produce an additional module diagnostic. Namespace aliases and selected members SHALL not affect
which module the closure resolves.

#### Scenario: Diagnose an unknown import target

- **WHEN** a loaded module imports an identity for which resolution returns absent
- **THEN** the closure records an unknown import fact carrying a module-phase diagnostic at the complete import path's exact span

#### Scenario: Retain an operational resolution failure

- **WHEN** resolving an imported identity fails with a typed source-resolution error
- **THEN** the closure records a failed import fact carrying that error and emits no unknown-module diagnostic for it

#### Scenario: Reject a self-import

- **WHEN** a module imports its own canonical identity through dotted source spelling
- **THEN** the import fact is rejected as redundant without invoking the resolver, and the module still loads

#### Scenario: Suppress cascades from recovered import syntax

- **WHEN** an import path contains missing syntax after parser recovery
- **THEN** the import fact is unavailable, the parser diagnostic stands alone, and the resolver is not invoked for the damaged path

#### Scenario: Ignore binding clauses while loading

- **WHEN** namespace-only, selective-only, and hybrid imports name the same canonical target in separate compilation fixtures
- **THEN** each form causes the closure to resolve the same target module while retaining its distinct concrete binding clause

### Requirement: Resolver failures preserve a partial closure

Module-closure loading SHALL capture each imported module's typed source-resolution failure as
immutable closure data and continue resolving other pending canonical modules. A failed target
SHALL be unavailable to downstream phases, while the root and every successfully loaded module
remain fully queryable. Captured operational failures SHALL remain separate from the closure's
source diagnostic collection.

#### Scenario: Continue around one unreadable import

- **WHEN** the root imports one readable module and one module whose resolution fails operationally
- **THEN** the closure contains the root and readable module, records the failed target and typed failure, and continues analysis without an unknown-module diagnostic for the failure

#### Scenario: Preserve failure facts across repeated loads

- **WHEN** equivalent resolver outcomes are loaded repeatedly in fresh processes
- **THEN** the partial modules, import facts, resolver failures, cycles, and diagnostics are identical and canonically ordered

### Requirement: Module cycles are explicit facts

Import cycles among distinct modules SHALL be permitted and SHALL be reported as deterministic
cycle facts naming every participating module in canonical order. Cycle facts MUST NOT depend on
traversal order and MUST NOT produce error diagnostics by themselves.

#### Scenario: Load a mutual import cycle

- **WHEN** two modules import each other and both are reachable from the root
- **THEN** both modules load completely and the closure records one cycle fact naming both modules in canonical order

#### Scenario: Keep acyclic closures cycle-free

- **WHEN** the closure's import graph has no cycle
- **THEN** the closure records no cycle facts

### Requirement: Source closure does not imply executable intrinsic reachability

Module closure SHALL continue to load and semantically analyze canonical source needed for ordinary
name and type resolution, including declarations that mention restricted intrinsics. It SHALL
publish enough call identity for later executable closure to determine which intrinsic operations
survive from the selected entry. Source-module presence alone MUST NOT be interpreted as executable
use or target incompatibility.

#### Scenario: Load a portable and native provider together

- **WHEN** module closure loads declarations for both portable source and a native-only provider
- **THEN** both declarations remain navigable while only calls reachable from the chosen entry participate in availability validation

#### Scenario: Retain a reachable intrinsic identity

- **WHEN** an ordinary reachable wrapper calls one sealed restricted intrinsic
- **THEN** executable closure preserves that canonical intrinsic identity for target validation rather than treating the wrapper's module as the availability unit
