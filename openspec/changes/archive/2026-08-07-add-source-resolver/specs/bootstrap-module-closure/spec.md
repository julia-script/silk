## MODIFIED Requirements

### Requirement: Load the reachable closure of one compilation request

A compilation request SHALL name one canonical root module, provide that root's exact source bytes,
and resolve imported sources by canonical logical module identity through the source-resolution
capability. Loading SHALL seed the closure with the explicit root, parse each loaded module into its
`SyntaxFile`, convert each syntactic dotted import path to a slash-separated canonical identity, and
follow resolved imports transitively until every reachable resolution has been attempted. Each
reachable canonical module SHALL be resolved and parsed at most once, and modules not reachable
from the root SHALL NOT be resolved or included. A request whose explicit root identity is not
canonical SHALL be rejected as a caller error rather than producing a source diagnostic.

#### Scenario: Load a diamond closure

- **WHEN** the root imports two modules that both import one shared module
- **THEN** the closure contains all four modules and the shared module is resolved and parsed exactly once

#### Scenario: Follow a multi-segment import

- **WHEN** a root source imports `compiler.Syntax` and the resolver provides canonical module `compiler/Syntax`
- **THEN** loading follows that exact canonical module and retains the dotted spelling in the import's source provenance

#### Scenario: Exclude unreachable modules

- **WHEN** the resolver could provide a module that no reachable module imports
- **THEN** the closure does not request or contain it and no diagnostics mention it

#### Scenario: Use the explicit root without resolving it

- **WHEN** a request provides canonical root `app/Main` and its exact bytes
- **THEN** loading parses those bytes as `app/Main` without requesting the root from the resolver

#### Scenario: Reject a noncanonical root identity

- **WHEN** the explicit root identity is an absolute path, includes a source extension, or contains an empty, `.` or `..` segment
- **THEN** loading rejects the request as a caller error before parsing the root bytes or resolving imports

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

## ADDED Requirements

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
