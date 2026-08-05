# bootstrap-module-closure Specification

## Purpose
Loading the complete source-module closure of one compilation request: syntactic import
following, canonical module identities and deterministic ordering, explicit import-resolution
facts, and module-cycle facts, so every later phase operates over a known, reproducible set of
`SyntaxFile` artifacts.
## Requirements
### Requirement: Load the reachable closure of one compilation request

A compilation request SHALL name one canonical root module and supply the available source bytes by
canonical logical module identity. Loading SHALL start at the root module, parse each loaded module
into its `SyntaxFile`, convert each syntactic dotted import path to a slash-separated canonical
identity, and follow resolved imports transitively until the complete reachable closure is known.
Each reachable canonical module SHALL be loaded and parsed exactly once, and supplied modules that
are not reachable from the root SHALL NOT be part of the closure. A request whose root module is
absent or whose root or supplied identities are not canonical SHALL be rejected as a caller error
rather than producing a source diagnostic.

#### Scenario: Load a diamond closure

- **WHEN** the root imports two modules that both import one shared module
- **THEN** the closure contains all four modules and the shared module is parsed exactly once

#### Scenario: Follow a multi-segment import

- **WHEN** a root source imports `compiler.Syntax` and the request supplies canonical module `compiler/Syntax`
- **THEN** loading follows that exact canonical module and retains the dotted spelling in the import's source provenance

#### Scenario: Exclude unreachable modules

- **WHEN** the supplied sources contain a module no reachable module imports
- **THEN** the closure does not contain it and no diagnostics mention it

#### Scenario: Reject a missing root

- **WHEN** a request names a root module absent from the supplied sources
- **THEN** loading rejects the request as a caller error

#### Scenario: Reject a noncanonical request identity

- **WHEN** a request key uses an absolute path, source extension, empty segment, `.` segment, or `..` segment
- **THEN** loading rejects the request as a caller error before parsing source bytes

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
target module is supplied SHALL resolve to that canonical module; an import naming the containing
module SHALL be rejected as redundant with a stable module-phase diagnostic; and an import whose
exact target is not supplied SHALL produce a stable module-phase diagnostic at the path's span.
Unknown and self-import facts SHALL retain the identity of the diagnostic that originated them. An
import whose path is unavailable after parser recovery SHALL remain unavailable and SHALL NOT
produce an additional module diagnostic. Namespace aliases and selected members SHALL not affect
which module the closure loads.

#### Scenario: Diagnose an unknown import target

- **WHEN** a loaded module imports an identity absent from the supplied sources
- **THEN** the closure records an unknown import fact carrying a module-phase diagnostic at the complete import path's exact span

#### Scenario: Reject a self-import

- **WHEN** a module imports its own canonical identity through dotted source spelling
- **THEN** the import fact is rejected as redundant with its own stable diagnostic and the module still loads

#### Scenario: Suppress cascades from recovered import syntax

- **WHEN** an import path contains missing syntax after parser recovery
- **THEN** the import fact is unavailable, the parser diagnostic stands alone, and no module-phase diagnostic repeats the mistake

#### Scenario: Ignore binding clauses while loading

- **WHEN** namespace-only, selective-only, and hybrid imports name the same canonical target in separate compilation fixtures
- **THEN** each form causes the closure to follow the same target module while retaining its distinct concrete binding clause

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
