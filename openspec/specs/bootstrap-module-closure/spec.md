# bootstrap-module-closure Specification

## Purpose
Loading the complete source-module closure of one compilation request: syntactic import
following, canonical module identities and deterministic ordering, explicit import-resolution
facts, and module-cycle facts, so every later phase operates over a known, reproducible set of
`SyntaxFile` artifacts.
## Requirements
### Requirement: Load the reachable closure of one compilation request

A compilation request SHALL name one root module and supply the available module sources as
logical module identities with exact bytes. Loading SHALL start at the root module, parse each
loaded module into its `SyntaxFile`, and follow syntactic imports transitively until the complete
reachable closure is known. Each reachable module SHALL be loaded and parsed exactly once, and
supplied modules that are not reachable from the root SHALL NOT be part of the closure. A request
whose root module is not among the supplied sources SHALL be rejected as a caller error rather
than producing a source diagnostic.

#### Scenario: Load a diamond closure

- **WHEN** the root imports two modules that both import one shared module
- **THEN** the closure contains all four modules and the shared module is parsed exactly once

#### Scenario: Exclude unreachable modules

- **WHEN** the supplied sources contain a module no reachable module imports
- **THEN** the closure does not contain it and no diagnostics mention it

#### Scenario: Reject a missing root

- **WHEN** a request names a root module absent from the supplied sources
- **THEN** loading rejects the request as a caller error

### Requirement: Canonical module identity orders the closure

Every loaded module SHALL carry its logical module identity as its canonical identity, and the
closure SHALL present its modules in canonical identity order. Closure content, order, and
diagnostics MUST NOT depend on filesystem traversal, source supply order, or import traversal
order.

#### Scenario: Reorder the supplied sources

- **WHEN** the same module set is supplied in two different orders and loaded from the same root
- **THEN** both closures present identical modules, module order, import facts, and diagnostics

#### Scenario: Order modules canonically

- **WHEN** the root imports modules whose identities sort before the root's own identity
- **THEN** the closure lists all modules in canonical identity order rather than traversal order

### Requirement: Import resolution stays explicit

Every parsed import SHALL produce one import fact retaining the spelled target and its exact
source provenance. An import whose target module is supplied SHALL resolve to that module; an
import naming the containing module SHALL be rejected as redundant with a stable module-phase
diagnostic; an import whose target is not supplied SHALL produce a stable module-phase diagnostic
at the import's name span. Unresolved and redundant import facts SHALL retain the identity of the
diagnostic that originated them, and an import whose name is unavailable after parser recovery
SHALL NOT produce an additional module diagnostic.

#### Scenario: Diagnose an unknown import target

- **WHEN** a loaded module imports an identity absent from the supplied sources
- **THEN** the closure records an unresolved import fact carrying a module-phase diagnostic at the import name's exact span

#### Scenario: Reject a self-import

- **WHEN** a module imports its own identity
- **THEN** the import fact is rejected as redundant with its own stable diagnostic and the module still loads

#### Scenario: Suppress cascades from recovered import syntax

- **WHEN** an import declaration's name is a missing token after parser recovery
- **THEN** the import fact is unavailable, the parser diagnostic stands alone, and no module-phase diagnostic repeats the mistake

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

