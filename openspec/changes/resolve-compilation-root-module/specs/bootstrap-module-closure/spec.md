## REMOVED Requirements

### Requirement: Load the reachable closure of one compilation request

**Reason**: The explicit-source contract and its root-resolution bypass scenario are superseded by resolver-owned root loading.
**Migration**: Supply a canonical root identity and register its bytes in the source resolver. The replacement requirement below preserves import traversal behavior while changing root acquisition.

## ADDED Requirements

### Requirement: Load the reachable closure from a root module identity

A compilation request SHALL name one canonical root module as a string and obtain both root and
imported sources by canonical logical module identity through the source-resolution capability.
The request SHALL NOT carry root source bytes. Loading SHALL resolve the root, parse each loaded module into its
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

- **WHEN** a root imports `silk.effect { Effect }` and the resolver provides canonical module `silk/effect`
- **THEN** loading follows that exact module using the retained bytes of both path segments

#### Scenario: Exclude unreachable modules

- **WHEN** the resolver could provide a module that no reachable module imports
- **THEN** the closure does not request or contain it and no diagnostics mention it

#### Scenario: Resolve the named root

- **WHEN** a request names canonical root `app/Main` and the resolver supplies its source
- **THEN** loading requests `app/Main` exactly once and parses the returned bytes with their returned origin

#### Scenario: Reject a noncanonical root identity

- **WHEN** the explicit root identity is an absolute path, includes a source extension, or contains an empty, `.` or `..` segment
- **THEN** loading rejects the request as a caller error before resolving any source

#### Scenario: An import reaches the root again

- **WHEN** root `app/Main` imports a module that imports `app.Main`
- **THEN** both modules load and the back edge reuses the root resolution without a second resolver request

### Requirement: Required root failures prevent successful snapshots

A missing or operationally failed explicitly requested application or project root SHALL fail
analysis through a typed error before a successful snapshot is returned. The error SHALL identify
the requested module and distinguish missing source from operational resolution failure. Operational
failure SHALL retain the original typed resolver error. No source span, empty root source, or
unknown-import diagnostic SHALL be fabricated for this condition. Invalid root requests SHALL also
use a typed caller error. Multiple required roots SHALL be attempted in canonical order and the
first unavailable root SHALL determine the returned error. Import failures after required roots
have loaded SHALL retain the existing partial-closure behavior.

#### Scenario: Missing requested root

- **WHEN** the resolver reports that requested root `app/Main` is absent
- **THEN** analysis fails with a typed missing-root error naming `app/Main` and returns no successful snapshot

#### Scenario: Unreadable requested root

- **WHEN** resolution of requested root `app/Main` fails operationally
- **THEN** analysis fails with a typed root error retaining the original resolver failure and produces no fabricated source diagnostic

#### Scenario: One project root is unavailable

- **WHEN** a multi-root request contains an unavailable root
- **THEN** no successful project revision or root views are published for that request

#### Scenario: Failed import preserves root queries

- **WHEN** every explicitly requested root resolves but an imported module fails operationally
- **THEN** analysis retains the failed import fact and publishes partial analysis with all requested roots queryable
