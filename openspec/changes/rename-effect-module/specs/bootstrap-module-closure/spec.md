## MODIFIED Requirements

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
