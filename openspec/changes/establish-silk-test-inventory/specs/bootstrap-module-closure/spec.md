## MODIFIED Requirements

### Requirement: Load the reachable closure of one compilation request

An ordinary compilation request SHALL name one canonical root module, provide that root's exact
source bytes, and resolve imported sources by canonical logical module identity through the
source-resolution capability. Loading SHALL seed the closure with the explicit root, parse each
loaded module into its `SyntaxFile`, convert each syntactic dotted import path to a slash-separated
canonical identity, and follow resolved imports transitively until every reachable resolution has
been attempted. Each reachable canonical module SHALL be resolved and parsed at most once, and
modules not reachable from the root SHALL NOT be resolved or included. A request whose explicit
root identity is not canonical SHALL be rejected as a caller error rather than producing a source
diagnostic.

A test-aware project request SHALL compose ordinary one-root loads for an explicit ordered set of
canonical test roots and one separately designated canonical executable runner root into one
de-duplicated project union. The runner root MAY also appear in the test-root set. The request SHALL
preserve exact canonical identity and retain test-root reachability independently of runner
reachability. Inventory consumers SHALL use test-root reachability only. Root order MUST NOT change
project or inventory order. An unavailable constituent root or import SHALL preserve the ordinary
partial closure and its causes while preventing a runnable partial inventory.

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

#### Scenario: Compose a distinct runner with two test roots

- **WHEN** a test-aware project request supplies one runner root and two test roots with overlapping imports
- **THEN** every ordinary root closure is composed once by canonical identity, runner entry discovery uses the runner root, and inventory reachability contains only modules reached from either test root

#### Scenario: Repeat test roots in another order

- **WHEN** the same canonical test roots are supplied in a different order
- **THEN** the loaded project facts and ordered test inventory are identical

#### Scenario: Preserve a partial invalid test closure

- **WHEN** one test root or reachable import is unavailable
- **THEN** the ordinary resolver facts and causes remain available, while no runnable partial inventory is published
