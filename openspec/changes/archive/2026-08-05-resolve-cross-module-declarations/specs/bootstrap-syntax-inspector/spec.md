## MODIFIED Requirements

### Requirement: Inspect the declaration index

The docs site SHALL expose a direct-link declaration-index lab presenting the collected headers of
a loaded closure: every declaration with its module, canonical identity state, explicit public or
default-private visibility, resolved signature, and cross-module importability, in canonical index
order, with duplicate and unavailable states explicit and the header-level diagnostics listed in
driver order. The lab SHALL keep its state in browser memory only.

#### Scenario: Inspect headers across modules

- **WHEN** a developer selects a preset whose modules declare functions with resolved signatures
- **THEN** the lab lists every header in canonical order with its module, canonical identity, visibility, parameters, and return type

#### Scenario: Distinguish public and private headers

- **WHEN** one module contains public and private functions
- **THEN** the lab shows both indexed headers and marks only the public functions as importable from another module

#### Scenario: Inspect duplicate and unavailable states

- **WHEN** a preset contains a duplicate declaration name and a declaration with a missing name
- **THEN** the duplicate header is marked as a caused duplicate of the original and the unnamed header is marked unidentified, while both remain listed

#### Scenario: Surface header diagnostics

- **WHEN** a preset contains an unknown parameter or return type
- **THEN** the lab lists the `SEM0001` diagnostic with its exact span in the unified panel

## ADDED Requirements

### Requirement: Inspect cross-module name resolution

The docs site SHALL expose a direct-link name-resolution lab presenting each module's flat scope:
local declarations, namespace bindings, selected-member bindings, aliases, canonical targets,
visibility decisions, conflicts, and lookup outcomes with exact syntax provenance. Presets SHALL
cover namespace, selective, and hybrid imports; private and unknown members; binding collisions;
damaged imports; and valid cyclic cross-module calls. The lab SHALL obtain all facts through the
analysis facade, keep state in browser memory only, and MUST NOT recreate scope or lookup logic.

#### Scenario: Inspect a hybrid scope

- **WHEN** a developer selects a module importing `compiler.Syntax as Tree { parse }`
- **THEN** the lab shows namespace `Tree` and selected member `parse` pointing to their canonical module and declaration identities

#### Scenario: Follow a cross-module call

- **WHEN** a root function calls a public function through a namespace alias
- **THEN** the lab links the call-site lookup, imported header, HIR call, and discovered instance by one canonical declaration identity

#### Scenario: Inspect a private member refusal

- **WHEN** a preset selects or qualifies a private function from another module
- **THEN** the lab shows the inaccessible candidate, unavailable binding or reference, diagnostic cause, and exact use-site span without presenting a successful call

#### Scenario: Inspect a flat-scope collision

- **WHEN** a local declaration and an import binding claim the same spelling
- **THEN** the lab shows every conflicting binding and the unavailable lookup without visually or textually choosing a winner

#### Scenario: Inspect a valid import cycle

- **WHEN** mutually importing modules call each other's public functions with complete contracts
- **THEN** the lab shows the module-cycle fact and both resolved canonical call edges without an error attributed solely to the cycle

#### Scenario: Inspect damaged import recovery

- **WHEN** an alias or selected-member list contains recovered syntax
- **THEN** the lab keeps its unavailable binding state beside the parser diagnostic while unrelated scopes and calls remain fully inspectable
