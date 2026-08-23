## MODIFIED Requirements

### Requirement: Function headers publish flow kind and failure contracts

Declaration indexing SHALL retain whether each declaration is ordinary or flow and SHALL resolve
every declared failure member to canonical nominal identity. Damaged, non-nominal, inaccessible,
or unknown members SHALL remain explicit unavailable facts with their originating diagnostics.
For every function declaration, the canonical header SHALL also retain whether the contextual
`test` marker is concretely present, independently of visibility, flow kind, body resolution,
eligibility, or inventory materialization. Downstream eligibility and inventory phases SHALL
consume this canonical marker fact rather than re-reading syntax or recognizing an actor name.

#### Scenario: Index a public flow contract

- **WHEN** a public flow declares a normalized row of imported nominal errors
- **THEN** its header exposes the flow kind and canonical row independently of body analysis order

#### Scenario: Retain a marked function header

- **WHEN** a private Effect function has a concrete `test` marker and its body has not yet resolved
- **THEN** the canonical indexed header retains the marker alongside its ordinary identity, visibility, flow kind, signature, and failure contract facts

#### Scenario: Keep an unmarked equivalent ordinary

- **WHEN** two otherwise equivalent function headers differ only because one has the contextual `test` marker
- **THEN** their ordinary header facts remain equal except for the marker-presence fact and neither gains changed visibility, flow kind, signature, failure, requirement, or call semantics
