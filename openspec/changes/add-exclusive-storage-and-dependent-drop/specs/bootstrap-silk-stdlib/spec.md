## ADDED Requirements

### Requirement: Vector supports borrowed shared and affine elements

The maintained ordinary-source Vector SHALL accept shared references and affine payloads containing exclusive references through ordinary make, detached-unit append/insert Effects, ordinary set and remove/pop, growth/failure and initialized-range destruction. Extracted payloads retain their external lifetime after container destruction. The collection owns occupancy and exactly-once cleanup policy. Owning its allocation does not make its elements Detached; borrowed Box.make outcomes remain outside this stage.

#### Scenario: Surviving extracted value keeps its source alive

- **WHEN** a borrowed element is extracted and its Vector destroyed
- **THEN** use remains accepted while the backing owner lives and invalidation of that owner before surviving use or required cleanup is rejected

#### Scenario: Growth failure cleans retained and incoming elements

- **WHEN** allocation fails while appending an affine dependent payload to a full Vector
- **THEN** the incoming value and previously initialized elements clean exactly once, before their respective storage is released
