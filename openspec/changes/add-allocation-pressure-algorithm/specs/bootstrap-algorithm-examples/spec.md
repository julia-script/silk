## ADDED Requirements

### Requirement: Breadth-first search exercises owned allocation

The suite SHALL contain an executable breadth-first search over a deterministic 5×5 grid whose
queue is an ordinary `Vector<QueueEntry>`. The search SHALL visit all 25 cells, return the committed
shortest-path result, and force vector capacities 4, 8, 16, and 32 through the explicit allocator
capability without using raw storage in the example source.

#### Scenario: Traverse the complete grid

- **WHEN** breadth-first search runs from the first cell to the opposite corner
- **THEN** evaluation, native execution, and direct WebAssembly return the same shortest distance

### Requirement: Allocation-sensitive examples retain resource evidence

An algorithm manifest MAY declare exact evaluation allocation evidence. When present, the harness
SHALL verify successful acquisitions, matching releases, and peak simultaneously live allocations
in addition to the ordinary result and target checks.

#### Scenario: Observe vector growth and cleanup

- **WHEN** the breadth-first-search queue grows through capacities 4, 8, 16, and 32 and then leaves scope
- **THEN** evaluation records four acquisitions, four releases, and a peak of two live allocations
