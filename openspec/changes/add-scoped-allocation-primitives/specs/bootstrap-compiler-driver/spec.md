## ADDED Requirements

### Requirement: Scoped allocation has three-engine acceptance

The compiler acceptance surface SHALL run canonical scoped-allocation fixtures through logical
evaluation, native execution, and direct Wasm execution. It SHALL compare observable values,
acquisition and cleanup order, `OutOfMemory`, unsafe slot behavior, and final live-allocation state. A
fail-at-each-allocation-ordinal sweep SHALL continue until the first uninjected successful run, and
every failed run SHALL leave the process reusable and live logical bytes at the pre-request baseline.

#### Scenario: Accept an initialized-slot fixture

- **WHEN** the canonical fixture uses a restricted owner hook around runtime-counted move-only slots and exits through success, typed failure, early return, and loop transfer paths
- **THEN** all three engines agree on results and exactly-once reverse-order cleanup

#### Scenario: Sweep allocation exhaustion

- **WHEN** acceptance injects `OutOfMemory` at each reachable allocation ordinal in turn
- **THEN** each failing run drops every initialized prefix owned by its construction guard before releasing bytes, emits no successful output, and is followed by a successful run in the same process
