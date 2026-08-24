## ADDED Requirements

### Requirement: Target layout plans the exact combined execution package

Target layout SHALL produce one validated Layout keyed by the target and concrete `A`, `F`, `O`,
and `R` plus the body's normalized suspension summary. The logical contents SHALL cover the owner
record, body environment, exact invoke/drop metadata, endpoint, stable wake-control state when
external parking is reachable, alignment and padding, and any statically selected initial
continuation segment. Physical field order, continuation placement, growth increments, and pooling
SHALL remain target-private. Layout planning SHALL detect size/alignment overflow and SHALL retain
canonical provenance consumed by the initializer.

#### Scenario: Plan a direct explicit body

- **WHEN** an explicit Execution body reaches no suspension and uses a zero-sized no-op endpoint
- **THEN** layout still covers erased-body ownership and invoke/drop metadata but omits continuation and readiness storage

#### Scenario: Plan a nested-only explicit body

- **WHEN** an explicit Execution body reaches nested suspension but not external parking
- **THEN** layout covers the owned package and any statically required initial nested continuation storage without a wake cell

#### Scenario: Plan an external-park body

- **WHEN** an explicit Execution body can reach external parking
- **THEN** layout includes the fixed endpoint and stable wake-control storage in the same package

#### Scenario: Keep physical layout private

- **WHEN** native and Wasm plan the same logical execution specialization
- **THEN** each returns its exact target Layout and common provenance facts without exposing backend field offsets or a stable ABI

#### Scenario: Reject layout overflow

- **WHEN** the complete package size or alignment cannot be represented for the selected target
- **THEN** target layout reports the canonical layout diagnostic and no initializer contract becomes available
