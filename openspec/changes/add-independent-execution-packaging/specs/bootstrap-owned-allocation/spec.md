## ADDED Requirements

### Requirement: One Allocation is the indivisible execution reclaim authority

The exact Execution initializer SHALL consume one self-contained Allocation and transfer its private
reclaim authority into one indivisible combined package. No separately reclaimable body, endpoint,
wake header, or initial segment SHALL be created by the intrinsic. Until external Wake retention is
introduced, completion or ordinary drop of the sole Execution SHALL release the package exactly
once after all live values are cleaned. A safe source wrapper SHALL procure the Allocation through
ordinary allocator policy and expose any typed failure before the initializer.

#### Scenario: Transfer one allocation owner

- **WHEN** a valid initializer consumes the matching Allocation
- **THEN** the source allocation binding ends and exactly one Execution package retains its reclaim authority

#### Scenario: Release after completion

- **WHEN** a running execution completes and no Wake can remain
- **THEN** all live package values are cleaned and the same transferred Allocation is released exactly once

#### Scenario: Roll back refused procurement

- **WHEN** source allocation fails before initialization
- **THEN** no reclaim authority transfers and source cleanup remains solely responsible for the body and endpoint values

#### Scenario: Forbid a hidden second package

- **WHEN** package construction is inspected for a parking-capable specialization
- **THEN** it contains one Allocation owner and no intrinsic-created second allocation or allocator access
