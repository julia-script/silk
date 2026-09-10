## Why

ISSUE-004 permits dependent Drop to observe an already destroyed installed referent. ISSUE-008 skips live field cleanup when a loop reacquires a pattern-bound owner after a conditional partial drop.

## What Changes

- Reject installed dependencies whose referents do not survive ordered cleanup.
- Reset conditional cleanup state whenever a pattern acquires a fresh owner.
- Preserve separate regression witnesses for both safety defects.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-ownership`: specify installed dependent cleanup and repeated pattern acquisition cases.

## Impact

LifetimeFlow cleanup validation, match and statement-pattern lowering, and compiler ownership/MIR tests. No new dependencies or public package exports.
