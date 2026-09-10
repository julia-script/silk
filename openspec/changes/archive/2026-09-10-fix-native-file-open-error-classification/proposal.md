## Why

JUL-149 identifies successful metadata inspection being classified through stale `errno`, so
opening a directory can report the wrong recovery reason.

## What Changes

- Separate failed metadata calls from successful nonregular metadata in generated file-open.
- Preserve a failed syscall's native error through cleanup and close the descriptor once.
- Add controlled generated-boundary regression evidence for both outcomes.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-os-file-system`: Make metadata classification and partial-open cleanup explicit within the existing WrongType contract.

## Impact

`packages/compiler/src/OsRuntime.ts` and the existing filesystem boundary tests. No new public API,
provider migration, or changes to the process error ABI.
