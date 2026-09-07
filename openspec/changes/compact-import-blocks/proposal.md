## Why

JUL-74 requests compact import blocks. The formatter currently inserts a blank line between every declaration, contrary to the import style favored by maintained Silk source.

## What Changes

- Consecutive top-level imports form a compact block across comments and whitespace.
- Preserve source order and comment attachment; keep one blank line at either boundary with a non-import declaration.
- Update the canonical spacing specification, focused formatter coverage, and only required canonical artifacts.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `silk-source-formatting`: Define compact consecutive import blocks and their boundaries.

## Impact

`SyntaxFormatter.ts`, its focused tests, and the maintained formatting specification. No semantic, dependency, or public API changes; canonical output changes for adjacent imports.
