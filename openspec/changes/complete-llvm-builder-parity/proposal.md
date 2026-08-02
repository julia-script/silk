## Why

After the feature slices land, the package still needs an explicit parity pass to close omissions, prove interoperability, stabilize its public API, and make upstream drift visible. Without that gate, "ported" would describe code volume rather than demonstrated compatibility.

## What Changes

- Audit the implementation against the pinned `Builder.zig`, `bitcode_writer.zig`, and `ir.zig` baseline and close every supported semantic gap or document an intentional platform-language difference.
- Complete the textual IR printer, bitcode record coverage, calling-convention and intrinsic tables, escaping, ordering, and forward-reference behavior.
- Add a maintained parity manifest mapping upstream constructs to implementation modules and tests.
- Expand differential fixtures and LLVM validation across representative combinations, malformed inputs, deterministic rebuilds, and boundary-sized numeric values.
- Measure the bitstream and instruction hot paths and retain `Effect.fnUntraced` or imperative loops only where measurements justify them.
- Finalize documentation, all explicit package subpath exports, release-candidate checks, and upstream update instructions.

## Capabilities

### New Capabilities

- `llvm-builder-parity`: Audited Zig semantic parity, LLVM interoperability, determinism, performance bounds, and releasable public packaging.

### Modified Capabilities

None.

## Impact

This touches the complete `@silk-effect/llvm` package, its tests, README, changelog, package exports, release-candidate validation, and provenance documentation. It depends on all five preceding LLVM changes and does not claim parity with behavior that the pinned Zig builder itself marks unsupported or incomplete.
