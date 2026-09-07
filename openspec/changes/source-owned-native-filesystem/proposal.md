## Why

JUL-131 replaces the eleven privileged filesystem operations and compiler-owned OsHandle protocol with ordinary selected source. Native record access, descriptor ownership, byte-preserving paths and cleanup policy must move together so the source provider owns its complete contract.

## What Changes

- Implement selected filesystem declarations, actual open/openat variadics and verified per-target stat/dirent layouts and flags.
- Replace compiler handles with affine source resources, descriptor-relative traversal, owned pending directory names and explicit close-result policy backed by Drop cleanup on structured exits.
- Preserve whole-file/stat/list/create/remove/unique-directory outcomes and exact path bytes; define predictable bounded unique-name generation with exclusive mkdirat, independent of Random.
- Add one minimal target-neutral unsafe typed-pointer-to-readonly-byte-view operation, needed to access variable-sized readdir records without loading sizeof(dirent).
- **BREAKING**: delete all eleven filesystem intrinsics, special handle construction/access/lowering and generated C filesystem policy; native provider members require selected libc.
- Pin source/C conformance and migrate all current consumers, documentation and inventories without compatibility paths.

## Capabilities

### New Capabilities

- `source-owned-native-filesystem`: exact selected calls, source resource/path/error policy and native conformance.
- `raw-pointer-byte-view`: readonly byte projection preserving raw-pointer identity without whole-record loads or integer reconstruction.

### Modified Capabilities

- `bootstrap-os-file-system`: replace intrinsic/handle contracts with source ownership, lossless paths and structured cleanup.

## Impact

Native filesystem and portable Path consumers, Pointer primitive/lowering, former OsHandle compiler machinery, shared native corpus and filesystem boundary fixtures, catalogs/docs and supply CI. No seek/fcntl, full POSIX surface, raw kernel API or additional filesystem service.
