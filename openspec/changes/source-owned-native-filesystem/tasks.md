## 1. Contract and evidence

- [x] 1.1 Pin prescriptive ABI/call/path/resource contracts and reviewed prior-art sources; validate OpenSpec strictly and compile independent C layout/signature checks for all three supplies.
- [x] 1.2 Add the minimal readonly pointer byte projection with strict MIR verification; verify pointer analysis/lowering tests and C short-record access.

## 2. Source provider and deletion

- [x] 2.1 Implement selected declarations, affine descriptors/streams and immediate-error cleanup policy; verify ownership analysis and fault-injected transfer/close/cancellation cases.
- [x] 2.2 Implement byte-root traversal, whole-file operations, stat/create/remove and bounded exclusive uniqueness; verify raw paths, symlinks, partial I/O, logical errors and collisions.
- [x] 2.3 Implement bounded dirent reads and owned pending entries; verify short records and insufficient-buffer retries without iterator advancement.
- [x] 2.4 Remove all eleven intrinsics and OsHandle compiler/C machinery, migrate every caller and document; verify inventories and repository searches contain no active superseded path.

## 3. Integration and publication

- [x] 3.1 Run six independent native conformance lanes and record exact supplies/results; retain real filesystem effects in shared native acceptance.
- [x] 3.2 Run typecheck, format:check, lint, test, check and release:candidate in order; record results and publish with gh stack without merging.
