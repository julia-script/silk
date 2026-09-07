## 1. Contract and evidence

- [ ] 1.1 Pin prescriptive ABI/call/path/resource contracts and reviewed prior-art sources; validate OpenSpec strictly and compile independent C layout/signature checks for all three supplies.
- [ ] 1.2 Add the minimal readonly pointer byte projection with strict MIR verification; verify pointer analysis/lowering tests and C short-record access.

## 2. Source provider and deletion

- [ ] 2.1 Implement selected declarations, affine descriptors/streams and immediate-error cleanup policy; verify ownership analysis and fault-injected transfer/close/cancellation cases.
- [ ] 2.2 Implement byte-root traversal, whole-file operations, stat/create/remove and bounded exclusive uniqueness; verify raw paths, symlinks, partial I/O, logical errors and collisions.
- [ ] 2.3 Implement bounded dirent reads and owned pending entries; verify short records and insufficient-buffer retries without iterator advancement.
- [ ] 2.4 Remove all eleven intrinsics and OsHandle compiler/C machinery, migrate every caller and document; verify inventories and repository searches contain no active superseded path.

## 3. Integration and publication

- [ ] 3.1 Run six independent native conformance lanes and record exact supplies/results; retain real filesystem effects in shared native acceptance.
- [ ] 3.2 Run typecheck, format:check, lint, test, check and release:candidate in order; record results and publish with gh stack without merging.
