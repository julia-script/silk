## 1. Buffered state actors

- [x] 1.1 Implement fixed-capacity `BufferedInput` construction, fill, peek, consume, read, and discard operations; verify focused analysis/runtime cases cover retained lookahead, compaction, sticky end, invalid minima/consumption, exact progress, and no-I/O zero cases.
- [x] 1.2 Implement fixed-capacity `BufferedOutput` construction, exact-prefix buffering, finite/vector writes, explicit flush/finish, Writer adaptation, and terminal failure accounting; verify focused cases cover short writes, overflow, partial failure, unknown Writer progress, and no drop-time flush.

## 2. Scoped composition and transfer

- [x] 2.1 Implement scoped `BufferedDuplex.withBuffered` with separate direction buffers, one exclusive provider lease, internal provider binding, and nonparking terminal close; verify structured ownership rejects session/peek escape and ambient provider aliases.
- [x] 2.2 Implement bounded and exact `BufferedTransfer` operations that consume only destination-accepted source prefixes; verify partial destination failure retains the complete unaccepted suffix and zero limit performs no I/O.

## 3. Public delivery

- [x] 3.1 Register all new namespaces and aliases in the standard-library manifest and regenerate committed catalogs/embeddings; verify the generated namespace surface resolves.
- [x] 3.2 Add public buffered-byte-I/O reference documentation with executable examples and verify generated documentation remains current.
- [x] 3.3 Add the minimum distinct native and LLVM-to-Wasm shared-corpus cases plus focused structured analysis evidence needed to protect the acceptance contract, consolidating redundant setup and runtime work.
