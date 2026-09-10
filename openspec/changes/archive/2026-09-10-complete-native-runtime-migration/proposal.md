## Why

JUL-129, JUL-134, JUL-136, JUL-130 and JUL-147 complete the core Native OS Integration Plan. Execution storage, child processes, startup, host inputs and reporting still contain compiler-owned platform policy; raw Linux has machine primitives but no complete source runtime.

## What Changes

- **BREAKING:** Replace generated coroutine allocation, TLS accounting and environment configuration with explicit selected source storage components and instance ownership.
- **BREAKING:** Replace the child execute/capture intrinsics and global capture buffers with source-owned staging, descriptor, capture and reaping lifecycles.
- Add static non-PIE Linux x86-64/ARM64 source entry, syscall arities zero through six, process-stack decoding, descriptor I/O and mapping allocation.
- **BREAKING:** Replace generated hosted entry/reporting and all four host-input operations with selected source compositions. Migrate affected LLVM-to-Wasm storage and entry consumers together.
- Audit every replacement against the complete WS/SPEC ledger, retained compiler privileges, prescriptive reference, catalogs, artifacts and required verification lanes.

## Capabilities

### New Capabilities

- `source-execution-storage`: Explicit allocation/release, capacity and instance contracts for compiler-owned continuation layouts.
- `source-process-lifecycle`: Source-owned blocking child startup, concurrent capture and resource completion.
- `raw-linux-runtime`: Source-owned static non-PIE Linux startup, syscall and mapping composition.
- `source-hosted-runtime`: Selected application invocation, host inputs and terminal reporting.
- `native-migration-closure`: Evidence ledger and absence checks for the completed core migration.

### Modified Capabilities

Existing suspension, ownership, typed failure and fatal-trap semantics remain obligations of these replacements; this change does not redesign them.

## Impact

Compiler runtime capability selection and lowering, artifact/build composition, standard-library source, native catalogs, CLI/editor entry consumers, LLVM-to-Wasm consumers, conformance and acceptance tests, generated documentation and cache identities. Baseline: `dd4510fa` on 2026-09-07, including merged JUL-127/128/131/132/133. No compatibility path or deferred deletion is permitted for a completed replacement. Later static PIE, networking, atomics, TLS, retained callbacks and broad pointer/ABI extensions remain individually accounted roadmap obligations.
