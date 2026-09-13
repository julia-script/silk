## 1. Core HTTP values

- [x] 1.1 Add `silk.http` error/component/limit reason data plus Version, Method, Status, and private validated Header representations; verify shared runtime cases for supported/unsupported versions, extension-token spelling, status 299 and 99/600, valid raw value octets, and every forbidden control.
- [x] 1.2 Add bounded RequestHead and ResponseHead construction, including optional reason-byte validation and no body/framing/connection fields; verify structured analysis and shared runtime assertions cover valid, invalid, empty, and over-limit reasons.
- [x] 1.3 Add `silk.http_target` RequestTarget/HttpAuthority parsing, URI conversion, Host validation, effective-authority selection, and caller-buffer formatting; verify table tests cover all four forms, CONNECT/OPTIONS rules, IPv6 brackets, fragments, userinfo, empty path/query, Host cardinality, and target-authority precedence.

## 2. Ordered headers and ownership

- [x] 2.1 Add `silk.http_headers` Limits, borrowed Headers representations, ordered HeaderIterator, ASCII-insensitive `getAll`/`getFirst`/`getUnique`, and field-only formatting; verify duplicates/order/case, Set-Cookie preservation, aggregate count/byte overflow, exact size, empty values, and unchanged insufficient output.
- [x] 2.2 Add lazy bounded Connection, Content-Encoding, and Transfer-Encoding iterators with unknown-token retention and field-index/value-offset errors; verify token-list versus transfer-parameter grammar and known-token recognition without framing or decoder policy.
- [x] 2.3 Add OwnedHeaders, OwnedRequestHead, and OwnedResponseHead byte-plus-offset storage with affine cleanup and borrowed views; verify copies survive source release, views/iterators cannot escape owners, exact/one-past owned budgets, invalid-before-allocation behavior, and deterministic failure at every acquisition ordinal.

## 3. Public integration and documentation

- [x] 3.1 Register all three actors and aliases in the stdlib manifest, regenerate source/API surfaces, and verify generated-file checks expose every public actor without compiler-known HTTP policy.
- [x] 3.2 Add focused compiler tests using one shared analysis snapshot per source plus selected memory-only cases in the existing native/LLVM-to-Wasm acceptance corpus; verify ownership diagnostics structurally and avoid bespoke binaries, timing/count assertions, or per-feature determinism tests.
- [x] 3.3 Add pinned Zig comparison fixtures/provenance and update the prescriptive runtime/standard-library reference and public examples for ownership, limits, target availability, strictness decisions, and dependent-issue exclusions; verify documentation policy and doctests pass.

## 4. Verification and review

- [ ] 4.1 Run the required final checks in repository order—`pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, then `pnpm check` and `pnpm release:candidate`—and record exact results for the final committed head.
- [x] 4.2 Obtain an independent correctness review and a distinct test-economics review of the final diff, address every verified finding, and re-run affected checks before marking the change ready.
