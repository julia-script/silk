## 1. Canonical Source and Generation

- [x] 1.1 Move the exact current `silk/vector` bytes into `packages/compiler/stdlib/silk/vector.silk` and add a deterministic module manifest.
- [x] 1.2 Add a deterministic generator for the embedded module byte table and replace the authored JavaScript template string with its generated artifact.
- [x] 1.3 Add tests that compare every generated module byte-for-byte with its canonical `.silk` file and verify manifest order/identity.

## 2. Resolution and Source Origin

- [x] 2.1 Extend successful source resolution with discriminated project-file, toolchain-file, and in-memory origins while preserving exact bytes.
- [x] 2.2 Route reserved `silk/` identities through the stdlib manifest/root and add tests for success, project collision, missing packaged files, deterministic closure loading, and in-memory tooling.
- [x] 2.3 Preserve source origin through analysis snapshots and diagnostics without introducing a second LSP-only source registry.

## 3. Navigation and Packaging

- [x] 3.1 Update LSP target URI selection to use analyzed source origin and add standard-library go-to-definition coverage alongside existing project/open-document cases.
- [x] 3.2 Include the stdlib manifest and `.silk` tree in compiler package contents and add packed-installation resolution coverage.
- [x] 3.3 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`; record any pre-existing failure precisely.
