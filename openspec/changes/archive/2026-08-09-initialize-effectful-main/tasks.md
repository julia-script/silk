## 1. Effectful Project Scaffold

- [x] 1.1 Replace the initializer's ordinary integer entry template with the canonical zero-argument `effect fn main() -> ()` source
- [x] 1.2 Update exact-source and CLI acceptance coverage to prove initialized projects use the effectful entry and still check, build for native and WebAssembly, and run with status zero

## 2. Verification

- [x] 2.1 Run focused CLI initializer and end-to-end command tests
- [x] 2.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
- [x] 2.3 Validate `initialize-effectful-main` strictly and confirm every implementation task is complete
