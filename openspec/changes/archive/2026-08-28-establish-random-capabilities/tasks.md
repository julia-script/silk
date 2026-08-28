## 1. Rename the deterministic capability

- [x] 1.1 Move the existing xoshiro256** source to `silk/insecure_random`, rename its service and documentation to `InsecureRandom`, update the manifest with no compatibility identity, and verify `StdlibResolution.test.ts` resolves only the new insecure surface.
- [x] 1.2 Migrate user-service tests and the differential corpus to `InsecureRandom` while preserving every known-answer sequence and byte-consumption assertion, and verify the focused `UserServices.test.ts` and `StdlibNamespaceAcceptance.test.ts` cases pass on evaluator and direct WebAssembly.
- [x] 1.3 Regenerate the standard-library embedding and documentation artifacts after the rename, remove all non-historical stale seeded `silk/random` references, and verify `pnpm --filter @silklang/compiler stdlib:check` plus the compiler documentation checks pass.

## 2. Add portable secure random and insecure seed

- [x] 2.1 Implement canonical `silk/random` with the exclusive exact-fill service, wrapper fast path, little-endian `nextU64`, bit-63 `nextBool`, and unbiased `below`, and verify focused ordinary-provider tests cover exact consumption, rejection, and zero provider advancement through both wrapper and direct service dispatch.
- [x] 2.2 Implement canonical `silk/insecure_seed` with private fields, `Copy` `Seed`, accessors, immutable shared fixed provider, and `fromRandom` one-time constructor; verify focused tests prove stable repeated `Effect.provide` reads, the exact `&InsecureSeed` requirement, rejected direct field access, and exactly sixteen initialization bytes.
- [x] 2.3 Add manifest/navigation/documentation entries for all portable actors, regenerate owned artifacts, and verify stdlib resolution, source-copy privilege, documentation policy/examples, and one shared evaluator/direct-Wasm source-provider case (including fixed and random-initialized `InsecureSeed`) succeed without an OS intrinsic or import.
- [x] 2.4 Run the first larger milestone gates with `pnpm typecheck`, `pnpm exec biome check .`, and focused compiler tests for random services; record and resolve every change-caused failure before starting the host boundary.

## 3. Add the sealed compiler and evaluator boundary

- [x] 3.1 Add exactly one evaluator/LLVM-only `Intrinsic.osRandomFill` catalog entry and thread it through HIR, MIR, lowering, declarations, reachability, and runtime-symbol inventories, verifying intrinsic inventory tests and direct-Wasm reachable-only rejection.
- [x] 3.2 Add the publicly exported `RandomHost` boundary actor and compiler package subpath with per-call scripted chunks, closed failure categories, explicit `BootstrapEvaluation.Options.randomHost` injection, staged exact-length writes, and no ambient source; verify a deep import typechecks and sentinel output remains unchanged after exhaustion, underfill, overfill, invalid-byte, explicit-failure, and thrown-host paths.
- [x] 3.3 Add `MissingRandomHost` blockage and random-call trace presentation across bootstrap trace, inspector, and flow views while retaining only length, outcome, call-site provenance, and normalized category; verify a generated-byte canary embedded in returned and thrown payloads appears nowhere in raw traces, inspector rows, flow models, or serialized snapshots.
- [x] 3.4 Implement and register canonical `silk/os_random` as the sole intrinsic consumer with stateless construction and fatal failure translation, regenerate the standard-library embedding, and verify source navigation, canonical-consumer validation, evaluator success, missing-host blockage, failure trapping, unused-provider direct-Wasm acceptance, and reachable-provider direct-Wasm rejection.

## 4. Add the Unix native provider

- [x] 4.1 Add the reachability-selected `silk_os_random_fill_v1` declaration and C runtime fragment using capped `getrandom(..., GRND_NONBLOCK)` on GNU/Linux and one `arc4random_buf` call on macOS, update the documented platform baselines, and verify symbol selection excludes random support from programs that do not reach `OsRandom`.
- [x] 4.2 Add a deterministic native C harness that forces and compiles both preprocessor branches; verify Linux zero length, count cap, `GRND_NONBLOCK`, short-fill pointer/count advancement, `EINTR`, `EAGAIN`/hard failure, and incomplete output, plus macOS zero-length no-call behavior and exactly one `arc4random_buf` call with the exact pointer/length for every nonempty request.
- [x] 4.3 Add one native acceptance corpus entry with a deterministic ordinary provider in `source` and an `OsRandom`-backed equivalent in `nativeSource`, both returning the same status without statistical or timing assertions, and verify `DriverNativeAcceptance.test.ts` passes on the current host.
- [x] 4.4 Generalize the macOS native-clocks CI job to run the OS-random harness and native acceptance case, regenerate toolchain integrity and other owned compiler artifacts, then verify toolchain checks, focused intrinsic/runtime tests, and the complete compiler test target pass.

## 5. Finish integration and release validation

- [x] 5.1 Add a changeset for the breaking standard-library rename and new random capabilities, audit generated documentation and repository text for obsolete security claims or compatibility paths, and verify only intentional historical OpenSpec artifacts retain the old wording.
- [x] 5.2 Run the required gates in order—`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`—and resolve every change-caused failure without weakening tests or retaining superseded APIs.
- [x] 5.3 Run `pnpm release:candidate` because package contents changed, review the final diff for Unix-only scope and accidental byte disclosure, and record the successful command results in the implementation handoff.
