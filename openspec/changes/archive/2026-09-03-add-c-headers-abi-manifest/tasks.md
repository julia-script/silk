## 1. Canonical companion content

- [x] 1.1 Add a `CHeader` actor that renders package guards, scalar/pointer declarations, callbacks, functions, and exported data from canonical ABI inventories; verify one pure table-driven test pins exact header bytes for representative nested declarators and empty arity.
- [x] 1.2 Add an `AbiManifest` actor with a closed versioned entry schema and deterministic UTF-8 JSON encoding; verify pure target-parameterized goldens pin exact Darwin and Linux bytes, ordering, and target-sized integer classes.

## 2. Driver and toolchain delivery

- [x] 2.1 Carry the validated package artifact name and complete import/export inventory through driver outcomes; verify focused driver tests distinguish library companion records from executable/WebAssembly outcomes.
- [x] 2.2 Commit `<package>.h` and `<package>.abi.json` for every cached and uncached native-library success path inside the native toolchain boundary, with typed failure cleanup; verify focused storage/cache tests cover identical bytes and absence after failure.
- [x] 2.3 Return and report all durable library paths through CLI workflow outcomes while emitting no companions for executables or WebAssembly; verify existing CLI build tests cover shared/static reporting and non-library absence.

## 3. Acceptance and documentation

- [x] 3.1 Replace the existing native library consumer's handwritten declarations with `#include` of the generated header and verify the same serialized shared/static compile-link-run acceptance passes without another native process.
- [x] 3.2 Document header and manifest filenames, schema, ordering, pointer erasure, callback syntax, cache behavior, and library-only scope; verify documentation and policy checks pass.
- [x] 3.3 Sync the three delta specs into the main specifications and verify strict OpenSpec validation before archive.

## 4. Repository verification and handoff

- [x] 4.1 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` in repository order and record exact outcomes.
- [x] 4.2 Prepare the exact committed issue diff for independent correctness and mandatory test-economics approval, then create and read back the stacked draft PR and Linear handoff evidence.
- [x] 4.3 Archive the completed OpenSpec change, repair any active-change links, and verify the archived change plus all active specs strictly.
