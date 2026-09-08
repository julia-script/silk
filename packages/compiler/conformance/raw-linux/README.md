# Raw Linux conformance

`run.mjs` compiles `program.silk` with the selected `silk/raw_start` source runtime,
then uses the compiler's physical native link planner to produce static non-PIE ELF
executables. It runs both unoptimized/debug and optimized configurations. Every
selected lane must execute successfully; there is no skip path.

The fixture checks kernel-stack arguments, environment presence, runtime AT_PAGESZ,
missing-page rejection, syscall arities zero through six, unsigned kernel-error
encoding, stdin EOF, invalid descriptor errors, stdout bytes, mapping zero-fill,
over-alignment, overflow rejection and explicit release. Final ELF inspection
requires ET_EXEC, no interpreter, no dynamic dependency and no undefined symbol.
The report retains the physical plan, inputs, helper inventory, tool versions,
header hashes and execution results. Object disassembly is saved alongside it.

The independent C fixture checks Linux UAPI constants and machine-word widths
against linux-libc-dev 6.1.180-1. Header hashes are checked before compilation.
LLVM tools must report version 22.1.8. The platform-supplies CI job uses its pinned
Debian Dockerfile and runs this fixture on native x86-64 and ARM64 runners.

For a configured supply, set SILK_SUPPLY_TARGET, SILK_SUPPLY_CLANG,
SILK_SUPPLY_AR, SILK_SUPPLY_LINKER and SILK_SUPPLY_IMAGE as in that job, then run:

```sh
node packages/compiler/conformance/raw-linux/run.mjs
```

Without supply settings, the runner uses the local Homebrew LLVM installation
and the two pinned `silk-jul124-conformance` images. This local mode executes both
architectures through Docker. Missing tools, images or differing identities fail.
Results are written to `.scratch/raw-linux-conformance`.

This fixture proves the synchronous raw subset. It does not establish completion
of execution-storage, hosted-reporting, hosted-input or child-process migration.
