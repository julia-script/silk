# JUL-115 removal inventory

Implementation began at `ee6ebb3d41a55fee65f1119b9c495c16d8dfec09`.

The baseline inventory was generated with:

```sh
rg -n 'Analysis\.evaluate|BootstrapEvaluation\.evaluate' --glob '!**/node_modules/**'
rg -n '@silklang/wasm|WasmBackend|WasmCleanup|WasmEmitContext|WasmLanes|WasmMemory' --glob '!**/node_modules/**'
```

It found 671 runtime-evaluator references in 174 files, and 266 direct-Wasm references in 82
files. The direct-Wasm package, compiler lowering actors, and Labs were deleted first. The native
acceptance corpus now treats each `program.expected` value as the runtime oracle rather than
comparing a compiled process to an evaluator result.

Every remaining evaluator-bearing test must be classified before removal as one of: retain through
an existing structural assertion, rewrite to a static/compiler assertion, add to the native
acceptance corpus, or delete as an implementation contract. This record is intentionally retained
until the final absence sweep replaces it with final counts.
