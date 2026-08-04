# Tiny language tutorial example

This directory is the completed consumer-shaped project developed by the
[Build Tiny tutorial](../../packages/llvm/docs/tutorials/tiny-language/01-meet-tiny.md).
It imports only public `@silk-effect/llvm/*` package subpaths and is intentionally outside the
repository's pnpm workspace.

After `@silk-effect/llvm` is published, install and verify it with:

```sh
pnpm install
pnpm typecheck
pnpm test
mkdir -p build
pnpm --silent smoke > build/score.ll
clang build/score.ll -o build/score
./build/score
```

The completed Tiny program returns process status `20`. The CLI writes LLVM IR to stdout and
diagnostics to stderr; it never invokes Clang itself.

Until publication, repository maintainers validate this project from a clean temporary directory.
Build and pack `packages/llvm`, copy this example into the temporary directory, delete the
unpublished `@silk-effect/llvm` dependency from that temporary `package.json`, and then install the
tarball with `pnpm add /absolute/path/to/silk-effect-llvm-0.0.0.tgz --save-exact`. Deleting the npm
declaration first prevents pnpm from trying to resolve the unpublished package before the tarball.
