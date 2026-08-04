# Build Tiny, a compiled language: Create the project

**Lesson 3 of 13** · [Previous: Understand LLVM's role](./02-understand-llvm.md) ·
[Next: Tokenize Tiny source](./04-tokenize-source.md)

In this lesson, we will create a standalone TypeScript project that consumes
`@silk-effect/llvm` exactly as an npm package. We will finish by rendering an empty LLVM module—a
small but real result that confirms the toolchain is ready before we write a lexer.

## Check the starting tools

Create an empty directory named `tiny-language`, open a terminal in it, and run:

```sh
node --version
pnpm --version
clang --version
```

Use Node.js 22.13 or newer, pnpm 11, and LLVM/Clang 22. The first line of the Clang output should
identify version 22. We will not invoke Clang until Lesson 7, but checking it now prevents a native
toolchain surprise later.

If `clang` is not found, install LLVM 22 for your operating system and repeat the command before
continuing.

## Initialize the consumer project

Run these commands inside the empty directory:

```sh
pnpm init
pnpm add effect@4.0.0-beta.102 @silk-effect/llvm@^0.1.0
pnpm add -D typescript@7.0.2 vitest@4.1.10 @effect/vitest@4.0.0-beta.102 @types/node@^22.13.0
```

Set `type` and the three scripts in `package.json` so it contains:

```json
{
  "name": "tiny-language-tutorial",
  "version": "0.0.0",
  "private": true,
  "type": "module",
  "engines": {
    "node": ">=22.13.0"
  },
  "scripts": {
    "smoke": "node --experimental-strip-types src/Cli.ts",
    "test": "vitest run --passWithNoTests",
    "typecheck": "tsc -p tsconfig.json --noEmit"
  },
  "dependencies": {
    "@silk-effect/llvm": "^0.1.0",
    "effect": "4.0.0-beta.102"
  },
  "devDependencies": {
    "@effect/vitest": "4.0.0-beta.102",
    "@types/node": "^22.13.0",
    "typescript": "7.0.2",
    "vitest": "4.1.10"
  }
}
```

<details>
<summary>Repository maintainers: test before npm publication</summary>

Build and pack the local package, then copy this project into a clean temporary directory. Delete
the `@silk-effect/llvm` entry from the temporary `package.json` before running
`pnpm add /absolute/path/to/silk-effect-llvm-0.0.0.tgz --save-exact`. Removing the npm declaration
first prevents pnpm from resolving the unpublished package before it considers the tarball. The
tutorial's learner path remains the npm commands above; this substitution only verifies that the
unpublished package exposes the same public surface.

</details>

Create `tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2023",
    "lib": ["ES2023"],
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "moduleDetection": "force",
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "verbatimModuleSyntax": true,
    "isolatedModules": true,
    "noEmitOnError": true,
    "forceConsistentCasingInFileNames": true,
    "skipLibCheck": true,
    "types": ["node"]
  },
  "include": ["src/**/*.ts", "test/**/*.ts"]
}
```

This configuration keeps the example strict and lets Node execute its transform-free TypeScript
directly. We will use interfaces and string-literal unions rather than TypeScript syntax that
requires code generation.

## Render an empty LLVM module

Create `src/Cli.ts`:

```typescript
import * as Builder from '@silk-effect/llvm/Builder'
import * as IrText from '@silk-effect/llvm/IrText'
import * as Effect from 'effect/Effect'

const program = Effect.gen(function* () {
  const builder = yield* Builder.make({
    moduleName: 'tiny-language',
    sourceFilename: 'empty.tiny',
  })

  return yield* IrText.render(builder)
})

console.log(await Effect.runPromise(program))
```

Notice the import boundary: the example imports public actor subpaths, not the package root and not
anything under `src` or `internal`. `Effect.runPromise` appears once at the application edge; the
compiler operations themselves remain Effect values.

Run the checks:

```sh
pnpm typecheck
pnpm smoke
```

The second command should print:

```llvm
; ModuleID = 'tiny-language'
source_filename = "empty.tiny"
```

You now own an empty, valid LLVM module. `Builder.make` created it, and `IrText.render` converted
its current snapshot into readable text. In the next lessons, the lexer and parser will decide
what belongs in that module.

## Recover from setup problems

**pnpm cannot find `@silk-effect/llvm`.** The package has not been published yet if you are reading
the repository version of this lesson. Use the maintainer tarball procedure above; do not change
the imports.

**An import mentions `src`, `dist/internal`, or a relative path into this repository.** Replace it
with the public subpath shown in the example. A consumer project must not know the package's source
layout.

**Node reports unsupported TypeScript syntax.** Confirm that Node is at least 22.13 and that your
files use transform-free TypeScript. Run `pnpm typecheck` first; do not add a second runtime just to
bypass a type error.

**`clang --version` fails.** Install LLVM/Clang 22 and make its `bin` directory available on `PATH`.
The smoke program does not require Clang, but Lesson 7's executable checkpoint will.

[Previous: Understand LLVM's role](./02-understand-llvm.md) ·
[Next: Tokenize Tiny source](./04-tokenize-source.md)
