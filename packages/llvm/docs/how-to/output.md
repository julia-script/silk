# How to emit and validate LLVM output

This guide shows you how to obtain deterministic LLVM assembly and bitcode from one builder. It
assumes the module has already been populated and validated.

```typescript
import * as Effect from 'effect/Effect'
import * as Bitcode from '@silk-lang/llvm/Bitcode'
import * as Builder from '@silk-lang/llvm/Builder'
import * as IrText from '@silk-lang/llvm/IrText'

const program = Effect.gen(function* () {
  const builder = yield* Builder.make({
    moduleName: 'output-example',
    sourceFilename: 'output.ll',
    targetTriple: 'wasm32-unknown-unknown',
  })

  // Render once for humans and encode twice to demonstrate repeatability.
  const text = yield* IrText.render(builder)
  const first = yield* Bitcode.encode(builder)
  const second = yield* Bitcode.encode(builder)

  return {
    text,
    bytes: first,
    deterministic: first.length === second.length && first.every((byte, index) => byte === second[index]),
  }
})

const output = await Effect.runPromise(program)
```

## Read the result

The three returned fields answer different questions:

- `text` is the human-readable module snapshot produced by `IrText.render`.
- `bytes` is the first encoded `Uint8Array`, ready for an application boundary to persist or send.
- `deterministic` compares a second encoding byte-for-byte with the first. Re-encoding is safe
  because `Bitcode.encode` does not consume or mutate the builder.

`output.text` is LLVM assembly, `output.bytes` is a `Uint8Array`, and `output.deterministic` is
`true`. The first four byte values are `66, 67, 192, 222` (`BC C0 DE`). Rendering and encoding take
a snapshot and do not consume or mutate the builder.

To compare repository fixtures with the pinned Zig and LLVM tools, run:

```sh
pnpm --filter @silk-lang/llvm fixtures:verify
pnpm --filter @silk-lang/llvm parity:validate
```

Runtime consumers do not need those external tools. Refer to
[`UPSTREAM.md`](../../UPSTREAM.md) for the pinned revisions and
[`parity/REPORT.md`](../../parity/REPORT.md) for the supported upstream surface.
