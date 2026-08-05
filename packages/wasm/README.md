# `@silk-effect/wasm`

Effect-native WebAssembly module construction with deterministic text and binary emission.

The runtime has no dependency on native WebAssembly toolchains, filesystem access, or external
executables. It returns WebAssembly text as a `string` and a binary module as a `Uint8Array`,
and every emitted module is validated against the WebAssembly specification before any bytes are
produced.

```typescript
import * as Effect from 'effect/Effect'
import * as Binary from '@silk-effect/wasm/Binary'
import * as Builder from '@silk-effect/wasm/Builder'
import * as Export from '@silk-effect/wasm/Export'
import * as Func from '@silk-effect/wasm/Func'
import * as Instr from '@silk-effect/wasm/Instr'
import * as Type from '@silk-effect/wasm/Type'
import * as ValType from '@silk-effect/wasm/ValType'
import * as WatText from '@silk-effect/wasm/WatText'

const program = Effect.gen(function* () {
  const builder = yield* Builder.make({ moduleName: 'demo' })

  // Declare add(i32, i32) -> i32 and export it.
  const signature = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
  const add = yield* Func.declare(builder, signature, { name: 'add' })
  yield* Func.define(builder, add, {
    body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
  })
  yield* Export.func(builder, 'add', add)

  return {
    text: yield* WatText.render(builder),
    bytes: yield* Binary.encode(builder),
  }
})

const output = await Effect.runPromise(program)
console.log(output.text)
const { instance } = await WebAssembly.instantiate(output.bytes)
```

The example moves through the package's main lifecycle:

1. Create one module-owning `Builder`.
2. Intern the `(i32, i32) -> i32` function type.
3. Declare `add`, then commit its body — a plain array of `Instr` values — with `Func.define`,
   which runs the specification's validation algorithm before anything is stored.
4. Export the function and emit both representations from the same committed state.

## Instructions are data

An `Instr` is a frozen plain value built without the builder. Bodies compose with ordinary array
logic, structured control flow nests as data, and entity references are opaque handles:

```typescript
import * as Instr from '@silk-effect/wasm/Instr'
import * as ValType from '@silk-effect/wasm/ValType'

const clamp = [
  Instr.localGet(0),
  Instr.i32Const(255),
  Instr.op('i32.gt_u'),
  Instr.ifElse(Instr.valueBlockType(ValType.i32), [Instr.i32Const(255)], [Instr.localGet(0)]),
]
```

Because handles resolve to numeric indices only at emission, imports may be declared at any time:
imported entities always claim the low indices of their space without invalidating existing
references.

## Errors and validation

Public operations fail with `WasmError`. Its `reason` discriminates rejected input, invalid state
or ownership, specification validation failures, and wrapped implementation failures. Function
bodies are validated when they are defined — value-stack typing, control frames, branch arities,
and polymorphic typing after unreachable code — and module-level constraints are checked at
emission, so an emitted module is guaranteed valid.

## Feature baseline

WebAssembly core 2.0 (multi-value, bulk memory, reference types, sign extension, saturating
float-to-int, mutable globals) plus tail calls, extended constant expressions, multiple
memories, fixed-width SIMD, relaxed SIMD, threads (shared memories and atomics), and memory64
(64-bit addressed memories and tables).

The package's destination surface is everything Chrome ships unflagged (Wasm 3.0 plus threads,
relaxed SIMD, and branch hinting). Exception handling, branch hinting, and GC with typed
function references arrive in planned follow-up changes. Legacy exception handling and
JS-API-only features are permanently out of scope, as are proposals below phase 4.

## Scope and compatibility

The package builds WebAssembly module state and emits `.wat` text or `.wasm` bytes. It does not
provide a runtime, interpreter, optimizer, parser for either format, or filesystem integration.

Verification is anchored to a pinned `wasm-tools` release: committed fixtures must be
byte-identical, every fixture binary must validate, and each fixture's rendered text must
assemble to the same bytes. See [UPSTREAM.md](./UPSTREAM.md) for the pin and the verification
layers.

## Development verification

```sh
pnpm --filter @silk-effect/wasm test
pnpm --filter @silk-effect/wasm parity:oracle
```

Fixture generation and oracle checks use the pinned `wasm-tools`, but runtime APIs never invoke
it.

Tests that return an Effect use `it.effect` from `@effect/vitest`; pure cases use ordinary `it`
and `assert`. Application examples may run their final program with `Effect.runPromise` at the
edge.
