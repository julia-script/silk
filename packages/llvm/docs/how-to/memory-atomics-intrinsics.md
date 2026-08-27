# How to emit memory, atomic, and intrinsic operations

This guide shows you how to emit aligned memory access, an atomic read-modify-write operation, and
an LLVM memory intrinsic. It assumes familiarity with LLVM pointers and atomic orderings.

```typescript
import * as Effect from 'effect/Effect'
import { pipe } from 'effect/Function'
import * as Alignment from '@silklang/llvm/Alignment'
import * as Block from '@silklang/llvm/Block'
import * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Intrinsic from '@silklang/llvm/Intrinsic'
import * as MemoryAccess from '@silklang/llvm/MemoryAccess'
import * as Type from '@silklang/llvm/Type'
import * as Value from '@silklang/llvm/Value'

const program = Effect.gen(function* () {
  const builder = yield* Builder.make()
  const voidType = yield* Type.voidType(builder)
  const i8 = yield* Type.integer(builder, 8)
  const i32 = yield* Type.integer(builder, 32)
  const i64 = yield* Type.integer(builder, 64)
  const pointer = yield* Type.pointer(builder)
  const signature = yield* Type.functionType(builder, voidType, [pointer, pointer, i32])
  const fn = yield* FunctionActor.declare(builder, 'update', signature)
  const alignment = yield* Alignment.fromByteUnits(4)
  const volatileRead = pipe(
    MemoryAccess.make({ alignment }),
    MemoryAccess.withVolatile(),
  )
  const monotonicUpdate = pipe(
    MemoryAccess.make({ alignment }),
    MemoryAccess.withAtomic('monotonic'),
  )

  yield* FunctionActor.buildBody(
    builder,
    fn,
    Effect.fn('Guide.updateBody')(function* (body) {
      yield* Block.make(body, 'entry')
      const destination = yield* Value.argument(body, 0)
      const source = yield* Value.argument(body, 1)
      const increment = yield* Value.argument(body, 2)

      // Perform an ordinary volatile read, calculation, and aligned write.
      const loaded = yield* FunctionBody.load(body, i32, destination, 'loaded', volatileRead)
      const sum = yield* FunctionBody.binary(body, 'add', loaded, increment, 'sum')
      yield* FunctionBody.store(body, sum, destination, { alignment })

      // Perform a separate atomic update on the same opaque pointer.
      yield* FunctionBody.atomicRmw(
        body,
        'add',
        destination,
        increment,
        'previous',
        monotonicUpdate,
      )

      // Invoke LLVM's overloaded memcpy intrinsic for a four-byte copy.
      const length = yield* Constant.integerUnsigned(builder, i64, 4)
      yield* Intrinsic.memcpy(body, destination, source, length)
      yield* FunctionBody.store(
        body,
        yield* Constant.integerUnsigned(builder, i8, 0),
        destination,
      )

      // Every basic block ends with exactly one terminator.
      yield* FunctionBody.returnVoid(body)
    }),
  )
})

await Effect.runPromise(program)
```

## What each operation contributes

The function receives two opaque pointers and one `i32` increment:

1. `load` reads an `i32` from `destination`. The operation is marked `volatile` and aligned to four
   bytes, so both properties appear on the emitted instruction.
2. `binary` computes the replacement value, and `store` writes it back with the same alignment.
   These are ordinary, non-atomic operations.
3. `atomicRmw` performs a separate atomic addition with `monotonic` ordering. Its result,
   `previous`, represents the value observed before the atomic update; this example does not need
   to consume it.
4. `Intrinsic.memcpy` copies four bytes from `source` to `destination`. The length is an `i64`
   constant because that choice participates in the intrinsic's overloaded LLVM name.
5. The final `i8` store writes one zero byte, then `returnVoid` terminates the only block. Opaque
   pointers allow the same address operand to be used with the explicitly supplied value type.

Use `Alignment.fromByteUnits` for byte alignments; it rejects zero and non-power-of-two inputs.
Memory options are immutable inputs, so the same alignment can be reused across operations.
`MemoryAccess.withVolatile` and `withAtomic` accept both data-first and pipeable forms; the example
uses `pipe` to keep successive transformations left-to-right.

Atomic validation happens before an instruction is appended. Loads reject `release` and
`acq_rel`; stores reject `acquire` and `acq_rel`; compare-exchange rejects a failure ordering that
is stronger than its success ordering. The enclosing function-body transaction remains unchanged
when validation fails.

`Intrinsic.resolve` covers the complete pinned intrinsic inventory when you need a lower-level
declaration. The named helpers such as `memcpy`, `memset`, and `assumeCold` handle their canonical
signatures directly.
