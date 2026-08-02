import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Block from '../src/Block.js'
import * as Builder from '../src/Builder.js'
import * as DIFlags from '../src/DIFlags.js'
import * as DISPFlags from '../src/DISPFlags.js'
import * as FunctionActor from '../src/Function.js'
import * as FunctionBody from '../src/FunctionBody.js'
import * as IrText from '../src/IrText.js'
import * as Metadata from '../src/Metadata.js'
import { SilkError } from '../src/SilkError.js'
import * as Type from '../src/Type.js'
import * as Value from '../src/Value.js'
import { raise } from './support/raise.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)
const required = <A>(value: A | undefined, message: string): A => value ?? raise(message)

test(
  'preserves metadata string bytes, uniqued identity, distinct identity, and builder ownership',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make({ strip: false })
    const other = yield* Builder.make({ strip: false })
    const first = yield* Metadata.string(builder, new Uint8Array([0x61, 0, 0xff]))
    const second = yield* Metadata.string(builder, new Uint8Array([0x61, 0, 0xff]))
    expect(first).toBe(second)
    const tuple = yield* Metadata.tuple(builder, [first])
    expect(yield* Metadata.tuple(builder, [first])).toBe(tuple)
    expect(yield* Metadata.distinctTuple(builder, [first])).not.toBe(
      yield* Metadata.distinctTuple(builder, [first]),
    )
    expect(yield* Metadata.emptyTuple(builder)).toBe(yield* Metadata.emptyTuple(builder))
    expect(yield* Metadata.inspect(builder, first)).toMatchObject({
      _tag: 'String',
      value: { bytes: [0x61, 0, 0xff] },
    })
    expect(yield* Effect.flip(Metadata.tuple(other, [first]))).toBeInstanceOf(SilkError)
    const diFlags = DIFlags.make({
      visibility: 'public',
      inheritance: 'virtual',
      forwardDeclaration: true,
      allCallsDescribed: true,
    })
    expect(DIFlags.toBitcode(diFlags)).toBe(3 | (1 << 2) | (3 << 16) | (1 << 29))
    expect(DIFlags.toText(diFlags)).toEqual([
      'DIFlagPublic',
      'DIFlagFwdDecl',
      'DIFlagVirtualInheritance',
      'DIFlagAllCallsDescribed',
    ])
    const spFlags = DISPFlags.make({ virtuality: 'pureVirtual', definition: true, optimized: true })
    expect(DISPFlags.toBitcode(spFlags)).toBe(2 | (1 << 3) | (1 << 4))
    expect(DISPFlags.toText(spFlags)).toEqual([
      'DISPFlagPureVirtual',
      'DISPFlagDefinition',
      'DISPFlagOptimized',
    ])
  }, TestRuntime.runPromise),
)

test(
  'resolves recursive metadata graphs once and rejects reachable unresolved references',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make({ strip: false })
    const forward = required(yield* Metadata.forward(builder, 'node'), 'expected forward')
    const recursive = yield* Metadata.tuple(builder, [forward])
    yield* Metadata.resolveForward(builder, forward, recursive)
    yield* Metadata.named(builder, 'recursive', [recursive])
    expect(yield* IrText.render(builder)).toContain('!recursive = !{!0}')
    expect(yield* Effect.flip(Metadata.resolveForward(builder, forward, recursive))).toBeInstanceOf(
      SilkError,
    )

    const unreachable = required(yield* Metadata.forward(builder), 'expected unreachable forward')
    expect(unreachable).toBeDefined()
    yield* IrText.render(builder)

    const leftForward = required(yield* Metadata.forward(builder), 'expected left forward')
    const rightForward = required(yield* Metadata.forward(builder), 'expected right forward')
    const left = yield* Metadata.tuple(builder, [rightForward])
    const right = yield* Metadata.tuple(builder, [leftForward])
    yield* Metadata.resolveForward(builder, leftForward, left)
    yield* Metadata.resolveForward(builder, rightForward, right)
    yield* Metadata.named(builder, 'mutual', [left])
    expect(yield* IrText.render(builder)).toContain('!mutual = !{')

    const unresolved = required(yield* Metadata.forward(builder), 'expected unresolved forward')
    yield* Metadata.named(builder, 'broken', [unresolved])
    const failure = yield* Effect.flip(IrText.render(builder))
    expect(failure.message).toContain('unresolved forward references')

    const typed = required(yield* Metadata.forward(builder, 'type'), 'expected typed forward')
    const filename = yield* Metadata.string(builder, 'source.c')
    const file = required(yield* Metadata.file(builder, filename), 'expected file')
    expect(yield* Effect.flip(Metadata.resolveForward(builder, typed, file))).toBeInstanceOf(
      SilkError,
    )
  }, TestRuntime.runPromise),
)

test(
  'renders recursive debug nodes, flags, attachments, and changing instruction locations',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make({ strip: false, sourceFilename: 'source.c' })
    const voidType = yield* Type.voidType(builder)
    const i32 = yield* Type.integer(builder, 32)
    const filename = yield* Metadata.string(builder, 'source.c')
    const directory = yield* Metadata.string(builder, '/src')
    const producer = yield* Metadata.string(builder, 'silk-effect')
    const intName = yield* Metadata.string(builder, 'int')
    const functionName = yield* Metadata.string(builder, 'debugged')
    const file = required(yield* Metadata.file(builder, filename, directory), 'expected file')
    const intType = required(
      yield* Metadata.signedType(builder, intName, 32),
      'expected basic type',
    )
    const recursiveForward = required(
      yield* Metadata.forward(builder, 'type'),
      'expected type forward',
    )
    const pointer = required(
      yield* Metadata.pointerType(builder, {
        underlyingType: recursiveForward,
        sizeInBits: 64,
        alignInBits: 64,
      }),
      'expected pointer type',
    )
    const fields = yield* Metadata.tuple(builder, [pointer])
    const structure = required(
      yield* Metadata.structureType(builder, {
        name: yield* Metadata.string(builder, 'Node'),
        file,
        sizeInBits: 64,
        alignInBits: 64,
        fields,
      }),
      'expected structure type',
    )
    yield* Metadata.resolveForward(builder, recursiveForward, structure)
    const types = yield* Metadata.tuple(builder, [undefined, intType, structure])
    const signature = required(
      yield* Metadata.subroutineType(builder, types, DIFlags.make({ prototyped: true })),
      'expected debug signature',
    )
    const globals = yield* Metadata.tuple(builder)
    const unit = required(
      yield* Metadata.compileUnit(builder, file, producer, { globals }),
      'expected compile unit',
    )
    yield* Metadata.named(builder, 'llvm.dbg.cu', [unit])
    const subprogram = required(
      yield* Metadata.subprogram(builder, file, functionName, {
        line: 7,
        scopeLine: 7,
        type: signature,
        diFlags: DIFlags.make({ prototyped: true, allCallsDescribed: true }),
        spFlags: DISPFlags.make({ definition: true }),
        compileUnit: unit,
      }),
      'expected subprogram',
    )
    const location = required(
      yield* Metadata.location(builder, 8, 3, subprogram),
      'expected location',
    )
    const nextLocation = required(
      yield* Metadata.location(builder, 9, 1, subprogram),
      'expected next location',
    )
    const functionType = yield* Type.functionType(builder, voidType, [i32])
    const fn = yield* FunctionActor.declare(builder, 'debugged', functionType)
    yield* FunctionActor.setSubprogram(builder, fn, subprogram)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const local = yield* FunctionBody.alloca(body, i32, 'local')
        const alloca = required(yield* Value.instruction(body, local), 'expected alloca')
        const argument = yield* Value.argument(body, 0)
        const store = yield* FunctionBody.store(body, argument, local)
        const returned = yield* FunctionBody.returnVoid(body)
        yield* FunctionBody.setDebugLocation(body, alloca, location)
        yield* FunctionBody.setDebugLocation(body, store, location)
        yield* FunctionBody.setDebugLocation(body, returned, nextLocation)
      }),
    )

    const ir = yield* IrText.render(builder)
    expect(ir).toContain('!DICompileUnit(')
    expect(ir).toContain('distinct !DISubprogram(')
    expect(ir).toContain('!DICompositeType(tag: DW_TAG_structure_type')
    expect(ir.match(/!dbg !/g)?.length).toBe(4)
    expect(ir).toContain('DIFlagAllCallsDescribed')
  }, TestRuntime.runPromise),
)

test(
  'strip mode omits debug allocation, attachments, and named metadata',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make({ strip: true })
    const filename = yield* Metadata.string(builder, 'stripped.c')
    expect(yield* Metadata.file(builder, filename)).toBeUndefined()
    expect(yield* Metadata.forward(builder, 'type')).toBeUndefined()
    yield* Metadata.named(builder, 'llvm.dbg.cu', [])
    expect(yield* IrText.render(builder)).not.toContain('!llvm.dbg.cu')

    const build = Effect.fnUntraced(function* (strip: boolean) {
      const current = yield* Builder.make({ strip })
      const voidType = yield* Type.voidType(current)
      const signature = yield* Type.functionType(current, voidType, [])
      const fn = yield* FunctionActor.declare(current, 'same_program', signature)
      const name = yield* Metadata.string(current, 'same_program')
      const source = required(
        (yield* Metadata.file(current, yield* Metadata.string(current, 'same.c'))) ?? name,
        'expected debug source',
      )
      const subprogram = yield* Metadata.subprogram(current, source, name, {
        spFlags: DISPFlags.make({ definition: true }),
      })
      yield* FunctionActor.setSubprogram(current, fn, subprogram)
      yield* FunctionActor.buildBody(
        current,
        fn,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'entry')
          const returned = yield* FunctionBody.returnVoid(body)
          if (subprogram !== undefined) {
            yield* FunctionBody.setDebugLocation(
              body,
              returned,
              yield* Metadata.location(current, 1, 1, subprogram),
            )
          }
        }),
      )
      return yield* IrText.render(current)
    })
    const stripped = yield* build(true)
    const preserving = yield* build(false)
    const executable = (ir: string): string =>
      ir
        .split('\n')
        .filter((line) => !line.startsWith('!'))
        .join('\n')
        .replaceAll(/, !dbg !\d+/g, '')
        .replaceAll(/ !dbg !\d+/g, '')
        .trimEnd()
    expect(executable(stripped)).toBe(executable(preserving))
  }, TestRuntime.runPromise),
)
