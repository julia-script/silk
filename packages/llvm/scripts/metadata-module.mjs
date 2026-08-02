import * as Effect from 'effect/Effect'
import * as Bitcode from '../dist/Bitcode.js'
import * as Block from '../dist/Block.js'
import * as Builder from '../dist/Builder.js'
import * as Constant from '../dist/Constant.js'
import * as DIFlags from '../dist/DIFlags.js'
import * as DISPFlags from '../dist/DISPFlags.js'
import * as FunctionActor from '../dist/Function.js'
import * as FunctionBody from '../dist/FunctionBody.js'
import * as IrText from '../dist/IrText.js'
import * as Metadata from '../dist/Metadata.js'
import * as Type from '../dist/Type.js'
import * as Value from '../dist/Value.js'
import * as Variable from '../dist/Variable.js'

const required = (value, message) => {
  if (value === undefined) throw new Error(message)
  return value
}

const output = await Effect.runPromise(
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      strip: false,
      sourceFilename: 'metadata.c',
      dataLayout: 'e-p:64:64-i32:32-i64:64-n32:64',
      targetTriple: 'aarch64-unknown-linux',
    })
    const voidType = yield* Type.voidType(builder)
    const i1 = yield* Type.integer(builder, 1)
    const i32 = yield* Type.integer(builder, 32)
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const two = yield* Constant.integerUnsigned(builder, i32, 2)
    const three = yield* Constant.integerUnsigned(builder, i32, 3)
    const five = yield* Constant.integerUnsigned(builder, i32, 5)
    const zeroMetadata = yield* Metadata.constant(builder, zero)
    const twoMetadata = yield* Metadata.constant(builder, two)
    const threeMetadata = yield* Metadata.constant(builder, three)
    const fiveMetadata = yield* Metadata.constant(builder, five)

    const s = (value) => Metadata.string(builder, value)
    const filename = yield* s('metadata.c')
    const directory = yield* s('/src')
    const producer = yield* s('silk-effect')
    const file = required(yield* Metadata.file(builder, filename, directory), 'file')
    const boolean = required(yield* Metadata.booleanType(builder, yield* s('bool'), 1), 'bool')
    const unsigned = required(yield* Metadata.unsignedType(builder, yield* s('u32'), 32), 'u32')
    const signed = required(yield* Metadata.signedType(builder, yield* s('i32'), 32), 'i32')
    const floating = required(yield* Metadata.floatingType(builder, yield* s('f64'), 64), 'f64')

    const recursiveForward = required(yield* Metadata.forward(builder, 'type'), 'forward')
    const pointer = required(
      yield* Metadata.pointerType(builder, {
        name: yield* s('NodePtr'),
        underlyingType: recursiveForward,
        sizeInBits: 64,
        alignInBits: 64,
      }),
      'pointer',
    )
    const member = required(
      yield* Metadata.memberType(builder, {
        name: yield* s('next'),
        file,
        underlyingType: pointer,
        sizeInBits: 64,
        alignInBits: 64,
      }),
      'member',
    )
    const fields = yield* Metadata.tuple(builder, [member])
    const structure = required(
      yield* Metadata.structureType(builder, {
        name: yield* s('Node'),
        file,
        sizeInBits: 64,
        alignInBits: 64,
        fields,
      }),
      'structure',
    )
    yield* Metadata.resolveForward(builder, recursiveForward, structure)
    const union = required(
      yield* Metadata.unionType(builder, {
        name: yield* s('Number'),
        file,
        sizeInBits: 64,
        alignInBits: 64,
        fields: yield* Metadata.tuple(builder, [signed, floating]),
      }),
      'union',
    )
    const enumerator = required(
      yield* Metadata.enumerator(builder, yield* s('Two'), 2n, { unsigned: true, bitWidth: 32 }),
      'enumerator',
    )
    const enumeration = required(
      yield* Metadata.enumerationType(builder, {
        name: yield* s('Kind'),
        file,
        underlyingType: unsigned,
        sizeInBits: 32,
        alignInBits: 32,
        fields: yield* Metadata.tuple(builder, [enumerator]),
      }),
      'enumeration',
    )
    const subrange = required(yield* Metadata.subrange(builder, zeroMetadata, twoMetadata), 'range')
    const array = required(
      yield* Metadata.arrayType(builder, {
        underlyingType: signed,
        sizeInBits: 64,
        alignInBits: 32,
        fields: yield* Metadata.tuple(builder, [subrange]),
      }),
      'array',
    )
    const vector = required(
      yield* Metadata.vectorType(builder, {
        underlyingType: signed,
        sizeInBits: 128,
        alignInBits: 128,
        fields: yield* Metadata.tuple(builder, [subrange]),
      }),
      'vector',
    )
    const typedef = required(
      yield* Metadata.typedefType(builder, {
        name: yield* s('Integer'),
        file,
        underlyingType: signed,
        sizeInBits: 32,
        alignInBits: 32,
      }),
      'typedef',
    )
    const typeTuple = yield* Metadata.tuple(builder, [
      undefined,
      boolean,
      unsigned,
      signed,
      floating,
      structure,
      union,
      enumeration,
      array,
      vector,
      typedef,
    ])
    const subroutine = required(
      yield* Metadata.subroutineType(builder, typeTuple, DIFlags.make({ prototyped: true })),
      'subroutine',
    )
    const globals = yield* Metadata.tuple(builder)
    const unit = required(
      yield* Metadata.compileUnit(builder, file, producer, { optimized: true, globals }),
      'unit',
    )
    yield* Metadata.named(builder, 'llvm.dbg.cu', [unit])
    yield* Metadata.named(builder, 'llvm.module.flags', [
      yield* Metadata.tuple(builder, [twoMetadata, yield* s('Dwarf Version'), fiveMetadata]),
      yield* Metadata.tuple(builder, [twoMetadata, yield* s('Debug Info Version'), threeMetadata]),
    ])
    const subprogram = required(
      yield* Metadata.subprogram(builder, file, yield* s('debugged'), {
        line: 10,
        scopeLine: 10,
        type: subroutine,
        diFlags: DIFlags.make({ prototyped: true, allCallsDescribed: true }),
        spFlags: DISPFlags.make({ definition: true, optimized: true }),
        compileUnit: unit,
      }),
      'subprogram',
    )
    const lexical = required(
      yield* Metadata.lexicalBlock(builder, subprogram, file, 11, 1),
      'block',
    )
    const firstLocation = required(yield* Metadata.location(builder, 11, 3, lexical), 'location')
    const secondLocation = required(yield* Metadata.location(builder, 12, 1, lexical), 'location')
    const expression = required(yield* Metadata.expression(builder, [0x23, 0]), 'expression')
    const local = required(
      yield* Metadata.localVariable(builder, {
        name: yield* s('value'),
        file,
        scope: lexical,
        line: 11,
        type: signed,
      }),
      'local',
    )
    const parameter = required(
      yield* Metadata.parameter(builder, 1, {
        name: yield* s('condition'),
        file,
        scope: subprogram,
        line: 10,
        type: boolean,
      }),
      'parameter',
    )

    const global = yield* Variable.make(builder, 'counter', i32, { initializer: zero })
    const debugGlobal = required(
      yield* Metadata.globalVariable(builder, {
        name: yield* s('counter'),
        linkageName: yield* s('counter'),
        file,
        scope: unit,
        line: 3,
        type: signed,
        variable: global,
      }),
      'global',
    )
    const globalExpression = required(
      yield* Metadata.globalVariableExpression(builder, debugGlobal, expression),
      'global expression',
    )
    yield* Variable.configure(builder, global, { debugExpressions: [globalExpression] })
    const inventory = yield* Metadata.distinctTuple(builder, [
      structure,
      union,
      enumeration,
      array,
      vector,
      typedef,
      enumerator,
      subrange,
      local,
      parameter,
      debugGlobal,
      globalExpression,
      yield* Metadata.local(builder, 'local-node'),
    ])
    yield* Metadata.named(builder, 'silk.metadata.inventory', [inventory])

    const signature = yield* Type.functionType(builder, voidType, [i1])
    const fn = yield* FunctionActor.declare(builder, 'debugged', signature)
    yield* FunctionActor.setSubprogram(builder, fn, subprogram)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const yes = yield* Block.make(body, 'yes')
        const no = yield* Block.make(body, 'no')
        yield* Block.setInsertionPoint(body, entry)
        const slot = yield* FunctionBody.alloca(body, i32, 'slot')
        const alloca = required(yield* Value.instruction(body, slot), 'alloca instruction')
        const store = yield* FunctionBody.store(body, one, slot)
        const branch = yield* FunctionBody.conditionalBranch(
          body,
          yield* Value.argument(body, 0),
          yes,
          no,
          'true-likely',
        )
        yield* FunctionBody.setDebugLocation(body, alloca, firstLocation)
        yield* FunctionBody.setDebugLocation(body, store, firstLocation)
        yield* FunctionBody.setDebugLocation(body, branch, secondLocation)
        yield* Block.setInsertionPoint(body, yes)
        yield* FunctionBody.returnVoid(body)
        yield* Block.setInsertionPoint(body, no)
        yield* FunctionBody.returnVoid(body)
      }),
    )

    return process.argv[2] === 'bitcode'
      ? Buffer.from(yield* Bitcode.encode(builder))
      : Buffer.from(yield* IrText.render(builder))
  }),
)

process.stdout.write(output)
