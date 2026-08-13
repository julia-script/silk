import { execFileSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import { llvmToolchain } from '../../../test/support/llvmToolchain.js'
import * as Bitcode from '../src/Bitcode.js'
import * as Block from '../src/Block.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FunctionActor from '../src/Function.js'
import * as FunctionBody from '../src/FunctionBody.js'
import * as IrText from '../src/IrText.js'
import * as Metadata from '../src/Metadata.js'
import * as Type from '../src/Type.js'
import * as Value from '../src/Value.js'
import { raise } from './support/raise.js'

const directory = mkdtempSync(join(tmpdir(), 'silk-effect-llvm-roundtrip-'))
afterAll(() => rmSync(directory, { recursive: true, force: true }))

const toolchain = llvmToolchain(['llvm-as', 'llvm-dis', 'opt'], 'the IR round-trip check')

const required = <A>(value: A | undefined, message: string): A => value ?? raise(message)

interface Canonical {
  /**
   * The module with locals renumbered. `Bitcode.encode` writes no function-level value symbol
   * table, so local names survive only along the textual path and the two can be compared on
   * instructions and constants only once both sides are numbered.
   */
  readonly body: string
  /**
   * The module's metadata definitions, which the renumbering pass discards along with the rest of
   * the debug graph and which therefore have to be compared before it runs.
   */
  readonly metadata: string
}

const write = (name: string, contents: Uint8Array | string): string => {
  const path = join(directory, name)
  writeFileSync(path, contents)
  return path
}

const llvmAs = (name: string, source: string): Uint8Array => {
  const assembled = join(directory, name)
  execFileSync(toolchain.command('llvm-as'), [source, '-o', assembled])
  return readFileSync(assembled)
}

const llvmDis = (name: string, bitcode: Uint8Array): string => {
  const output = join(directory, name)
  execFileSync(toolchain.command('llvm-dis'), [write(`${name}.bc`, bitcode), '-o', output])
  // `llvm-dis` names the module after whichever file it read.
  return readFileSync(output, 'utf8')
    .split('\n')
    .filter((line) => !line.startsWith('; ModuleID ='))
    .join('\n')
}

/**
 * Reduces a module to the form the two emission paths can be compared in.
 *
 * Both sides go through `llvm-as` first. The textual parser resolves an omitted `align` against the
 * module's data layout as it reads, while bitcode leaves the operand unspecified for its consumer
 * to resolve, so only modules that entered through the same door are comparable.
 */
const canonical = (name: string, bitcode: Uint8Array): Canonical => {
  const materialized = llvmAs(
    `${name}.materialized.bc`,
    write(`${name}.dis.ll`, llvmDis(`${name}.dis`, bitcode)),
  )
  const stripped = join(directory, `${name}.stripped.bc`)
  execFileSync(toolchain.command('opt'), [
    '-passes=strip',
    write(`${name}.materialized.input.bc`, materialized),
    '-o',
    stripped,
  ])
  return {
    body: llvmDis(`${name}.stripped.ll`, readFileSync(stripped)),
    metadata: llvmDis(`${name}.whole.ll`, materialized)
      .split('\n')
      .filter((line) => line.startsWith('!'))
      .join('\n'),
  }
}

/**
 * Assembles rendered text and reduces it the same way.
 *
 * `llvm-as` is the arbiter the golden tests cannot be: goldens compare rendered text against
 * stored rendered text, so a systematic renderer flaw compares equal to itself forever.
 */
const assemble = (name: string, text: string): Canonical => {
  const assembled = llvmAs(`${name}.assembled.bc`, write(`${name}.source.ll`, text))
  execFileSync(toolchain.command('opt'), [
    '-passes=verify',
    write(`${name}.verify.bc`, assembled),
    '-disable-output',
  ])
  return canonical(`${name}-text`, assembled)
}

/**
 * A module built from the shapes that made rendered text unassemblable.
 *
 * Two values and a block all request the same name — values and blocks share one symbol table per
 * function, so all three collide — and every scalar and aggregate lane is zeroed through
 * `Constant.nullValue`, which is how a front end spells a zero it does not know to be a pointer.
 */
const collidingModule = Effect.fnUntraced(function* (options: { readonly named: boolean }) {
  const builder = yield* Builder.make({
    moduleName: 'roundtrip',
    sourceFilename: 'roundtrip.ll',
    targetTriple: 'aarch64-apple-darwin',
    strip: false,
  })
  const voidType = yield* Type.voidType(builder)
  const i32 = yield* Type.integer(builder, 32)
  const pointer = yield* Type.pointer(builder)
  const float = yield* Type.float(builder)
  const double = yield* Type.double(builder)
  const half = yield* Type.half(builder)
  const fp128 = yield* Type.fp128(builder)
  const x86Fp80 = yield* Type.x86Fp80(builder)
  const ppcFp128 = yield* Type.ppcFp128(builder)
  const vector = yield* Type.vector(builder, i32, 4)
  const array = yield* Type.array(builder, i32, 2)
  const structure = yield* Type.structure(builder, [i32, pointer])

  const signature = yield* Type.functionType(builder, voidType, [pointer])
  const fn = yield* FunctionActor.declare(builder, 'zeroes', signature)
  yield* FunctionActor.buildBody(
    builder,
    fn,
    Effect.fnUntraced(function* (body) {
      // The entry block takes the name two of the instructions below also ask for.
      yield* Block.make(body, 'same')
      const destination = yield* Value.argument(body, 0)
      for (const type of [
        pointer,
        i32,
        half,
        float,
        double,
        x86Fp80,
        fp128,
        ppcFp128,
        vector,
        array,
        structure,
      ]) {
        yield* FunctionBody.store(body, yield* Constant.nullValue(builder, type), destination)
      }
      // Both loads request `same`, which the entry block already holds.
      yield* FunctionBody.load(body, i32, destination, 'same')
      yield* FunctionBody.load(body, i32, destination, 'same')
      yield* FunctionBody.returnVoid(body)
      return body
    }),
  )

  const file = options.named
    ? yield* Metadata.file(
        builder,
        yield* Metadata.string(builder, 'roundtrip.silk'),
        yield* Metadata.string(builder, '/src'),
      )
    : yield* Metadata.file(builder)
  const unit = required(
    yield* Metadata.compileUnit(builder, file, yield* Metadata.string(builder, 'silk')),
    'expected a compile unit',
  )
  yield* Metadata.named(builder, 'llvm.dbg.cu', [unit])
  // Without the version flag LLVM drops debug info on load, and the verifier never sees it.
  const two = yield* Metadata.constant(builder, yield* Constant.integerUnsigned(builder, i32, 2))
  const three = yield* Metadata.constant(builder, yield* Constant.integerUnsigned(builder, i32, 3))
  yield* Metadata.named(builder, 'llvm.module.flags', [
    yield* Metadata.tuple(builder, [
      two,
      yield* Metadata.string(builder, 'Debug Info Version'),
      three,
    ]),
  ])

  return builder
})

it.effect('assembles a name-colliding, zero-constant module into the bitcode module', () =>
  Effect.gen(function* () {
    if (toolchain.unavailable()) return
    const builder = yield* collidingModule({ named: true })
    const text = yield* IrText.render(builder)

    assert.notInclude(text, 'i32 null')
    // Each of the three requests for `same` gets a distinct name in the one symbol table.
    assert.include(text, '%same = load')
    assert.include(text, '%same.1 = load')
    assert.include(text, 'same.2:')

    // The text is not merely well-formed: assembled, it is the module the bitcode path emits.
    assert.deepEqual(
      assemble('colliding-text', text),
      canonical('colliding-bitcode', yield* Bitcode.encode(builder)),
    )
  }),
)

it.effect('renders both required DIFile fields when neither operand is present', () =>
  Effect.gen(function* () {
    if (toolchain.unavailable()) return
    const builder = yield* collidingModule({ named: false })
    const text = yield* IrText.render(builder)

    assert.include(text, '!DIFile(filename: "", directory: "")')
    assert.deepEqual(
      assemble('anonymous-file-text', text),
      canonical('anonymous-file-bitcode', yield* Bitcode.encode(builder)),
    )
  }),
)
