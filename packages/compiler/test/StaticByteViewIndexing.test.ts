import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const moduleName = 'static-byte-view-indexing/main'
const bytesSource = `fn sum(input: &[u8]) -> i32 {
  let mut total = 0
  let mut index = usize.add(0, 0)
  while index < input.length {
    total = total + u8.toI32(input[index])
    index = index + 1
  }
  return total
}
pub fn main() -> i32 { return sum(b"\\x99\\x13\\x1d\\x00") }`

const directSource = `pub fn main() -> i32 {
  let bytes = b"\\x99\\x13\\x1d\\x00"
  let decoy = [1, 2]
  let index = usize.add(0, 1)
  return u8.toI32(bytes[index]) + usize.toI32(bytes.length)
}`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-static-byte-view-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang: '/usr/bin/clang',
})

const replaceFunction = (module: Mir.Module, index: number, fn: Mir.MirFunction): Mir.Module =>
  Object.freeze({
    ...module,
    functions: Object.freeze([
      ...module.functions.slice(0, index),
      fn,
      ...module.functions.slice(index + 1),
    ]),
  })

const rewriteOperations = (
  fn: Mir.MirFunction,
  rewrite: (operation: Mir.Operation) => Mir.Operation,
): Mir.MirFunction =>
  Object.freeze({
    ...fn,
    regions: Object.freeze(
      fn.regions.map((region) =>
        region._tag === 'OperationRegion'
          ? Object.freeze({
              ...region,
              operations: Object.freeze(region.operations.map(rewrite)),
            })
          : region,
      ),
    ),
  })

it.effect('keeps byte literals as shared u8 slices through semantic facts, HIR, and MIR', () =>
  Effect.gen(function* () {
    const first = yield* Analysis.ofSourceRealized(moduleName, ascii(directSource))
    const second = yield* Analysis.ofSourceRealized(moduleName, ascii(directSource))
    assert.deepEqual(Analysis.diagnostics(first), [])

    const hir = Analysis.hirOf(first, moduleName)
    const expressions =
      hir?.functions.flatMap((fn) =>
        fn.statements.flatMap(Hir.statementExpressions).flatMap(Hir.expressionTree),
      ) ?? []
    const literal = expressions.find((expression) => expression._tag === 'StaticTextLiteral')
    assert.strictEqual(literal?._tag, 'StaticTextLiteral')
    if (literal?._tag === 'StaticTextLiteral') {
      assert.strictEqual(Type.key(literal.type), Type.key(Type.slice('Shared', 'u8')))
      assert.deepEqual(literal.data.bytes, [153, 19, 29, 0])
    }
    assert.isTrue(expressions.some((expression) => expression._tag === 'SliceIndexPlace'))
    assert.isTrue(expressions.some((expression) => expression._tag === 'SliceLength'))

    const mir = Analysis.loweredMir(first)
    assert.deepEqual(Mir.verify(mir), [])
    assert.strictEqual(Mir.encode(mir), Mir.encode(Analysis.loweredMir(second)))
    assert.deepEqual(
      mir.staticData?.map((data) => data.bytes),
      [[153, 19, 29, 0]],
    )
    const operations = mir.functions.flatMap(Mir.operations)
    assert.isTrue(operations.some((operation) => operation._tag === 'StaticView'))
    assert.isTrue(
      operations.some(
        (operation) =>
          operation._tag === 'ReadPlace' &&
          operation.selectors.some((selector) => selector._tag === 'SliceElementSelector'),
      ),
    )
  }),
)

it.effect('accepts canonical static selectors and rejects malformed roots, indices, and data', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(moduleName, ascii(directSource))
    const mir = Analysis.loweredMir(snapshot)
    const fnIndex = mir.functions.findIndex((fn) => fn.id.name === 'main')
    const fn = mir.functions.at(fnIndex)
    if (fn === undefined) throw new RangeError('expected static-view main MIR')
    const operations = Mir.operations(fn)
    const staticView = operations.find(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'StaticView' }> =>
        operation._tag === 'StaticView',
    )
    const read = operations.find(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'ReadPlace' }> =>
        operation._tag === 'ReadPlace' &&
        operation.selectors.some((selector) => selector._tag === 'SliceElementSelector'),
    )
    const aggregate = operations.find(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'ConstructArray' }> =>
        operation._tag === 'ConstructArray',
    )
    if (staticView === undefined || read === undefined || aggregate === undefined) {
      throw new RangeError('expected static view, aggregate, and indexed read')
    }
    const sliceSelector = read.selectors.find(
      (
        selector,
      ): selector is Extract<Mir.PlaceSelector, { readonly _tag: 'SliceElementSelector' }> =>
        selector._tag === 'SliceElementSelector',
    )
    if (sliceSelector === undefined) throw new RangeError('expected static slice selector')

    const wrongRoot = rewriteOperations(fn, (operation) =>
      operation === read ? Object.freeze({ ...read, root: sliceSelector.index }) : operation,
    )
    assert.include(
      Mir.verify(replaceFunction(mir, fnIndex, wrongRoot)).map((violation) => violation.rule),
      'InvalidSliceOperation',
    )

    const aggregateRoot = rewriteOperations(fn, (operation) =>
      operation === read ? Object.freeze({ ...read, root: aggregate.destination }) : operation,
    )
    assert.include(
      Mir.verify(replaceFunction(mir, fnIndex, aggregateRoot)).map((violation) => violation.rule),
      'InvalidSliceOperation',
    )

    const wrongIndex = rewriteOperations(fn, (operation) =>
      operation === read
        ? Object.freeze({
            ...read,
            selectors: Object.freeze(
              read.selectors.map((selector) =>
                selector._tag === 'SliceElementSelector'
                  ? Object.freeze({ ...selector, index: read.root })
                  : selector,
              ),
            ),
          })
        : operation,
    )
    assert.include(
      Mir.verify(replaceFunction(mir, fnIndex, wrongIndex)).map((violation) => violation.rule),
      'InvalidSliceOperation',
    )

    const wrongData = rewriteOperations(fn, (operation) =>
      operation === staticView
        ? Object.freeze({ ...staticView, length: staticView.length + 1 })
        : operation,
    )
    assert.include(
      Mir.verify(replaceFunction(mir, fnIndex, wrongData)).map((violation) => violation.rule),
      'InvalidSliceOperation',
    )
  }),
)

it.effect(
  'evaluates every byte, repeated reads, empty views, bounds, and provenance without allocation',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(moduleName, ascii(bytesSource))
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 201)
      assert.deepEqual(
        evaluated.trace.flatMap((event) =>
          event._tag === 'PlaceRead'
            ? event.selectors.flatMap((selector) =>
                selector._tag === 'StaticElement' ? [selector.index] : [],
              )
            : [],
        ),
        [0, 1, 2, 3],
      )
      assert.isFalse(
        evaluated.trace.some(
          (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
        ),
      )

      const repeated = yield* Analysis.ofSourceRealized(
        `${moduleName}/repeated`,
        ascii(`pub fn main() -> i32 {
  let bytes = b"\\x99"
  let index = usize.add(0, 0)
  return u8.toI32(bytes[index]) + u8.toI32(bytes[index])
}`),
      )
      const repeatedResult = Analysis.evaluate(repeated)
      assert.strictEqual(repeatedResult._tag, 'Completed')
      if (repeatedResult._tag === 'Completed') assert.strictEqual(repeatedResult.result.value, 306)

      const empty = yield* Analysis.ofSourceRealized(
        `${moduleName}/empty`,
        ascii('pub fn main() -> i32 { let bytes = b"" return usize.toI32(bytes.length) }'),
      )
      const emptyResult = Analysis.evaluate(empty)
      assert.strictEqual(emptyResult._tag, 'Completed')
      if (emptyResult._tag === 'Completed') assert.strictEqual(emptyResult.result.value, 0)

      const boundsSource = `pub fn main() -> i32 {
  let bytes = b"\\x99\\x13\\x1d\\x00"
  let index = usize.add(0, 4)
  return u8.toI32(bytes[index])
}`
      const bounds = yield* Analysis.ofSourceRealized(`${moduleName}/bounds`, ascii(boundsSource))
      const blocked = Analysis.evaluate(bounds)
      assert.strictEqual(blocked._tag, 'Blocked')
      if (blocked._tag === 'Blocked') {
        assert.strictEqual(blocked.reason._tag, 'Trap')
        if (blocked.reason._tag === 'Trap') {
          assert.include(blocked.reason.reason, 'slice index 4 is outside length 4')
          assert.strictEqual(blocked.reason.span.start, boundsSource.indexOf('bytes[index]'))
        }
      }
    }),
)

it.effect('loads immutable static storage on LLVM and Wasm and traps before overruns', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(
      moduleName,
      ascii(bytesSource),
      'aarch64-apple-darwin',
    )
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, 'constant [4 x i8]')
    assert.include(llvm.ir, 'load i8')
    assert.notInclude(llvm.ir, 'malloc')

    const wasm = yield* Analysis.ofSourceRealized(
      `${moduleName}/wasm`,
      ascii(bytesSource),
      'wasm32-unknown-unknown',
    )
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'debug' })
    assert.include(artifact.wat, '(data')
    assert.include(artifact.wat, 'i32.load8_u')
    assert.notInclude(artifact.wat, 'memory.copy')
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const wasmMain = instance.exports.silk_main
    assert.strictEqual(typeof wasmMain, 'function')
    if (typeof wasmMain === 'function') assert.strictEqual(wasmMain(), 201)

    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make(moduleName, ascii(bytesSource)) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'valid'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag === 'Compiled') {
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 201, run.stderr)
    }

    const boundsSource = `pub fn main() -> i32 {
  let bytes = b"\\x99\\x13\\x1d\\x00"
  let index = usize.add(0, 4)
  return u8.toI32(bytes[index])
}`
    const wasmBounds = yield* Analysis.ofSourceRealized(
      `${moduleName}/wasm-bounds`,
      ascii(boundsSource),
      'wasm32-unknown-unknown',
    )
    const wasmBoundsArtifact = yield* Analysis.codegenWasm(wasmBounds, { mode: 'release' })
    const wasmBoundsInstance = new WebAssembly.Instance(
      new WebAssembly.Module(wasmBoundsArtifact.bytes.slice()),
      {},
    )
    const wasmBoundsMain = wasmBoundsInstance.exports.silk_main
    assert.strictEqual(typeof wasmBoundsMain, 'function')
    if (typeof wasmBoundsMain === 'function') {
      assert.throws(() => wasmBoundsMain(), WebAssembly.RuntimeError)
    }

    const nativeBounds = yield* Driver.compile({
      compilation: { root: SourceFile.make(`${moduleName}/native-bounds`, ascii(boundsSource)) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'bounds'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(nativeBounds._tag, 'Compiled')
    if (nativeBounds._tag === 'Compiled') {
      const run = spawnSync(nativeBounds.path, [], { encoding: 'utf8' })
      assert.notStrictEqual(run.signal, null)
    }
  }),
)
