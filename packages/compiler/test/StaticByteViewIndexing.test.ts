import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const moduleName = 'static-byte-view-indexing/main'

const directSource = `import silk.u8 as u8
import silk.usize as usize
pub fn main() -> i32 {
  let bytes = b"\\x99\\x13\\x1d\\x00"
  let decoy = [1, 2]
  let index = usize.add(0, 1)
  return u8.toI32(bytes[index]) + usize.toI32(bytes.length)
}`

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

    const hir = Projections.hirOf(first, moduleName)
    const expressions =
      hir?.functions.flatMap((fn) =>
        fn.statements.flatMap(Hir.statementExpressions).flatMap(Hir.expressionTree),
      ) ?? []
    const literal = expressions.find((expression) => expression._tag === 'StaticByteViewLiteral')
    assert.strictEqual(literal?._tag, 'StaticByteViewLiteral')
    if (literal?._tag === 'StaticByteViewLiteral') {
      assert.strictEqual(Type.key(literal.type), Type.key(Type.slice('Shared', 'u8')))
      assert.deepEqual(literal.data.bytes, [153, 19, 29, 0])
    }
    assert.isTrue(expressions.some((expression) => expression._tag === 'SliceIndexPlace'))
    assert.isTrue(expressions.some((expression) => expression._tag === 'SliceLength'))

    const mir = Analysis.loweredMir(first)
    assert.deepEqual(MirVerification.verify(mir), [])
    assert.strictEqual(MirEncoding.encode(mir), MirEncoding.encode(Analysis.loweredMir(second)))
    assert.deepEqual(
      mir.staticData?.map((data) => data.bytes),
      [[153, 19, 29, 0]],
    )
    const operations = mir.functions.flatMap(MirVerification.operations)
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
    const operations = MirVerification.operations(fn)
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
      MirVerification.verify(replaceFunction(mir, fnIndex, wrongRoot)).map(
        (violation) => violation.rule,
      ),
      'InvalidSliceOperation',
    )

    const aggregateRoot = rewriteOperations(fn, (operation) =>
      operation === read ? Object.freeze({ ...read, root: aggregate.destination }) : operation,
    )
    assert.include(
      MirVerification.verify(replaceFunction(mir, fnIndex, aggregateRoot)).map(
        (violation) => violation.rule,
      ),
      'InvalidSliceOperation',
    )

    const wrongIndex = rewriteOperations(fn, (operation) => {
      if (operation !== read) return operation
      return Object.freeze({
        ...read,
        selectors: Object.freeze(
          read.selectors.map((selector) => {
            if (selector._tag !== 'SliceElementSelector') return selector
            return Object.freeze({ ...selector, index: read.root })
          }),
        ),
      })
    })
    assert.include(
      MirVerification.verify(replaceFunction(mir, fnIndex, wrongIndex)).map(
        (violation) => violation.rule,
      ),
      'InvalidSliceOperation',
    )

    const wrongData = rewriteOperations(fn, (operation) =>
      operation === staticView
        ? Object.freeze({ ...staticView, length: staticView.length + 1 })
        : operation,
    )
    assert.include(
      MirVerification.verify(replaceFunction(mir, fnIndex, wrongData)).map(
        (violation) => violation.rule,
      ),
      'InvalidSliceOperation',
    )
  }),
)
