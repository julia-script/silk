import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import type * as Instances from '../src/Instances.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const source = `fn view(bytes: &[u8]) -> string {
  unsafe { return Intrinsic.stringFromUtf8Unchecked(bytes) }
  return ""
}
pub fn compare(bytes: &[u8], expected: string) -> bool {
  let actual = view(bytes)
  let raw = Intrinsic.stringUtf8Bytes(actual)
  let length = Intrinsic.stringByteLength(actual)
  return Intrinsic.stringEqualsExact(actual, expected)
}`

it('publishes the minimal portable string intrinsic catalog with exact signatures', () => {
  const operations = [
    'stringFromUtf8Unchecked',
    'stringUtf8Bytes',
    'stringByteLength',
    'stringEqualsExact',
  ].map((name) => Intrinsic.findOperation('Intrinsic', name))
  assert.isTrue(operations.every((operation) => operation !== undefined))
  assert.deepEqual(
    operations.map((operation) =>
      operation === undefined
        ? undefined
        : {
            signature: Intrinsic.signature(operation),
            unsafe: operation.unsafe,
            availability: operation.availability,
            returnedBorrowParameter: operation.returnedBorrowParameter,
          },
    ),
    [
      {
        signature: 'unsafe fn Intrinsic.stringFromUtf8Unchecked(bytes: &[u8]) -> string',
        unsafe: true,
        availability: { _tag: 'Executable', targets: ['Evaluator', 'LLVM', 'Wasm'] },
        returnedBorrowParameter: 0,
      },
      {
        signature: 'fn Intrinsic.stringUtf8Bytes(value: string) -> &[u8]',
        unsafe: false,
        availability: { _tag: 'Executable', targets: ['Evaluator', 'LLVM', 'Wasm'] },
        returnedBorrowParameter: 0,
      },
      {
        signature: 'fn Intrinsic.stringByteLength(value: string) -> usize',
        unsafe: false,
        availability: { _tag: 'Executable', targets: ['Evaluator', 'LLVM', 'Wasm'] },
        returnedBorrowParameter: undefined,
      },
      {
        signature: 'fn Intrinsic.stringEqualsExact(left: string, right: string) -> bool',
        unsafe: false,
        availability: { _tag: 'Executable', targets: ['Evaluator', 'LLVM', 'Wasm'] },
        returnedBorrowParameter: undefined,
      },
    ],
  )
  const inventory = Intrinsic.inventory().filter((entry) =>
    entry.operation.startsWith('Intrinsic.string'),
  )
  assert.deepEqual(
    inventory.map((entry) => entry.operation),
    [
      'Intrinsic.stringFromUtf8Unchecked',
      'Intrinsic.stringUtf8Bytes',
      'Intrinsic.stringByteLength',
      'Intrinsic.stringEqualsExact',
    ],
  )
  assert.isTrue(
    inventory.every(
      (entry) =>
        entry.admission === 'Representation' &&
        entry.consumer.startsWith('silk/string.') &&
        !/alloc|normal|travers|grapheme|owned|stringString/i.test(entry.operation),
    ),
  )
})

it.effect('retains runtime string identity and provenance in HIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('string/intrinsics', encoder.encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const hir = Analysis.hirOf(snapshot, 'string/intrinsics')
    assert.isDefined(hir)
    if (hir === undefined) return

    const expressions = hir.functions.flatMap((fn) =>
      fn.statements.flatMap(Hir.statementExpressions).flatMap(Hir.expressionTree),
    )
    const runtimeView = expressions.find((expression) => expression._tag === 'RuntimeStringView')
    assert.strictEqual(runtimeView?._tag, 'RuntimeStringView')
    if (runtimeView?._tag === 'RuntimeStringView') {
      assert.isTrue(Type.isString(runtimeView.type))
      assert.strictEqual(runtimeView.source._tag, 'ParameterReference')
      assert.deepEqual(runtimeView.heldLoans, [])
    }
    assert.deepEqual(
      expressions.flatMap((expression) =>
        expression._tag === 'BuiltinCall' &&
        (expression.operation === 'StringUtf8Bytes' || expression.operation === 'StringByteLength')
          ? [expression.operation]
          : [],
      ),
      ['StringUtf8Bytes', 'StringByteLength'],
    )
    assert.isTrue(
      expressions.some((expression) => expression._tag === 'StringEquality' && !expression.negated),
    )
    assert.include(Hir.encode(hir), 'runtime-string-view loans=none : string')
    assert.deepEqual(Hir.verify(hir), [])

    const formation = Intrinsic.findOperation('Intrinsic', 'stringFromUtf8Unchecked')
    if (formation === undefined || runtimeView === undefined) {
      throw new Error('expected runtime string formation metadata')
    }
    const call: Instances.IntrinsicCall = Object.freeze({
      _tag: 'ReachableIntrinsicCall',
      operation: formation.id,
      span: runtimeView.span,
      origins: Object.freeze([runtimeView.span]),
    })
    assert.strictEqual(IntrinsicAvailability.select([call], 'Wasm')._tag, 'Available')
    const restrictedTargets: ReadonlyArray<Intrinsic.ExecutionTarget> = Object.freeze([
      'Evaluator',
      'LLVM',
    ])
    const restricted: Intrinsic.Operation = Object.freeze({
      ...formation,
      availability: Object.freeze({ _tag: 'Executable', targets: restrictedTargets }),
    })
    const catalog = Intrinsic.all()
      .flatMap((actor) => actor.operations)
      .map((operation) =>
        Intrinsic.operationText(operation.id) === Intrinsic.operationText(formation.id)
          ? restricted
          : operation,
      )
    const unavailable = IntrinsicAvailability.select([call], 'Wasm', catalog)
    assert.strictEqual(unavailable._tag, 'Unavailable')
    if (unavailable._tag === 'Unavailable') {
      assert.deepEqual(
        unavailable.diagnostics.map((diagnostic) => diagnostic.code),
        ['SEM0093'],
      )
    }
  }),
)

it.effect('routes string equality operators through exact equality only', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'string/operators',
      encoder.encode(`fn equal(left: string, right: string) -> bool { return left == right }
fn different(left: string, right: string) -> bool { return left != right }
fn scalar() -> bool { return 1 == 1 }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const hir = Analysis.hirOf(snapshot, 'string/operators')
    assert.isDefined(hir)
    if (hir !== undefined) {
      const expressions = hir.functions.flatMap((fn) =>
        fn.statements.flatMap(Hir.statementExpressions).flatMap(Hir.expressionTree),
      )
      const equality = expressions.filter((expression) => expression._tag === 'StringEquality')
      assert.deepEqual(
        equality.map((expression) => expression.negated),
        [false, true],
      )
      assert.isTrue(
        equality.every(
          (expression) =>
            Intrinsic.operationText(expression.intrinsic) === 'Intrinsic.stringEqualsExact',
        ),
      )
      assert.isTrue(
        expressions.some(
          (expression) => expression._tag === 'BuiltinCall' && expression.operation === 'Equals',
        ),
      )
      assert.include(Hir.encode(hir), 'string-not-equals intrinsic=Intrinsic.stringEqualsExact')
      assert.deepEqual(Hir.verify(hir), [])
    }

    const mixed = yield* Analysis.ofSource(
      'string/mixed-operator',
      encoder.encode('fn mixed(left: string, right: &[u8]) -> bool { return left == right }'),
    )
    assert.deepEqual(
      Analysis.diagnostics(mixed).map((diagnostic) => diagnostic.code),
      ['SEM0012'],
    )
  }),
)

it.effect('rejects unchecked UTF-8 formation outside an unsafe boundary', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'string/unsafe',
      encoder.encode(
        'fn view(bytes: &[u8]) -> string { return Intrinsic.stringFromUtf8Unchecked(bytes) }',
      ),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        reason: diagnostic.reason._tag,
      })),
      [{ code: 'SEM0082', reason: 'MissingUnsafeBoundary' }],
    )
  }),
)
