import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'
import { elaborate } from './support/elaborate.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (source: string): Elaboration.Result =>
  elaborate(Parser.parse(Lexer.lex(SourceFile.make('slices/Semantics', ascii(source)))))

const returnedCall = (
  result: Elaboration.Result,
  ordinal: number,
): Extract<Elaboration.ExpressionFact, { readonly _tag: 'Call' }> => {
  const expression = result.functions.at(ordinal)?.returnedExpression
  if (expression?._tag !== 'Call') throw new RangeError(`expected call in function ${ordinal}`)
  return expression
}

it('forms shared and exclusive whole-array borrows without encoding source length', () => {
  const result = analyze(`fn scan<T>(values: &[T]) -> i32 { return 1 }
fn edit(values: &mut [i32]) -> i32 { return 2 }
fn short() -> i32 { let values = [10, 20, 30] return scan(&values) }
fn longRead() -> i32 { let values = [1, 2, 3, 4, 5, 6] return scan(&values) }
fn long() -> i32 { let mut values = [1, 2, 3, 4, 5, 6] return edit(&mut values) }
pub fn main() -> i32 { return short() }`)

  assert.deepEqual(result.diagnostics, [])
  const shared = returnedCall(result, 2).arguments.at(0)?.expression
  const sharedLong = returnedCall(result, 3).arguments.at(0)?.expression
  const exclusive = returnedCall(result, 4).arguments.at(0)?.expression
  assert.strictEqual(shared?._tag, 'Borrow')
  assert.strictEqual(exclusive?._tag, 'Borrow')
  if (shared?._tag === 'Borrow' && shared.formation._tag === 'FixedArrayBorrow') {
    assert.strictEqual(shared.access, 'Shared')
    assert.strictEqual(shared.formation.array.length, 3)
    assert.strictEqual(shared.type._tag === 'Available' && Type.isSlice(shared.type.type), true)
  }
  if (
    shared?._tag === 'Borrow' &&
    shared.type._tag === 'Available' &&
    sharedLong?._tag === 'Borrow' &&
    sharedLong.type._tag === 'Available'
  ) {
    assert.strictEqual(Type.key(shared.type.type), Type.key(sharedLong.type.type))
  }
  if (exclusive?._tag === 'Borrow' && exclusive.formation._tag === 'FixedArrayBorrow') {
    assert.strictEqual(exclusive.access, 'Exclusive')
    assert.strictEqual(exclusive.formation.array.length, 6)
  }
  const hirCall = result.hir.functions.at(2)
  const returned = hirCall === undefined ? undefined : Hir.returned(hirCall)
  assert.strictEqual(returned?._tag, 'Call')
  if (returned?._tag === 'Call') {
    assert.strictEqual(returned.arguments.at(0)?._tag, 'SliceBorrow')
    assert.deepEqual(
      returned.loanEnds.map((loan) => loan.ordinal),
      [0],
    )
  }
  assert.deepEqual(Hir.verify(result.hir), [])
})

it('retains compatible reborrows and rejects access strengthening', () => {
  const result = analyze(`fn read(values: &[i32]) -> i32 { return 1 }
fn edit(values: &mut [i32]) -> i32 { return 2 }
fn share(values: &mut [i32]) -> i32 { return read(&values) }
fn forward(values: &mut [i32]) -> i32 { return edit(&mut values) }
fn strengthen(values: &[i32]) -> i32 { return edit(&mut values) }
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0058'],
  )
  const shared = returnedCall(result, 2).arguments.at(0)?.expression
  const exclusive = returnedCall(result, 3).arguments.at(0)?.expression
  assert.strictEqual(
    shared?._tag === 'Borrow' && shared.formation._tag === 'SliceReborrow'
      ? shared.formation.suspendsParent
      : undefined,
    true,
  )
  assert.strictEqual(
    exclusive?._tag === 'Borrow' && exclusive.formation._tag === 'SliceReborrow'
      ? exclusive.formation.suspendsParent
      : undefined,
    true,
  )
})

it('admits one-source ordinary returned views and rejects ambiguous or strengthened headers', () => {
  const result = analyze(`fn identity(values: &[i32]) -> &[i32] { return values }
fn share(values: &mut [i32]) -> &[i32] { return &values }
fn ambiguous(left: &[i32], right: &[i32]) -> &[i32] { return left }
fn strengthen(values: &[i32]) -> &mut [i32] { return &mut values }
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0091', 'SEM0091', 'SEM0055'],
  )
  assert.strictEqual(result.functions.at(0)?.returnedBorrow?.parameter.id.ordinal, 0)
  assert.strictEqual(result.functions.at(1)?.returnedBorrow?.access, 'Shared')
})

it('rejects a returned view whose body does not preserve the declared source', () => {
  const result = analyze(`fn invalid(values: &[i32]) -> &[i32] {
  let local = [1, 2]
  return &local
}
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0092'],
  )
})

it('keeps returned views out of effects, services, captures, and owned storage', () => {
  const result = analyze(`service Reader {
  fn read(values: &[i32]) -> &[i32]
}
effect fn delayed(values: &[i32]) -> &[i32] { return values }
fn captured(values: &[i32]) -> once fn() -> &[i32] {
  return fn() -> &[i32] { return values }
}
struct Stored { view: &[i32] }
fn array(values: &[i32]) -> i32 {
  let stored = [values]
  return 0
}
pub fn main() -> i32 { return 0 }`)

  const codes = result.diagnostics.map((diagnostic) => diagnostic.code)
  assert.isAtLeast(codes.filter((code) => code === 'SEM0091').length, 2)
  assert.include(codes, 'SEM0054')
})

it('admits lexical and temporary owners while rejecting decay, invalid exclusivity, and unstable subplaces', () => {
  const result = analyze(`fn read(values: &[i32]) -> i32 { return 1 }
fn edit(values: &mut [i32]) -> i32 { return 2 }
fn decay() -> i32 { let values = [1, 2] return read(values) }
fn immutable() -> i32 { let values = [1, 2] return edit(&mut values) }
fn local() -> i32 { let values = [1, 2] let view = &values return 0 }
fn temporary() -> i32 { return read(&[1, 2]) }
fn subplace() -> i32 { let values = [[1, 2]] return read(&values[0]) }
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0059', 'SEM0057'],
  )
  const temporary = returnedCall(result, 5).arguments.at(0)?.expression
  assert.strictEqual(
    temporary?._tag === 'Borrow' ? temporary.formation._tag : undefined,
    'FixedArrayBorrow',
  )
  if (temporary?._tag === 'Borrow' && temporary.formation._tag !== 'Unavailable') {
    assert.strictEqual(temporary.formation.root._tag, 'TemporaryRoot')
  }
  const subplace = returnedCall(result, 6).arguments.at(0)?.expression
  if (subplace?._tag === 'Borrow' && subplace.formation._tag !== 'Unavailable') {
    assert.strictEqual(subplace.formation.root._tag, 'BindingRoot')
    assert.strictEqual(subplace.formation.root.path.at(0)?._tag, 'Index')
  } else {
    assert.fail('expected a borrow retaining indexed-place provenance')
  }
})

it.effect('keeps a borrowed temporary alive through the call on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const source = `fn sum(values: &[i32]) -> i32 { return values[0] + values[1] }
pub fn main() -> i32 { return sum(&[40, 2]) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'slices/temporary-owner',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('borrows an indexed subplace without copying it on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const source = `fn edit(values: &mut [i32]) -> () { values[0] = 40 }
pub fn main() -> i32 {
  let mut matrix = [[1, 2], [3, 4]]
  edit(&mut matrix[0])
  return matrix[0][0] + matrix[0][1]
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'slices/indexed-place',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('retains runtime indexed-place provenance on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const source = `fn edit(values: &mut [i32]) -> () { values[0] = 40 }
fn change(index: usize) -> i32 {
  let mut matrix = [[1, 2], [3, 4]]
  edit(&mut matrix[index])
  return matrix[index][0] + matrix[index][1]
}
pub fn main() -> i32 { return change(0) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'slices/runtime-indexed-place',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('executes a lexical shared slice binding on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 {
  let values = [40, 2]
  let view = &values
  return view[0] + view[1]
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'slices/local-view',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      `${JSON.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? value.toString() : value))}\n${Hir.encode(Analysis.rootAnalysis(snapshot).hir)}`,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it('types slice length and runtime-bounded borrowed places with preserved access', () => {
  const result = analyze(`struct Token { pub kind: i32 }
fn length(values: &[i32]) -> i32 { return Intrinsic.usizeToI32(values.length) }
fn inspect(values: &[Token], index: usize) -> i32 { return values[index].kind }
fn replace(values: &mut [i32], index: usize) -> i32 {
  values[index] = 42
  return Intrinsic.usizeToI32(values.length)
}
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(result.diagnostics, [])
  const length = result.functions.at(0)?.returnedExpression
  const lengthSubject = length?._tag === 'Call' ? length.arguments.at(0)?.expression : undefined
  assert.strictEqual(
    lengthSubject?._tag === 'FieldProjection' ? lengthSubject.state._tag : undefined,
    'SliceLength',
  )

  const projected = result.functions.at(1)?.returnedExpression
  assert.strictEqual(projected?._tag, 'FieldProjection')
  if (projected?._tag === 'FieldProjection') {
    assert.strictEqual(projected.borrowAccess, 'Shared')
    assert.strictEqual(projected.subject._tag, 'IndexProjection')
    if (projected.subject._tag === 'IndexProjection') {
      assert.strictEqual(projected.subject.bounds._tag, 'RuntimeSlice')
      assert.strictEqual(projected.subject.borrowAccess, 'Shared')
    }
  }

  const write = result.functions.at(2)?.statements.at(0)
  assert.strictEqual(write?._tag, 'WriteStatement')
  if (write?._tag === 'WriteStatement' && write.destination._tag === 'IndexProjection') {
    assert.strictEqual(write.destination.bounds._tag, 'RuntimeSlice')
    assert.strictEqual(write.destination.borrowAccess, 'Exclusive')
    assert.strictEqual(write.root?._tag, 'ParameterDeclaration')
  }
  const lengthHir = result.hir.functions.at(0)
  const lengthReturn = lengthHir === undefined ? undefined : Hir.returned(lengthHir)
  assert.strictEqual(
    lengthReturn?._tag === 'BuiltinCall' ? lengthReturn.arguments.at(0)?._tag : undefined,
    'SliceLength',
  )
  const inspectHir = result.hir.functions.at(1)
  const inspectReturn = inspectHir === undefined ? undefined : Hir.returned(inspectHir)
  assert.strictEqual(inspectReturn?._tag, 'Project')
  if (inspectReturn?._tag === 'Project') {
    assert.strictEqual(inspectReturn.borrowAccess, 'Shared')
    assert.strictEqual(inspectReturn.subject._tag, 'SliceIndexPlace')
  }
  const replaceHir = result.hir.functions.at(2)?.statements.at(0)
  assert.strictEqual(
    replaceHir?._tag === 'Write' ? replaceHir.place._tag : undefined,
    'BorrowedWritePlace',
  )
  assert.deepEqual(Hir.verify(result.hir), [])
})

it('rejects slices nested through explicit generic type arguments', () => {
  const result = analyze(`fn make<T>() -> i32 { return 0 }
pub fn main() -> i32 { return make<&[i32]>() }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0054'],
  )
  assert.strictEqual(result.diagnostics.at(0)?.reason._tag, 'SliceTypePosition')
})

it('retains an unavailable borrow fact after damaged operand syntax', () => {
  const result = analyze(`fn read(values: &[i32]) -> i32 { return 0 }
pub fn main() -> i32 { return read(&) }`)
  const argument = returnedCall(result, 1).arguments.at(0)?.expression

  assert.strictEqual(argument?._tag, 'Borrow')
  assert.strictEqual(
    argument?._tag === 'Borrow' ? argument.formation._tag : undefined,
    'Unavailable',
  )
  const main = result.hir.functions.at(1)
  assert.strictEqual(main === undefined ? undefined : Hir.hasUnavailable(main), true)
})

it('verifies mismatched HIR loan endings without introducing graph cycles', () => {
  const result = analyze(`fn read(values: &[i32]) -> i32 { return 0 }
pub fn main() -> i32 { let values = [1, 2] return read(&values) }`)
  const fn = result.hir.functions.at(1)
  const statement = fn?.statements.at(-1)
  if (fn === undefined || statement?._tag !== 'Return' || statement.expression._tag !== 'Call') {
    throw new RangeError('expected slice call return HIR')
  }
  const expression: Hir.Expression = Object.freeze({
    ...statement.expression,
    loanEnds: Object.freeze([]),
  })
  const returned: Hir.Statement = Object.freeze({ ...statement, expression })
  const malformedFunction: Hir.HirFunction = Object.freeze({
    ...fn,
    statements: Object.freeze([...fn.statements.slice(0, -1), returned]),
  })
  const malformed: Hir.Module = Object.freeze({
    ...result.hir,
    functions: Object.freeze([
      ...result.hir.functions.slice(0, 1),
      malformedFunction,
      ...result.hir.functions.slice(2),
    ]),
  })

  assert.deepEqual(
    Hir.verify(malformed).map((issue) => issue._tag),
    ['InvalidLoanEnd'],
  )
})

it.effect('discovers one generic slice instance across distinct source lengths', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'slices/Instances',
      ascii(`fn scan<T>(values: &[T]) -> i32 { return usize.toI32(values.length) }
fn short() -> i32 { let values = [1, 2, 3] return scan(&values) }
fn long() -> i32 { let values = [1, 2, 3, 4, 5, 6] return scan(&values) }
pub fn main() -> i32 { let left = short() let right = long() return left + right }`),
    )
    const scans = Analysis.instancesOf(self).instances.filter(
      (instance) => instance.key.declaration.name === 'scan',
    )

    assert.strictEqual(scans.length, 1)
    assert.deepEqual(scans.at(0)?.key.typeArguments, ['i32'])
    assert.strictEqual(
      scans.at(0)?.key.contractRow.some((entry) => entry.includes(';3')),
      false,
    )
    assert.strictEqual(
      scans.at(0)?.key.contractRow.some((entry) => entry.includes(';6')),
      false,
    )
  }),
)
