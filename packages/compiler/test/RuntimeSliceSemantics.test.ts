import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Lifetime from '../src/Lifetime.js'
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

it.effect('proves specialized function-item bounds before retaining them as formation facts', () =>
  Effect.gen(function* () {
    const source = `fn extend<'a: 'static>(value: &'a i32) -> &'static i32 { return value }
fn required<T: 'static>(value: T) -> T { return move value }
fn invalidLifetime<'b>() -> fn<'static>(&'b i32) -> &'static i32 { return extend }
fn invalidType<'b>() -> fn<'static>(&'b i32) -> &'b i32 { return required }
fn validLifetime<'b: 'static>() -> fn<'static>(&'b i32) -> &'static i32 { return extend }
fn validType<'b: 'static>() -> fn<'static>(&'b i32) -> &'b i32 { return required }
fn uninstantiated() -> () { let generic = required drop generic return () }
pub fn main() -> i32 { return 0 }`
    const snapshot = yield* Analysis.ofSource('lifetimes/ItemFormation', ascii(source))
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        source: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      })),
      [
        {
          code: 'SEM0212',
          source:
            "fn invalidLifetime<'b>() -> fn<'static>(&'b i32) -> &'static i32 { return extend }",
        },
        {
          code: 'SEM0213',
          source: 'required',
        },
      ],
    )
  }),
)

it.effect(
  'rejects finite storage at static call preconditions even when the result is unused',
  () =>
    Effect.gen(function* () {
      const source = `fn lifetimeRequired<'a: 'static>(value: &'a i32) -> () { return () }
fn typeRequired<T: 'static>(value: T) -> () { drop value return () }
struct View<'a> { value: &'a i32 }
fn namedRequired<'a>(value: &'a i32) -> () { return () }
fn invalidNamed<'a>() -> () {
  let value = 42
  namedRequired<'a>(&value)
  return ()
}
fn validNamed<'a>(value: &'a i32) -> () { namedRequired<'a>(&value.*) return () }
fn valid(value: &'static i32, text: string<'static>) -> () {
  lifetimeRequired(value)
  lifetimeRequired(&value.*)
  let lifetimeItem = lifetimeRequired
  lifetimeItem(value)
  lifetimeItem(&value.*)
  typeRequired(value)
  typeRequired(text)
  let typeItem = typeRequired
  typeItem(&value.*)
  typeItem("static text")
  return ()
}
pub fn main() -> i32 {
  let value = 42
  lifetimeRequired(&value)
  let lifetimeItem = lifetimeRequired
  lifetimeItem(&value)
  typeRequired(&value)
  let typeItem = typeRequired
  typeItem(&value)
  typeItem(View { value: &value })
  return 0
}`
      const snapshot = yield* Analysis.ofSource('lifetimes/StaticCallPreconditions', ascii(source))
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => ({
          code: diagnostic.code,
          source: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
        })),
        Array.from({ length: 6 }, () => ({ code: 'SEM0212', source: '&value' })),
      )
    }),
)

it.effect('checks transitive generic storage admission without realizing the caller', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'lifetimes/GenericStorageAdmission',
      ascii(`
struct Guard<T> { value: T }
impl<T> Drop for Guard<T> { fn drop(self: &mut Guard<T>) -> () { return () } }
fn forward<T>(value: T) -> () { return hidden(move value) }
fn hidden<T>(value: T) -> () { let owner = Guard<T> { value: move value } return () }
pub fn main() -> i32 { let value = 42 forward(&value) return 0 }`),
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0214'],
    )
    const reason = diagnostics.at(0)?.reason
    assert.strictEqual(reason?._tag, 'UnsupportedLifetimeFeature')
    if (reason?._tag === 'UnsupportedLifetimeFeature')
      assert.strictEqual(reason.feature, 'DependentDrop')
  }),
)

it.effect(
  'checks storage admission through selected generic and concrete interface implementations',
  () =>
    Effect.gen(function* () {
      const source = `struct Guard<T> { value: T }
impl<T> Drop for Guard<T> { fn drop(self: &mut Guard<T>) -> () { return () } }
interface Consume { fn consume(value: Self) -> () }
struct Wrapper<T> { value: T }
impl<T> Consume for Wrapper<T> {
 fn consume(value: Wrapper<T>) -> () { let guard = Guard<T> { value: move value.value } return () }
}
fn invoke<T: Consume>(value: T) -> () { return Consume.consume(move value) }
fn identity<T>(value: T) -> T { return move value }
fn hidden<T>(value: T) -> () { let guard = Guard<T> { value: move value } return () }
fn stamped<T: Consume>(value: T, stamp: i32) -> () { return Consume.consume(move value) }
pub fn main() -> i32 {
 let value = 42
 invoke(Wrapper { value: &value })
 Consume.consume(Wrapper { value: &value })
 let item = invoke
 item(Wrapper { value: &value })
 let section = stamped(0)
 section(Wrapper { value: &value })
 let forwarded = identity(invoke)
 forwarded(Wrapper { value: &value })
 let indirect = identity(hidden)
 indirect(&value)
 return 0
}`
      const snapshot = yield* Analysis.ofSource(
        'lifetimes/InterfaceStorageAdmission',
        ascii(source),
      )
      const calls = [
        'invoke(Wrapper { value: &value })',
        'Consume.consume(Wrapper { value: &value })',
        'item(Wrapper { value: &value })',
        'section(Wrapper { value: &value })',
        'forwarded(Wrapper { value: &value })',
        'indirect(&value)',
      ]
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => ({
          code: diagnostic.code,
          sourceId: diagnostic.span.sourceId,
          selected: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
        })),
        calls.map((call) => ({
          code: 'SEM0214',
          sourceId: 'lifetimes/InterfaceStorageAdmission',
          selected: call,
        })),
      )
    }),
)

it('discards lifetime constraints from a rejected union alternative', () => {
  const result = analyze(`pub struct Data { value: i32 }
pub fn wrap<'a, 'b>(value: &'b Data) -> &'a bool | &'b i32 { return &value.value }`)
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(Hir.verify(result.hir), [])
})

it('instantiates callback invocation lifetimes after inferring a borrowed branch state', () => {
  const result = analyze(`struct State { value: i32 }
fn dispatch<'env, D, C: once fn<'env>(D, ()) -> ()>(state: D, complete: C) -> () {
  return complete(move state, ())
}
fn completed(state: &mut State, result: ()) -> () {
  drop result
  state.value = 42
  return ()
}
fn invoke(state: &mut State) -> () { return dispatch(move state, completed) }
pub fn main() -> i32 { let mut state = State { value: 0 } invoke(&mut state) return state.value }`)
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(Hir.verify(result.hir), [])
})

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
    assert.strictEqual(Type.runtimeKey(shared.type.type), Type.runtimeKey(sharedLong.type.type))
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

it('keeps uncaptured invocation lifetimes quantified in function items and sections', () => {
  const result = analyze(`struct Cell { value: i32 }
fn apply<'env>(use: for<'a> once fn<'env>(&'a Cell) -> i32, value: &Cell) -> i32 {
  return use(value)
}
fn read(value: &Cell) -> i32 { return value.value }
fn add(value: &Cell, amount: i32) -> i32 { return value.value + amount }
pub fn main() -> i32 {
  let value = Cell {value: 20}
  let first = apply(read, &value)
  return first + apply(add(2), &value)
}`)
  assert.deepEqual(result.diagnostics, [])
  const main = result.functions.at(3)
  const returned = main?.returnedExpression
  assert.isDefined(returned)
  assert.deepEqual(Hir.verify(result.hir), [])
})

it('elides returned view lifetimes and rejects ambiguous or strengthened headers', () => {
  const result = analyze(`fn identity(values: &[i32]) -> &[i32] { return values }
fn share(values: &mut [i32]) -> &[i32] { return &values }
fn ambiguous(left: &[i32], right: &[i32]) -> &[i32] { return left }
fn strengthen(values: &[i32]) -> &mut [i32] { return &mut values }
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0210', 'SEM0058'],
  )
  for (const fn of result.functions.slice(0, 2)) {
    const input = fn.declaration.parameters.at(0)?.declaredType
    const output = fn.declaration.returnType
    assert.isTrue(input?._tag === 'Resolved' && output._tag === 'Resolved')
    if (input?._tag === 'Resolved' && output._tag === 'Resolved')
      assert.deepEqual(Type.storageLifetimes(output.type), Type.storageLifetimes(input.type))
  }
})

it('preserves elided source lifetimes through ordinary reference results', () => {
  const result = analyze(`struct Counter { value: i32 }
fn shared(counter: &Counter) -> &Counter { return counter }
fn exclusive(counter: &mut Counter) -> &mut Counter { return move counter }
fn projected(counter: &Counter) -> &i32 { return &counter.value }
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(result.diagnostics, [])
  for (const fn of result.functions.slice(0, 3)) {
    const input = fn.declaration.parameters.at(0)?.declaredType
    const output = fn.declaration.returnType
    assert.isTrue(input?._tag === 'Resolved' && output._tag === 'Resolved')
    if (input?._tag === 'Resolved' && output._tag === 'Resolved')
      assert.deepEqual(Type.storageLifetimes(output.type), Type.storageLifetimes(input.type))
  }
})

it('uses returned-view and ordinary argument diagnostics for reference failures', () => {
  const returnSource = `struct Counter { value: i32 }
fn ambiguous(left: &Counter, right: &Counter) -> &Counter { return left }
fn strengthen(counter: &Counter) -> &mut Counter { return &mut counter }
fn local(counter: &Counter) -> &Counter {
  let owned = Counter { value: 0 }
  return &owned
}
pub fn main() -> i32 { return 0 }`
  const invalidReturns = analyze(returnSource)
  assert.deepEqual(
    invalidReturns.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      span: returnSource.slice(diagnostic.span.start, diagnostic.span.end).trim(),
    })),
    [
      { code: 'SEM0210', span: '&Counter' },
      { code: 'SEM0056', span: 'counter' },
      { code: 'OWN0019', span: '&owned' },
      { code: 'SEM0212', span: '&owned' },
    ],
  )

  const ownedArguments = analyze(`struct Counter { value: i32 }
fn take(counter: Counter) -> i32 { return counter.value }
fn direct(counter: &Counter) -> i32 { return take(counter) }
fn piped(counter: &Counter) -> i32 { return counter |> take }
pub fn main() -> i32 { return 0 }`)
  const argumentCodes = ownedArguments.diagnostics.map((diagnostic) => diagnostic.code)

  assert.deepEqual(argumentCodes, ['SEM0012', 'SEM0012'])
  assert.notInclude(argumentCodes, 'SEM0055')
})

it('validates mutable owned parameter storage at the declaration boundary', () => {
  const result = analyze(`struct Counter { value: i32 }
fn immutable(counter: Counter) -> Counter {
  counter.value = counter.value + 1
  return move counter
}
fn borrowed(mut counter: &mut Counter) -> i32 { return counter.value }
fn borrowedSlice(mut values: &mut [i32]) -> i32 { return values[0] }
service Transformer {
  fn transform(mut counter: Counter) -> Counter
}
interface Transformable {
  fn transform(mut counter: Counter) -> Counter
}
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      context:
        diagnostic.reason._tag === 'InvalidMutableParameter'
          ? diagnostic.reason.context
          : undefined,
    })),
    [
      { code: 'SEM0035', context: undefined },
      { code: 'SEM0143', context: 'Contract' },
      { code: 'SEM0143', context: 'Contract' },
    ],
  )
})

it('rejects a returned view whose body does not preserve the declared source', () => {
  const source = `fn invalid(values: &[i32]) -> &[i32] {
  let local = [1, 2]
  return &local
}
pub fn main() -> i32 { return 0 }`
  const result = analyze(source)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      span: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
    })),
    [
      { code: 'OWN0019', span: '&local' },
      { code: 'SEM0212', span: '&local' },
    ],
  )
})

it('retains shared dependencies in stored values and closure environments', () => {
  const result = analyze(`struct Stored<'a> { view: &'a [i32] }
fn store<'a>(values: &'a [i32]) -> Stored<'a> {
  return Stored<'a> { view: values }
}
fn captured<'a>(values: &'a [i32]) -> once fn<'a>() -> &'a [i32] {
  return fn() -> &'a [i32] { return values }
}
fn array(values: &[i32]) -> i32 {
  let stored = [values]
  return 0
}
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(result.diagnostics, [])
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

it('admits shared slices through explicit generic type arguments', () => {
  const result = analyze(`fn make<T>() -> i32 { return 0 }
pub fn main() -> i32 { return make<&[i32]>() }`)

  assert.deepEqual(result.diagnostics, [])
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
  assert.notStrictEqual(main === undefined ? undefined : Hir.firstUnavailable(main), undefined)
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
      ascii(`import silk.usize as usize
fn scan<T>(values: &[T]) -> i32 { return usize.toI32(values.length) }
fn short() -> i32 { let values = [1, 2, 3] return scan(&values) }
fn long() -> i32 { let values = [1, 2, 3, 4, 5, 6] return scan(&values) }
pub fn main() -> i32 { let left = short() let right = long() return left + right }`),
    )
    const scans = Analysis.instancesOf(self).instances.filter(
      (instance) => instance.key.declaration.name === 'scan',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(scans.length, 1)
    assert.deepEqual(
      scans.at(0)?.key.typeArguments.filter((argument) => !Lifetime.isLifetime(argument)),
      ['i32'],
    )
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
