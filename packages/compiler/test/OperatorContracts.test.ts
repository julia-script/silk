import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('operator-contracts/main', encoder.encode(source))

const parse = (id: string, source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

const vectorContracts = `struct Vector { value: i32 }

fn scale(left: Vector, right: i32) -> Vector {
  return Vector { value: left.value * right }
}

fn dot(left: Vector, right: Vector) -> i32 {
  return left.value * right.value
}

interface Multiply<Right, Output> {
  operator * fn multiply(left: Self, right: Right) -> Output
}

impl Multiply<i32, Vector> for Vector { multiply: Vector.scale }
impl Multiply<Vector, i32> for Vector { multiply: Vector.dot }
`

it.effect('selects heterogeneous concrete operator conformances statically', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${vectorContracts}
pub fn main() -> i32 {
  let source = Vector { value: 6 }
  let scaled = move source * 7
  let unit = Vector { value: 1 }
  return move scaled * move unit
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('selects a marked operation through an ordinary generic bound', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${vectorContracts}
fn doubled<T: Multiply<i32, T>>(value: T) -> T {
  return move value * 2
}

pub fn main() -> i32 {
  let result = doubled(Vector { value: 21 })
  return result.value
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('does not grant operator syntax to a conventional operation name', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Value { raw: i32 }
fn multiply(left: Value, right: Value) -> Value {
  drop right
  return move left
}
interface Named { fn multiply(left: Self, right: Self) -> Self }
impl Named for Value { multiply: Value.multiply }
pub fn main() -> i32 {
  let left = Value { raw: 1 }
  let right = Value { raw: 2 }
  let value = move left * move right
  return value.raw
}`)
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      [Diagnostic.operatorNotApplicableCode],
    )
  }),
)

it.effect('rejects control markers, service markers, and ambiguous marked contracts', () =>
  Effect.gen(function* () {
    const invalid = yield* snapshot(`interface Invalid {
  operator && fn both(left: Self, right: Self) -> bool
}
service Runtime {
  operator + fn add(left: i32, right: i32) -> i32
}
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(
      Analysis.diagnostics(invalid).map((diagnostic) => diagnostic.code),
      [Diagnostic.invalidOperatorContractCode, Diagnostic.invalidOperatorContractCode],
    )

    const ambiguous = yield* snapshot(`struct Value { raw: i32 }
fn first(left: Value, right: Value) -> Value {
  drop right
  return move left
}
fn second(left: Value, right: Value) -> Value {
  drop left
  return move right
}
interface First { operator + fn apply(left: Self, right: Self) -> Self }
interface Second { operator + fn apply(left: Self, right: Self) -> Self }
impl First for Value { apply: Value.first }
impl Second for Value { apply: Value.second }
pub fn main() -> i32 {
  let left = Value { raw: 1 }
  let right = Value { raw: 2 }
  let value = move left + move right
  return value.raw
}`)
    assert.deepEqual(
      Analysis.diagnostics(ambiguous).map((diagnostic) => diagnostic.code),
      [Diagnostic.ambiguousOperatorCode],
    )
  }),
)

it.effect('formats the contextual marker before ordinary and effect contracts', () =>
  Effect.gen(function* () {
    const ordinary = yield* Formatter.format(
      parse(
        'operator-contracts/format',
        'interface M{operator   * fn  mul(left:Self,right:i32)->Self}',
      ),
    )
    assert.strictEqual(
      new TextDecoder().decode(FormattedDocument.toUint8Array(ordinary)),
      'interface M {\n  operator * fn mul(left: Self, right: i32) -> Self\n}\n',
    )
    const effectful = yield* Formatter.format(
      parse(
        'operator-contracts/effect-format',
        'interface M{operator * effect fn mul(left:Self,right:i32)->Self}',
      ),
    )
    assert.strictEqual(
      new TextDecoder().decode(FormattedDocument.toUint8Array(effectful)),
      'interface M {\n  operator * effect fn mul(left: Self, right: i32) -> Self\n}\n',
    )
  }),
)
