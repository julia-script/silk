import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as InstanceDiagnostics from '../src/InstanceDiagnostics.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'
import * as WasmMain from './support/WasmMain.js'

/**
 * The bound-operation call at a source witness.
 *
 * A bound reaches its operations two ways: through the operator that spells one, and through the
 * bound's own name for one no operator spells. A provider answers either with a sealed intrinsic or
 * with a function of its own actor. The two spellings and the two witness kinds are independent, so
 * every combination has to lower — and the non-operator call at a source witness is the one that
 * did not, because bound-operation lowering read only the intrinsic a witness named.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (
  name: string,
  source: string,
  target?: string,
  options?: Parameters<typeof Analysis.ofSourceRealized>[3],
) => Analysis.ofSourceRealized(name, ascii(source), target, options)

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

/** Evaluated scalars can be BigInt, which plain JSON serialization refuses. */
const describe = (outcome: unknown): string =>
  JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value))

const evaluatedValue = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source)
    assert.deepEqual(messages(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    return outcome._tag === 'Completed' ? Number(outcome.result.value) : undefined
  })

/** Runs one source on the bootstrap evaluator and the direct WebAssembly backend. */
const twoEngineValue = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source, 'wasm32-unknown-unknown')
    assert.deepEqual(messages(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    const bootstrap = evaluated._tag === 'Completed' ? Number(evaluated.result.value) : undefined

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const direct = yield* WasmMain.invoke(wasm.bytes, 'BoundOperationWitness.invokeWasm')

    return Object.freeze({ bootstrap, direct })
  })

/**
 * One user-declared key type answering two operations: an equivalence an operator spells and a
 * digest none does. Both witnesses are ordinary Silk, so the digest is reached only by naming the
 * bound and can only be answered by the provider's own function.
 */
const userKey = `import silk.i32 as i32
import silk.u64 as u64
interface Keyed {
  operator == fn equals(left: &Self, right: &Self) -> bool
  fn digest(left: &Self, right: &Self) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellDigest(left: &Cell, right: &Cell) -> u64 {
  return u64.wrappingAdd(i32.toU64(left.weight), i32.toU64(right.weight))
}

impl Keyed for Cell { equals: Cell.cellEquals digest: Cell.cellDigest }

fn digestOf<T: Keyed>(left: T, right: T) -> u64 { return Keyed.digest(&left, &right) }`

it.effect('selects explicit interface applications in direct and pipeline effect calls', () =>
  Effect.gen(function* () {
    const module = 'interface-operation-witness/applied-calls'
    const source = `import silk.u32 as u32

pub interface Encodable<A> {
  effect fn encode(self: &Self) -> A
}

struct Age { value: u32 }

impl Encodable<u32> for Age {
  effect fn encode(self: &Self) -> u32 { return self.value }
}

impl Encodable<string> for Age {
  effect fn encode(self: &Self) -> string { return "32" }
}

pub fn main() -> i32 {
  let schema = Age { value: 32 }
  let encodedU32 = run Encodable<u32>.encode(&schema)
  let encodedString = run &schema |> Encodable<string>.encode
  let groupedU32 = run &schema |> (Encodable<u32>.encode)
  if groupedU32 != encodedU32 { return 0 }
  return u32.toI32(encodedU32) + 10
}`
    const snapshot = yield* analyzed(module, source, 'wasm32-unknown-unknown')
    assert.deepEqual(messages(snapshot), [])

    const main = Projections.hirOf(snapshot, module)?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'main',
    )
    const calls =
      main?.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .filter((expression) => expression._tag === 'InterfaceOperationCall') ?? []
    assert.strictEqual(calls.length, 3)
    assert.deepEqual(
      calls.map((call) => Type.encode(call.capability)),
      [`${module}.Encodable<u32>`, `${module}.Encodable<string>`, `${module}.Encodable<u32>`],
    )
    assert.deepEqual(
      calls.map((call) => Type.encode(call.provider)),
      [`${module}.Age`, `${module}.Age`, `${module}.Age`],
    )
    assert.isTrue(calls.every((call) => call.witnessEffectSite !== undefined))
    assert.strictEqual(
      new Set(calls.map((call) => call.witnessEffectSite?.ordinal)).size,
      calls.length,
    )

    const appliedOffset = source.indexOf('Encodable<u32>.encode(&schema)')
    const ownerOccurrence = Analysis.semanticOccurrenceAt(snapshot, module, appliedOffset)
    const argumentOccurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      module,
      appliedOffset + 'Encodable<'.length,
    )
    const operationOccurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      module,
      appliedOffset + 'Encodable<u32>.'.length,
    )
    assert.strictEqual(ownerOccurrence?.role, 'Type')
    assert.strictEqual(ownerOccurrence?.resolution._tag, 'Available')
    assert.strictEqual(argumentOccurrence?.role, 'Type')
    assert.strictEqual(operationOccurrence?.role, 'Operation')
    assert.strictEqual(operationOccurrence?.resolution._tag, 'Available')

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const direct = yield* WasmMain.invoke(wasm.bytes, 'BoundOperationWitness.appliedCalls')
    assert.strictEqual(direct, 42)

    const effectEntry = yield* analyzed(
      'interface-operation-witness/applied-effect-entry',
      `pub interface Encodable<A> {
  effect fn encode(self: &Self) -> A
}
struct Age { value: u32 }
impl Encodable<u32> for Age {
  effect fn encode(self: &Self) -> u32 { return self.value }
}
pub effect fn main() -> () {
  let schema = Age { value: 32 }
  let encoded = run Encodable<u32>.encode(&schema)
}`,
    )
    assert.deepEqual(messages(effectEntry), [])
  }),
)

it.effect('keeps invalid applied interface operations out of realization', () =>
  Effect.gen(function* () {
    const prelude = `pub interface Encodable<A> {
  effect fn encode(self: &Self) -> A
}
struct Age { value: u32 }
struct Other { value: u32 }
impl Encodable<u32> for Age {
  effect fn encode(self: &Self) -> u32 { return self.value }
}`
    const escaping = yield* analyzed(
      'interface-operation-witness/escaping-section',
      `${prelude}
pub fn main() -> i32 {
  let encoder = Encodable<u32>.encode
  return 0
}`,
    )
    assert.include(
      Analysis.diagnostics(escaping).map((diagnostic) => diagnostic.code),
      'SEM0099',
    )

    const missing = yield* analyzed(
      'interface-operation-witness/missing-conformance',
      `${prelude}
pub fn main() -> i32 {
  let value = Other { value: 32 }
  let encoded = run Encodable<u32>.encode(&value)
  return 0
}`,
    )
    assert.include(
      Analysis.diagnostics(missing).map((diagnostic) => diagnostic.code),
      'SEM0121',
    )
    const calls = Projections.hirOf(
      missing,
      'interface-operation-witness/missing-conformance',
    )?.functions.flatMap((fn) =>
      fn.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .filter((expression) => expression._tag === 'InterfaceOperationCall'),
    )
    assert.deepEqual(calls, [])

    const ambiguous = yield* analyzed(
      'interface-operation-witness/ambiguous-fallback',
      `interface Seed<A> { fn seed() -> A }
fn ambiguous<A: Seed<i32>, B: Seed<i32>>() -> i32 {
  return Seed<i32>.seed()
}
pub fn main() -> i32 { return 0 }`,
    )
    assert.include(
      Analysis.diagnostics(ambiguous).map((diagnostic) => diagnostic.code),
      'SEM0097',
    )

    const appliedService = yield* analyzed(
      'interface-operation-witness/applied-service',
      `service Source<A> { effect fn read() -> A ? &Source<A> }
pub fn main() -> i32 {
  let value = run Source<i32>.read()
  return 0
}`,
    )
    assert.include(
      Analysis.diagnostics(appliedService).map((diagnostic) => diagnostic.code),
      'SEM0010',
    )

    const resultDirected = yield* analyzed(
      'interface-operation-witness/result-does-not-select',
      `interface Seed<A> { fn seed() -> A }
struct Age { value: i32 }
impl Seed<i32> for Age { fn seed() -> i32 { return 32 } }
fn selectedByResult() -> i32 { return Seed<i32>.seed() }
pub fn main() -> i32 { return 0 }`,
    )
    assert.include(
      Analysis.diagnostics(resultDirected).map((diagnostic) => diagnostic.code),
      'SEM0099',
    )

    const conflictingSelf = yield* analyzed(
      'interface-operation-witness/conflicting-self',
      `interface Pair<A> { fn combine(left: &Self, right: &Self) -> A }
struct Age { value: i32 }
struct Other { value: i32 }
impl Pair<i32> for Age { fn combine(left: &Self, right: &Self) -> i32 { return 1 } }
pub fn main() -> i32 {
  let age = Age { value: 1 }
  let other = Other { value: 2 }
  return Pair<i32>.combine(&age, &other)
}`,
    )
    const conflict = Analysis.diagnostics(conflictingSelf).find(
      (diagnostic) => diagnostic.code === 'SEM0100',
    )
    assert.strictEqual(conflict?.reason._tag, 'TypeArgumentConflict')
    if (conflict?.reason._tag === 'TypeArgumentConflict') {
      assert.strictEqual(conflict.reason.parameter, 'Self')
      assert.include(conflict.reason.written, 'Age')
      assert.include(conflict.reason.implied, 'Other')
    }

    const invalidOwnerApplications: ReadonlyArray<readonly [string, string, string]> = [
      [
        'unknown-member',
        `${prelude}
pub fn main() -> i32 {
  let value = Age { value: 32 }
  let encoded = run Encodable<u32>.missing(&value)
  return 0
}`,
        'SEM0010',
      ],
      [
        'wrong-arity',
        `${prelude}
pub fn main() -> i32 {
  let value = Age { value: 32 }
  let encoded = run Encodable<u32, string>.encode(&value)
  return 0
}`,
        'SEM0051',
      ],
      [
        'wrong-kind',
        `interface Needs<?R> { fn inspect(self: &Self) -> i32 }
struct Age { value: i32 }
pub fn main() -> i32 {
  let age = Age { value: 32 }
  return Needs<i32>.inspect(&age)
}`,
        'SEM0088',
      ],
    ]
    for (const [name, source, code] of invalidOwnerApplications) {
      const snapshot = yield* analyzed(`interface-operation-witness/${name}`, source)
      assert.include(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        code,
      )
    }
  }),
)

it.effect('uses operand provider evidence before one enclosing-bound fallback', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'interface-operation-witness/provider-evidence',
      `interface Seed<A> {
  fn seed() -> A
}
interface Encodable<A> {
  fn encode(self: &Self) -> A
}
struct Age { value: i32 }
impl Seed<i32> for Age {
  fn seed() -> i32 { return 20 }
}
impl Encodable<i32> for Age {
  fn encode(self: &Self) -> i32 { return self.value }
}
fn seeded<T: Seed<i32>>() -> i32 {
  return Seed<i32>.seed()
}
fn encodeConcrete<T: Encodable<i32>>(age: &Age) -> i32 {
  return Encodable<i32>.encode(age)
}
pub fn main() -> i32 {
  let age = Age { value: 22 }
  return seeded<Age>() + encodeConcrete<Age>(&age)
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('preserves ownership and Effect rows through applied interface calls', () =>
  Effect.gen(function* () {
    const module = 'interface-operation-witness/applied-contract-shapes'
    const snapshot = yield* analyzed(
      module,
      `import silk.effect as Effect

service Clock {}
struct Problem {}
interface Access<A> {
  fn consume(value: Self) -> A
  fn mutate(value: &mut Self) -> A
}
interface Pending<A> {
  effect fn read(value: &Self) -> A ! Problem ? &Clock
}
struct Cell { value: i32 }
impl Access<i32> for Cell {
  fn consume(value: Self) -> i32 { return value.value }
  fn mutate(value: &mut Self) -> i32 {
    value.value = 22
    return value.value
  }
}
impl Pending<i32> for Cell {
  effect fn read(value: &Self) -> i32 { return value.value }
}
fn consume(value: Cell) -> i32 { return Access<i32>.consume(move value) }
fn mutate(value: &mut Cell) -> i32 { return Access<i32>.mutate(value) }
fn pending(value: &Cell) -> Effect<i32 ! Problem ? &Clock> {
  return Pending<i32>.read(value)
}
pub fn main() -> i32 {
  let owned = consume(Cell { value: 20 })
  let mut cell = Cell { value: 0 }
  return owned + mutate(&mut cell)
}`,
    )
    assert.deepEqual(messages(snapshot), [])

    const pending = Projections.hirOf(snapshot, module)?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'pending',
    )
    const call = pending?.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find((expression) => expression._tag === 'InterfaceOperationCall')
    assert.strictEqual(call?._tag, 'InterfaceOperationCall')
    if (call?._tag === 'InterfaceOperationCall') {
      assert.deepEqual(call.contract.failureRow.failures.map(Type.encode), [`${module}.Problem`])
      assert.deepEqual(
        call.contract.requirementRow.requirements.map((requirement) => ({
          capability: Type.encode(requirement.capability),
          access: requirement.access,
        })),
        [{ capability: `${module}.Clock`, access: 'Shared' }],
      )
    }

    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('indexes namespace-qualified applied interface owners without duplicate call paths', () =>
  Effect.gen(function* () {
    const module = 'interface-operation-witness/qualified-tooling'
    const source = `import model.Encoding as Model
pub fn main() -> i32 {
  let age = Model.Age { value: 42 }
  return Model.Encodable<i32>.encode(&age)
}`
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make(module, ascii(source)),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'model/Encoding',
              ascii(`pub interface Encodable<A> { fn encode(value: &Self) -> A }
pub struct Age { pub value: i32 }
impl Encodable<i32> for Age {
  fn encode(value: &Self) -> i32 { return value.value }
}`),
            ],
          ]),
        ),
      ),
    )
    assert.deepEqual(messages(snapshot), [])

    const appliedOffset = source.indexOf('Model.Encodable<i32>.encode')
    const namespace = Analysis.semanticOccurrenceAt(snapshot, module, appliedOffset)
    const owner = Analysis.semanticOccurrenceAt(snapshot, module, appliedOffset + 'Model.'.length)
    const operation = Analysis.semanticOccurrenceAt(
      snapshot,
      module,
      appliedOffset + 'Model.Encodable<i32>.'.length,
    )
    assert.strictEqual(namespace?.role, 'Actor')
    assert.strictEqual(namespace?.resolution._tag, 'Available')
    assert.strictEqual(owner?.role, 'Type')
    assert.strictEqual(owner?.resolution._tag, 'Available')
    assert.strictEqual(operation?.role, 'Operation')
    assert.strictEqual(operation?.resolution._tag, 'Available')

    const inaccessible = yield* Analysis.makeRealized({
      root: SourceFile.make(
        `${module}/inaccessible`,
        ascii(`import model.Hidden as Model
pub fn main() -> i32 {
  let age = Model.Age { value: 42 }
  return Model.Encodable<i32>.encode(&age)
}`),
      ),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'model/Hidden',
              ascii(`interface Encodable<A> { fn encode(value: &Self) -> A }
pub struct Age { pub value: i32 }
impl Encodable<i32> for Age {
  fn encode(value: &Self) -> i32 { return value.value }
}`),
            ],
          ]),
        ),
      ),
    )
    assert.include(
      Analysis.diagnostics(inaccessible).map((diagnostic) => diagnostic.code),
      'SEM0009',
    )
    const inaccessibleCalls = Projections.hirOf(
      inaccessible,
      `${module}/inaccessible`,
    )?.functions.flatMap((fn) =>
      fn.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .filter((expression) => expression._tag === 'InterfaceOperationCall'),
    )
    assert.deepEqual(inaccessibleCalls, [])
  }),
)

it.effect(
  'returns the source witness result from a non-operator bound call on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      // The digest is 20 + 22, computed by ordinary Silk the specialization selected — so a wrong
      // witness, a missing call, or a placeholder result cannot produce 42 by accident.
      const outcome = yield* twoEngineValue(
        'bound-operation-witness/user-digest',
        `import silk.u64 as u64
${userKey}
pub fn main() -> i32 {
  let digest = digestOf<Cell>(Cell { weight: 20 }, Cell { weight: 22 })
  if digest == 42 { return u64.toI32(digest) }
  return 1
}`,
      )
      assert.strictEqual(outcome.bootstrap, 42)
      assert.strictEqual(outcome.direct, 42)
    }),
)

it.effect('reaches each provider’s own witness from one bound-operation call site', () =>
  Effect.gen(function* () {
    // Two providers, two source witnesses, one generic body. The two answers are unrelated, so a
    // single body cannot serve both by lowering the operation width-neutrally.
    const value = yield* evaluatedValue(
      'bound-operation-witness/per-specialization',
      `import silk.u64 as u64
interface Keyed {
  fn digest(left: &Self, right: &Self) -> u64
}

struct Cell { weight: i32 }
struct Tag { code: u64 }

fn cellDigest(left: &Cell, right: &Cell) -> u64 { return 10 }
fn tagDigest(left: &Tag, right: &Tag) -> u64 { return u64.wrappingAdd(left.code, right.code) }

impl Keyed for Cell { digest: Cell.cellDigest }
impl Keyed for Tag { digest: Tag.tagDigest }

fn digestOf<T: Keyed>(left: T, right: T) -> u64 { return Keyed.digest(&left, &right) }

pub fn main() -> i32 {
  let cell = digestOf<Cell>(Cell { weight: 1 }, Cell { weight: 2 })
  let tag = digestOf<Tag>(Tag { code: 30 }, Tag { code: 2 })
  if cell != 10 { return 1 }
  if tag != 32 { return 2 }
  return u64.toI32(u64.wrappingAdd(cell, tag))
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('forwards value operands to a source witness without inserting hidden borrows', () =>
  Effect.gen(function* () {
    // Literal ownership works in both directions: this interface transfers its operands, so its
    // ordinary witness receives values and the generic call site consumes them exactly once.
    const value = yield* evaluatedValue(
      'bound-operation-witness/value-operands',
      `interface Combined {
  fn combine(left: Self, right: Self) -> i32
}

struct Cell { weight: i32 }

fn cellCombine(left: Cell, right: Cell) -> i32 { return left.weight + right.weight }

impl Combined for Cell { combine: Cell.cellCombine }

fn combineOf<T: Combined>(left: T, right: T) -> i32 {
  return Combined.combine(move left, move right)
}

pub fn main() -> i32 { return combineOf<Cell>(Cell { weight: 20 }, Cell { weight: 22 }) }`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('accepts a shared source witness for a take-owned interface contract', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'bound-operation-witness/value-contract-borrowed-witness',
      `interface Decoder {
  fn decode(value: Self) -> i32
}

struct Cell { code: i32 }

fn decodeCell(value: &Cell) -> i32 { return value.code }

impl Decoder for Cell { decode: Cell.decodeCell }

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('requires explicit moves for operator-spelled value operands', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'bound-operation-witness/operator-value-ownership',
      `interface Combined {
  operator + fn add(left: Self, right: Self) -> Self
}

struct Token { code: i32 }

fn tokenAdd(left: Token, right: Token) -> Token {
  return Token { code: left.code + right.code }
}

impl Combined for Token { add: Token.tokenAdd }

fn implicitTwice<T: Combined>(left: T, right: T) -> T {
  let first = left + right
  return left + right
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [
      'Moving left requires an explicit move',
      'Moving right requires an explicit move',
      'Moving left requires an explicit move',
      'Moving right requires an explicit move',
    ])
  }),
)

it.effect('weakens exclusive contract operands only at a selected source witness', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'bound-operation-witness/weaker-source-access',
      `interface Observe {
  fn inspect(value: &mut Self) -> i32
}

struct Cell { code: i32 }

fn inspectCell(value: &Cell) -> i32 { return value.code }

impl Observe for Cell { inspect: Cell.inspectCell }

fn inspectOf<T: Observe>(value: &mut T) -> i32 { return Observe.inspect(value) }

pub fn main() -> i32 {
  let mut cell = Cell { code: 42 }
  return inspectOf<Cell>(&mut cell)
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect(
  'releases a weaker witness reborrow after propagating a typed failure on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const outcome = yield* twoEngineValue(
        'bound-operation-witness/fallible-weaker-access',
        `import silk.effect as Effect
import silk.result { Result }

pub struct Problem { code: i32 }

interface Decoder {
  effect fn decode(value: &mut Self) -> i32 ! Problem
}

struct Cell { code: i32 }

effect fn decodeCell(value: &Cell) -> i32 ! Problem {
  fail Problem { code: 1 }
}

impl Decoder for Cell { decode: Cell.decodeCell }

fn pending<T: Decoder>(value: &mut T) -> Effect<i32 ! Problem> {
  return Decoder.decode(value)
}

fn observe(result: Result<i32, Problem>) -> i32 {
  return match move result {
      Result<i32, Problem>.Success { value } => value
      Result<i32, Problem>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let mut cell = Cell { code: 40 }
  let failure = observe(run Effect.result(pending<Cell>(&mut cell)))
  cell.code = cell.code + 1
  return failure + cell.code
}`,
      )
      assert.strictEqual(outcome.bootstrap, 42)
      assert.strictEqual(outcome.direct, 42)
    }),
)

it.effect('runs an effectful inline scalar witness with its failure and requirement rows', () =>
  Effect.gen(function* () {
    const module = 'bound-operation-witness/inline-scalar-effect'
    const source = `import silk.effect as Effect
import silk.result { Result }

pub struct Problem { code: i32 }

service Output {
  effect fn emit(number: i32) -> i32 ? &Output
}

struct FixedOutput {}

effect fn emit(self: &FixedOutput, number: i32) -> i32 { return number }

impl Output for FixedOutput { emit: FixedOutput.emit }

interface Present {
  effect fn present(value: &Self) -> i32 ! Problem ? &Output
}

impl Present for i32 {
  effect fn present(value: &Self) -> i32 ! Problem ? &Output {
    return run Output.emit(42)
  }
}

fn pending<T: Present>(value: &T) -> Effect<i32 ! Problem ? &Output> {
  return Present.present(value)
}

fn observe(result: Result<i32, Problem>) -> i32 {
  return match move result {
      Result<i32, Problem>.Success { value } => value
      Result<i32, Problem>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let output = FixedOutput {}
  let value = 7
  let provided = pending<i32>(&value) |> Effect.provide(&output)
  return observe(run Effect.result(provided))
}`
    const snapshot = yield* analyzed(module, source, 'wasm32-unknown-unknown')
    assert.deepEqual(messages(snapshot), [])

    const pending = Projections.hirOf(snapshot, module)?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'pending',
    )
    const bound = pending?.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find((expression) => expression._tag === 'InterfaceOperationCall')
    assert.strictEqual(bound?._tag, 'InterfaceOperationCall')
    if (bound?._tag !== 'InterfaceOperationCall') return
    assert.deepEqual(bound.contract.failureRow.failures.map(Type.encode), [`${module}.Problem`])
    assert.deepEqual(
      bound.contract.requirementRow.requirements.map((requirement) => ({
        capability: Type.encode(requirement.capability),
        access: requirement.access,
      })),
      [{ capability: `${module}.Output`, access: 'Shared' }],
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const direct = yield* WasmMain.invoke(
      wasm.bytes,
      'BoundOperationWitness.invokeInlineScalarEffectWasm',
    )
    assert.strictEqual(direct, 42)
  }),
)

it.effect('weakens implicit operator borrows to a source witness demand', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'bound-operation-witness/weaker-operator-access',
      `interface Combined {
  operator + fn add(left: &mut Self, right: &mut Self) -> Self
}

struct Cell { code: i32 }

fn cellAdd(left: &Cell, right: &Cell) -> Cell {
  return Cell { code: left.code + right.code }
}

impl Combined for Cell { add: Cell.cellAdd }

fn combine<T: Combined>(left: T, right: T) -> T {
  let mut ownedLeft = move left
  let mut ownedRight = move right
  return (&mut ownedLeft) + (&mut ownedRight)
}

pub fn main() -> i32 {
  let out = combine<Cell>(Cell { code: 20 }, Cell { code: 22 })
  return out.code
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('freshens replayed non-direct witness operand loans across mutually exclusive arms', () =>
  Effect.gen(function* () {
    const source = `service Counter { effect fn read(number: i32) -> i32 ? &Counter }
struct Fixed {}
effect fn read(self: &Fixed, number: i32) -> i32 { return number }
impl Counter for Fixed { read: Fixed.read }

interface Combined { operator + fn add(left: &mut Self, right: &mut Self) -> i32 }
struct Cell { code: i32 }
fn cellAdd(left: &Cell, right: &Cell) -> i32 { return left.code + right.code }
impl Combined for Cell { add: Cell.cellAdd }

effect fn branch<Self: Combined>(flag: bool, left: Self, right: Self) -> i32 {
  let fixed = Fixed {}
  let mut ownedLeft = move left
  let mut ownedRight = move right
  let number = (&mut ownedLeft) + (&mut ownedRight)
  let pending = Intrinsic.bindRequirement<Counter>(
    Counter.read(number),
    &fixed,
  )
  if flag {
    return run move pending
  }
  return run move pending
}

pub fn main() -> i32 {
  return (run branch<Cell>(true, Cell { code: 20 }, Cell { code: 22 }))
    + (run branch<Cell>(false, Cell { code: 19 }, Cell { code: 23 }))
}`
    const snapshot = yield* analyzed('bound-operation-witness/replayed-source-operand', source)
    assert.deepEqual(messages(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 84n)
  }),
)

it.effect('widens a pure source witness to the exact interface Effect contract', () =>
  Effect.gen(function* () {
    const module = 'bound-operation-witness/pure-effect-boundary'
    const source = `import silk.effect as Effect
import silk.result { Result }

pub struct Problem {}

interface Decoder {
  effect fn decode(value: &Self) -> i32 ! Problem
}

struct Cell { code: i32 }

fn decodeCell(value: &Cell) -> i32 { return value.code }

impl Decoder for Cell { decode: Cell.decodeCell }

fn pending<T: Decoder>(value: &T) -> Effect<i32 ! Problem> {
  return Decoder.decode(value)
}

fn observe(result: Result<i32, Problem>) -> i32 {
  return match move result {
      Result<i32, Problem>.Success { value } => value
      Result<i32, Problem>.Failure { error } => 0
  }
}

pub fn main() -> i32 {
  let cell = Cell { code: 42 }
  return observe(run Effect.result(pending<Cell>(&cell)))
}`
    const raw = yield* analyzed(module, source, 'wasm32-unknown-unknown', {
      normalizeMir: false,
    })
    const normalized = yield* analyzed(module, source, 'wasm32-unknown-unknown', {
      normalizeMir: true,
    })
    assert.deepEqual(messages(raw), [])
    assert.deepEqual(messages(normalized), [])

    const pending = Projections.hirOf(raw, module)?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'pending',
    )
    const bound = pending?.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find((expression) => expression._tag === 'InterfaceOperationCall')
    assert.strictEqual(bound?._tag, 'InterfaceOperationCall')
    if (bound?._tag !== 'InterfaceOperationCall') return
    assert.deepEqual(bound.contract.failureRow.failures.map(Type.encode), [`${module}.Problem`])

    const owner =
      pending?.declaration.canonical._tag === 'Canonical'
        ? pending.declaration.canonical.id
        : undefined
    assert.isDefined(bound.witnessEffectSite)
    assert.isDefined(owner)
    if (bound.witnessEffectSite === undefined || owner === undefined) return

    const rawMir = Analysis.loweredMir(raw)
    const runnerId = Hir.effectRunnerId(owner, bound.witnessEffectSite)
    const runner = rawMir.functions.find(
      (fn) => fn.id.module === runnerId.module && fn.id.name === runnerId.name,
    )
    assert.strictEqual(runner?.result._tag, 'EffectOutcome')
    if (runner?.result._tag !== 'EffectOutcome') return
    assert.deepEqual(Type.failureMembers(runner.result.type).map(Type.encode), [
      `${module}.Problem`,
    ])
    const operations = MirVerification.operations(runner)
    assert.strictEqual(
      operations.filter(
        (operation) => operation._tag === 'Call' && operation.target.name === 'decodeCell',
      ).length,
      1,
    )
    assert.isFalse(
      operations.some(
        (operation) => operation._tag === 'RunEffectValue' || operation._tag === 'RunEffect',
      ),
      'a pure witness should not retain unreachable failure execution machinery',
    )
    const encoded = MirEncoding.encode(rawMir)
    for (const spelling of [
      'dictionary',
      'vtable',
      'witnessTable',
      'ServiceCall',
      'ServiceEffectConstruct',
    ])
      assert.isFalse(encoded.includes(spelling), `${spelling} reached interface-only MIR`)

    const normalizedMir = Analysis.loweredMir(normalized)
    const normalizedRunner = normalizedMir.functions.find(
      (fn) => fn.id.module === runnerId.module && fn.id.name === runnerId.name,
    )
    const staticRun = normalizedMir.functions
      .flatMap(MirVerification.operations)
      .find(
        (operation) =>
          operation._tag === 'RunStaticEffect' &&
          operation.runner.module === runnerId.module &&
          operation.runner.name === runnerId.name,
      )
    if (normalizedRunner !== undefined) {
      assert.strictEqual(normalizedRunner.result._tag, 'EffectOutcome')
      if (normalizedRunner.result._tag !== 'EffectOutcome') return
      assert.deepEqual(Type.failureMembers(normalizedRunner.result.type).map(Type.encode), [
        `${module}.Problem`,
      ])
    } else {
      assert.strictEqual(staticRun?._tag, 'RunStaticEffect')
      if (staticRun?._tag !== 'RunStaticEffect') return
      assert.deepEqual(Type.failureMembers(staticRun.outcomeType.type).map(Type.encode), [
        `${module}.Problem`,
      ])
    }

    const outcome = Analysis.evaluate(normalized)
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('widens a smaller Effect witness row at the interface boundary', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'bound-operation-witness/smaller-effect-row',
      `import silk.effect as Effect
import silk.result { Result }

pub struct Problem {}
pub struct Extra {}

interface Decoder {
  effect fn decode(value: &Self) -> i32 ! Problem | Extra
}

struct Cell { code: i32 }

effect fn decodeCell(value: &Cell) -> i32 ! Problem { return value.code }

impl Decoder for Cell { decode: Cell.decodeCell }

fn pending<T: Decoder>(value: &T) -> Effect<i32 ! Problem | Extra> {
  return Decoder.decode(value)
}

fn observe(result: Result<i32, Problem | Extra>) -> i32 {
  return match move result {
      Result<i32, Problem | Extra>.Success { value } => value
      Result<i32, Problem | Extra>.Failure { error } => match move error {
        Problem {} => 0
        Extra {} => 0
      }
  }
}

pub fn main() -> i32 {
  let cell = Cell { code: 42 }
  return observe(run Effect.result(pending<Cell>(&cell)))
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('retains exact caller requirements while widening witness access and rows', () =>
  Effect.gen(function* () {
    const module = 'bound-operation-witness/requirement-access-widening'
    const snapshot = yield* analyzed(
      module,
      `import silk.effect as Effect
service Clock {}
service Meter {}
struct FixedClock { token: i32 }
struct FixedMeter { token: i32 }
impl Clock for FixedClock {}
impl Meter for FixedMeter {}

interface Decoder {
  effect fn decode(value: &mut Self) -> i32 ? &mut Clock | &Meter
}

struct Cell { code: i32 }

effect fn readClock() -> i32 ? &Clock { return 42 }

effect fn decodeCell(value: &Cell) -> i32 ? &Clock {
  return run readClock()
}

impl Decoder for Cell { decode: Cell.decodeCell }

fn pending<T: Decoder>(value: &mut T) -> Effect<i32 ? &mut Clock | &Meter> {
  return Decoder.decode(value)
}

pub fn main() -> i32 {
  let mut cell = Cell { code: 0 }
  let mut clock = FixedClock { token: 0 }
  let meter = FixedMeter { token: 0 }
  let provided = pending<Cell>(&mut cell)
    |> Effect.provideMut(&mut clock)
    |> Effect.provide(&meter)
  return run provided
}`,
    )
    assert.deepEqual(messages(snapshot), [])

    const hir = Projections.hirOf(snapshot, module)
    const pending = hir?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'pending',
    )
    assert.strictEqual(pending?.contract._tag, 'Contract')
    if (pending?.contract._tag !== 'Contract') return
    assert.isTrue(Type.isEffect(pending.contract.result))
    if (!Type.isEffect(pending.contract.result)) return
    assert.deepEqual(
      Type.requirementMembers(pending.contract.result).map((requirement) => ({
        capability: Type.encode(requirement.capability),
        access: requirement.access,
      })),
      [
        { capability: `${module}.Clock`, access: 'Exclusive' },
        { capability: `${module}.Meter`, access: 'Shared' },
      ],
    )

    const call = pending.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find((expression) => expression._tag === 'InterfaceOperationCall')
    assert.strictEqual(call?._tag, 'InterfaceOperationCall')
    if (call?._tag !== 'InterfaceOperationCall') return
    assert.deepEqual(
      call.contract.requirementRow.requirements.map((requirement) => ({
        capability: Type.encode(requirement.capability),
        access: requirement.access,
      })),
      [
        { capability: `${module}.Clock`, access: 'Exclusive' },
        { capability: `${module}.Meter`, access: 'Shared' },
      ],
    )

    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('wraps an operator-spelled pure source witness in Effect', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'bound-operation-witness/effect-operator-boundary',
      `interface Combined {
  operator + effect fn add(left: &Self, right: &Self) -> Self
}

struct Cell { code: i32 }

fn cellAdd(left: &Cell, right: &Cell) -> Cell {
  return Cell { code: left.code + right.code }
}

impl Combined for Cell { add: Cell.cellAdd }
fn combined<T: Combined>(left: T, right: T) -> T { return run ((&left) + (&right)) }

pub fn main() -> i32 {
  let cell = combined<Cell>(Cell { code: 20 }, Cell { code: 1 })
  return cell.code + 21
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('lets one bound operation select an intrinsic witness and a source witness', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'bound-operation-witness/mixed-witnesses',
      `interface Ranked {
  operator < fn lessThan(left: &Self, right: &Self) -> bool
}

struct Cell { weight: i32 }

fn cellLessThan(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Ranked for Cell { lessThan: Cell.cellLessThan }
impl Ranked for i32 { lessThan: Intrinsic.i32LessThan }

fn ranksBelow<T: Ranked>(left: T, right: T) -> bool { return (&left) < (&right) }

pub fn main() -> i32 {
  if ranksBelow<Cell>(Cell { weight: 20 }, Cell { weight: 22 }) {
    if ranksBelow<i32>(20, 22) { return 42 }
  }
  return 1
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('borrows a bound operand whose type the interface never parameterizes', () =>
  Effect.gen(function* () {
    // The contract shape a seeded hash wants: one operand is the key and the other is a fixed type
    // of the interface's own choosing, so operands are borrowed at their own types, not the
    // provider's.
    const value = yield* evaluatedValue(
      'bound-operation-witness/seeded-operand',
      `import silk.i32 as i32
import silk.u64 as u64
struct Seed { value: u64 }

interface Keyed {
  fn hash(value: &Self, seed: &Seed) -> u64
}

struct Cell { weight: i32 }

fn cellHash(value: &Cell, seed: &Seed) -> u64 {
  return u64.wrappingAdd(i32.toU64(value.weight), seed.value)
}

impl Keyed for Cell { hash: Cell.cellHash }

fn hashOf<T: Keyed>(value: T, seed: Seed) -> u64 { return Keyed.hash(&value, &seed) }

pub fn main() -> i32 {
  let hashed = hashOf<Cell>(Cell { weight: 20 }, Seed { value: 22 })
  return u64.toI32(hashed)
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('keeps the operator-spelled half of the same conformance unchanged', () =>
  Effect.gen(function* () {
    // Both spellings over one conformance, in one body: the operator still reaches `equals` and the
    // bound's name still reaches `digest`.
    const value = yield* evaluatedValue(
      'bound-operation-witness/both-spellings',
      `import silk.u64 as u64
${userKey}
fn probe<T: Keyed>(left: T, right: T) -> u64 {
  if (&left) == (&right) { return 0 }
  return Keyed.digest(&left, &right)
}

pub fn main() -> i32 {
  let same = probe<Cell>(Cell { weight: 7 }, Cell { weight: 7 })
  let mixed = probe<Cell>(Cell { weight: 20 }, Cell { weight: 22 })
  if same != 0 { return 1 }
  return u64.toI32(mixed)
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

/**
 * The second half of #155, which outlives the lowering fix: a call that passes analysis and lowers
 * to nothing is a silent miscompile, because lowering answers an unlowerable call by dropping it
 * and the specialized instance then fails MIR validation with no diagnostic naming a cause.
 *
 * The conformance checker admits exactly the two witness kinds lowering knows, so no source the
 * frontend accepts reaches the state today — that agreement is the invariant, and it is precisely
 * what broke when witness admissibility widened in #142 while lowering kept reading only intrinsics.
 * The check is therefore driven against an index in which the named witness is absent, which is the
 * shape any future disagreement takes.
 */
const module = 'bound-operation-witness/unlowerable'

const witnessed = `interface Keyed {
  fn digest(left: &Self, right: &Self) -> u64
}

struct Cell { weight: i32 }

fn cellDigest(left: &Cell, right: &Cell) -> u64 { return 7 }

impl Keyed for Cell { digest: Cell.cellDigest }

fn digestOf<T: Keyed>(left: T, right: T) -> u64 { return Keyed.digest(&left, &right) }

pub fn main() -> i32 {
  let out = digestOf<Cell>(Cell { weight: 1 }, Cell { weight: 2 })
  if out == 7 { return 42 }
  return 1
}`

it.effect('reports a bound operation whose selected witness cannot be lowered', () =>
  Effect.gen(function* () {
    const reachable = yield* analyzed(module, witnessed)
    assert.deepEqual(messages(reachable), [])
    // The same program with the witness function absent: the conformance still names it, so the
    // call still checks, and the index has nothing to lower the call to.
    const absent = yield* analyzed(module, witnessed.replace('fn cellDigest', 'fn cellDigested'))
    const violations = InstanceDiagnostics.unlowerableWitnessViolations(
      Analysis.instancesOf(reachable),
      Analysis.declarationIndex(absent),
    )
    assert.deepEqual(
      violations.map((violation) => `${violation.code}: ${violation.message}`),
      [`SEM0101: Keyed.digest has no witness that can be lowered for ${module}.Cell`],
    )
    // The diagnostic points at the call the specialization cannot run, not at the conformance.
    assert.deepEqual(
      violations.map((violation) =>
        witnessed.slice(violation.span.start, violation.span.end).trim(),
      ),
      ['Keyed.digest(&left, &right)'],
    )
  }),
)

it.effect('reports nothing for either witness kind a conformance may name', () =>
  Effect.gen(function* () {
    // The check must not fire on source that lowers: a source witness, an intrinsic witness, and
    // the operator spelling of both.
    const sourceWitness = yield* analyzed(module, witnessed)
    const intrinsicWitness = yield* analyzed(
      'bound-operation-witness/intrinsic-unlowerable',
      `interface Keyed {
  fn digest(left: Self, right: Self) -> Self
}

impl Keyed for i32 { digest: Intrinsic.i32WrappingAdd }

fn digestOf<T: Keyed>(left: T, right: T) -> T {
  return Keyed.digest(move left, move right)
}

pub fn main() -> i32 { return digestOf<i32>(20, 22) }`,
    )
    for (const snapshot of [sourceWitness, intrinsicWitness]) {
      assert.deepEqual(messages(snapshot), [])
      assert.deepEqual(
        InstanceDiagnostics.unlowerableWitnessViolations(
          Analysis.instancesOf(snapshot),
          Analysis.declarationIndex(snapshot),
        ),
        [],
      )
    }
  }),
)
