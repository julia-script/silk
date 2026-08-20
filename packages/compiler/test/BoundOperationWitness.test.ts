import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Instances from '../src/Instances.js'
import * as Mir from '../src/Mir.js'
import * as Type from '../src/Type.js'

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
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const direct = (instance.exports.silk_main as () => number)()

    return Object.freeze({ bootstrap, direct })
  })

/**
 * One user-declared key type answering two operations: an equivalence an operator spells and a
 * digest none does. Both witnesses are ordinary Silk, so the digest is reached only by naming the
 * bound and can only be answered by the provider's own function.
 */
const userKey = `interface Keyed {
  fn equals(left: &Self, right: &Self) -> bool
  fn digest(left: &Self, right: &Self) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellDigest(left: &Cell, right: &Cell) -> u64 {
  return u64.wrappingAdd(i32.toU64(left.weight), i32.toU64(right.weight))
}

impl Keyed for Cell { equals: Cell.cellEquals digest: Cell.cellDigest }

fn digestOf<T: Keyed>(left: T, right: T) -> u64 { return Keyed.digest(&left, &right) }`

it.effect(
  'returns the source witness result from a non-operator bound call on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      // The digest is 20 + 22, computed by ordinary Silk the specialization selected — so a wrong
      // witness, a missing call, or a placeholder result cannot produce 42 by accident.
      const outcome = yield* twoEngineValue(
        'bound-operation-witness/user-digest',
        `${userKey}
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
      `interface Keyed {
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

it.effect('rejects a borrowed source witness for a value-owned interface contract', () =>
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
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [
        'SEM0083: Invalid conformance: Cell.decodeCell is incompatible with Decoder.decode: parameter value requires shared ownership but the interface promises take ownership',
      ],
    )
  }),
)

it.effect('requires explicit moves for operator-spelled value operands', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'bound-operation-witness/operator-value-ownership',
      `interface Combined {
  fn add(left: Self, right: Self) -> Self
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
        `import silk.result { Result, Success, Failure }

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
    Result<i32, Problem> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Problem> { error } => error.code
    }
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

it.effect('weakens implicit operator borrows to a source witness demand', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'bound-operation-witness/weaker-operator-access',
      `interface Combined {
  fn add(left: &mut Self, right: &mut Self) -> Self
}

struct Cell { code: i32 }

fn cellAdd(left: &Cell, right: &Cell) -> Cell {
  return Cell { code: left.code + right.code }
}

impl Combined for Cell { add: Cell.cellAdd }

fn combine<T: Combined>(left: T, right: T) -> T { return left + right }

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

interface Combined { fn add(left: &mut Self, right: &mut Self) -> i32 }
struct Cell { code: i32 }
fn cellAdd(left: &Cell, right: &Cell) -> i32 { return left.code + right.code }
impl Combined for Cell { add: Cell.cellAdd }

effect fn branch<Self: Combined>(flag: bool, left: Self, right: Self) -> i32 {
  let fixed = Fixed {}
  let pending = Intrinsic.bindRequirement<Counter>(Counter.read(left + right), &fixed)
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
    assert.deepEqual(Mir.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 84n)
  }),
)

it.effect('widens a pure source witness to the exact interface Effect contract', () =>
  Effect.gen(function* () {
    const module = 'bound-operation-witness/pure-effect-boundary'
    const source = `import silk.result { Result, Success, Failure }

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
    Result<i32, Problem> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Problem> { error } => 0
    }
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

    const pending = Analysis.hirOf(raw, module)?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'pending',
    )
    const bound = pending?.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find((expression) => expression._tag === 'BoundOperationCall')
    assert.strictEqual(bound?._tag, 'BoundOperationCall')
    if (bound?._tag !== 'BoundOperationCall') return
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
    const operations = Mir.operations(runner)
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
    const encoded = Mir.encode(rawMir)
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
      .flatMap(Mir.operations)
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
      `import silk.result { Result, Success, Failure }

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
    Result<i32, Problem | Extra> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Problem | Extra> { error } => match move error {
        Problem {} => 0
        Extra {} => 0
      }
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
      `service Clock {}
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

    const hir = Analysis.hirOf(snapshot, module)
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
      .find((expression) => expression._tag === 'BoundOperationCall')
    assert.strictEqual(call?._tag, 'BoundOperationCall')
    if (call?._tag !== 'BoundOperationCall') return
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
  effect fn add(left: &Self, right: &Self) -> Self
}

struct Cell { code: i32 }

fn cellAdd(left: &Cell, right: &Cell) -> Cell {
  return Cell { code: left.code + right.code }
}

impl Combined for Cell { add: Cell.cellAdd }
fn combined<T: Combined>(left: T, right: T) -> T { return run (left + right) }

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
  fn lessThan(left: &Self, right: &Self) -> bool
}

struct Cell { weight: i32 }

fn cellLessThan(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Ranked for Cell { lessThan: Cell.cellLessThan }
impl Ranked for i32 { lessThan: Intrinsic.i32LessThan }

fn ranksBelow<T: Ranked>(left: T, right: T) -> bool { return left < right }

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
      `struct Seed { value: u64 }

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
      `${userKey}
fn probe<T: Keyed>(left: T, right: T) -> u64 {
  if left == right { return 0 }
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
    const violations = Instances.unlowerableWitnessViolations(
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
        Instances.unlowerableWitnessViolations(
          Analysis.instancesOf(snapshot),
          Analysis.declarationIndex(snapshot),
        ),
        [],
      )
    }
  }),
)
