import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Instances from '../src/Instances.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

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

const analyzed = (name: string, source: string, target?: string) =>
  Analysis.ofSourceRealized(name, ascii(source), target)

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
    return outcome._tag === 'Completed' ? outcome.result.value : undefined
  })

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-bound-operation-witness-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/** Runs one source on the bootstrap evaluator, the direct WebAssembly backend, and native LLVM. */
const threeEngineValue = (name: string, source: string, artifact: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source, 'wasm32-unknown-unknown')
    assert.deepEqual(messages(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    const bootstrap = evaluated._tag === 'Completed' ? evaluated.result.value : undefined

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const direct = (instance.exports.silk_main as () => number)()

    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make(name, ascii(source)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, artifact),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    const run =
      compiled._tag === 'Compiled'
        ? spawnSync(compiled.path, [], { encoding: 'utf8' })
        : { status: undefined, stderr: 'native compilation did not produce an artifact' }

    return Object.freeze({ bootstrap, direct, native: run.status, stderr: run.stderr })
  })

/**
 * One user-declared key type answering two operations: an equivalence an operator spells and a
 * digest none does. Both witnesses are ordinary Silk, so the digest is reached only by naming the
 * bound and can only be answered by the provider's own function.
 */
const userKey = `interface Keyed<T> {
  fn equals(left: &T, right: &T) -> bool
  fn digest(left: &T, right: &T) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellDigest(left: &Cell, right: &Cell) -> u64 {
  return u64.wrappingAdd(i32.toU64(left.weight), i32.toU64(right.weight))
}

impl Keyed<Cell> for Cell { equals: Cell.cellEquals digest: Cell.cellDigest }

fn digestOf<T: Keyed>(left: T, right: T) -> u64 { return Keyed.digest(&left, &right) }`

it.effect(
  'returns the source witness result from a non-operator bound call on all three engines',
  () =>
    Effect.gen(function* () {
      // The digest is 20 + 22, computed by ordinary Silk the specialization selected — so a wrong
      // witness, a missing call, or a placeholder result cannot produce 42 by accident.
      const outcome = yield* threeEngineValue(
        'bound-operation-witness/user-digest',
        `${userKey}
pub fn main() -> i32 {
  let digest = digestOf<Cell>(Cell { weight: 20 }, Cell { weight: 22 })
  if digest == 42 { return u64.toI32(digest) }
  return 1
}`,
        'user-digest',
      )
      assert.strictEqual(outcome.bootstrap, 42)
      assert.strictEqual(outcome.direct, 42)
      assert.strictEqual(outcome.native, 42, outcome.stderr)
    }),
  120_000,
)

it.effect('reaches each provider’s own witness from one bound-operation call site', () =>
  Effect.gen(function* () {
    // Two providers, two source witnesses, one generic body. The two answers are unrelated, so a
    // single body cannot serve both by lowering the operation width-neutrally.
    const value = yield* evaluatedValue(
      'bound-operation-witness/per-specialization',
      `interface Keyed<T> {
  fn digest(left: &T, right: &T) -> u64
}

struct Cell { weight: i32 }
struct Tag { code: u64 }

fn cellDigest(left: &Cell, right: &Cell) -> u64 { return 10 }
fn tagDigest(left: &Tag, right: &Tag) -> u64 { return u64.wrappingAdd(left.code, right.code) }

impl Keyed<Cell> for Cell { digest: Cell.cellDigest }
impl Keyed<Tag> for Tag { digest: Tag.tagDigest }

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
      `interface Combined<T> {
  fn combine(left: T, right: T) -> i32
}

struct Cell { weight: i32 }

fn cellCombine(left: Cell, right: Cell) -> i32 { return left.weight + right.weight }

impl Combined<Cell> for Cell { combine: Cell.cellCombine }

fn combineOf<T: Combined>(left: T, right: T) -> i32 { return Combined.combine(left, right) }

pub fn main() -> i32 { return combineOf<Cell>(Cell { weight: 20 }, Cell { weight: 22 }) }`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('lets one bound operation select an intrinsic witness and a source witness', () =>
  Effect.gen(function* () {
    // The intrinsic half of the same call site keeps selecting its sealed operation: the fallback
    // is reached only where the conformance names source.
    const value = yield* evaluatedValue(
      'bound-operation-witness/mixed-witnesses',
      `interface Keyed<T> {
  fn digest(left: &T, right: &T) -> T
}

struct Cell { weight: i32 }

fn cellDigest(left: &Cell, right: &Cell) -> Cell {
  return Cell { weight: left.weight + right.weight }
}

impl Keyed<Cell> for Cell { digest: Cell.cellDigest }
impl Keyed<i32> for i32 { digest: Intrinsic.i32WrappingAdd }

fn digestOf<T: Keyed>(left: T, right: T) -> T { return Keyed.digest(&left, &right) }

pub fn main() -> i32 {
  let cell = digestOf<Cell>(Cell { weight: 20 }, Cell { weight: 1 })
  let scalar = digestOf<i32>(2147483647, 1)
  if scalar != -2147483648 { return 1 }
  return cell.weight * 2
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

interface Keyed<T> {
  fn hash(value: &T, seed: &Seed) -> u64
}

struct Cell { weight: i32 }

fn cellHash(value: &Cell, seed: &Seed) -> u64 {
  return u64.wrappingAdd(i32.toU64(value.weight), seed.value)
}

impl Keyed<Cell> for Cell { hash: Cell.cellHash }

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

const witnessed = `interface Keyed<T> {
  fn digest(left: &T, right: &T) -> u64
}

struct Cell { weight: i32 }

fn cellDigest(left: &Cell, right: &Cell) -> u64 { return 7 }

impl Keyed<Cell> for Cell { digest: Cell.cellDigest }

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
      `interface Keyed<T> {
  fn digest(left: T, right: T) -> T
}

impl Keyed<i32> for i32 { digest: Intrinsic.i32WrappingAdd }

fn digestOf<T: Keyed>(left: T, right: T) -> T { return Keyed.digest(left, right) }

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
