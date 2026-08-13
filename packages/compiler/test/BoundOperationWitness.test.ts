import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
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
  fn equals(left: T, right: T) -> bool
  fn digest(left: T, right: T) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellDigest(left: &Cell, right: &Cell) -> u64 {
  return u64.wrappingAdd(i32.toU64(left.weight), i32.toU64(right.weight))
}

impl Keyed<Cell> for Cell { equals: Cell.cellEquals digest: Cell.cellDigest }

fn digestOf<T: Keyed>(left: T, right: T) -> u64 { return Keyed.digest(left, right) }`

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
  fn digest(left: T, right: T) -> u64
}

struct Cell { weight: i32 }
struct Tag { code: u64 }

fn cellDigest(left: &Cell, right: &Cell) -> u64 { return 10 }
fn tagDigest(left: &Tag, right: &Tag) -> u64 { return u64.wrappingAdd(left.code, right.code) }

impl Keyed<Cell> for Cell { digest: Cell.cellDigest }
impl Keyed<Tag> for Tag { digest: Tag.tagDigest }

fn digestOf<T: Keyed>(left: T, right: T) -> u64 { return Keyed.digest(left, right) }

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

it.effect('lets one bound operation select an intrinsic witness and a source witness', () =>
  Effect.gen(function* () {
    // The intrinsic half of the same call site keeps selecting its sealed operation: the fallback
    // is reached only where the conformance names source.
    const value = yield* evaluatedValue(
      'bound-operation-witness/mixed-witnesses',
      `interface Keyed<T> {
  fn digest(left: T, right: T) -> T
}

struct Cell { weight: i32 }

fn cellDigest(left: &Cell, right: &Cell) -> Cell {
  return Cell { weight: left.weight + right.weight }
}

impl Keyed<Cell> for Cell { digest: Cell.cellDigest }
impl Keyed<i32> for i32 { digest: Intrinsic.i32WrappingAdd }

fn digestOf<T: Keyed>(left: T, right: T) -> T { return Keyed.digest(left, right) }

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
  fn hash(value: T, seed: Seed) -> u64
}

struct Cell { weight: i32 }

fn cellHash(value: &Cell, seed: &Seed) -> u64 {
  return u64.wrappingAdd(i32.toU64(value.weight), seed.value)
}

impl Keyed<Cell> for Cell { hash: Cell.cellHash }

fn hashOf<T: Keyed>(value: T, seed: Seed) -> u64 { return Keyed.hash(value, seed) }

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
  return Keyed.digest(left, right)
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
