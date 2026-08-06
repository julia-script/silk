import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (text: string): Effect.Effect<Analysis.Snapshot> =>
  Analysis.ofSource('golden/program', ascii(text))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

it.effect('discovers reachable call chains once and terminates recursion', () =>
  Effect.gen(function* () {
    const nested = Analysis.instancesOf(yield* snapshot(nestedSource))
    const direct = Analysis.instancesOf(yield* snapshot('pub fn main() -> I32 { return main() }'))
    const mutual = Analysis.instancesOf(
      yield* snapshot(`pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`),
    )
    assert.deepEqual(
      nested.instances.map((instance) => instance.key.declaration.name),
      ['main', 'identity'],
    )
    assert.deepEqual(
      direct.instances.map((instance) => instance.key.declaration.name),
      ['main'],
    )
    assert.deepEqual(
      mutual.instances.map((instance) => instance.key.declaration.name),
      ['main', 'other'],
    )
  }),
)

it.effect('excludes unreachable declarations and reports unavailable entries', () =>
  Effect.gen(function* () {
    const reachable = Analysis.instancesOf(
      yield* snapshot(`pub fn unused() -> I32 { return 1 }
pub fn main() -> I32 { return 42 }`),
    )
    const missing = Analysis.instancesOf(yield* snapshot('pub fn answer() -> I32 { return 42 }'))
    const parameterized = Analysis.instancesOf(
      yield* snapshot('pub fn main(value: I32) -> I32 { return value }'),
    )
    assert.deepEqual(
      reachable.instances.map((instance) => instance.key.declaration.name),
      ['main'],
    )
    assert.deepEqual(missing.entry, { _tag: 'Unavailable', reason: 'MissingEntry' })
    assert.deepEqual(parameterized.entry, { _tag: 'Unavailable', reason: 'ParameterizedEntry' })
  }),
)

it.effect('lowers discovered instances deterministically to verifier-clean MIR', () =>
  Effect.gen(function* () {
    const first = Mir.encode(Analysis.loweredMir(yield* snapshot(nestedSource)))
    const second = Mir.encode(Analysis.loweredMir(yield* snapshot(nestedSource)))
    assert.strictEqual(first, golden('lowered.mir.txt'))
    assert.strictEqual(first, second)
  }),
)

const bindingSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { let value = identity(42) let extra = 1 return value }`

const nestedMatchSource = `pub struct Token { kind: I32 }
pub struct Box { token: Token }
pub fn adjust(value: I32) -> I32 { return I32.add(value, 1) }
pub fn main() -> I32 {
  let boxed = Box { token: Token { kind: 41 } }
  return match move boxed {
    Box { token } => match move token {
      Token { kind: answer } => adjust(answer)
    }
  }
}`

it.effect('lowers bindings and ownership violations with generated cleanup or traps', () =>
  Effect.gen(function* () {
    const bindings = Analysis.loweredMir(yield* snapshot(bindingSource))
    const bindingFunction = bindings.functions.at(0)
    assert.deepEqual(Mir.verify(bindings), [])
    assert.strictEqual(Mir.encode(bindings), golden('bindings.mir.txt'))
    assert.deepEqual(
      bindingFunction === undefined
        ? []
        : Mir.operations(bindingFunction).map((operation) => operation._tag),
      ['Literal', 'Call', 'Move', 'Literal', 'Move', 'Drop', 'Drop'],
    )

    const violated = Analysis.loweredMir(
      yield* snapshot(`pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { let value = 42 return choose(move value, value) }`),
    )
    const violatedFunction = violated.functions.at(0)
    const outcome =
      violatedFunction === undefined ? undefined : Mir.outcomes(violatedFunction).at(0)
    assert.strictEqual(outcome?._tag, 'Trap')
    assert.strictEqual(outcome?._tag === 'Trap' ? outcome.reason : '', 'ownership violation')
  }),
)

it.effect('lowers built-ins and unavailable bodies to explicit trapping MIR', () =>
  Effect.gen(function* () {
    const builtins = Analysis.loweredMir(
      yield* snapshot('pub fn main() -> I32 { return I32.subtract(I32.multiply(6, 7), 0) }'),
    )
    const builtinFunction = builtins.functions.at(0)
    assert.deepEqual(
      builtinFunction === undefined
        ? []
        : Mir.operations(builtinFunction).map((operation) =>
            operation._tag === 'Binary' ? `Binary:${operation.operator}` : operation._tag,
          ),
      ['Literal', 'Literal', 'Binary:Multiply', 'Literal', 'Binary:Subtract'],
    )
    const unavailable = Analysis.loweredMir(
      yield* snapshot('pub fn main() -> I32 { return missing() }'),
    )
    const unavailableFunction = unavailable.functions.at(0)
    assert.strictEqual(
      unavailableFunction === undefined ? undefined : Mir.outcomes(unavailableFunction).at(0)?._tag,
      'Trap',
    )
  }),
)

it.effect('discovers calls and lowers nested matches as structured acyclic operations', () =>
  Effect.gen(function* () {
    const result = yield* snapshot(nestedMatchSource)
    assert.deepEqual(
      Analysis.diagnostics(result).map((diagnostic) => [diagnostic.code, diagnostic.message]),
      [],
    )
    assert.deepEqual(
      Analysis.instancesOf(result).instances.map((instance) => instance.key.declaration.name),
      ['main', 'adjust'],
    )
    const mir = Analysis.loweredMir(result)
    assert.deepEqual(Mir.verify(mir), [])
    const main = mir.functions.find((fn) => fn.id.name === 'main')
    const matches =
      main === undefined
        ? []
        : Mir.operations(main).filter((operation) => operation._tag === 'Match')
    assert.strictEqual(matches.length, 2)
    assert.strictEqual(matches.at(0)?.arms.at(0)?.selected.operations.at(0)?._tag, 'Match')
    assert.strictEqual(matches.at(0)?.decisions.at(0)?.member.name, 'Box')
    assert.strictEqual(matches.at(1)?.decisions.at(0)?.member.name, 'Token')
    assert.strictEqual(
      Mir.encode(mir),
      Mir.encode(Analysis.loweredMir(yield* snapshot(nestedMatchSource))),
    )
  }),
)

it.effect('rejects hand-built match decisions before evaluation or emission', () =>
  Effect.gen(function* () {
    const mir = Analysis.loweredMir(yield* snapshot(nestedMatchSource))
    let changed = false
    const malformed: Mir.Module = {
      ...mir,
      functions: mir.functions.map((fn) => ({
        ...fn,
        regions: fn.regions.map((region) =>
          region._tag !== 'OperationRegion'
            ? region
            : {
                ...region,
                operations: region.operations.map((operation) => {
                  if (changed || operation._tag !== 'Match') return operation
                  changed = true
                  return { ...operation, decisions: [] }
                }),
              },
        ),
      })),
    }

    assert.strictEqual(changed, true)
    assert.include(
      Mir.verify(malformed).map((violation) => violation.rule),
      'InvalidMatchDecision',
    )
  }),
)

const branchProgram =
  'pub fn main() -> I32 { let base = 40 if I32.equals(base, 40) { let bonus = 2 return I32.add(base, bonus) } return 0 }'

it.effect('lowers branch diamonds identically across runs', () =>
  Effect.gen(function* () {
    const first = Analysis.loweredMir(yield* snapshot(branchProgram))
    const second = Analysis.loweredMir(yield* snapshot(branchProgram))
    assert.deepEqual(Mir.verify(first), [])
    assert.strictEqual(Mir.encode(first), golden('branch-program.mir.txt'))
    assert.strictEqual(Mir.encode(first), Mir.encode(second))
  }),
)
