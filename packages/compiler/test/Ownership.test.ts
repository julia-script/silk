import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Lexer from '../src/Lexer.js'
import * as Ownership from '../src/Ownership.js'
import * as OwnershipEncoding from '../src/OwnershipEncoding.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'
import { elaborate, ownership } from './support/elaborate.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const acceptedSource = `pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(1, 2) }`
const damagedSource = `pub fn puzzle(value: Mystery) -> i32 { return value }
pub fn main() -> i32 { return missing() }`
const localSharedSource = `fn retain<T>(value: Intrinsic.SharedCore<T>) -> Intrinsic.SharedCore<T> {
  return move value
}
pub fn main() -> i32 { return 0 }`

const check = (id: string, text: string): Ownership.ModuleOwnership =>
  ownership(elaborate(Parser.parse(Lexer.lex(SourceFile.make(id, ascii(text))))))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it('publishes copyable binding facts live through the function body', () => {
  const facts = check('golden://accepted.silk', acceptedSource)
  const choose = facts.functions.at(0)

  assert.strictEqual(choose?.verdict._tag, 'Satisfied')
  assert.deepEqual(
    choose?.bindings.map((binding) => ({
      name: binding.name,
      category: binding.category._tag,
      from: binding.liveFrom.start,
      to: binding.liveTo.end,
    })),
    [
      { name: 'left', category: 'Copyable', from: 14, to: 59 },
      { name: 'right', category: 'Copyable', from: 24, to: 59 },
    ],
  )
})

it('plans one empty-release return exit per frozen-slice function', () => {
  const facts = check('golden://accepted.silk', acceptedSource)

  for (const fn of facts.functions) {
    assert.strictEqual(fn.exits.length, 1)
    assert.strictEqual(fn.exits.at(0)?.kind, 'Return')
    assert.deepEqual(fn.exits.at(0)?.releases, [])
  }
})

it('keeps unavailable verdicts explicit with causes', () => {
  const facts = check('golden://damaged.silk', damagedSource)
  const puzzle = facts.functions.at(0)
  const main = facts.functions.at(1)

  assert.strictEqual(puzzle?.verdict._tag, 'Unavailable')
  if (puzzle?.verdict._tag !== 'Unavailable') return
  assert.strictEqual(puzzle.verdict.cause?.code, 'SEM0001')
  assert.strictEqual(main?.verdict._tag, 'Unavailable')
  if (main?.verdict._tag !== 'Unavailable') return
  assert.strictEqual(main.verdict.cause?.code, 'SEM0004')
})

it('matches the ownership golden encodings byte-for-byte', () => {
  assert.strictEqual(
    OwnershipEncoding.encode(check('golden://accepted.silk', acceptedSource)),
    golden('accepted.ownership.txt'),
  )
  assert.strictEqual(
    OwnershipEncoding.encode(check('golden://damaged.silk', damagedSource)),
    golden('damaged.ownership.txt'),
  )
})

it('matches the local-shared ownership golden and repeats byte-for-byte in process', () => {
  const first = OwnershipEncoding.encode(check('golden://local-shared.silk', localSharedSource))
  const second = OwnershipEncoding.encode(check('golden://local-shared.silk', localSharedSource))

  assert.strictEqual(first, golden('local-shared.ownership.txt'))
  assert.strictEqual(second, first)
})

it('checks and encodes identically across repeated fresh runs', () => {
  const first = check('golden://repeat.silk', damagedSource)
  const second = check('golden://repeat.silk', damagedSource)

  assert.deepEqual(first, second)
  assert.strictEqual(OwnershipEncoding.encode(first), OwnershipEncoding.encode(second))
})

const bindingSource = `pub fn main() -> i32 { let first = 1 let second = 2 return first }`

it('ranges let bindings from their statement to the end of the body', () => {
  const facts = check('golden://bindings.silk', bindingSource)
  const main = facts.functions.at(0)

  assert.strictEqual(main?.verdict._tag, 'Satisfied')
  assert.deepEqual(
    main?.bindings.map((binding) => ({
      site: binding.site._tag,
      name: binding.name,
      from: binding.liveFrom.start,
      to: binding.liveTo.end,
    })),
    [
      { site: 'Let', name: 'first', from: 22, to: 66 },
      { site: 'Let', name: 'second', from: 36, to: 66 },
    ],
  )
  assert.strictEqual(facts.diagnostics.length, 0)
})

it('consumes an owner at explicit drop and rejects later use', () => {
  const facts = check(
    'ownership://explicit-drop.silk',
    `struct Token { value: i32 }
fn inspect(token: Token) -> i32 { return token.value }
fn main() -> i32 {
  let token = Token { value: 1 }
  drop token
  return inspect(token)
}`,
  )
  assert.include(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
  )
})

it('releases live let bindings in reverse binding order at the return exit', () => {
  const facts = check('golden://bindings.silk', bindingSource)
  const exit = facts.functions.at(0)?.exits.at(0)

  assert.deepEqual(
    exit?.releases.map((release) => release.binding.name),
    ['second', 'first'],
  )
})

it('ends liveness at a consuming move and skips the moved binding at the exit', () => {
  const facts = check(
    'golden://moved.silk',
    `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let value = 42 return identity(move value) }`,
  )
  const main = facts.functions.at(1)
  const binding = main?.bindings.at(0)

  assert.strictEqual(main?.verdict._tag, 'Satisfied')
  assert.strictEqual(binding?.name, 'value')
  assert.notStrictEqual(binding?.movedAt, undefined)
  assert.strictEqual(binding?.liveTo.end, binding?.movedAt?.end)
  assert.deepEqual(main?.exits.at(0)?.releases, [])
})

it('diagnoses a use after move as an OWN0001 violation with published facts', () => {
  const facts = check(
    'golden://violation.silk',
    `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { let value = 42 return choose(move value, value) }`,
  )
  const main = facts.functions.at(1)

  assert.strictEqual(main?.verdict._tag, 'Violation')
  if (main?.verdict._tag !== 'Violation') return
  assert.strictEqual(main.verdict.cause.code, 'OWN0001')
  assert.strictEqual(main.verdict.cause.phase, 'ownership')
  assert.deepEqual(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    ['OWN0001'],
  )
  const diagnostic = facts.diagnostics.at(0)
  assert.strictEqual(diagnostic?.relatedSpans?.at(0)?.label, 'moved here')
  assert.strictEqual(main.bindings.length, 1)
})

it('accepts an ordinary read before the consuming move', () => {
  const facts = check(
    'golden://read-then-move.silk',
    `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { let value = 42 return choose(value, move value) }`,
  )

  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Satisfied')
  assert.strictEqual(facts.diagnostics.length, 0)
})

it('matches the binding ownership golden encoding byte-for-byte', () => {
  assert.strictEqual(
    OwnershipEncoding.encode(check('golden://bindings.silk', bindingSource)),
    golden('bindings.ownership.txt'),
  )
})

const branchSource = `pub fn main() -> i32 { let outer = 2 if outer == 2 { let inner = 1 return inner } return outer }`

it('scopes arm bindings to their arm with per-return exits', () => {
  const facts = check('golden://arms.silk', branchSource)
  const main = facts.functions.at(0)

  assert.strictEqual(main?.verdict._tag, 'Satisfied')
  const exits = main?.exits ?? []
  assert.deepEqual(
    exits.map((exit) => ({
      kind: exit.kind,
      releases: exit.releases.map((release) => release.binding.name),
    })),
    [
      { kind: 'Return', releases: ['inner', 'outer'] },
      { kind: 'Return', releases: ['outer'] },
    ],
  )
})

it('releases an unmoved arm binding at the arm end when the arm falls through', () => {
  const facts = check(
    'golden://arm-end.silk',
    'pub fn main() -> i32 { if 1 == 1 { let side = 5 } return 0 }',
  )
  const main = facts.functions.at(0)
  const armEnd = main?.exits.find((exit) => exit.kind === 'ArmEnd')

  assert.notStrictEqual(armEnd, undefined)
  assert.deepEqual(
    armEnd?.releases.map((release) => release.binding.name),
    ['side'],
  )
})

it('keeps a value live when the only arm that moves it returns', () => {
  const facts = check(
    'golden://conditional-move.silk',
    `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let value = 1 if 1 == 1 { return identity(move value) } return value }`,
  )
  const main = facts.functions.at(1)

  assert.strictEqual(main?.verdict._tag, 'Satisfied')
  assert.deepEqual(facts.diagnostics, [])
})

it('consumes a take recipe on its first run and rejects a repeated run', () => {
  const facts = check(
    'ownership://take-effect.silk',
    `struct Payload { value: i32 }
effect fn inspect(payload: Payload) -> i32 { return payload.value }
pub fn main() -> i32 {
  let payload = Payload { value: 21 }
  let recipe = inspect(move payload)
  let first = run recipe
  return first + run recipe
}`,
  )

  assert.deepEqual(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    ['OWN0001'],
  )
  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Violation')
})

it('consumes a take effect parameter on its first run and rejects a repeated run', () => {
  const facts = check(
    'ownership://take-effect-parameter.silk',
    `pub effect fn twice<A, E, ?R>(self: once Effect<A ! E ? R>) -> A ! E ? R {
  let first = run self
  return run self
}`,
  )

  assert.deepEqual(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    ['OWN0001'],
  )
  assert.strictEqual(facts.functions.at(0)?.verdict._tag, 'Violation')
})

it('accepts a single run of a take effect parameter', () => {
  const facts = check(
    'ownership://take-effect-parameter-single.silk',
    `pub effect fn flattenLike<A, E, F, ?R, ?S>(
  self: once Effect<Effect<A ! F ? S> ! E ? R>
) -> A ! E | F ? R | S {
  let inner = run self
  return run inner
}`,
  )

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(facts.functions.at(0)?.verdict._tag, 'Satisfied')
})

it('allows a shared effect parameter to run repeatedly', () => {
  const facts = check(
    'ownership://shared-effect-parameter.silk',
    `pub effect fn twiceShared<?R>(self: Effect<i32 ? R>) -> i32 ? R {
  return (run self) + (run self)
}`,
  )

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(facts.functions.at(0)?.verdict._tag, 'Satisfied')
})

it('derives take-once execution from an effect-block moved capture', () => {
  const facts = check(
    'ownership://take-effect-block.silk',
    `struct Payload { value: i32 }
pub fn main(payload: Payload) -> i32 {
  let pending = effect { return move payload }
  let first = run pending
  let second = run pending
  return first.value + second.value
}`,
  )

  assert.include(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
  )
  assert.strictEqual(facts.functions.at(0)?.verdict._tag, 'Violation')
})

it('allows a shared copy-only recipe to run repeatedly', () => {
  const facts = check(
    'ownership://shared-effect.silk',
    `effect fn inspect(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let recipe = inspect(21)
  return (run recipe) + (run recipe)
}`,
  )

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Satisfied')
})

it('keeps a borrowed effect capture loan live until its last run', () => {
  const facts = check(
    'ownership://borrowed-effect.silk',
    `effect fn inspect(values: &[i32]) -> i32 { return values[0] }
pub fn main() -> i32 {
  let mut values = [1]
  let recipe = inspect(&values)
  values[0] = 2
  return run recipe
}`,
  )
  const main = facts.functions.at(1)
  const loan = main?.loans.at(0)

  assert.deepEqual(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    ['OWN0011'],
  )
  assert.strictEqual(main?.verdict._tag, 'Violation')
  assert.notStrictEqual(loan, undefined)
  assert.ok((loan?.endSpan.start ?? 0) > (loan?.startSpan.start ?? 0))
})

it('keeps an existing borrowed provider live until its last run', () => {
  const facts = check(
    'ownership://borrowed-provider.silk',
    `struct Clock { tick: i32 }
effect fn read() -> i32 ? &Clock { return 42 }
pub fn main() -> i32 {
  let mut clock = Clock { tick: 0 }
  let recipe = read() |> Intrinsic.bindRequirement(&clock)
  clock.tick = 1
  return run recipe
}`,
  )

  assert.deepEqual(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    ['OWN0011'],
  )
  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Violation')
})

it('moves an owned provider into a take-once Effect wrapper', () => {
  const facts = check(
    'ownership://moved-provider.silk',
    `struct Token { action: once fn() -> i32 }
struct Clock { token: Token }
fn tick() -> i32 { return 1 }
effect fn read() -> i32 ? &mut Clock { return 42 }
pub fn main() -> i32 {
  let clock = Clock { token: Token { action: tick } }
  let recipe = read() |> Intrinsic.bindRequirementOwned(move clock)
  let first = run recipe
  return first + run recipe
}`,
  )

  assert.include(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
  )
  assert.strictEqual(facts.functions.at(2)?.verdict._tag, 'Violation')
})

it('copies an owned Copy provider so its bound Effect remains repeatable', () => {
  const facts = check(
    'ownership://copied-provider.silk',
    `struct Clock { tick: i32 }
impl Copy for Clock {}
effect fn read() -> i32 ? &mut Clock { return 42 }
pub fn main() -> i32 {
  let clock = Clock { tick: 0 }
  let recipe = read() |> Intrinsic.bindRequirementOwned(move clock)
  return (run recipe) + (run recipe)
}`,
  )

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Satisfied')
})

it('allows an owner to change after the borrowed recipe has finished its last run', () => {
  const facts = check(
    'ownership://finished-borrowed-effect.silk',
    `effect fn inspect(values: &[i32]) -> i32 { return values[0] }
pub fn main() -> i32 {
  let mut values = [1]
  let recipe = inspect(&values)
  let seen = run recipe
  values[0] = 2
  return seen + values[0]
}`,
  )

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Satisfied')
})

it('treats Evaluate expressions as ownership roots without inventing bindings', () => {
  const moved = check(
    'ownership://evaluate-move.silk',
    `struct Token { value: i32 }
fn consume(token: Token) -> () { drop move token return () }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  consume(move token)
  consume(move token)
  return 0
}`,
  )
  const borrowed = check(
    'ownership://evaluate-borrow.silk',
    `effect fn inspect(values: &[i32]) -> () { return () }
pub fn main() -> i32 {
  let mut values = [1]
  let recipe = inspect(&values)
  run recipe
  values[0] = 2
  return values[0]
}`,
  )
  const propagated = check(
    'ownership://evaluate-propagation.silk',
    `struct Token { value: i32 }
struct Problem {}
effect fn stop() -> () ! Problem { fail move Problem {} }
effect fn outer() -> () ! Problem {
  let token = Token { value: 1 }
  run stop()
  return ()
}`,
  )
  const outer = propagated.functions.at(1)
  const propagation = outer?.exits.find((exit) => exit.kind === 'Propagation')

  assert.include(
    moved.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
  )
  assert.deepEqual(borrowed.diagnostics, [])
  assert.strictEqual(borrowed.functions.at(1)?.loans.length, 1)
  assert.deepEqual(propagated.diagnostics, [])
  assert.deepEqual(
    outer?.deferredBindings
      .filter((binding) => binding.site._tag === 'Let')
      .map((binding) => binding.name),
    ['token'],
  )
  assert.deepEqual(
    propagation?.releases.map((release) => release.binding.name),
    ['token'],
  )
})

it('keeps ownership checking active for acknowledged unsafe source calls', () => {
  const facts = check(
    'ownership://unsafe-call-move.silk',
    `struct Token { value: i32 }
unsafe fn consume(token: Token) -> () { drop move token return () }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  unsafe consume(move token)
  unsafe consume(move token)
  return 0
}`,
  )

  assert.include(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
  )
})

it('plans a stored callable environment and drops moved slots in reverse capture order', () => {
  const facts = check(
    'ownership://callable-environment.silk',
    `struct Token { value: i32 }
fn select(value: i32, first: Token, second: Token) -> i32 { return value }
pub fn main() -> i32 {
  let first = Token { value: 1 }
  let second = Token { value: 2 }
  let callback = select(move first, move second)
  return 0
}`,
  )
  const main = facts.functions.at(1)
  const environment = main?.callables.at(0)
  const callback = main?.bindings.find((binding) => binding.name === 'callback')

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(environment?.mode, 'Take')
  assert.deepEqual(environment?.dropOrder, [1, 0])
  assert.strictEqual(callback?.cleanup._tag, 'CallableCleanup')
  if (callback?.cleanup._tag !== 'CallableCleanup') return
  assert.deepEqual(
    callback.cleanup.slots.map((slot) => slot.ordinal),
    [1, 0],
  )
})

it('rejects a second invocation of a take-once callable binding', () => {
  const facts = check(
    'ownership://take-callable-reuse.silk',
    `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  let callback = consume(move token)
  let first = callback(20)
  let second = callback(22)
  return first + second
}`,
  )

  assert.include(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
  )
  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Violation')
})

it('retains a callable capture loan until the callable is dropped', () => {
  const blocked = check(
    'ownership://borrowed-callable.silk',
    `fn read(value: i32, values: &mut [i32]) -> i32 { return value }
pub fn main() -> i32 {
  let mut values = [1]
  let callback = read(&mut values)
  values[0] = 2
  return callback(42)
}`,
  )
  const released = check(
    'ownership://dropped-borrowed-callable.silk',
    `fn read(value: i32, values: &mut [i32]) -> i32 { return value }
pub fn main() -> i32 {
  let mut values = [1]
  let callback = read(&mut values)
  drop callback
  values[0] = 2
  return values[0]
}`,
  )

  assert.include(
    blocked.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0011',
  )
  assert.deepEqual(released.diagnostics, [])
  assert.strictEqual(released.functions.at(1)?.loans.at(0)?.origin, 'CallableCapture')
})

it('checks an affine pipeline input once before its callable section', () => {
  const facts = check(
    'ownership://affine-pipeline.silk',
    `struct Token { value: i32 }
fn consume(token: Token, adjustment: i32) -> i32 { return token.value + adjustment }
pub fn main() -> i32 {
  let token = Token { value: 40 }
  let result = move token |> consume(2)
  drop token
  return result
}`,
  )

  assert.deepEqual(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    ['OWN0001'],
  )
})

it('assigns Allocation one private active reclaim ticket', () => {
  const facts = check(
    'ownership://allocation-ticket.silk',
    `fn consume(allocation: Allocation) -> i32 { return 42 }
pub fn main() -> i32 { return 0 }`,
  )
  const release = facts.functions.at(0)?.exits.at(0)?.releases.at(0)

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(release?.binding.name, 'allocation')
  assert.strictEqual(release?.binding.category._tag, 'MoveOnly')
  assert.deepEqual(release?.cleanup, {
    _tag: 'AllocationCleanup',
    type: Type.allocation,
    ticket: 'ActiveReclaimTicket',
  })
})

it('ends exclusive service access when a provided operation returns', () => {
  const facts = check(
    'ownership://service-provider-loan.silk',
    `service Counter { effect fn next() -> i32 ? &mut Counter }
struct Provider { value: i32 }
effect fn next(self: &mut Provider) -> i32 { return self.value }
impl Counter for Provider { next: Provider.next }
effect fn readTwice(provider: Provider) -> i32 {
  let mut allocator = move provider
  let firstRecipe = Counter.next() |> Intrinsic.bindRequirementMut(&mut allocator)
  let first = run firstRecipe
  let secondRecipe = Counter.next() |> Intrinsic.bindRequirementMut(&mut allocator)
  let second = run secondRecipe
  return first + second
}
pub fn main() -> i32 { return 0 }`,
  )

  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(facts.functions.at(0)?.verdict._tag, 'Satisfied')
})

it('ends unsafe lexical owners at the explicit boundary', () => {
  const facts = check(
    'ownership://unsafe-boundary.silk',
    'struct Token { value: i32 } pub fn main() -> i32 { unsafe { let token = Token { value: 1 } } return 42 }',
  )
  const scopeExit = facts.functions.at(0)?.exits.find((exit) => exit.kind === 'ScopeEnd')
  assert.deepEqual(facts.diagnostics, [])
  assert.strictEqual(scopeExit?.releases.at(0)?.binding.name, 'token')
  assert.strictEqual(scopeExit?.releases.at(0)?.cleanup._tag, 'StructCleanup')
})

it('keeps a Slot loan active until the lexical Slot is consumed', () => {
  const facts = check(
    'ownership://slot-loan.silk',
    `fn misuse(buffer: RawBuffer<i32>) -> i32 {
  let mut owner = move buffer
  unsafe {
    let slot = Intrinsic.rawBufferSlot(&mut owner, 0)
    drop owner
    let value = Intrinsic.slotTake(move slot)
    return value
  }
  return 0
}`,
  )

  assert.include(
    facts.diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0011',
  )
  assert.strictEqual(facts.functions.at(0)?.loans.at(0)?.origin, 'ValueBorrow')
})

it('reports moves and double drops inside lazy effect bodies', () => {
  const doubled = check(
    'ownership://effect-body-double-drop.silk',
    `import silk.effect as Effect
struct Token { value: i32 }
struct Problem { code: i32 }
effect fn store() -> i32 ! Problem {
  let token = Token { value: 1 }
  drop token
  drop token
  return 1
}
effect fn recover(error: Problem) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
  )
  assert.deepEqual(
    doubled.diagnostics.map((diagnostic) => diagnostic.code),
    ['OWN0001'],
  )

  // A healthy body stays clean, and the deferred walk publishes no facts for it: the body's
  // exits and bindings belong to its own compiled function, not to the enclosing one.
  const healthy = check(
    'ownership://effect-body-healthy.silk',
    `import silk.effect as Effect
struct Token { value: i32 }
struct Problem { code: i32 }
effect fn store() -> i32 ! Problem {
  let token = Token { value: 1 }
  drop token
  return 1
}
effect fn recover(error: Problem) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
  )
  assert.deepEqual(healthy.diagnostics, [])
  const store = healthy.functions.at(0)
  assert.deepEqual(
    store?.bindings.filter((binding) => binding.site._tag === 'Let'),
    [],
  )
})

it('publishes an effect fn body pattern binding as a deferred fact that allBindings joins', () => {
  const source = (modifiers: string): string =>
    `struct Left { value: i32 }
struct Right { value: i32 }
${modifiers} fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } => value
    Right { value } => value
  }
}`
  const plain = check('ownership://pattern-plain.silk', source('pub'))
  const deferred = check('ownership://pattern-effect.silk', source('pub effect'))
  const names = (facts: ReadonlyArray<Ownership.BindingFact>): ReadonlyArray<string | undefined> =>
    facts.filter((binding) => binding.site._tag === 'Pattern').map((binding) => binding.name)

  assert.deepEqual(plain.diagnostics, [])
  assert.deepEqual(deferred.diagnostics, [])

  // A plain body's pattern bindings reach the enclosing fact set.
  const inspected = plain.functions.at(0)
  assert.deepEqual(names(inspected?.bindings ?? []), ['value', 'value'])
  assert.deepEqual(names(inspected?.deferredBindings ?? []), [])

  // An effect fn is entirely a deferred body, so the same patterns publish on the other field.
  // This is the documented publication boundary, not a tracking hole — see FunctionOwnership.
  const lazy = deferred.functions.at(0)
  assert.deepEqual(names(lazy?.bindings ?? []), [])
  assert.deepEqual(names(lazy?.deferredBindings ?? []), ['value', 'value'])

  // allBindings is the join a consumer needing completeness must use: it answers the same for
  // both shapes, where reading `bindings` alone does not.
  assert.deepEqual(names(Ownership.allBindings(inspected)), ['value', 'value'])
  assert.deepEqual(names(Ownership.allBindings(lazy)), ['value', 'value'])
  assert.deepEqual(Ownership.allBindings(undefined), [])
})
