import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Elaboration from '../src/Elaboration.js'
import * as Lexer from '../src/Lexer.js'
import * as Ownership from '../src/Ownership.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const acceptedSource = `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1, 2) }`
const damagedSource = `pub fn puzzle(value: Mystery) -> I32 { return value }
pub fn main() -> I32 { return missing() }`

const check = (id: string, text: string): Ownership.ModuleOwnership =>
  Ownership.checkModule(
    Elaboration.elaborateModule(Parser.parse(Lexer.lex(SourceFile.make(id, ascii(text))))),
  )

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
    Ownership.encode(check('golden://accepted.silk', acceptedSource)),
    golden('accepted.ownership.txt'),
  )
  assert.strictEqual(
    Ownership.encode(check('golden://damaged.silk', damagedSource)),
    golden('damaged.ownership.txt'),
  )
})

it('checks and encodes identically across repeated fresh runs', () => {
  const first = check('golden://repeat.silk', damagedSource)
  const second = check('golden://repeat.silk', damagedSource)

  assert.deepEqual(first, second)
  assert.strictEqual(Ownership.encode(first), Ownership.encode(second))
})

const bindingSource = `pub fn main() -> I32 { let first = 1 let second = 2 return first }`

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
    `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { let value = 42 return identity(move value) }`,
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
    `pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { let value = 42 return choose(move value, value) }`,
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
    `pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { let value = 42 return choose(value, move value) }`,
  )

  assert.strictEqual(facts.functions.at(1)?.verdict._tag, 'Satisfied')
  assert.strictEqual(facts.diagnostics.length, 0)
})

it('matches the binding ownership golden encoding byte-for-byte', () => {
  assert.strictEqual(
    Ownership.encode(check('golden://bindings.silk', bindingSource)),
    golden('bindings.ownership.txt'),
  )
})
