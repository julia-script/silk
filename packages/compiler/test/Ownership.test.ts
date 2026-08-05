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
