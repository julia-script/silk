import { assert, it } from '@effect/vitest'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SemanticContext from '../src/SemanticContext.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Tir from '../src/Tir.js'
import * as TirCodec from '../src/TirCodec.js'
import { elaborate } from './support/elaborate.js'
import { raise } from './support/raise.js'

const analyze = (text: string) =>
  elaborate(Parser.parse(Lexer.lex(SourceFile.make('codec/main', new TextEncoder().encode(text)))))

/** One program per body category the contract names. */
const categories: ReadonlyArray<readonly [string, string]> = [
  ['ordinary', `fn add(left: i32, right: i32) -> i32 { let sum = left + right return sum }`],
  [
    'generic with borrows and a match',
    `struct Token { value: i32 }
union Maybe { Some { token: Token }, None }
fn pick<T>(value: T) -> T { return value }
fn touch(token: &mut Token) -> i32 { return token.value }
pub fn main() -> i32 {
  let mut values = [1, 2]
  values[0] = pick(2)
  let first = touch(&mut Token { value: 3 })
  let picked = match Maybe.None { Maybe.Some { token } => token.value, Maybe.None => 0 }
  return values[0] + picked + first
}`,
  ],
  [
    'compiler-made callable',
    `fn apply(input: i32) -> i32 {
  let offset = 2
  let transform = fn(value: i32) -> i32 { return value + offset }
  return transform(input)
}`,
  ],
  ['static', `static fn base() -> i32 { return 1 }\npub fn main() -> i32 { return base() }`],
  ['rejected', `fn broken() -> i32 { let value = missing() return value }`],
]

for (const [category, program] of categories)
  it(`round-trips every ${category} body through the canonical encoding`, () => {
    const result = analyze(program)
    const context = SemanticContext.make(result.authored)
    assert.isAbove(result.bodies.length, 0)
    for (const body of result.bodies) {
      // Every body publishes nodes, a `static fn` included.
      assert.isAbove(body.function.statements.length, 0)
      const nodes = Tir.nodesOf(body.function)
      assert.isAbove(nodes.length, 0)
      assert.deepEqual(
        nodes.map((node) => node.id.ordinal),
        nodes.map((_, ordinal) => ordinal),
      )
      for (const node of nodes) assert.strictEqual(Tir.nodeOf(body.function, node.id), node)
      const locals = body.function.locals ?? raise('published local table')
      assert.deepEqual(
        locals.map((local) => local.id.ordinal),
        locals.map((_, ordinal) => ordinal),
      )
      for (const local of locals) assert.strictEqual(Tir.localOf(body.function, local.id), local)
      assert.isArray(body.results.evidence)
      assert.isArray(body.results.causes)
      const text = TirCodec.encode(body)
      const decoded = TirCodec.decode(
        text,
        (id) =>
          result.bodies.find((candidate) => candidate.declaration.id.ordinal === id.ordinal)
            ?.declaration ?? raise('declaration of a decoded body'),
        context,
      )
      assert.deepEqual(decoded, body)
      assert.strictEqual(TirCodec.encode(decoded), text)
    }
  })

it('encodes no position: moved source gives the same bytes', () => {
  const [, program] = categories[1] ?? raise('program')
  const before = analyze(program)
  const after = analyze(`// moved\n\n${program}`)
  assert.deepEqual(after.bodies.map(TirCodec.encode), before.bodies.map(TirCodec.encode))
  for (const encoded of before.bodies.map(TirCodec.encode)) {
    const visit = (value: unknown): void => {
      if (typeof value !== 'object' || value === null) return
      if (Array.isArray(value)) {
        for (const item of value) visit(item)
        return
      }
      const record = value as Readonly<Record<string, unknown>>
      if (record['$'] === 'span') assert.deepEqual(record, { $: 'span' })
      for (const child of Object.values(record)) visit(child)
    }
    visit(JSON.parse(encoded))
  }
})

it('identifies each body as an artifact, a compiler-made one under its parent', () => {
  const [, program] = categories[2] ?? raise('program')
  const result = analyze(program)
  const own = result.bodies.find((body) => !body.hidden) ?? raise('source body')
  const made = result.bodies.find((body) => body.hidden) ?? raise('compiler-made body')
  assert.deepEqual(own.artifact.request, { _tag: 'Check' })
  assert.isUndefined(own.artifact.parent)
  assert.deepEqual(made.artifact.parent, own.artifact)
  assert.notStrictEqual(Tir.artifactKey(made.artifact), Tir.artifactKey(own.artifact))
  // Identity is authored, never positional: the same bodies elsewhere in the file are the same.
  const moved = analyze(`fn before() -> i32 { return 0 }\n${program}`)
  assert.includeMembers(
    moved.bodies.map((body) => Tir.artifactKey(body.artifact)),
    result.bodies.map((body) => Tir.artifactKey(body.artifact)),
  )
})
