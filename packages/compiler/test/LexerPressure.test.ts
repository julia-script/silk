import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Lexer from '../src/Lexer.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import type * as Token from '../src/Token.js'

const sourcePath = fileURLToPath(
  new URL('../../../examples/language-pressure/lexer/main.silk', import.meta.url),
)
const pressureSource = readFileSync(sourcePath, 'utf8')
const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const tokenKinds = [
  'Whitespace',
  'LineComment',
  'DocComment',
  'ModuleDocComment',
  'Identifier',
  'DecimalInteger',
  'DecimalFloat',
  'TextLiteral',
  'ByteStringLiteral',
  'PubKeyword',
  'StructKeyword',
  'EffectKeyword',
  'FnKeyword',
  'RunKeyword',
  'FailKeyword',
  'DropKeyword',
  'UnsafeKeyword',
  'ImplKeyword',
  'ForKeyword',
  'ReturnKeyword',
  'ImportKeyword',
  'AsKeyword',
  'LetKeyword',
  'MutKeyword',
  'OnceKeyword',
  'MoveKeyword',
  'MatchKeyword',
  'IfKeyword',
  'ElseKeyword',
  'WhileKeyword',
  'BreakKeyword',
  'ContinueKeyword',
  'TrueKeyword',
  'FalseKeyword',
  'LeftParenthesis',
  'RightParenthesis',
  'LeftBrace',
  'RightBrace',
  'LeftBracket',
  'RightBracket',
  'Colon',
  'Semicolon',
  'Comma',
  'Equals',
  'EqualEqual',
  'FatArrow',
  'Minus',
  'Plus',
  'Star',
  'Slash',
  'Percent',
  'Bang',
  'BangEqual',
  'Question',
  'At',
  'Less',
  'LessEqual',
  'Greater',
  'GreaterEqual',
  'Pipe',
  'PipeGreater',
  'Ampersand',
  'Dot',
  'DotDot',
  'Arrow',
  'Invalid',
  'EndOfFile',
  'ConstKeyword',
  'InvalidStaticLiteral',
  'ServiceKeyword',
  'InterfaceKeyword',
] as const satisfies ReadonlyArray<Token.TokenKind>

const tokenCode: Readonly<Record<Token.TokenKind, number>> = Object.freeze({
  Whitespace: 0,
  LineComment: 1,
  DocComment: 2,
  ModuleDocComment: 3,
  Identifier: 4,
  DecimalInteger: 5,
  DecimalFloat: 6,
  TextLiteral: 7,
  ByteStringLiteral: 8,
  PubKeyword: 9,
  StructKeyword: 10,
  EffectKeyword: 11,
  FnKeyword: 12,
  RunKeyword: 13,
  FailKeyword: 14,
  DropKeyword: 15,
  UnsafeKeyword: 16,
  ImplKeyword: 17,
  ForKeyword: 18,
  ReturnKeyword: 19,
  ImportKeyword: 20,
  AsKeyword: 21,
  LetKeyword: 22,
  MutKeyword: 23,
  OnceKeyword: 24,
  MoveKeyword: 25,
  MatchKeyword: 26,
  IfKeyword: 27,
  ElseKeyword: 28,
  WhileKeyword: 29,
  BreakKeyword: 30,
  ContinueKeyword: 31,
  TrueKeyword: 32,
  FalseKeyword: 33,
  LeftParenthesis: 34,
  RightParenthesis: 35,
  LeftBrace: 36,
  RightBrace: 37,
  LeftBracket: 38,
  RightBracket: 39,
  Colon: 40,
  Semicolon: 41,
  Comma: 42,
  Equals: 43,
  EqualEqual: 44,
  FatArrow: 45,
  Minus: 46,
  Plus: 47,
  Star: 48,
  Slash: 49,
  Percent: 50,
  Bang: 51,
  BangEqual: 52,
  Question: 53,
  At: 54,
  Less: 55,
  LessEqual: 56,
  Greater: 57,
  GreaterEqual: 58,
  Pipe: 59,
  PipeGreater: 60,
  Ampersand: 61,
  Dot: 62,
  DotDot: 63,
  Arrow: 64,
  Invalid: 65,
  EndOfFile: 66,
  ConstKeyword: 67,
  InvalidStaticLiteral: 68,
  ServiceKeyword: 69,
  InterfaceKeyword: 70,
})

interface ExpectedToken {
  readonly kind: Token.TokenKind
  readonly code: number
  readonly start: number
  readonly end: number
}

interface ExpectedCase {
  readonly tokens: ReadonlyArray<ExpectedToken>
  readonly diagnostics: ReadonlyArray<{ readonly start: number; readonly end: number }>
  readonly fingerprint: number
}

const expectedCase = (input: string, id: string): ExpectedCase => {
  const lexical = Lexer.lex(SourceFile.make(id, ascii(input)))
  const tokens = lexical.tokens.map(({ kind, span }) =>
    Object.freeze({ kind, code: tokenCode[kind], start: span.start, end: span.end }),
  )
  const diagnostics = lexical.diagnostics.map(({ span }) =>
    Object.freeze({ start: span.start, end: span.end }),
  )
  let fingerprint = 0
  for (const token of tokens) {
    fingerprint = (fingerprint * 17 + token.code + token.start * 3 + token.end * 5) % 197
  }
  for (const diagnostic of diagnostics) {
    fingerprint = (fingerprint * 19 + diagnostic.start * 7 + diagnostic.end * 11) % 197
  }
  return Object.freeze({
    tokens: Object.freeze(tokens),
    diagnostics: Object.freeze(diagnostics),
    fingerprint,
  })
}

const replaceExactlyOnce = (source: string, search: string, replacement: string): string => {
  assert.strictEqual(source.split(search).length - 1, 1, search)
  return source.replace(search, replacement)
}

const byteLiteral = (input: string): string =>
  `b"${Array.from(ascii(input), (byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

const sourceFor = (
  input: string,
  id: string,
): { readonly source: string; readonly expected: ExpectedCase } => {
  const expected = expectedCase(input, id)
  const withInput = replaceExactlyOnce(
    pressureSource,
    '  let source = b"pub fn main() -> i32 { return 42 }\\n"',
    `  let source = ${byteLiteral(input)}`,
  )
  const source = replaceExactlyOnce(
    withInput,
    '  if value != 0 { let mismatch = 1 / 0 }',
    `  if value != ${expected.fingerprint} { let mismatch = 1 / 0 }`,
  )
  return Object.freeze({ source, expected })
}

const quotaSourceFor = (input: string, id: string, quota: number): string => {
  const generated = sourceFor(input, id).source
  const withAllocator = replaceExactlyOnce(
    generated,
    'impl Report for OutOfMemory {}',
    `impl Report for OutOfMemory {}

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  if self.remaining == 0 { fail OutOfMemory {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }`,
  )
  return replaceExactlyOnce(
    withAllocator,
    '  let mut allocator = SystemAllocator.make()',
    `  let mut allocator = QuotaAllocator { remaining: ${quota} }`,
  )
}

const corpus = [
  Object.freeze({ id: 'trivia', input: ' \t\r\n// line\n/// docs\n//! module\n//// plain\n' }),
  Object.freeze({
    id: 'keywords',
    input:
      'pub struct service interface effect fn run fail drop unsafe impl for return import as let mut once move match if else while break continue true false const name _x2',
  }),
  Object.freeze({ id: 'numbers', input: '0 42 1.25 2e3 3E+4 4e- 5..6' }),
  Object.freeze({
    id: 'literals',
    input:
      '"text\\"tail" b"\\x41" """line 1\r\n// body\n\\"\\"\\"""" b"""bytes\n""" future"value" "unterminated\nnext',
  }),
  Object.freeze({
    id: 'punctuation',
    input: '( ) { } [ ] : ; , = == => - + * / % ! != ? @ < <= > >= | |> & . .. ->',
  }),
  Object.freeze({ id: 'invalid', input: '#~ pub \u0000? $x' }),
] as const

interface ObservedToken {
  readonly code: number
  readonly start: number
  readonly end: number
}

const observations = (
  outcome: Extract<ReturnType<typeof Analysis.evaluate>, { _tag: 'Completed' }>,
) => {
  const scalarValues = (name: string): ReadonlyArray<number> =>
    outcome.trace.flatMap((event) => {
      if (event._tag !== 'Binding' || event.target.name !== name) return []
      if (event.value._tag === 'ScalarIntegerValue') return [Number(event.value.value)]
      if (event.value._tag === 'UsizeValue') return [Number(event.value.value)]
      return []
    })
  const codes = scalarValues('observeTokenKind')
  const starts = scalarValues('observeTokenStart')
  const ends = scalarValues('observeTokenEnd')
  assert.strictEqual(starts.length, codes.length)
  assert.strictEqual(ends.length, codes.length)
  const tokens: ReadonlyArray<ObservedToken> = codes.map((code, index) =>
    Object.freeze({ code, start: starts[index] ?? -1, end: ends[index] ?? -1 }),
  )
  const diagnosticStarts = scalarValues('observeDiagnosticStart')
  const diagnosticEnds = scalarValues('observeDiagnosticEnd')
  assert.strictEqual(diagnosticEnds.length, diagnosticStarts.length)
  return Object.freeze({
    tokens,
    diagnostics: diagnosticStarts.map((start, index) =>
      Object.freeze({ start, end: diagnosticEnds[index] ?? -1 }),
    ),
  })
}

it.effect('matches the canonical lexer over a complete table-driven corpus', () =>
  Effect.gen(function* () {
    const covered = new Set<Token.TokenKind>()
    for (const entry of corpus) {
      const generated = sourceFor(entry.input, `lexer-pressure/oracle/${entry.id}`)
      for (const token of generated.expected.tokens) covered.add(token.kind)
      const snapshot = yield* Analysis.ofSourceRealized(
        `lexer-pressure/${entry.id}`,
        ascii(generated.source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], entry.id)
      const outcome = Analysis.evaluate(snapshot)
      assert.strictEqual(outcome._tag, 'Completed', entry.id)
      if (outcome._tag !== 'Completed') continue
      const observed = observations(outcome)
      assert.deepEqual(
        observed.tokens,
        generated.expected.tokens.map(({ code, start, end }) => ({ code, start, end })),
        entry.id,
      )
      assert.deepEqual(observed.diagnostics, generated.expected.diagnostics, entry.id)
    }
    assert.deepEqual([...covered].sort(), [...tokenKinds].sort())
  }),
)

it.effect('publishes only general MIR operations for the pressure program', () =>
  Effect.gen(function* () {
    const generated = sourceFor(corpus[1].input, 'lexer-pressure/general-mir')
    const snapshot = yield* Analysis.ofSourceRealized(
      'lexer-pressure/general-mir',
      ascii(generated.source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const tags = new Set(
      Analysis.loweredMir(snapshot).functions.flatMap((fn) =>
        Mir.operations(fn).map((operation) => operation._tag),
      ),
    )
    assert.strictEqual(tags.has('Allocate'), true)
    assert.strictEqual(
      [...tags].some((tag) => tag.startsWith('Lex')),
      false,
    )
    assert.strictEqual(
      [...tags].some((tag) => tag.startsWith('Token')),
      false,
    )
  }),
)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-lexer-pressure-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

for (const representative of [corpus[1], corpus[3], corpus[5]]) {
  it.effect(
    `keeps ${representative.id} evaluation, native, and Wasm execution in parity`,
    () =>
      Effect.gen(function* () {
        const generated = sourceFor(
          representative.input,
          `lexer-pressure/representative/${representative.id}`,
        )
        const snapshot = yield* Analysis.ofSourceRealized(
          `lexer-pressure/representative/${representative.id}`,
          ascii(generated.source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [])
        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(evaluated._tag, 'Completed')
        if (evaluated._tag !== 'Completed') return
        assert.deepEqual(observations(evaluated).diagnostics, generated.expected.diagnostics)
        const acquires = evaluated.trace.filter(
          (event) => event._tag === 'AllocationAcquire',
        ).length
        const releases = evaluated.trace.filter(
          (event) => event._tag === 'AllocationRelease',
        ).length
        assert.isAbove(acquires, 0)
        assert.strictEqual(releases, acquires)

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual((instance.exports.silk_main as () => number)(), 0)

        const compiled = yield* Driver.compile({
          compilation: {
            root: SourceFile.make(
              `lexer-pressure/representative/${representative.id}`,
              ascii(generated.source),
            ),
          },
          toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
          profile: 'release',
          destination: join(destinationRoot, representative.id),
        }).pipe(Effect.provide(SourceResolver.empty))
        assert.strictEqual(compiled._tag, 'Compiled')
        if (compiled._tag !== 'Compiled') return
        const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
        assert.strictEqual(run.status, 0, run.stderr)
        assert.strictEqual(run.stderr, '')
      }),
    180_000,
  )
}

it.effect(
  'rolls back every token and diagnostic allocation failure on all three engines',
  () =>
    Effect.gen(function* () {
      const representative = corpus[3]
      const baseline = sourceFor(representative.input, 'lexer-pressure/quota/baseline')
      const baselineSnapshot = yield* Analysis.ofSourceRealized(
        'lexer-pressure/quota/baseline',
        ascii(baseline.source),
        'wasm32-unknown-unknown',
      )
      const baselineOutcome = Analysis.evaluate(baselineSnapshot)
      assert.strictEqual(baselineOutcome._tag, 'Completed')
      if (baselineOutcome._tag !== 'Completed') return
      const allocationCount = baselineOutcome.trace.filter(
        (event) => event._tag === 'AllocationAcquire',
      ).length
      assert.isAtLeast(allocationCount, 4)

      for (let quota = 0; quota <= allocationCount; quota += 1) {
        const id = `lexer-pressure/quota/q${quota}`
        const source = quotaSourceFor(representative.input, id, quota)
        const snapshot = yield* Analysis.ofSourceRealized(
          id,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [], id)
        const expectedTag = quota === allocationCount ? 'Completed' : 'UnhandledFailure'
        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(evaluated._tag, expectedTag, id)
        const acquired = evaluated.trace.filter(
          (event) => event._tag === 'AllocationAcquire',
        ).length
        const released = evaluated.trace.filter(
          (event) => event._tag === 'AllocationRelease',
        ).length
        assert.strictEqual(acquired, quota, id)
        assert.strictEqual(released, acquired, id)
        const again = Analysis.evaluate(snapshot)
        assert.strictEqual(again._tag, expectedTag, id)
        assert.deepEqual(
          again.trace.filter(
            (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
          ),
          evaluated.trace.filter(
            (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
          ),
          id,
        )

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual(
          (instance.exports.silk_main as () => number)(),
          quota === allocationCount ? 0 : 1,
          id,
        )

        const compiled = yield* Driver.compile({
          compilation: { root: SourceFile.make(id, ascii(source)) },
          toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
          profile: 'release',
          destination: join(destinationRoot, `quota-${quota}`),
        }).pipe(Effect.provide(SourceResolver.empty))
        assert.strictEqual(compiled._tag, 'Compiled', id)
        if (compiled._tag !== 'Compiled') continue
        const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
        assert.strictEqual(run.status, quota === allocationCount ? 0 : 1, `${id}: ${run.stderr}`)
        if (quota === allocationCount) assert.strictEqual(run.stderr, '')
        else assert.strictEqual(run.stderr, 'Error: silk/core.OutOfMemory\n')
      }
    }),
  180_000,
)
