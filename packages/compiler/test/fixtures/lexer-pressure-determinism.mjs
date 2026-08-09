import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Lexer from '../../dist/Lexer.js'
import * as Mir from '../../dist/Mir.js'
import * as Ownership from '../../dist/Ownership.js'
import * as SourceFile from '../../dist/SourceFile.js'

const pressureSource = readFileSync(
  new URL('../../../../examples/language-pressure/lexer/main.silk', import.meta.url),
  'utf8',
)
const input =
  'pub struct effect fn run fail drop unsafe impl for return import as let mut once move match if else while break continue true false name'
const bytes = new TextEncoder().encode(input)
const literal = `b"${Array.from(bytes, (byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join(
  '',
)}"`
const kinds = [
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
]
const code = new Map(kinds.map((kind, index) => [kind, index]))
const canonical = Lexer.lex(SourceFile.make('lexer-pressure/determinism-oracle', bytes))
let fingerprint = 0
for (const token of canonical.tokens) {
  fingerprint =
    (fingerprint * 17 + code.get(token.kind) + token.span.start * 3 + token.span.end * 5) % 197
}
for (const diagnostic of canonical.diagnostics) {
  fingerprint = (fingerprint * 19 + diagnostic.span.start * 7 + diagnostic.span.end * 11) % 197
}
const source = pressureSource
  .replace('  let source = b"pub fn main() -> i32 { return 42 }\\n"', `  let source = ${literal}`)
  .replace(
    '  if value != 0 { let mismatch = 1 / 0 }',
    `  if value != ${fingerprint} { let mismatch = 1 / 0 }`,
  )

const snapshot = (target) =>
  Effect.runPromise(
    Analysis.ofSource('lexer-pressure/determinism', new TextEncoder().encode(source), target),
  )
const hash = (value) => createHash('sha256').update(value).digest('hex')
const json = (value) =>
  JSON.stringify(value, (_key, candidate) =>
    typeof candidate === 'bigint' ? candidate.toString() : candidate,
  )

const native = await snapshot('aarch64-apple-darwin')
const wasm = await snapshot('wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))

const encodeSnapshot = (self) => {
  const evaluated = Analysis.evaluate(self)
  return {
    diagnostics: Analysis.diagnostics(self),
    modules: Analysis.modules(self).map((module) => module.name),
    hir: hash(
      Analysis.modules(self)
        .map((module) => Hir.encode(Analysis.hirOf(self, module.name)))
        .join('\n'),
    ),
    ownership: hash(
      Analysis.modules(self)
        .map((module) => {
          const value = Analysis.ownershipOf(self, module.name)
          return value === undefined ? '' : Ownership.encode(value)
        })
        .join('\n'),
    ),
    layout: hash(Layout.encode(Analysis.layoutOf(self).value)),
    mir: hash(Mir.encode(Analysis.loweredMir(self))),
    evaluation: hash(json(Analysis.traceOf(evaluated))),
    outcome: evaluated._tag,
    allocations: Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag),
  }
}

process.stdout.write(
  json({
    native: encodeSnapshot(native),
    wasm: encodeSnapshot(wasm),
    nativeSymbols: nativeArtifact.symbols,
    wasmSymbols: wasmArtifact.symbols,
    nativeText: hash(nativeArtifact.ir),
    wasmText: hash(wasmArtifact.wat),
    nativeBytes: hash(nativeArtifact.bitcode),
    wasmBytes: hash(wasmArtifact.bytes),
  }),
)
