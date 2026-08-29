import { assert, it } from '@effect/vitest'
import * as Lexer from '../src/Lexer.js'
import * as ModuleSummary from '../src/ModuleSummary.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const summarize = (module: string, text: string): ModuleSummary.ModuleSummary =>
  ModuleSummary.make(Parser.parse(Lexer.lex(SourceFile.make(module, ascii(text)))))

it('extracts ordered imports and unique public declaration headers', () => {
  const summary = summarize(
    'app.main',
    `import silk.bytes { Bytes }
import app.log as Log
pub fn execute() -> i32 { return 1 }
fn helper() -> i32 { return 2 }
pub struct User {}
pub union Choice { Empty }
pub service Clock { fn now() -> i32 }
pub interface Show { fn show() -> i32 }
pub const answer: i32 = 42`,
  )

  assert.deepEqual(summary.imports, ['silk/bytes', 'app/log'])
  assert.deepEqual(
    summary.exports.map(({ spelling, declarationKind, namespace, ordinal }) => ({
      spelling,
      declarationKind,
      namespace,
      ordinal,
    })),
    [
      { spelling: 'execute', declarationKind: 'Function', namespace: 'Value', ordinal: 0 },
      { spelling: 'User', declarationKind: 'Struct', namespace: 'ValueAndType', ordinal: 2 },
      { spelling: 'Choice', declarationKind: 'Union', namespace: 'ValueAndType', ordinal: 3 },
      { spelling: 'Clock', declarationKind: 'Service', namespace: 'Type', ordinal: 4 },
      { spelling: 'Show', declarationKind: 'Interface', namespace: 'Type', ordinal: 5 },
      { spelling: 'answer', declarationKind: 'Constant', namespace: 'Value', ordinal: 6 },
    ],
  )
})

it('omits recovered and duplicate public names rather than guessing', () => {
  const summary = summarize(
    'broken',
    `pub fn same() -> i32 { return 1 }
pub fn same() -> i32 { return 2 }
fn shadowed() -> i32 { return 2 }
pub fn shadowed() -> i32 { return 2 }
pub fn () -> i32 { return 3 }
pub fn usable() -> i32 { return 4 }`,
  )

  assert.deepEqual(
    summary.exports.map((exported) => exported.spelling),
    ['usable'],
  )
})
