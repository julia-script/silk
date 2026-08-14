import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as ImportPlan from '../src/ImportPlan.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const apply = (text: string, module = 'lib', spelling = 'Wanted'): string | undefined => {
  const syntax = Parser.parse(Lexer.lex(SourceFile.make('main', ascii(text))))
  const plan = Option.getOrUndefined(ImportPlan.make({ syntax, module, spelling }))
  const edits = plan?.changes.get('main')
  if (edits === undefined) return undefined
  let output = text
  for (const edit of edits.toReversed())
    output = `${output.slice(0, edit.span.start)}${edit.replacement}${output.slice(edit.span.end)}`
  return output
}

it('inserts a new selected-member import before declarations', () => {
  assert.strictEqual(
    apply('fn main() -> i32 { return 1 }'),
    'import lib { Wanted }\nfn main() -> i32 { return 1 }',
  )
  assert.strictEqual(
    apply('//! Module documentation.\n\nfn main() -> i32 { return 1 }'),
    '//! Module documentation.\n\nimport lib { Wanted }\nfn main() -> i32 { return 1 }',
  )
})

it('renders canonical module identities with Silk import-path spelling', () => {
  assert.strictEqual(
    apply('fn main() -> () {\n  Vector\n  return ()\n}', 'silk/vector', 'Vector'),
    'import silk.vector { Vector }\nfn main() -> () {\n  Vector\n  return ()\n}',
  )
  assert.strictEqual(
    apply(
      'import silk.vector { make }\nfn main() -> () {\n  Vector\n  return ()\n}',
      'silk/vector',
      'Vector',
    ),
    'import silk.vector { make, Vector }\nfn main() -> () {\n  Vector\n  return ()\n}',
  )
})

it('extends inline, multiline, hybrid, and namespace-aliased imports', () => {
  assert.strictEqual(
    apply('import lib { Existing }\nfn main() -> i32 { return 1 }'),
    'import lib { Existing, Wanted }\nfn main() -> i32 { return 1 }',
  )
  assert.strictEqual(
    apply('import lib {\n  Existing,\n}\nfn main() -> i32 { return 1 }'),
    'import lib {\n  Existing,\n  Wanted,\n}\nfn main() -> i32 { return 1 }',
  )
  assert.strictEqual(
    apply('import lib as Library { Existing as Local }\nfn main() -> i32 { return 1 }'),
    'import lib as Library { Existing as Local, Wanted }\nfn main() -> i32 { return 1 }',
  )
  assert.strictEqual(
    apply('import lib as Library\nfn main() -> i32 { return 1 }'),
    'import lib as Library { Wanted }\nfn main() -> i32 { return 1 }',
  )
})

it('preserves member aliases and withholds duplicate or damaged plans', () => {
  assert.strictEqual(
    apply('import lib { Existing as Wanted }\nfn main() -> i32 { return 1 }'),
    undefined,
  )
  assert.strictEqual(apply('import lib { Wanted }\nfn main() -> i32 { return 1 }'), undefined)
  assert.strictEqual(
    apply('import lib { Existing, }\nimport lib as Library\nfn main() -> i32 { return 1 }'),
    undefined,
  )
  assert.strictEqual(apply('import lib { Existing\nfn main() -> i32 { return 1 }'), undefined)
})
