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

const applyAliased = (text: string, localSpelling: string): string | undefined => {
  const syntax = Parser.parse(Lexer.lex(SourceFile.make('main', ascii(text))))
  const plan = Option.getOrUndefined(
    ImportPlan.make({ syntax, module: 'silk/logging', spelling: 'Logger', localSpelling }),
  )
  const edits = plan?.changes.get('main')
  if (edits === undefined) return undefined
  let output = text
  for (const edit of edits.toReversed())
    output = `${output.slice(0, edit.span.start)}${edit.replacement}${output.slice(edit.span.end)}`
  return output
}

const applyNamespace = (
  source: string,
  module = 'silk/effect',
  spelling = 'Effect',
  localSpelling?: string,
): { readonly output: string; readonly localSpelling: string } | undefined => {
  const syntax = Parser.parse(Lexer.lex(SourceFile.make('main', ascii(source))))
  const plan = Option.getOrUndefined(
    ImportPlan.namespace({
      syntax,
      module,
      spelling,
      ...(localSpelling === undefined ? {} : { localSpelling }),
    }),
  )
  if (plan === undefined) return undefined
  let output = source
  const edits = plan.change?.changes.get('main') ?? []
  for (const edit of edits.toReversed())
    output = `${output.slice(0, edit.span.start)}${edit.replacement}${output.slice(edit.span.end)}`
  return { output, localSpelling: plan.localSpelling }
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

it('renders a collision-selected local alias for new and existing module imports', () => {
  assert.strictEqual(
    applyAliased('struct Logger {}\npub fn main() -> i32 { return 0 }', 'LoggingLogger'),
    'import silk.logging { Logger as LoggingLogger }\nstruct Logger {}\npub fn main() -> i32 { return 0 }',
  )
  assert.strictEqual(
    applyAliased(
      'import silk.logging { LogError }\nstruct Logger {}\npub fn main() -> i32 { return 0 }',
      'LoggingLogger',
    ),
    'import silk.logging { LogError, Logger as LoggingLogger }\nstruct Logger {}\npub fn main() -> i32 { return 0 }',
  )
})

it('inserts, extends, and reuses explicit namespace imports', () => {
  assert.deepEqual(applyNamespace('fn main() -> i32 { return 1 }'), {
    output: 'import silk.effect as Effect\nfn main() -> i32 { return 1 }',
    localSpelling: 'Effect',
  })
  assert.deepEqual(applyNamespace('import silk.effect { map }\nfn main() -> i32 { return 1 }'), {
    output: 'import silk.effect as Effect { map }\nfn main() -> i32 { return 1 }',
    localSpelling: 'Effect',
  })
  assert.deepEqual(applyNamespace('import silk.effect as Effect\nfn main() -> i32 { return 1 }'), {
    output: 'import silk.effect as Effect\nfn main() -> i32 { return 1 }',
    localSpelling: 'Effect',
  })
})

it('uses and preserves a deterministic collision alias for namespace imports', () => {
  assert.deepEqual(
    applyNamespace(
      'struct Effect {}\nfn main() -> i32 { return 1 }',
      'silk/effect',
      'Effect',
      'SilkEffect',
    ),
    {
      output: 'import silk.effect as SilkEffect\nstruct Effect {}\nfn main() -> i32 { return 1 }',
      localSpelling: 'SilkEffect',
    },
  )
  assert.deepEqual(
    applyNamespace('import silk.effect as Operations\nfn main() -> i32 { return 1 }'),
    {
      output: 'import silk.effect as Operations\nfn main() -> i32 { return 1 }',
      localSpelling: 'Operations',
    },
  )
})
