import * as LiteralForm from '@silk-effect/compiler/LiteralForm'
import type * as Token from '@silk-effect/compiler/Token'

/** Every keyword token kind the compiler defines. */
export type KeywordKind = Extract<Token.TokenKind, `${string}Keyword`>

/**
 * Spelling of every keyword token kind. Exhaustive over the compiler's keyword kinds, so adding or
 * removing a keyword in the compiler fails this package's typecheck until the grammar follows.
 */
export const keywords: Record<KeywordKind, string> = {
  PubKeyword: 'pub',
  StructKeyword: 'struct',
  ServiceKeyword: 'service',
  InterfaceKeyword: 'interface',
  RoleKeyword: 'role',
  FnKeyword: 'fn',
  EffectKeyword: 'effect',
  RunKeyword: 'run',
  FailKeyword: 'fail',
  DropKeyword: 'drop',
  UnsafeKeyword: 'unsafe',
  ImplKeyword: 'impl',
  ForKeyword: 'for',
  ReturnKeyword: 'return',
  ImportKeyword: 'import',
  AsKeyword: 'as',
  LetKeyword: 'let',
  ConstKeyword: 'const',
  MutKeyword: 'mut',
  OnceKeyword: 'once',
  MoveKeyword: 'move',
  MatchKeyword: 'match',
  IfKeyword: 'if',
  ElseKeyword: 'else',
  WhileKeyword: 'while',
  BreakKeyword: 'break',
  ContinueKeyword: 'continue',
  TrueKeyword: 'true',
  FalseKeyword: 'false',
}

/** Control-flow keyword kinds (theme: `keyword.control`). */
const controlKeywordKinds = [
  'IfKeyword',
  'ElseKeyword',
  'WhileKeyword',
  'BreakKeyword',
  'ContinueKeyword',
  'ReturnKeyword',
  'MatchKeyword',
] as const satisfies ReadonlyArray<KeywordKind>

const booleanSpellings = [keywords.TrueKeyword, keywords.FalseKeyword]
const controlSpellings: ReadonlyArray<string> = controlKeywordKinds.map((kind) => keywords[kind])
const controlSpellingSet = new Set(controlSpellings)
// `fn` is matched by the function-declaration capture rule, not the storage alternation.
const declarationSpellings = Object.values(keywords).filter(
  (spelling) =>
    !booleanSpellings.includes(spelling) &&
    !controlSpellingSet.has(spelling) &&
    spelling !== keywords.FnKeyword,
)

/** One TextMate match rule (optional top-level name when using captures). */
export interface GrammarPattern {
  readonly name?: string
  readonly match?: string
  readonly captures?: { readonly [group: string]: { readonly name: string } }
  readonly begin?: string
  readonly end?: string
  readonly beginCaptures?: { readonly [group: string]: { readonly name: string } }
  readonly endCaptures?: { readonly [group: string]: { readonly name: string } }
  readonly include?: string
  readonly patterns?: ReadonlyArray<GrammarPattern>
}

/** The subset of the TextMate grammar shape Silk needs. */
export interface Grammar {
  readonly name: string
  readonly scopeName: string
  readonly fileTypes: ReadonlyArray<string>
  readonly patterns: ReadonlyArray<GrammarPattern>
}

const literalPatterns: ReadonlyArray<GrammarPattern> = LiteralForm.forms.map((form) => {
  const quote = String.fromCharCode(form.delimiter)
  const delimiter = quote.repeat(form.delimiterWidth)
  const byte = form.category === 'Bytes'
  const character = form.category === 'Character'
  const width = character ? 'single' : form.delimiterWidth === 3 ? 'triple' : 'double'
  const raw = form.escapePolicy === 'Raw'
  const begin = form.modifier.length === 0 ? `(${delimiter})` : `(${form.modifier})(${delimiter})`
  const delimiterCapture = form.modifier.length === 0 ? '1' : '2'
  const guard = raw ? '' : '(?<!\\\\)(?:\\\\\\\\)*'
  return {
    name: `string.quoted.${width}${byte ? '.byte' : raw ? '.raw' : ''}.silk`,
    begin,
    end: form.delimiterWidth === 3 ? `${guard}(${delimiter})` : `${guard}(${quote})|$`,
    beginCaptures: {
      ...(form.modifier.length === 0
        ? {}
        : { '1': { name: `storage.modifier.${byte ? 'byte' : 'raw'}.silk` } }),
      [delimiterCapture]: { name: 'punctuation.definition.string.begin.silk' },
    },
    endCaptures: {
      '1': { name: 'punctuation.definition.string.end.silk' },
    },
    patterns: raw
      ? []
      : [
          {
            name: 'constant.character.escape.silk',
            match: `\\\\(?:[nrt0"${character ? "'" : ''}\\\\]|x[0-9A-Fa-f]{2}|u\\{[0-9A-Fa-f]{1,6}\\})`,
          },
        ],
  }
})

/**
 * The Silk TextMate grammar. Keyword alternations are built from `keywords`, so the grammar cannot
 * drift from the compiler without failing the typecheck. `name` doubles as the Shiki language id.
 */
export const grammar: Grammar = {
  name: 'silk',
  scopeName: 'source.silk',
  fileTypes: ['silk'],
  patterns: [
    ...literalPatterns,
    {
      name: 'comment.block.documentation.silk',
      begin: '^\\s*(///)\\s*(```)(silk)\\s*$',
      end: '^\\s*(///)\\s*(```)\\s*$',
      beginCaptures: {
        '1': { name: 'punctuation.definition.comment.documentation.silk' },
        '2': { name: 'punctuation.definition.raw.code-fence.silk' },
        '3': { name: 'fenced_code.block.language.silk' },
      },
      endCaptures: {
        '1': { name: 'punctuation.definition.comment.documentation.silk' },
        '2': { name: 'punctuation.definition.raw.code-fence.silk' },
      },
      patterns: [
        {
          begin: '^\\s*(///)\\s?',
          end: '$',
          beginCaptures: {
            '1': { name: 'punctuation.definition.comment.documentation.silk' },
          },
          patterns: [{ include: '$self' }],
        },
      ],
    },
    {
      name: 'comment.line.documentation.module.silk',
      begin: '(//!)',
      end: '$',
      beginCaptures: {
        '1': { name: 'punctuation.definition.comment.documentation.module.silk' },
      },
      patterns: [
        {
          match: '(\\[)(`)([^`\\]\\r\\n]+)(`)(\\])',
          captures: {
            '1': { name: 'punctuation.definition.link.begin.silk' },
            '2': { name: 'punctuation.definition.raw.silk' },
            '3': { name: 'markup.underline.link.silk' },
            '4': { name: 'punctuation.definition.raw.silk' },
            '5': { name: 'punctuation.definition.link.end.silk' },
          },
        },
        { name: 'markup.heading.silk', match: '#{1,6}(?=\\s)' },
        { name: 'markup.bold.silk', match: '\\*\\*[^*\\n]+\\*\\*' },
        { name: 'markup.italic.silk', match: '(?<!\\*)\\*[^*\\n]+\\*' },
        { name: 'markup.inline.raw.silk', match: '`[^`\\n]+`' },
      ],
    },
    {
      name: 'comment.line.documentation.silk',
      begin: '(///)',
      end: '$',
      beginCaptures: {
        '1': { name: 'punctuation.definition.comment.documentation.silk' },
      },
      patterns: [
        {
          match: '(\\[)(`)([^`\\]\\r\\n]+)(`)(\\])',
          captures: {
            '1': { name: 'punctuation.definition.link.begin.silk' },
            '2': { name: 'punctuation.definition.raw.silk' },
            '3': { name: 'markup.underline.link.silk' },
            '4': { name: 'punctuation.definition.raw.silk' },
            '5': { name: 'punctuation.definition.link.end.silk' },
          },
        },
        { name: 'markup.heading.silk', match: '#{1,6}(?=\\s)' },
        { name: 'markup.bold.silk', match: '\\*\\*[^*\\n]+\\*\\*' },
        { name: 'markup.italic.silk', match: '(?<!\\*)\\*[^*\\n]+\\*' },
        { name: 'markup.inline.raw.silk', match: '`[^`\\n]+`' },
      ],
    },
    { name: 'comment.line.double-slash.silk', match: '//[^\\n]*' },
    {
      // Import paths accept keyword tokens contextually, so classify every path segment as a
      // namespace before the general keyword rules can color spellings such as `effect`.
      name: 'meta.import.silk',
      begin: '\\b(import)\\s+',
      end: '(?=\\s*(?:as\\b|\\{))|$',
      beginCaptures: {
        '1': { name: 'storage.type.silk' },
      },
      patterns: [
        { name: 'entity.name.namespace.silk', match: '\\b[A-Za-z_][A-Za-z0-9_]*\\b' },
        { name: 'punctuation.silk', match: '\\.' },
      ],
    },
    {
      // Color `fn` and the declaration name together so themes can style entity.name.function.
      match: '\\b(fn)\\s+([A-Za-z_][A-Za-z0-9_]*)',
      captures: {
        '1': { name: 'storage.type.silk' },
        '2': { name: 'entity.name.function.silk' },
      },
    },
    {
      name: 'keyword.control.silk',
      match: `\\b(?:${controlSpellings.join('|')})\\b`,
    },
    {
      name: 'storage.type.silk',
      match: `\\b(?:${declarationSpellings.join('|')})\\b`,
    },
    { name: 'constant.language.boolean.silk', match: `\\b(?:${booleanSpellings.join('|')})\\b` },
    { name: 'support.type.builtin.silk', match: '\\b(?:i32|bool|never)\\b' },
    // PascalCase identifiers are types by Silk convention (patterns, signatures, generics).
    { name: 'entity.name.type.silk', match: '\\b[A-Z][A-Za-z0-9_]*\\b' },
    { name: 'variable.language.wildcard.silk', match: '\\b_\\b' },
    { name: 'constant.numeric.integer.silk', match: '\\b[0-9]+\\b' },
    {
      name: 'punctuation.definition.type-arguments.begin.silk',
      match: '<(?=\\s*[A-Za-z_][A-Za-z0-9_]*(?:\\s*[,><]))',
    },
    {
      name: 'punctuation.definition.type-arguments.end.silk',
      match: '(?<=[A-Za-z0-9_>\\]\\)])>(?=\\s*(?:>|\\(|\\{|\\[|,|\\)|->|[A-Z][A-Za-z0-9_]*\\b))',
    },
    { name: 'keyword.operator.silk', match: '=>|->|\\|>|\\||[=!<>]=|[-+*/%!<>=&^~]' },
    { name: 'punctuation.definition.pattern.rest.silk', match: '\\.\\.' },
    { name: 'punctuation.silk', match: '[(){}\\[\\]:,.]' },
  ],
}

/** VS Code language configuration for Silk: comment toggling, brackets, auto-closing pairs. */
export const languageConfiguration = {
  comments: { lineComment: '//' },
  brackets: [
    ['{', '}'],
    ['[', ']'],
    ['(', ')'],
  ],
  autoClosingPairs: [
    { open: '{', close: '}' },
    { open: '[', close: ']' },
    { open: '(', close: ')' },
  ],
  surroundingPairs: [
    ['{', '}'],
    ['[', ']'],
    ['(', ')'],
  ],
}
