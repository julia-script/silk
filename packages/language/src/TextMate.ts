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
  FnKeyword: 'fn',
  ReturnKeyword: 'return',
  ImportKeyword: 'import',
  AsKeyword: 'as',
  LetKeyword: 'let',
  MutKeyword: 'mut',
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

const booleanSpellings = [keywords.TrueKeyword, keywords.FalseKeyword]
const plainSpellings = Object.values(keywords).filter(
  (spelling) => !booleanSpellings.includes(spelling),
)

/** One TextMate match rule. */
export interface GrammarPattern {
  readonly name: string
  readonly match: string
}

/** The subset of the TextMate grammar shape Silk needs. */
export interface Grammar {
  readonly name: string
  readonly scopeName: string
  readonly fileTypes: ReadonlyArray<string>
  readonly patterns: ReadonlyArray<GrammarPattern>
}

/**
 * The Silk TextMate grammar. Keyword alternations are built from `keywords`, so the grammar cannot
 * drift from the compiler without failing the typecheck. `name` doubles as the Shiki language id.
 */
export const grammar: Grammar = {
  name: 'silk',
  scopeName: 'source.silk',
  fileTypes: ['silk'],
  patterns: [
    { name: 'comment.line.documentation.silk', match: '///[^\\n]*' },
    { name: 'comment.line.double-slash.silk', match: '//[^\\n]*' },
    { name: 'keyword.other.silk', match: `\\b(?:${plainSpellings.join('|')})\\b` },
    { name: 'constant.language.boolean.silk', match: `\\b(?:${booleanSpellings.join('|')})\\b` },
    { name: 'support.type.builtin.silk', match: '\\b(?:I32|Bool|Never)\\b' },
    { name: 'entity.name.type.pattern.silk', match: '\\b[A-Z][A-Za-z0-9_]*(?=\\s*\\{)' },
    { name: 'variable.language.wildcard.silk', match: '\\b_\\b' },
    { name: 'constant.numeric.integer.silk', match: '\\b[0-9]+\\b' },
    {
      name: 'punctuation.definition.type-arguments.begin.silk',
      match: '<(?=\\s*[A-Za-z_][A-Za-z0-9_]*(?:\\s*[,><]))',
    },
    {
      name: 'punctuation.definition.type-arguments.end.silk',
      match: '(?<=[A-Za-z0-9_>\\]\\)])>(?=\\s*(?:>|\\(|\\{|\\[|,|\\)|->))',
    },
    { name: 'keyword.operator.silk', match: '=>|->|\\|>|\\||[=!<>]=|[-+*/%!<>=&]' },
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
