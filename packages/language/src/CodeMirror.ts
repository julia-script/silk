import { defaultHighlightStyle, highlightingFor, syntaxHighlighting } from '@codemirror/language'
import type { EditorState, Extension } from '@codemirror/state'
import { RangeSetBuilder, StateField } from '@codemirror/state'
import type { DecorationSet } from '@codemirror/view'
import { Decoration, EditorView } from '@codemirror/view'
import type { Tag } from '@lezer/highlight'
import { tags } from '@lezer/highlight'
import * as Lexer from '@silk-effect/compiler/Lexer'
import * as Parser from '@silk-effect/compiler/Parser'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SyntaxTree from '@silk-effect/compiler/SyntaxTree'
import type * as Token from '@silk-effect/compiler/Token'
import * as DocumentationHighlight from '@silk-effect/documentation/Highlight'

/** A style bucket: the stable class suffix and the highlight tags themes color it with. */
interface Category {
  readonly name: string
  readonly tags: ReadonlyArray<Tag>
}

const keyword: Category = { name: 'keyword', tags: [tags.keyword] }
const boolean_: Category = { name: 'boolean', tags: [tags.bool] }
const operator: Category = { name: 'operator', tags: [tags.operator] }
const punctuation: Category = { name: 'punctuation', tags: [tags.punctuation] }
const builtinType: Category = { name: 'type', tags: [tags.typeName] }
const typePunctuation: Category = { name: 'type-punctuation', tags: [tags.punctuation] }

/** Exhaustive over `TokenKind`, so a lexer token change fails this package's typecheck. */
const categories: Record<Token.TokenKind, Category | undefined> = {
  Whitespace: undefined,
  LineComment: { name: 'line-comment', tags: [tags.lineComment] },
  DocComment: { name: 'doc-comment', tags: [tags.docComment] },
  ModuleDocComment: { name: 'module-doc-comment', tags: [tags.docComment] },
  Identifier: { name: 'identifier', tags: [tags.variableName] },
  DecimalInteger: { name: 'number', tags: [tags.integer] },
  DecimalFloat: { name: 'number', tags: [tags.float] },
  TextLiteral: { name: 'string', tags: [tags.string] },
  ByteStringLiteral: { name: 'string', tags: [tags.string] },
  CharLiteral: { name: 'character', tags: [tags.character] },
  InvalidStaticLiteral: { name: 'invalid', tags: [tags.invalid] },
  PubKeyword: keyword,
  StructKeyword: keyword,
  EnumKeyword: keyword,
  ServiceKeyword: keyword,
  InterfaceKeyword: keyword,
  RoleKeyword: keyword,
  FnKeyword: keyword,
  EffectKeyword: keyword,
  RunKeyword: keyword,
  FailKeyword: keyword,
  DropKeyword: keyword,
  UnsafeKeyword: keyword,
  ImplKeyword: keyword,
  ForKeyword: keyword,
  ReturnKeyword: keyword,
  ImportKeyword: keyword,
  AsKeyword: keyword,
  LetKeyword: keyword,
  ConstKeyword: keyword,
  MutKeyword: keyword,
  OnceKeyword: keyword,
  MoveKeyword: keyword,
  MatchKeyword: keyword,
  IfKeyword: keyword,
  ElseKeyword: keyword,
  WhileKeyword: keyword,
  BreakKeyword: keyword,
  ContinueKeyword: keyword,
  TrueKeyword: boolean_,
  FalseKeyword: boolean_,
  LeftParenthesis: punctuation,
  RightParenthesis: punctuation,
  LeftBrace: punctuation,
  RightBrace: punctuation,
  LeftBracket: punctuation,
  RightBracket: punctuation,
  Colon: punctuation,
  Semicolon: punctuation,
  Comma: punctuation,
  Dot: punctuation,
  Equals: operator,
  EqualEqual: operator,
  FatArrow: operator,
  Minus: operator,
  Plus: operator,
  Star: operator,
  Slash: operator,
  Percent: operator,
  Bang: operator,
  BangEqual: operator,
  Question: typePunctuation,
  At: typePunctuation,
  Less: operator,
  LessEqual: operator,
  Greater: operator,
  GreaterEqual: operator,
  Pipe: operator,
  PipeGreater: operator,
  PipePipe: operator,
  Ampersand: operator,
  AmpersandAmpersand: operator,
  Caret: operator,
  Tilde: operator,
  Arrow: operator,
  DotDot: punctuation,
  Invalid: { name: 'invalid', tags: [tags.invalid] },
  EndOfFile: undefined,
}

const tagsByCategory = new Map(
  [...Object.values(categories), builtinType, typePunctuation].flatMap((category) =>
    category === undefined ? [] : [[category.name, category.tags] as const],
  ),
)

/** One highlighted span in UTF-16 editor offsets. */
export interface HighlightRange {
  readonly from: number
  readonly to: number
  readonly category: string
}

const encoder = new TextEncoder()

/** UTF-16 offset for every UTF-8 byte offset; bytes inside a character map to its start. */
const byteToCharMap = (doc: string, byteLength: number): Uint32Array => {
  const map = new Uint32Array(byteLength + 1)
  let byte = 0
  let char = 0
  for (const character of doc) {
    const code = character.codePointAt(0) ?? 0
    const width = code <= 0x7f ? 1 : code <= 0x7ff ? 2 : code <= 0xffff ? 3 : 4
    for (let index = 0; index < width; index++) map[byte + index] = char
    byte += width
    char += character.length
  }
  map[byte] = char
  return map
}

/**
 * Classifies `doc` with the compiler's bootstrap lexer. Token spans are UTF-8 byte offsets;
 * returned ranges are UTF-16 editor offsets, with an identity fast path for ASCII documents.
 */
export const highlightRanges = (doc: string): ReadonlyArray<HighlightRange> => {
  const bytes = encoder.encode(doc)
  const lexical = Lexer.lex(SourceFile.make('editor', bytes))
  const syntax = Parser.parse(lexical)
  const genericAngles = new Set<string>()
  const collect = (node: SyntaxTree.Node): void => {
    if (
      node.kind === 'TypeParameterList' ||
      node.kind === 'TypeArgumentList' ||
      node.kind === 'CallTypeArgumentList'
    ) {
      for (const token of SyntaxTree.tokens(node)) {
        if (token.kind === 'Less' || token.kind === 'Greater') {
          genericAngles.add(`${token.span.start}:${token.span.end}`)
        }
      }
    }
    for (const child of node.children) if (SyntaxTree.isNode(child)) collect(child)
  }
  collect(syntax.root)
  const map = bytes.length === doc.length ? undefined : byteToCharMap(doc, bytes.length)
  const ranges: Array<HighlightRange> = []
  for (const token of lexical.tokens) {
    const from = map === undefined ? token.span.start : (map[token.span.start] ?? 0)
    const to = map === undefined ? token.span.end : (map[token.span.end] ?? 0)
    const text = doc.slice(from, to)
    if (token.kind === 'DocComment' || token.kind === 'ModuleDocComment') continue
    const category =
      (token.kind === 'Less' || token.kind === 'Greater') &&
      genericAngles.has(`${token.span.start}:${token.span.end}`)
        ? typePunctuation
        : token.kind === 'Identifier' && (text === 'i32' || text === 'bool' || text === 'never')
          ? builtinType
          : categories[token.kind]
    if (category === undefined) continue
    if (to > from) ranges.push({ from, to, category: category.name })
  }
  ranges.push(...DocumentationHighlight.ranges(doc))
  return ranges.sort((left, right) => left.from - right.from || left.to - right.to)
}

/** Translates a UTF-16 editor offset into the UTF-8 byte offset used by compiler spans. */
export const charOffsetToByteOffset = (doc: string, offset: number): number =>
  encoder.encode(doc.slice(0, offset)).length

/** Translates a UTF-8 byte offset from compiler spans into a UTF-16 editor offset. */
export const byteOffsetToCharOffset = (doc: string, byteOffset: number): number => {
  const bytes = encoder.encode(doc)
  if (bytes.length === doc.length) return Math.min(byteOffset, doc.length)
  const map = byteToCharMap(doc, bytes.length)
  return map[Math.min(byteOffset, bytes.length)] ?? doc.length
}

const decorations = (state: EditorState): DecorationSet => {
  const builder = new RangeSetBuilder<Decoration>()
  for (const range of highlightRanges(state.doc.toString())) {
    const themed = highlightingFor(state, [...(tagsByCategory.get(range.category) ?? [])])
    const stable = `cm-silk-${range.category}`
    const cls = themed === null ? stable : `${stable} ${themed}`
    builder.add(range.from, range.to, Decoration.mark({ class: cls }))
  }
  return builder.finish()
}

/**
 * Decorations recomputed by a full re-lex on every document change.
 * ponytail: full re-lex per edit; incremental lexing if documents outgrow workbench scale.
 */
export const field = StateField.define<DecorationSet>({
  create: decorations,
  update: (value, transaction) => (transaction.docChanged ? decorations(transaction.state) : value),
  provide: (self) => EditorView.decorations.from(self),
})

/** Styling the active highlight theme cannot supply: invalid bytes and doc-comment emphasis. */
const baseTheme = EditorView.baseTheme({
  '.cm-silk-invalid': { textDecoration: 'underline wavy #e45649' },
  '.cm-silk-doc-comment': { fontStyle: 'italic' },
})

/**
 * Silk syntax highlighting: the compiler lexer classifies, the active highlight style colors.
 * Falls back to CodeMirror's default highlight style when the app installs no theme.
 */
export const extension = (): Extension => [
  field,
  baseTheme,
  syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
]
