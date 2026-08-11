import * as Analysis from '@silk-effect/compiler/Analysis'
import type * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import * as FormattedDocument from '@silk-effect/compiler/FormattedDocument'
import * as Formatter from '@silk-effect/compiler/Formatter'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SyntaxTree from '@silk-effect/compiler/SyntaxTree'
import * as Documentation from '@silk-effect/documentation/Document'
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import {
  type CompletionItem,
  CompletionItemKind,
  type CompletionList,
  DiagnosticSeverity,
  type DocumentSymbol,
  type Hover,
  type InlayHint,
  InlayHintKind,
  type LocationLink,
  type Diagnostic as LspDiagnostic,
  type Position,
  type Range,
  SymbolKind,
  type TextEdit,
} from 'vscode-languageserver-types'
import * as LineIndex from './LineIndex.js'

const decoder = new TextDecoder()

/** One open Silk source document with its canonical module identity and line map. */
export interface Document {
  readonly _tag: 'Document'
  readonly uri: string
  readonly version: number
  readonly workspace: string
  readonly module: string
  readonly sourceRoot: string
  readonly bytes: Uint8Array
  readonly index: LineIndex.LineIndex
}

/** Creates an immutable document snapshot from one text synchronization state. */
export const make = (options: {
  readonly uri: string
  readonly version: number
  readonly workspace: string
  readonly module: string
  readonly sourceRoot: string
  readonly bytes: Uint8Array
}): Document =>
  Object.freeze({
    _tag: 'Document',
    uri: options.uri,
    version: options.version,
    workspace: options.workspace,
    module: options.module,
    sourceRoot: options.sourceRoot,
    bytes: Uint8Array.from(options.bytes),
    index: LineIndex.make(options.bytes),
  })

const noteSuffix = (diagnostic: Diagnostic.Diagnostic): string =>
  diagnostic.notes === undefined || diagnostic.notes.length === 0
    ? ''
    : `\n${diagnostic.notes.map((note) => `note: ${note}`).join('\n')}`

/** Converts the document's own compiler diagnostics into protocol diagnostics. */
export const diagnostics = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<LspDiagnostic> => {
  // Sibling modules' line maps, built once per module from the snapshot's exact loaded bytes.
  const siblingIndexes = new Map<string, LineIndex.LineIndex>()
  const indexOf = (module: string): LineIndex.LineIndex | undefined => {
    if (module === self.module) return self.index
    const existing = siblingIndexes.get(module)
    if (existing !== undefined) return existing
    const source = Analysis.sources(snapshot).get(module)
    if (source === undefined) return undefined
    const index = LineIndex.make(SourceFile.toUint8Array(source))
    siblingIndexes.set(module, index)
    return index
  }
  return Analysis.diagnostics(snapshot)
    .filter((diagnostic) => diagnostic.span.sourceId === self.module)
    .map((diagnostic) => {
      const related = (diagnostic.relatedSpans ?? []).flatMap((relatedSpan) => {
        const module = relatedSpan.span.sourceId
        const uri = module === self.module ? self.uri : uriOf(module)
        const index = indexOf(module)
        return uri === undefined || index === undefined
          ? []
          : [
              {
                location: { uri, range: LineIndex.rangeOf(index, relatedSpan.span) },
                message: relatedSpan.label,
              },
            ]
      })
      return {
        range: LineIndex.rangeOf(self.index, diagnostic.span),
        severity: DiagnosticSeverity.Error,
        code: diagnostic.code,
        source: 'silk',
        message: `${diagnostic.message}${noteSuffix(diagnostic)}`,
        ...(related.length > 0 ? { relatedInformation: related } : {}),
      }
    })
}

/** Returns the source-like semantic presentation under one position. */
export const hover = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
): Hover | undefined => {
  const subject = Analysis.hoverSubjectAt(
    snapshot,
    self.module,
    LineIndex.offsetOf(self.index, position),
  )
  if (subject === undefined) return undefined
  const span =
    subject._tag === 'OccurrenceHoverSubject' ? subject.occurrence.span : subject.expression.span
  const raw =
    subject._tag === 'OccurrenceHoverSubject'
      ? Analysis.documentationAt(snapshot, self.module, LineIndex.offsetOf(self.index, position))
      : undefined
  const source = raw === undefined ? undefined : Analysis.sources(snapshot).get(raw.span.sourceId)
  const documentation =
    raw === undefined || source === undefined
      ? undefined
      : Documentation.toMarkdown(Documentation.parse(source, raw))
  const signature = `\`\`\`silk\n${subject.presentation.text}\n\`\`\``
  return {
    contents: {
      kind: 'markdown',
      value:
        documentation === undefined || documentation.length === 0
          ? signature
          : `${signature}\n\n${documentation}`,
    },
    range: LineIndex.rangeOf(self.index, span),
  }
}

/** Converts one semantic target into an exact snapshot-owned definition link. */
export const definition = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
  uriOf: (module: string) => string | undefined,
): LocationLink | undefined => {
  const offset = LineIndex.offsetOf(self.index, position)
  const occurrence = Analysis.semanticOccurrenceAt(snapshot, self.module, offset)
  if (occurrence?.resolution._tag !== 'Available' || occurrence.declaration === undefined)
    return undefined
  const location = occurrence.declaration
  const uri = location.module === self.module ? self.uri : uriOf(location.module)
  if (uri === undefined) return undefined
  const targetIndex =
    location.module === self.module
      ? self.index
      : (() => {
          const source = Analysis.sources(snapshot).get(location.module)
          return source === undefined ? undefined : LineIndex.make(SourceFile.toUint8Array(source))
        })()
  if (targetIndex === undefined) return undefined
  return {
    originSelectionRange: LineIndex.rangeOf(self.index, occurrence.span),
    targetUri: uri,
    targetRange: LineIndex.rangeOf(targetIndex, location.span),
    targetSelectionRange: LineIndex.rangeOf(targetIndex, location.selectionSpan),
  }
}

/** Converts compiler-owned inferred local types into standard protocol inlay hints. */
export const inlayHints = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  range: Range,
): ReadonlyArray<InlayHint> => {
  const start = LineIndex.offsetOf(self.index, range.start)
  const end = LineIndex.offsetOf(self.index, range.end)
  return Analysis.typeHints(snapshot, self.module, start, end).map((hint) => ({
    position: LineIndex.positionOf(self.index, hint.span.end),
    label: `: ${hint.presentation.text}`,
    kind: InlayHintKind.Type,
    paddingLeft: false,
    paddingRight: false,
  }))
}

const completionKind = (kind: string): CompletionItemKind => {
  switch (kind) {
    case 'Binding':
    case 'Parameter':
      return CompletionItemKind.Variable
    case 'Function':
    case 'Operation':
      return CompletionItemKind.Function
    case 'Constructor':
      return CompletionItemKind.Constructor
    case 'Type':
      return CompletionItemKind.Struct
    case 'Field':
      return CompletionItemKind.Field
    case 'Actor':
      return CompletionItemKind.Module
    case 'Keyword':
      return CompletionItemKind.Keyword
    default:
      return CompletionItemKind.Text
  }
}

/** Converts compiler-owned semantic candidates into deterministic protocol completion items. */
export const completion = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
): CompletionList => {
  const result = Analysis.completionAt(
    snapshot,
    self.module,
    LineIndex.offsetOf(self.index, position),
  )
  if (result === undefined) return { isIncomplete: false, items: [] }
  const range = LineIndex.rangeOf(self.index, result.replacement)
  const items: ReadonlyArray<CompletionItem> = result.candidates.map((candidate, ordinal) => ({
    label: candidate.label,
    kind: completionKind(candidate.kind),
    insertText: candidate.insertText,
    textEdit: { range, newText: candidate.insertText },
    sortText: `${String(candidate.sortGroup).padStart(2, '0')}-${String(ordinal).padStart(4, '0')}-${candidate.label}`,
    ...(candidate.detail === undefined ? {} : { detail: candidate.detail.text }),
  }))
  return { isIncomplete: false, items: [...items] }
}

/** Returns the document's top-level declarations as symbols. */
export const symbols = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
): ReadonlyArray<DocumentSymbol> => {
  const headers = Analysis.declarationIndex(snapshot).modules.find(
    (candidate) => candidate.module === self.module,
  )
  if (headers === undefined) return []
  return headers.members.flatMap((member): ReadonlyArray<DocumentSymbol> => {
    if (member.name._tag !== 'Present') return []
    const range = LineIndex.rangeOf(self.index, SyntaxTree.span(member.syntax))
    const selectionRange = LineIndex.rangeOf(self.index, member.name.token.span)
    if (member._tag === 'FunctionDeclaration') {
      return [
        {
          name: member.name.spelling,
          kind: SymbolKind.Function,
          range,
          selectionRange,
        },
      ]
    }
    if (member._tag === 'ConstantDeclaration') {
      return [
        {
          name: member.name.spelling,
          kind: SymbolKind.Constant,
          range,
          selectionRange,
        },
      ]
    }
    if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
      return [
        {
          name: member.name.spelling,
          kind: SymbolKind.Interface,
          range,
          selectionRange,
          children: member.operations.flatMap((operation) =>
            operation.name._tag === 'Present'
              ? [
                  {
                    name: operation.name.spelling,
                    kind: SymbolKind.Method,
                    range: LineIndex.rangeOf(self.index, SyntaxTree.span(operation.syntax)),
                    selectionRange: LineIndex.rangeOf(self.index, operation.name.token.span),
                  },
                ]
              : [],
          ),
        },
      ]
    }
    const fields = member.fields.flatMap((field) =>
      field.name._tag === 'Present'
        ? [
            {
              name: field.name.spelling,
              kind: SymbolKind.Field,
              range: LineIndex.rangeOf(self.index, SyntaxTree.span(field.syntax)),
              selectionRange: LineIndex.rangeOf(self.index, field.name.token.span),
            },
          ]
        : [],
    )
    return [
      {
        name: member.name.spelling,
        kind: SymbolKind.Struct,
        range,
        selectionRange,
        ...(fields.length > 0 ? { children: fields } : {}),
      },
    ]
  })
}

/** Formats the whole document, yielding no edits for damaged or already canonical sources. */
export const format = Effect.fnUntraced(function* (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
): Effect.fn.Return<ReadonlyArray<TextEdit>, never> {
  const syntax = Analysis.syntaxOf(snapshot, self.module)
  if (syntax === undefined) return []
  const formatted = yield* Effect.result(Formatter.format(syntax))
  if (Result.isFailure(formatted) || !formatted.success.changed) return []
  return [
    {
      range: LineIndex.fullRange(self.index),
      newText: decoder.decode(FormattedDocument.toUint8Array(formatted.success)),
    },
  ]
})
