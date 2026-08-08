import * as Analysis from '@silk-effect/compiler/Analysis'
import type * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import type * as Elaboration from '@silk-effect/compiler/Elaboration'
import * as FormattedDocument from '@silk-effect/compiler/FormattedDocument'
import * as Formatter from '@silk-effect/compiler/Formatter'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import type * as SourceSpan from '@silk-effect/compiler/SourceSpan'
import * as SyntaxTree from '@silk-effect/compiler/SyntaxTree'
import * as Type from '@silk-effect/compiler/Type'
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import {
  DiagnosticSeverity,
  type DocumentSymbol,
  type Hover,
  type LocationLink,
  type Diagnostic as LspDiagnostic,
  type Position,
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
  snapshot: Analysis.Snapshot,
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

/** One inferred type anchored to the byte span of the fact that carries it. */
export interface TypedSpan {
  readonly type: Type.Type
  readonly span: SourceSpan.SourceSpan
}

const factType = (fact: {
  readonly type: Elaboration.ExpressionTypeFact
  readonly syntax: SyntaxTree.Element
}): TypedSpan | undefined =>
  fact.type._tag === 'Available'
    ? { type: fact.type.type, span: SyntaxTree.span(fact.syntax) }
    : undefined

/** Returns the smallest typed expression or binding fact under one position. */
export const typeAt = (
  self: Document,
  snapshot: Analysis.Snapshot,
  position: Position,
): TypedSpan | undefined => {
  const offset = LineIndex.offsetOf(self.index, position)
  const candidates = [
    ...Analysis.expressionsOf(snapshot, self.module).flatMap((fact) => {
      const typed = factType(fact)
      return typed === undefined ? [] : [typed]
    }),
    ...Analysis.bindingsOf(snapshot, self.module).flatMap((binding) =>
      binding.name._tag === 'Present' && binding.inferredType._tag === 'Available'
        ? [{ type: binding.inferredType.type, span: binding.name.token.span }]
        : [],
    ),
  ].filter(
    (candidate) =>
      candidate.span.sourceId === self.module &&
      candidate.span.start <= offset &&
      offset < candidate.span.end,
  )
  if (candidates.length === 0) return undefined
  return candidates.reduce((best, candidate) =>
    candidate.span.end - candidate.span.start < best.span.end - best.span.start ? candidate : best,
  )
}

/** Returns the type of the smallest typed expression or binding under one position. */
export const hover = (
  self: Document,
  snapshot: Analysis.Snapshot,
  position: Position,
): Hover | undefined => {
  const typed = typeAt(self, snapshot, position)
  if (typed === undefined) return undefined
  return {
    contents: { kind: 'markdown', value: `\`\`\`silk\n${Type.encode(typed.type)}\n\`\`\`` },
    range: LineIndex.rangeOf(self.index, typed.span),
  }
}

/** Converts one semantic target into an exact snapshot-owned definition link. */
export const definition = (
  self: Document,
  snapshot: Analysis.Snapshot,
  position: Position,
  uriOf: (module: string) => string | undefined,
): LocationLink | undefined => {
  const offset = LineIndex.offsetOf(self.index, position)
  const target = Analysis.semanticTargetAt(snapshot, self.module, offset)
  if (target?.resolution._tag !== 'Available') return undefined
  const location = target.resolution.declaration
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
    originSelectionRange: LineIndex.rangeOf(self.index, target.origin),
    targetUri: uri,
    targetRange: LineIndex.rangeOf(targetIndex, location.span),
    targetSelectionRange: LineIndex.rangeOf(targetIndex, location.selectionSpan),
  }
}

/** Returns the document's top-level function and struct declarations as symbols. */
export const symbols = (
  self: Document,
  snapshot: Analysis.Snapshot,
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
  snapshot: Analysis.Snapshot,
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
