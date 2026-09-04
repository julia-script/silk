import * as Data from 'effect/Data'
import * as EffectResult from 'effect/Result'
import {
  type CallHierarchyIncomingCall,
  type CallHierarchyItem,
  type CallHierarchyOutgoingCall,
  CodeAction,
  type CompletionList,
  type Diagnostic,
  type DocumentSymbol,
  type FoldingRange,
  type Hover,
  type InlayHint,
  type Location,
  type LocationLink,
  Position,
  Range,
  type SemanticTokens,
  type SignatureHelp,
  type TextEdit,
} from 'vscode-languageserver-types'
import type * as Document from './Document.js'
import type * as Inspection from './Inspection.js'

/** Input and output contract for every semantic operation executed beside a worker snapshot. */
export interface Catalog {
  readonly Diagnostics: {
    readonly parameters: { readonly previousResultId?: string }
    readonly result: DiagnosticResult
  }
  readonly Hover: { readonly parameters: Position; readonly result: Hover | undefined }
  readonly Completion: { readonly parameters: Position; readonly result: CompletionList }
  readonly SignatureHelp: {
    readonly parameters: Position
    readonly result: SignatureHelp | undefined
  }
  readonly CodeActions: { readonly parameters: Range; readonly result: ReadonlyArray<CodeAction> }
  readonly ResolveCodeAction: { readonly parameters: CodeAction; readonly result: CodeAction }
  readonly Definition: { readonly parameters: Position; readonly result: LocationLink | undefined }
  readonly References: {
    readonly parameters: { readonly position: Position; readonly includeDeclaration: boolean }
    readonly result: ReadonlyArray<Location> | undefined
  }
  readonly PrepareRename: {
    readonly parameters: Position
    readonly result: Document.PreparedRename | undefined
  }
  readonly Rename: {
    readonly parameters: { readonly position: Position; readonly newName: string }
    readonly result: Document.Rename | undefined
  }
  readonly InlayHints: { readonly parameters: Range; readonly result: ReadonlyArray<InlayHint> }
  readonly Symbols: {
    readonly parameters: undefined
    readonly result: ReadonlyArray<DocumentSymbol>
  }
  readonly Formatting: { readonly parameters: undefined; readonly result: ReadonlyArray<TextEdit> }
  readonly SemanticTokens: { readonly parameters: undefined; readonly result: SemanticTokens }
  readonly FoldingRanges: {
    readonly parameters: undefined
    readonly result: ReadonlyArray<FoldingRange>
  }
  readonly PrepareCallHierarchy: {
    readonly parameters: Position
    readonly result: ReadonlyArray<CallHierarchyItem>
  }
  readonly IncomingCalls: {
    readonly parameters: CallHierarchyItem
    readonly result: ReadonlyArray<CallHierarchyIncomingCall>
  }
  readonly OutgoingCalls: {
    readonly parameters: CallHierarchyItem
    readonly result: ReadonlyArray<CallHierarchyOutgoingCall>
  }
  readonly Inspection: {
    readonly parameters: Inspection.ViewParameters
    readonly result: Inspection.ViewResponse | Inspection.UnknownView
  }
}

/** Pull-diagnostic payload tagged by the exact worker/project/document revision that produced it. */
export interface DiagnosticResult {
  readonly resultId: string
  readonly items: ReadonlyArray<Diagnostic>
}

export type Tag = keyof Catalog

/** Serializable query whose result type is selected by its discriminant. */
export type EditorQuery<K extends Tag = Tag> = K extends Tag
  ? {
      readonly _tag: K
      readonly uri: string
      readonly parameters: Catalog[K]['parameters']
    }
  : never

export type Result<K extends Tag> = Catalog[K]['result']

export const tags: ReadonlyArray<Tag> = Object.freeze([
  'Diagnostics',
  'Hover',
  'Completion',
  'SignatureHelp',
  'CodeActions',
  'ResolveCodeAction',
  'Definition',
  'References',
  'PrepareRename',
  'Rename',
  'InlayHints',
  'Symbols',
  'Formatting',
  'SemanticTokens',
  'FoldingRanges',
  'PrepareCallHierarchy',
  'IncomingCalls',
  'OutgoingCalls',
  'Inspection',
])

export class InvalidEditorQuery extends Data.TaggedError('InvalidEditorQuery')<{
  readonly message: string
}> {}

interface UnknownRecord {
  readonly [key: string]: unknown
}

const record = (value: unknown): value is UnknownRecord =>
  typeof value === 'object' && value !== null

const callHierarchyItem = (value: unknown): value is CallHierarchyItem =>
  record(value) &&
  typeof value.name === 'string' &&
  typeof value.kind === 'number' &&
  typeof value.uri === 'string' &&
  Range.is(value.range) &&
  Range.is(value.selectionRange)

const inspectionKeys = new Set(['uri', 'view', 'filter', 'showTrivia'])

const inspection = (value: unknown): value is Inspection.ViewParameters =>
  record(value) &&
  Object.keys(value).every((key) => inspectionKeys.has(key)) &&
  typeof value.uri === 'string' &&
  typeof value.view === 'string' &&
  (value.filter === undefined || typeof value.filter === 'string') &&
  (value.showTrivia === undefined || typeof value.showTrivia === 'boolean')

const invalid = (): EffectResult.Result<never, InvalidEditorQuery> =>
  EffectResult.fail(new InvalidEditorQuery({ message: 'Malformed editor query' }))

/** Validates an unknown structured-clone query and restores tag/parameter correlation. */
export const decode = (input: unknown): EffectResult.Result<EditorQuery, InvalidEditorQuery> => {
  if (!record(input) || typeof input._tag !== 'string' || typeof input.uri !== 'string')
    return invalid()
  const base = { uri: input.uri }
  switch (input._tag) {
    case 'Diagnostics': {
      if (!record(input.parameters)) return invalid()
      const previousResultId = input.parameters.previousResultId
      if (previousResultId !== undefined && typeof previousResultId !== 'string') return invalid()
      return EffectResult.succeed({
        ...base,
        _tag: 'Diagnostics',
        parameters: previousResultId === undefined ? {} : { previousResultId },
      })
    }
    case 'Hover':
    case 'Completion':
    case 'SignatureHelp':
    case 'Definition':
    case 'PrepareRename':
    case 'PrepareCallHierarchy':
      return Position.is(input.parameters)
        ? EffectResult.succeed({ ...base, _tag: input._tag, parameters: input.parameters })
        : invalid()
    case 'CodeActions':
    case 'InlayHints':
      return Range.is(input.parameters)
        ? EffectResult.succeed({ ...base, _tag: input._tag, parameters: input.parameters })
        : invalid()
    case 'ResolveCodeAction':
      return CodeAction.is(input.parameters)
        ? EffectResult.succeed({ ...base, _tag: input._tag, parameters: input.parameters })
        : invalid()
    case 'References':
      return record(input.parameters) &&
        Position.is(input.parameters.position) &&
        typeof input.parameters.includeDeclaration === 'boolean'
        ? EffectResult.succeed({
            ...base,
            _tag: 'References',
            parameters: {
              position: input.parameters.position,
              includeDeclaration: input.parameters.includeDeclaration,
            },
          })
        : invalid()
    case 'Rename':
      return record(input.parameters) &&
        Position.is(input.parameters.position) &&
        typeof input.parameters.newName === 'string'
        ? EffectResult.succeed({
            ...base,
            _tag: 'Rename',
            parameters: {
              position: input.parameters.position,
              newName: input.parameters.newName,
            },
          })
        : invalid()
    case 'Symbols':
    case 'Formatting':
    case 'SemanticTokens':
    case 'FoldingRanges':
      return input.parameters === undefined
        ? EffectResult.succeed({ ...base, _tag: input._tag, parameters: undefined })
        : invalid()
    case 'IncomingCalls':
    case 'OutgoingCalls':
      return callHierarchyItem(input.parameters)
        ? EffectResult.succeed({ ...base, _tag: input._tag, parameters: input.parameters })
        : invalid()
    case 'Inspection':
      return inspection(input.parameters)
        ? EffectResult.succeed({ ...base, _tag: 'Inspection', parameters: input.parameters })
        : invalid()
    default:
      return invalid()
  }
}
