import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import * as FormattedDocument from '@silk-effect/compiler/FormattedDocument'
import * as Formatter from '@silk-effect/compiler/Formatter'
import * as SemanticOccurrence from '@silk-effect/compiler/SemanticOccurrence'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceSpan from '@silk-effect/compiler/SourceSpan'
import * as SyntaxTree from '@silk-effect/compiler/SyntaxTree'
import * as Documentation from '@silk-effect/documentation/Document'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import {
  type CodeAction,
  CodeActionKind,
  type CompletionItem,
  CompletionItemKind,
  type CompletionList,
  DiagnosticSeverity,
  type DocumentSymbol,
  type Hover,
  type InlayHint,
  InlayHintKind,
  type Location,
  type LocationLink,
  type Diagnostic as LspDiagnostic,
  type Position,
  type Range,
  SymbolKind,
  type TextEdit,
  type WorkspaceEdit,
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

/**
 * Resolves any project module's line map from the snapshot's exact analyzed bytes, memoizing
 * siblings so one request builds each foreign module's map at most once.
 */
const lineIndexes = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
): ((module: string) => LineIndex.LineIndex | undefined) => {
  const siblingIndexes = new Map<string, LineIndex.LineIndex>()
  return (module) => {
    if (module === self.module) return self.index
    const existing = siblingIndexes.get(module)
    if (existing !== undefined) return existing
    const source = Analysis.sources(snapshot).get(module)
    if (source === undefined) return undefined
    const index = LineIndex.make(SourceFile.toUint8Array(source))
    siblingIndexes.set(module, index)
    return index
  }
}

const noteSuffix = (diagnostic: Diagnostic.Diagnostic): string =>
  diagnostic.notes === undefined || diagnostic.notes.length === 0
    ? ''
    : `\n${diagnostic.notes.map((note) => `note: ${note}`).join('\n')}`

/** The document's own compiler diagnostics, in the deterministic order the phases produced. */
const owned = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.span.sourceId === self.module)

/** Converts the document's own compiler diagnostics into protocol diagnostics. */
export const diagnostics = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<LspDiagnostic> => {
  // Sibling modules' line maps, built once per module from the snapshot's exact loaded bytes.
  const indexOf = lineIndexes(self, snapshot)
  return owned(self, snapshot).map((diagnostic) => {
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

/**
 * Names the correction one edit-carrying diagnostic applies.
 *
 * A `Diagnostic.Edit` carries corrected bytes but no prose, so the title comes from the code that
 * emitted it. A code absent from this table carries no edit, so its diagnostic yields no action.
 */
const correctionTitles: Partial<Record<Diagnostic.Code, string>> = {
  [Diagnostic.duplicateImportCode]: 'Remove the repeated import',
  [Diagnostic.redundantAliasCode]: 'Remove the redundant alias',
}

/** Tests whether two protocol ranges share at least one position. */
const overlaps = (left: Range, right: Range): boolean =>
  !(
    left.end.line < right.start.line ||
    (left.end.line === right.start.line && left.end.character < right.start.character) ||
    right.end.line < left.start.line ||
    (right.end.line === left.start.line && right.end.character < left.start.character)
  )

/**
 * Offers each machine-applicable edit of the diagnostics touching one range as a quick fix.
 *
 * The actions are recomputed from the analyzed snapshot rather than read from the request's
 * client-supplied diagnostics, so an action always carries the edit the current source produces.
 * A diagnostic that carries no edit contributes no action, and the actions follow diagnostic
 * order, which is deterministic because every phase is.
 */
export const codeActions = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  range: Range,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<CodeAction> => {
  // `diagnostics` maps the same `owned` list one-to-one, so the two stay index-aligned.
  const published = diagnostics(self, snapshot, uriOf)
  return owned(self, snapshot).flatMap((diagnostic, order) => {
    const title = correctionTitles[diagnostic.code]
    const source = published[order]
    if (title === undefined || source === undefined || !overlaps(source.range, range)) return []
    return (diagnostic.edits ?? []).map((edit) => ({
      title,
      kind: CodeActionKind.QuickFix,
      diagnostics: [source],
      edit: {
        changes: {
          [self.uri]: [
            { range: LineIndex.rangeOf(self.index, edit.span), newText: edit.replacement },
          ],
        },
      },
    }))
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

/** One project-wide occurrence sharing the semantic identity selected by a request. */
interface Match {
  readonly module: string
  readonly occurrence: SemanticOccurrence.SemanticOccurrence
}

/**
 * Collects every occurrence of one semantic identity across the whole analyzed project. The
 * occurrence index spans every module of the accepted project revision even though a root view's
 * closure names one root, so this reads the same index go-to-definition reads, in reverse.
 */
const matchesOfIdentity = (
  snapshot: Analysis.FrontendSnapshot,
  identity: SemanticOccurrence.Identity,
): ReadonlyArray<Match> => {
  const key = SemanticOccurrence.identityKey(identity)
  const sources = Analysis.sources(snapshot)
  const modules = [...sources.keys()].sort((left, right) =>
    left < right ? -1 : left > right ? 1 : 0,
  )
  const seen = new Set<string>()
  const matches: Array<Match> = []
  for (const module of modules) {
    const source = sources.get(module)
    if (source === undefined) continue
    const whole = Option.getOrUndefined(SourceSpan.make(source, 0, SourceFile.length(source)))
    if (whole === undefined) continue
    for (const occurrence of Analysis.semanticOccurrencesInRange(snapshot, module, whole)) {
      if (occurrence.resolution._tag !== 'Available') continue
      if (SemanticOccurrence.identityKey(occurrence.resolution.identity) !== key) continue
      const at = `${module}:${occurrence.span.start}:${occurrence.span.end}`
      if (seen.has(at)) continue
      seen.add(at)
      matches.push(Object.freeze({ module, occurrence }))
    }
  }
  return Object.freeze(matches)
}

/**
 * Tests whether one module's exact bytes came from the installed compiler toolchain. Origin is the
 * only trustworthy signal: a module-name prefix such as `silk/` is spoofable by a project module
 * and would miss toolchain sources shipped outside the reserved namespace.
 */
const isToolchainModule = (snapshot: Analysis.FrontendSnapshot, module: string): boolean =>
  Analysis.sources(snapshot).get(module)?.origin._tag === 'ToolchainFile'

/** Returns one span's exact analyzed spelling, or `undefined` for an unloaded module. */
const spellingOf = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  span: SourceSpan.SourceSpan,
): string | undefined => {
  const source = Analysis.sources(snapshot).get(module)
  return source === undefined ? undefined : Option.getOrUndefined(SourceFile.spelling(source, span))
}

/** Returns every project occurrence of the semantic identity selected at one position. */
export const references = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
  includeDeclaration: boolean,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<Location> | undefined => {
  const occurrence = Analysis.semanticOccurrenceAt(
    snapshot,
    self.module,
    LineIndex.offsetOf(self.index, position),
  )
  if (occurrence?.resolution._tag !== 'Available') return undefined
  const indexOf = lineIndexes(self, snapshot)
  return Object.freeze(
    matchesOfIdentity(snapshot, occurrence.resolution.identity).flatMap((match) => {
      if (!includeDeclaration && match.occurrence.role === 'Declaration') return []
      const uri = match.module === self.module ? self.uri : uriOf(match.module)
      const index = indexOf(match.module)
      return uri === undefined || index === undefined
        ? []
        : [{ uri, range: LineIndex.rangeOf(index, match.occurrence.span) }]
    }),
  )
}

/** The renameable name token under one position and the spelling an editor should preselect. */
export interface PreparedRename {
  readonly range: Range
  readonly placeholder: string
}

/** One refused rename carrying the compiler diagnostic that explains the refusal. */
export interface RenameRefusal {
  readonly _tag: 'RenameRefusal'
  readonly code: string
  readonly message: string
}

/** One accepted rename covering every module of the analyzed project. */
export interface RenameEdit {
  readonly _tag: 'RenameEdit'
  readonly edit: WorkspaceEdit
}

export type Rename = RenameEdit | RenameRefusal

/** Everything a rename request derives from one position, shared by prepare and rename. */
interface RenameSubject {
  readonly occurrence: SemanticOccurrence.SemanticOccurrence
  readonly identity: SemanticOccurrence.Identity
  readonly spelling: string
  /** Every occurrence the rename would edit: the identity's, narrowed to the selected spelling. */
  readonly matches: ReadonlyArray<Match>
}

/**
 * Tests whether one spelling names, in one module, a declaration the module reaches through its own
 * `as` clause. Such an alias is the importing module's own name for a declaration it does not own:
 * the clause introduces it, Silk's one flat module namespace makes the spelling resolve to it
 * throughout that module, and no other module can see it. A member imported under the declaration's
 * own spelling introduces no separate name, so the source half of `make as vectorMake` — and a
 * plain `{ equals }` — still reach the declaration itself.
 */
const bindsLocalAlias = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  spelling: string,
  identity: SemanticOccurrence.Identity,
): boolean => {
  const key = SemanticOccurrence.identityKey(identity)
  for (const imported of Analysis.moduleScope(snapshot, module)?.imports ?? []) {
    if (imported._tag !== 'Available') continue
    for (const binding of imported.bindings) {
      if (binding._tag !== 'ImportedMember') continue
      if (binding.spelling !== spelling || binding.spelling === binding.sourceSpelling) continue
      const bound = SemanticOccurrence.identityKey(
        Object.freeze({ _tag: 'DeclarationIdentity', id: binding.declaration }),
      )
      if (bound === key) return true
    }
  }
  return false
}

/**
 * Resolves the occurrences one rename request would rewrite. A declaration's identity is canonical
 * and module-independent, so it alone cannot say which occurrences a rename owns; two narrowings
 * finish the job.
 *
 * The selected spelling separates an imported member from its local alias, which share that one
 * identity: renaming the alias `eq` of `equals as eq` edits the `eq` occurrences alone.
 *
 * Spelling is not enough on its own, because unrelated modules pick the same alias for the same
 * declaration all the time — `silk/bytes` and a project module both writing `make as vectorMake`
 * agree on identity *and* spelling. An `as` clause binds a name in one module only, so when the
 * selection resolves through such a binding the rename is confined to the module that wrote it.
 * The declaration site and the source half of an import clause name the declaration itself and
 * stay project-wide.
 *
 * Narrowing here, before any refusal is decided, is what lets prepare and rename answer from the
 * same facts.
 */
const renameSubjectAt = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
): RenameSubject | undefined => {
  const occurrence = Analysis.semanticOccurrenceAt(
    snapshot,
    self.module,
    LineIndex.offsetOf(self.index, position),
  )
  if (occurrence?.resolution._tag !== 'Available' || occurrence.declaration === undefined)
    return undefined
  const spelling = spellingOf(snapshot, self.module, occurrence.span)
  if (spelling === undefined) return undefined
  const identity = occurrence.resolution.identity
  const aliased = bindsLocalAlias(snapshot, self.module, spelling, identity)
  return Object.freeze({
    occurrence,
    identity,
    spelling,
    matches: matchesOfIdentity(snapshot, identity).filter(
      (match) =>
        (!aliased || match.module === self.module) &&
        spellingOf(snapshot, match.module, match.occurrence.span) === spelling,
    ),
  })
}

/**
 * Refuses a rename that would rewrite a source the installed toolchain owns. Ownership is decided
 * by where the edits land, never by where the identity was declared: a project module's own alias
 * of a standard-library member is the project's name to change, while the member's own spelling
 * reaches the declaration inside the installation and stays untouchable. Editors apply a workspace
 * edit unprompted, so a request that reaches the installation is refused whole, never trimmed.
 */
const toolchainRefusal = (
  snapshot: Analysis.FrontendSnapshot,
  matches: ReadonlyArray<Match>,
  spelling: string,
): RenameRefusal | undefined => {
  for (const match of matches) {
    if (!isToolchainModule(snapshot, match.module)) continue
    return Object.freeze({
      _tag: 'RenameRefusal',
      code: 'LSP0002',
      message: `Renaming ${spelling} would edit ${match.module}, which the installed toolchain owns`,
    })
  }
  return undefined
}

/**
 * Returns the name token a rename would replace. A token with no source-backed declaration, such
 * as a keyword, trivia, or an intrinsic with no Silk declaration, has no renameable name. A name
 * whose rename would reach the installed toolchain has none either: prepare answers from the same
 * occurrences and the same refusal `rename` uses, so no editor greys out a rename that would
 * succeed, nor offers one that only ever fails.
 */
export const prepareRename = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
): PreparedRename | undefined => {
  const subject = renameSubjectAt(self, snapshot, position)
  if (subject === undefined) return undefined
  if (toolchainRefusal(snapshot, subject.matches, subject.spelling) !== undefined) return undefined
  return Object.freeze({
    range: LineIndex.rangeOf(self.index, subject.occurrence.span),
    placeholder: subject.spelling,
  })
}

/** Silk's one flat module namespace holds top-level declarations and import bindings only. */
const occupiesFlatNamespace = (identity: SemanticOccurrence.Identity): boolean =>
  identity._tag === 'DeclarationIdentity' || identity._tag === 'ImportNamespaceIdentity'

/**
 * Refuses a new spelling already claimed in a flat namespace the rename would extend. Only the
 * declaration site and import bindings put a name into a module's flat namespace, so a module that
 * merely reaches the declaration through a qualifier keeps its own unrelated top-level names.
 */
const flatNamespaceRefusal = (
  snapshot: Analysis.FrontendSnapshot,
  identity: SemanticOccurrence.Identity,
  matches: ReadonlyArray<Match>,
  newName: string,
  span: SourceSpan.SourceSpan,
): RenameRefusal | undefined => {
  if (!occupiesFlatNamespace(identity)) return undefined
  for (const match of matches) {
    if (match.occurrence.role !== 'Declaration' && match.occurrence.role !== 'Import') continue
    if (Analysis.lookupName(snapshot, match.module, newName)._tag === 'Missing') continue
    const diagnostic = Diagnostic.bindingConflict(newName, span)
    return Object.freeze({
      _tag: 'RenameRefusal',
      code: diagnostic.code,
      message: diagnostic.message,
    })
  }
  return undefined
}

/**
 * Renames one semantic identity across every module of the analyzed project. Only occurrences
 * whose analyzed spelling equals the selected name are edited: an imported member and its local
 * alias share one identity, so an alias keeps the local name its own module chose. The rename is
 * refused rather than partially applied when any occurrence cannot be placed in a document, or
 * when any occurrence lives in a source the installed toolchain owns. Toolchain ownership is
 * decided first: no replacement spelling can make such a rename legal, so reporting a name
 * collision instead would send the user looking for a fix that does not exist.
 */
export const rename = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
  newName: string,
  uriOf: (module: string) => string | undefined,
): Rename | undefined => {
  const subject = renameSubjectAt(self, snapshot, position)
  if (subject === undefined) return undefined
  const { identity, matches, occurrence, spelling } = subject
  const owned = toolchainRefusal(snapshot, matches, spelling)
  if (owned !== undefined) return owned
  const refusal = flatNamespaceRefusal(snapshot, identity, matches, newName, occurrence.span)
  if (refusal !== undefined) return refusal
  const indexOf = lineIndexes(self, snapshot)
  const changes: Record<string, Array<TextEdit>> = {}
  for (const match of matches) {
    // `toolchainRefusal` already cleared every match, but an edit aimed at the installation is
    // damaging enough that the guard stays here too, where the edits are actually built.
    if (isToolchainModule(snapshot, match.module))
      return Object.freeze({
        _tag: 'RenameRefusal',
        code: 'LSP0002',
        message: `Renaming ${spelling} would edit ${match.module}, which the installed toolchain owns`,
      })
    const uri = match.module === self.module ? self.uri : uriOf(match.module)
    const index = indexOf(match.module)
    if (uri === undefined || index === undefined)
      return Object.freeze({
        _tag: 'RenameRefusal',
        code: 'LSP0001',
        message: `Module ${match.module} has no document to rename ${spelling} in`,
      })
    const edits = changes[uri]
    const edit = { range: LineIndex.rangeOf(index, match.occurrence.span), newText: newName }
    if (edits === undefined) changes[uri] = [edit]
    else edits.push(edit)
  }
  return Object.freeze({ _tag: 'RenameEdit', edit: { changes } })
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
