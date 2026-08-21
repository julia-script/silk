import * as Analysis from '@silk-effect/compiler/Analysis'
import type * as AutoImport from '@silk-effect/compiler/AutoImport'
import type * as DeclarationIndex from '@silk-effect/compiler/DeclarationIndex'
import * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import * as FormattedDocument from '@silk-effect/compiler/FormattedDocument'
import * as ImportPlan from '@silk-effect/compiler/ImportPlan'
import * as Presentation from '@silk-effect/compiler/Presentation'
import * as SemanticOccurrence from '@silk-effect/compiler/SemanticOccurrence'
import type * as SourceAction from '@silk-effect/compiler/SourceAction'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceSpan from '@silk-effect/compiler/SourceSpan'
import type * as SyntaxFile from '@silk-effect/compiler/SyntaxFile'
import * as SyntaxTree from '@silk-effect/compiler/SyntaxTree'
import type * as Token from '@silk-effect/compiler/Token'
import type * as WorkspaceInventory from '@silk-effect/compiler/WorkspaceInventory'
import * as Documentation from '@silk-effect/documentation/Document'
import * as Formatter from '@silk-effect/formatter/Formatter'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import {
  type CallHierarchyIncomingCall,
  type CallHierarchyItem,
  type CallHierarchyOutgoingCall,
  type CodeAction,
  CodeActionKind,
  type CompletionItem,
  CompletionItemKind,
  type CompletionList,
  DiagnosticSeverity,
  type DocumentSymbol,
  type FoldingRange,
  FoldingRangeKind,
  type Hover,
  type InlayHint,
  InlayHintKind,
  type Location,
  type LocationLink,
  type Diagnostic as LspDiagnostic,
  type Position,
  type Range,
  type SemanticTokens,
  type SemanticTokensLegend,
  SemanticTokenTypes,
  type SignatureHelp,
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

const compilerDiagnostics = (
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

interface ImportRedundancy {
  readonly diagnostic: LspDiagnostic
  readonly title: string
  readonly edits: ReadonlyArray<TextEdit>
}

const sourceText = (
  source: SourceFile.SourceFile,
  element: SyntaxTree.Element,
): string | undefined => Option.getOrUndefined(SourceFile.spelling(source, element.span))

const aliasClause = (
  source: SourceFile.SourceFile,
  alias: SyntaxTree.Node,
): SourceSpan.SourceSpan => {
  let start = SyntaxTree.directToken(alias, 'AsKeyword')?.span.start ?? alias.span.start
  while (start > 0 && [0x20, 0x09, 0x0d].includes(source.bytes[start - 1] ?? Number.NaN)) start -= 1
  return Option.getOrElse(SourceSpan.make(source, start, alias.span.end), () => alias.span)
}

const importLine = (
  source: SourceFile.SourceFile,
  declaration: SyntaxTree.Node,
): SourceSpan.SourceSpan => {
  let start =
    SyntaxTree.directToken(declaration, 'ImportKeyword')?.span.start ?? declaration.span.start
  while (start > 0 && [0x20, 0x09].includes(source.bytes[start - 1] ?? Number.NaN)) start -= 1
  let end = declaration.span.end
  while (end < source.bytes.length && [0x20, 0x09].includes(source.bytes[end] ?? Number.NaN))
    end += 1
  const preceding = start === 0 ? undefined : source.bytes[start - 1]
  const ownsLine = start === 0 || preceding === 0x0a || preceding === 0x0d
  if (ownsLine && source.bytes[end] === 0x0d) end += 1
  if (ownsLine && source.bytes[end] === 0x0a) end += 1
  return Option.getOrElse(SourceSpan.make(source, start, end), () => declaration.span)
}

const importRedundancies = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
): ReadonlyArray<ImportRedundancy> => {
  const syntax = Analysis.syntaxOf(snapshot, self.module)
  if (syntax === undefined) return []
  const source = syntax.source
  const result: Array<ImportRedundancy> = []
  const seen = new Set<string>()
  const firstByPath = new Map<string, SyntaxTree.Node>()
  for (const declaration of SyntaxTree.directNodes(syntax.root, 'ImportDeclaration')) {
    const normalized = sourceText(source, declaration)?.replace(/\s+/g, ' ').trim()
    if (normalized !== undefined && seen.has(normalized)) {
      const span = importLine(source, declaration)
      result.push({
        title: 'Remove the repeated import',
        diagnostic: {
          range: LineIndex.rangeOf(self.index, declaration.span),
          severity: DiagnosticSeverity.Warning,
          code: 'LSP0001',
          source: 'silk-lsp',
          message: 'This exact import is repeated',
        },
        edits: [{ range: LineIndex.rangeOf(self.index, span), newText: '' }],
      })
      continue
    } else if (normalized !== undefined) seen.add(normalized)

    const path = SyntaxTree.directNode(declaration, 'ImportPath')
    const pathText = path === undefined ? undefined : sourceText(source, path)?.trim()
    const first = pathText === undefined ? undefined : firstByPath.get(pathText)
    if (pathText !== undefined && first === undefined) firstByPath.set(pathText, declaration)
    if (
      pathText !== undefined &&
      first !== undefined &&
      SyntaxTree.directNode(first, 'ImportAlias') === undefined &&
      SyntaxTree.directNode(declaration, 'ImportAlias') === undefined
    ) {
      const firstList = SyntaxTree.directNode(first, 'ImportMemberList')
      const repeatedList = SyntaxTree.directNode(declaration, 'ImportMemberList')
      if (firstList !== undefined && repeatedList !== undefined) {
        const members = [firstList, repeatedList]
          .flatMap((list) => SyntaxTree.directNodes(list, 'ImportMember'))
          .flatMap((member) => {
            const rendered = sourceText(source, member)?.trim()
            return rendered === undefined ? [] : [rendered]
          })
          .filter((member, index, all) => all.indexOf(member) === index)
        if (members.length > 0) {
          result.push({
            title: 'Consolidate imports from this module',
            diagnostic: {
              range: LineIndex.rangeOf(self.index, declaration.span),
              severity: DiagnosticSeverity.Warning,
              code: 'LSP0003',
              source: 'silk-lsp',
              message: 'Imports from this module can be consolidated',
            },
            edits: [
              {
                range: LineIndex.rangeOf(self.index, first.span),
                newText: `import ${pathText} { ${members.join(', ')} }`,
              },
              {
                range: LineIndex.rangeOf(self.index, importLine(source, declaration)),
                newText: '',
              },
            ],
          })
        }
      }
    }
    const defaultName =
      path === undefined
        ? undefined
        : SyntaxTree.tokens(path)
            .filter((token) => token.kind === 'Identifier')
            .at(-1)
    const aliases = [
      { owner: declaration, source: defaultName },
      ...SyntaxTree.directNodes(
        SyntaxTree.directNode(declaration, 'ImportMemberList') ?? declaration,
        'ImportMember',
      ).map((member) => ({ owner: member, source: SyntaxTree.directToken(member, 'Identifier') })),
    ]
    for (const candidate of aliases) {
      const alias = SyntaxTree.directNode(candidate.owner, 'ImportAlias')
      const local = alias === undefined ? undefined : SyntaxTree.directToken(alias, 'Identifier')
      const original = candidate.source
      if (
        alias === undefined ||
        local === undefined ||
        original === undefined ||
        sourceText(source, local) !== sourceText(source, original)
      )
        continue
      const span = aliasClause(source, alias)
      result.push({
        title: 'Remove the redundant alias',
        diagnostic: {
          range: LineIndex.rangeOf(self.index, span),
          severity: DiagnosticSeverity.Warning,
          code: 'LSP0002',
          source: 'silk-lsp',
          message: 'This alias does not change the imported name',
        },
        edits: [{ range: LineIndex.rangeOf(self.index, span), newText: '' }],
      })
    }
  }
  return result
}

/** Publishes compiler errors plus non-semantic import style warnings. */
export const diagnostics = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<LspDiagnostic> => [
  ...compilerDiagnostics(self, snapshot, uriOf),
  ...importRedundancies(self, snapshot).map((entry) => entry.diagnostic),
]

/** Tests whether two protocol ranges share at least one position. */
const overlaps = (left: Range, right: Range): boolean =>
  !(
    left.end.line < right.start.line ||
    (left.end.line === right.start.line && left.end.character < right.start.character) ||
    right.end.line < left.start.line ||
    (right.end.line === left.start.line && right.end.character < left.start.character)
  )

/** Serializable data carried by a revalidatable auto-import action. */
export interface AutoImportData {
  readonly _tag: 'SilkAutoImport'
  readonly uri: string
  readonly version: number
  readonly module: string
  readonly target: { readonly start: number; readonly end: number }
  readonly candidate: AutoImport.CandidateKey
}

const autoImportData = (
  self: Document,
  target: SourceSpan.SourceSpan,
  candidate: AutoImport.CandidateKey,
): AutoImportData =>
  Object.freeze({
    _tag: 'SilkAutoImport',
    uri: self.uri,
    version: self.version,
    module: self.module,
    target: Object.freeze({ start: target.start, end: target.end }),
    candidate,
  })

const record = (value: unknown): value is Readonly<Record<string, unknown>> =>
  typeof value === 'object' && value !== null

/** Validates untrusted protocol data before it is used for exact-version reacquisition. */
export const parseAutoImportData = (value: unknown): AutoImportData | undefined => {
  if (!record(value) || value._tag !== 'SilkAutoImport') return undefined
  const target = value.target
  const candidate = value.candidate
  if (
    typeof value.uri !== 'string' ||
    typeof value.version !== 'number' ||
    typeof value.module !== 'string' ||
    !record(target) ||
    typeof target.start !== 'number' ||
    typeof target.end !== 'number' ||
    !record(candidate) ||
    candidate._tag !== 'AutoImportCandidateKey' ||
    typeof candidate.module !== 'string' ||
    typeof candidate.spelling !== 'string' ||
    typeof candidate.ordinal !== 'number' ||
    !['Function', 'Constant', 'Struct', 'Service', 'Interface'].includes(
      typeof candidate.declarationKind === 'string' ? candidate.declarationKind : '',
    )
  )
    return undefined
  return value as unknown as AutoImportData
}

const workspaceEdit = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  plan: SourceAction.ChangePlan,
  uriOf: (module: string) => string | undefined,
): WorkspaceEdit | undefined => {
  const indexOf = lineIndexes(self, snapshot)
  const changes: Record<string, Array<TextEdit>> = {}
  for (const [module, edits] of plan.changes) {
    const uri = module === self.module ? self.uri : uriOf(module)
    const index = indexOf(module)
    if (uri === undefined || index === undefined) return undefined
    changes[uri] = edits.map((edit) => ({
      range: LineIndex.rangeOf(index, edit.span),
      newText: edit.replacement,
    }))
  }
  return { changes }
}

const sourcePoint = (
  self: Document,
  source: SourceFile.SourceFile,
  offset: number,
): Range | undefined => {
  const span = Option.getOrUndefined(SourceSpan.make(source, offset, offset))
  return span === undefined ? undefined : LineIndex.rangeOf(self.index, span)
}

const sourceType = (rendered: string): string => {
  const separator = Math.max(rendered.lastIndexOf('.'), rendered.lastIndexOf('/'))
  return separator < 0 ? rendered : rendered.slice(separator + 1)
}

const sourceRequirement = (rendered: string): string =>
  rendered.replace(/(&\s*(?:mut\s+)?)(?:[^\s.]+\.)+([A-Za-z_][A-Za-z0-9_]*)/, '$1$2')

const enclosingFunctionDeclaration = (
  syntax: SyntaxFile.SyntaxFile,
  span: SourceSpan.SourceSpan,
): SyntaxTree.Node | undefined =>
  SyntaxTree.directNodes(syntax.root, 'FunctionDeclaration').find(
    (declaration) => declaration.span.start <= span.start && span.end <= declaration.span.end,
  )

const propagationEdit = (
  self: Document,
  syntax: SyntaxFile.SyntaxFile,
  diagnostic: Diagnostic.Diagnostic,
): { readonly title: string; readonly edit: TextEdit } | undefined => {
  const declaration = enclosingFunctionDeclaration(syntax, diagnostic.span)
  if (
    declaration === undefined ||
    SyntaxTree.directToken(declaration, 'EffectKeyword') === undefined
  )
    return undefined
  if (diagnostic.reason._tag === 'UnhandledEffectFailures') {
    const failures = diagnostic.reason.failures.map(sourceType).join(' | ')
    const existing = SyntaxTree.directNode(declaration, 'FailureRow')
    const anchor = existing ?? SyntaxTree.directNode(declaration, 'ReturnType')
    if (anchor === undefined) return undefined
    const range = sourcePoint(self, syntax.source, anchor.span.end)
    return range === undefined
      ? undefined
      : {
          title: `Propagate ${failures} from this Effect`,
          edit: { range, newText: existing === undefined ? ` ! ${failures}` : ` | ${failures}` },
        }
  }
  if (diagnostic.reason._tag === 'UnhandledEffectRequirements') {
    const requirements = diagnostic.reason.requirements.map(sourceRequirement).join(' | ')
    const existing = SyntaxTree.directNode(declaration, 'RequirementRow')
    const anchor =
      existing ??
      SyntaxTree.directNode(declaration, 'FailureRow') ??
      SyntaxTree.directNode(declaration, 'ReturnType')
    if (anchor === undefined) return undefined
    const range = sourcePoint(self, syntax.source, anchor.span.end)
    return range === undefined
      ? undefined
      : {
          title: `Propagate ${requirements} from this Effect`,
          edit: {
            range,
            newText: existing === undefined ? ` ? ${requirements}` : ` | ${requirements}`,
          },
        }
  }
  return undefined
}

const handledEffectEdit = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  syntax: SyntaxFile.SyntaxFile,
  diagnostic: Diagnostic.Diagnostic,
  published: LspDiagnostic,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<CodeAction> => {
  const raw = Option.getOrUndefined(SourceFile.spelling(syntax.source, diagnostic.span))
  const trimmed = raw?.trimStart()
  if (raw === undefined || trimmed === undefined || !trimmed.startsWith('run ')) return []
  const leading = raw.slice(0, raw.length - trimmed.length)
  const operation = trimmed.slice('run '.length)
  const text = decoder.decode(SourceFile.toUint8Array(syntax.source))
  const make = (
    title: string,
    imported: string,
    localSpelling: string,
    replacement: string,
  ): ReadonlyArray<CodeAction> => {
    const plan = Option.getOrUndefined(
      ImportPlan.make({
        syntax,
        module: 'silk/effects',
        spelling: imported,
        localSpelling,
      }),
    )
    if (plan === undefined) return []
    const importEdit = workspaceEdit(self, snapshot, plan, uriOf)
    const changes = importEdit?.changes?.[self.uri]
    if (changes === undefined) return []
    return [
      {
        title,
        kind: CodeActionKind.QuickFix,
        diagnostics: [published],
        edit: {
          changes: {
            [self.uri]: [
              ...changes,
              {
                range: LineIndex.rangeOf(self.index, diagnostic.span),
                newText: `${leading}${replacement}`,
              },
            ],
          },
        },
      },
    ]
  }
  if (
    diagnostic.reason._tag === 'UnhandledEffectFailures' &&
    /\beffect\s+fn\s+recover\s*\(/.test(text)
  )
    return make(
      'Recover this Effect with recover',
      'catchAll',
      'effectCatchAll',
      `run effectCatchAll(${operation}, recover)`,
    )
  if (
    diagnostic.reason._tag === 'UnhandledEffectRequirements' &&
    diagnostic.reason.requirements.length === 1
  ) {
    const mutable = diagnostic.reason.requirements[0]?.startsWith('&mut ') ?? false
    const provider = mutable
      ? /\blet\s+mut\s+provider\s*=/.test(text)
      : /\blet\s+(?:mut\s+)?provider\s*=/.test(text)
    if (!provider) return []
    return mutable
      ? make(
          'Provide this Effect with provider',
          'provideMut',
          'effectProvideMut',
          `run effectProvideMut(${operation}, &mut provider)`,
        )
      : make(
          'Provide this Effect with provider',
          'provide',
          'effectProvide',
          `run effectProvide(${operation}, &provider)`,
        )
  }
  return []
}

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
  inventory?: WorkspaceInventory.WorkspaceInventory,
): ReadonlyArray<CodeAction> => {
  // `diagnostics` maps the same `owned` list one-to-one, so the two stay index-aligned.
  const published = compilerDiagnostics(self, snapshot, uriOf)
  const compiler = owned(self, snapshot).flatMap((diagnostic, order) => {
    const source = published[order]
    if (source === undefined || !overlaps(source.range, range)) return []
    const syntax = Analysis.syntaxOf(snapshot, self.module)
    const propagation = syntax === undefined ? undefined : propagationEdit(self, syntax, diagnostic)
    const contract =
      propagation === undefined
        ? []
        : [
            {
              title: propagation.title,
              kind: CodeActionKind.QuickFix,
              diagnostics: [source],
              edit: { changes: { [self.uri]: [propagation.edit] } },
            } satisfies CodeAction,
          ]
    const handled =
      syntax === undefined
        ? []
        : handledEffectEdit(self, snapshot, syntax, diagnostic, source, uriOf)
    if (inventory === undefined) return [...contract, ...handled]
    const imports = Analysis.autoImportsAt(snapshot, inventory, self.module, diagnostic.span.start)
    return [
      ...contract,
      ...handled,
      ...imports.flatMap((action): ReadonlyArray<CodeAction> => {
        const plan = Option.getOrUndefined(
          Analysis.resolveAutoImport(
            snapshot,
            inventory,
            self.module,
            action.descriptor.target.start,
            action.candidate,
          ),
        )
        if (plan === undefined) return []
        const edit = workspaceEdit(self, snapshot, plan, uriOf)
        return edit === undefined
          ? []
          : [
              {
                title: action.descriptor.title,
                kind: CodeActionKind.QuickFix,
                diagnostics: [source],
                data: autoImportData(self, action.descriptor.target, action.candidate),
                edit,
              },
            ]
      }),
    ]
  })
  const redundancy = importRedundancies(self, snapshot).flatMap(
    (entry): ReadonlyArray<CodeAction> =>
      overlaps(entry.diagnostic.range, range)
        ? [
            {
              title: entry.title,
              kind: CodeActionKind.QuickFix,
              diagnostics: [entry.diagnostic],
              edit: { changes: { [self.uri]: [...entry.edits] } },
            },
          ]
        : [],
  )
  return [...compiler, ...redundancy]
}

export const disableCodeAction = (action: CodeAction, reason: string): CodeAction => {
  const { edit: discarded, ...withoutEdit } = action
  void discarded
  return { ...withoutEdit, disabled: { reason } }
}

/** Revalidates a descriptor into one revision-checked protocol workspace edit. */
export const resolveCodeAction = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  inventory: WorkspaceInventory.WorkspaceInventory,
  action: CodeAction,
  uriOf: (module: string) => string | undefined,
): CodeAction => {
  const data = parseAutoImportData(action.data)
  if (data === undefined || data.uri !== self.uri || data.version !== self.version)
    return disableCodeAction(action, 'The source revision for this action is no longer available')
  const plan = Option.getOrUndefined(
    Analysis.resolveAutoImport(snapshot, inventory, data.module, data.target.start, data.candidate),
  )
  if (plan === undefined)
    return disableCodeAction(action, 'This import is no longer applicable in the accepted revision')
  const edit = workspaceEdit(self, snapshot, plan, uriOf)
  return edit === undefined
    ? disableCodeAction(action, 'The import target could not be mapped to a workspace document')
    : { ...action, edit }
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

/**
 * The innermost call whose argument list encloses one byte offset, with that list.
 *
 * The search reads the concrete syntax tree rather than elaborated facts, because the parser keeps
 * a recovered call form for a source that does not compile and signature help is most wanted while
 * the arguments are still half-written. An offset exactly on the closing parenthesis is outside the
 * call; one on an absent closing parenthesis is inside it, since the user is still typing there.
 */
const enclosingCall = (
  root: SyntaxTree.Node,
  offset: number,
): { readonly callee: SyntaxTree.Element; readonly argumentList: SyntaxTree.Node } | undefined => {
  let selected:
    | { readonly callee: SyntaxTree.Element; readonly argumentList: SyntaxTree.Node }
    | undefined
  const visit = (node: SyntaxTree.Node): void => {
    if (node.kind === 'CallExpression') {
      const callee = node.children[0]
      const argumentList = node.children.find(
        (child): child is SyntaxTree.Node =>
          SyntaxTree.isNode(child) && child.kind === 'ArgumentList',
      )
      if (callee !== undefined && argumentList !== undefined) {
        // The opening parenthesis is the list's first byte, so `>` puts the cursor after it.
        const closed = argumentList.children.some(
          (child) => SyntaxTree.isToken(child) && child.kind === 'RightParenthesis',
        )
        const end = closed ? argumentList.span.end - 1 : argumentList.span.end
        if (
          offset > argumentList.span.start &&
          offset <= end &&
          (selected === undefined ||
            argumentList.span.end - argumentList.span.start <=
              selected.argumentList.span.end - selected.argumentList.span.start)
        )
          selected = Object.freeze({ callee, argumentList })
      }
    }
    for (const child of node.children) if (SyntaxTree.isNode(child)) visit(child)
  }
  visit(root)
  return selected
}

/** The innermost struct literal whose initializer body contains one byte offset. */
const enclosingStructLiteral = (
  root: SyntaxTree.Node,
  offset: number,
):
  | {
      readonly target: SyntaxTree.Element
      readonly literal: SyntaxTree.Node
      readonly initializers: ReadonlyArray<SyntaxTree.Node>
    }
  | undefined => {
  let selected:
    | {
        readonly target: SyntaxTree.Element
        readonly literal: SyntaxTree.Node
        readonly initializers: ReadonlyArray<SyntaxTree.Node>
      }
    | undefined
  const visit = (node: SyntaxTree.Node): void => {
    if (node.kind === 'StructLiteralExpression') {
      const target = node.children[0]
      const leftBrace = node.children.find(
        (child) => SyntaxTree.isToken(child) && child.kind === 'LeftBrace',
      )
      const rightBrace = node.children.find(
        (child) => SyntaxTree.isToken(child) && child.kind === 'RightBrace',
      )
      if (target !== undefined && leftBrace !== undefined) {
        const end = rightBrace === undefined ? node.span.end : rightBrace.span.start
        if (
          offset > leftBrace.span.start &&
          offset <= end &&
          (selected === undefined ||
            node.span.end - node.span.start <=
              selected.literal.span.end - selected.literal.span.start)
        )
          selected = Object.freeze({
            target,
            literal: node,
            initializers: SyntaxTree.directNodes(node, 'StructFieldInitializer'),
          })
      }
    }
    for (const child of node.children) if (SyntaxTree.isNode(child)) visit(child)
  }
  visit(root)
  return selected
}

const markdownDocumentation = (
  snapshot: Analysis.FrontendSnapshot,
  identity: SemanticOccurrence.Identity,
): string | undefined => {
  const raw = Analysis.documentationOfIdentity(snapshot, identity)
  const source = raw === undefined ? undefined : Analysis.sources(snapshot).get(raw.span.sourceId)
  return raw === undefined || source === undefined
    ? undefined
    : Documentation.toMarkdown(Documentation.parse(source, raw))
}

/**
 * Describes the call the cursor sits inside, selecting the parameter the cursor is writing.
 *
 * The label and the parameter labels come from the same presentations hover and completion detail
 * render, so one declaration reads identically everywhere. The active parameter counts the commas
 * this argument list owns before the cursor: a comma nested in an inner call belongs to that call's
 * own list, so it never advances the outer selection.
 */
export const signatureHelp = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
): SignatureHelp | undefined => {
  const syntax = Analysis.syntaxOf(snapshot, self.module)
  if (syntax === undefined) return undefined
  const offset = LineIndex.offsetOf(self.index, position)
  const call = enclosingCall(syntax.root, offset)
  if (call === undefined) {
    const structLiteral = enclosingStructLiteral(syntax.root, offset)
    if (structLiteral === undefined) return undefined
    const targetPath =
      SyntaxTree.isNode(structLiteral.target) && structLiteral.target.kind === 'AppliedType'
        ? (SyntaxTree.directNode(structLiteral.target, 'TypePath') ?? structLiteral.target)
        : structLiteral.target
    const occurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      self.module,
      SyntaxTree.span(targetPath).end - 1,
    )
    if (occurrence?.resolution._tag !== 'Available') return undefined
    const identity = occurrence.resolution.identity
    if (identity._tag !== 'DeclarationIdentity') return undefined
    const declaration = Analysis.declarationForIdentity(snapshot, identity)
    if (declaration?._tag !== 'StructDeclaration') return undefined
    const fields = declaration.fields.filter(
      (field) => field.visibility === 'Public' || occurrence.declaration?.module === self.module,
    )
    const activeInitializer = structLiteral.initializers.find(
      (initializer) => offset >= initializer.span.start && offset <= initializer.span.end,
    )
    const fieldOccurrence =
      activeInitializer === undefined
        ? undefined
        : Analysis.semanticOccurrenceAt(snapshot, self.module, activeInitializer.span.start)
    const fieldIdentity =
      fieldOccurrence?.resolution._tag === 'Available' &&
      fieldOccurrence.resolution.identity._tag === 'FieldIdentity'
        ? fieldOccurrence.resolution.identity
        : undefined
    const activeField =
      fieldIdentity === undefined
        ? -1
        : fields.findIndex((field) => field.id.ordinal === fieldIdentity.id.ordinal)
    const precedingInitializers = structLiteral.initializers.filter(
      (initializer) => initializer.span.end < offset,
    ).length
    const activeParameter =
      activeField >= 0
        ? activeField
        : Math.min(precedingInitializers, Math.max(0, fields.length - 1))
    const documentation = markdownDocumentation(snapshot, identity)
    return {
      signatures: [
        {
          label: `${Presentation.structDeclaration(declaration).text} { ${fields
            .map((field) => Presentation.field(field).text)
            .join(', ')} }`,
          parameters: fields.map((field) => ({ label: Presentation.field(field).text })),
          ...(documentation === undefined || documentation.length === 0
            ? {}
            : { documentation: { kind: 'markdown' as const, value: documentation } }),
        },
      ],
      activeSignature: 0,
      activeParameter,
    }
  }
  // The callee's last byte is inside its name token, which is where its occurrence is indexed.
  const occurrence = Analysis.semanticOccurrenceAt(
    snapshot,
    self.module,
    SyntaxTree.span(call.callee).end - 1,
  )
  if (occurrence?.resolution._tag !== 'Available') return undefined
  const identity = occurrence.resolution.identity
  if (identity._tag !== 'DeclarationIdentity') return undefined
  const declaration = Analysis.declarationForIdentity(snapshot, identity)
  if (declaration?._tag !== 'FunctionDeclaration') return undefined
  const documentation = markdownDocumentation(snapshot, identity)
  const activeParameter = call.argumentList.children.filter(
    (child) =>
      SyntaxTree.isToken(child) && child.kind === 'Comma' && SyntaxTree.span(child).end <= offset,
  ).length
  return {
    signatures: [
      {
        label: Presentation.functionDeclaration(declaration).text,
        parameters: declaration.parameters.map((parameter) => ({
          label: Presentation.parameter(parameter).text,
        })),
        ...(documentation === undefined || documentation.length === 0
          ? {}
          : { documentation: { kind: 'markdown' as const, value: documentation } }),
      },
    ],
    activeSignature: 0,
    activeParameter,
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
  inventory?: WorkspaceInventory.WorkspaceInventory,
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
  if (
    inventory === undefined ||
    result.context._tag === 'ActorMemberContext' ||
    result.context._tag === 'ValueMemberContext'
  )
    return { isIncomplete: false, items: [...items] }

  const syntax = Analysis.syntaxOf(snapshot, self.module)
  if (syntax === undefined) return { isIncomplete: false, items: [...items] }
  const visible = new Set(items.map((item) => item.label))
  const imported = new Set<string>()
  for (const entry of Analysis.moduleScope(snapshot, self.module)?.imports ?? []) {
    if (entry._tag !== 'Available') continue
    for (const binding of entry.bindings)
      if (binding._tag === 'ImportedMember')
        imported.add(`${binding.declaration.module}:${binding.sourceSpelling}`)
  }
  const capitalize = (value: string): string =>
    value.length === 0 ? value : `${value[0]?.toUpperCase() ?? ''}${value.slice(1)}`
  const prefix = (module: string): string =>
    module.split('/').at(-1)?.split('_').map(capitalize).join('') ?? 'Imported'
  const alias = (candidate: WorkspaceInventory.Candidate): string => {
    const base =
      candidate.exported.namespace === 'Value'
        ? `${(prefix(candidate.module)[0] ?? 'i').toLowerCase()}${prefix(candidate.module).slice(1)}${capitalize(candidate.exported.spelling)}`
        : `${prefix(candidate.module)}${capitalize(candidate.exported.spelling)}`
    let selected = base
    let suffix = 2
    while (visible.has(selected)) {
      selected = `${base}${suffix}`
      suffix += 1
    }
    return selected
  }
  const completionItemKind = (candidate: WorkspaceInventory.Candidate): CompletionItemKind => {
    switch (candidate.exported.declarationKind) {
      case 'Function':
        return CompletionItemKind.Function
      case 'Constant':
        return CompletionItemKind.Constant
      case 'Struct':
        return CompletionItemKind.Struct
      case 'Service':
      case 'Interface':
        return CompletionItemKind.Interface
    }
  }
  const catalog = [...inventory.byName.values()]
    .flat()
    .filter(
      (candidate) =>
        candidate.module !== self.module &&
        !imported.has(`${candidate.module}:${candidate.exported.spelling}`),
    )
    .flatMap((candidate, ordinal): ReadonlyArray<CompletionItem> => {
      const localSpelling = visible.has(candidate.exported.spelling)
        ? alias(candidate)
        : candidate.exported.spelling
      const plan = Option.getOrUndefined(
        ImportPlan.make({
          syntax,
          module: candidate.module,
          spelling: candidate.exported.spelling,
          localSpelling,
        }),
      )
      const edits = plan?.changes.get(self.module)
      if (edits === undefined) return []
      return [
        {
          label: candidate.exported.spelling,
          labelDetails: { description: candidate.module },
          kind: completionItemKind(candidate),
          detail: `Import from ${candidate.module}`,
          sortText: `20-${candidate.tier === 'Project' ? '0' : '1'}-${String(ordinal).padStart(5, '0')}-${candidate.module}`,
          textEdit: { range, newText: localSpelling },
          additionalTextEdits: edits.map((edit) => ({
            range: LineIndex.rangeOf(self.index, edit.span),
            newText: edit.replacement,
          })),
        },
      ]
    })
  return { isIncomplete: false, items: [...items, ...catalog] }
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
    if (member._tag === 'RoleDeclaration') {
      return [
        {
          name: member.name.spelling,
          kind: SymbolKind.Enum,
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

/**
 * The token types the server reports, in the order the protocol's numeric encoding indexes them.
 *
 * Every entry is reachable from a Silk token kind or occurrence role, so the legend advertises no
 * type the server never sends. `keyword`, `comment`, `string`, `number`, and `operator` come from
 * the lexer's own kinds — the same kinds `@silk-effect/language`'s TextMate grammar colors — while
 * an identifier's type is decided by the semantic occurrence covering it, which is what separates a
 * type name from a variable name that the grammar's regular expressions must color alike.
 */
export const semanticTokenTypes: ReadonlyArray<string> = Object.freeze([
  SemanticTokenTypes.keyword,
  SemanticTokenTypes.comment,
  SemanticTokenTypes.string,
  SemanticTokenTypes.number,
  SemanticTokenTypes.operator,
  SemanticTokenTypes.type,
  SemanticTokenTypes.typeParameter,
  SemanticTokenTypes.function,
  SemanticTokenTypes.method,
  SemanticTokenTypes.parameter,
  SemanticTokenTypes.variable,
  SemanticTokenTypes.property,
  SemanticTokenTypes.namespace,
])

/** The legend one client negotiates once and then decodes every token reply against. */
export const semanticTokensLegend: SemanticTokensLegend = Object.freeze({
  tokenTypes: [...semanticTokenTypes],
  tokenModifiers: [],
})

const typeIndexes = new Map(semanticTokenTypes.map((name, index) => [name, index] as const))

/**
 * The token type of one lexer kind alone, before any semantic occurrence refines it.
 *
 * An identifier is deliberately absent: its type depends on what it names, not on how it lexed, so
 * an identifier with no occurrence covering it contributes no token rather than a guessed one.
 */
const lexicalTokenType = (kind: Token.TokenKind): string | undefined => {
  if (kind.endsWith('Keyword')) return SemanticTokenTypes.keyword
  switch (kind) {
    case 'LineComment':
    case 'DocComment':
    case 'ModuleDocComment':
      return SemanticTokenTypes.comment
    case 'TextLiteral':
    case 'ByteStringLiteral':
    case 'CharLiteral':
      return SemanticTokenTypes.string
    case 'DecimalInteger':
    case 'DecimalFloat':
      return SemanticTokenTypes.number
    case 'Equals':
    case 'EqualEqual':
    case 'FatArrow':
    case 'Minus':
    case 'Plus':
    case 'Star':
    case 'Slash':
    case 'Percent':
    case 'Bang':
    case 'BangEqual':
    case 'Question':
    case 'Less':
    case 'LessEqual':
    case 'Greater':
    case 'GreaterEqual':
    case 'Pipe':
    case 'PipeGreater':
    case 'PipePipe':
    case 'Ampersand':
    case 'AmpersandAmpersand':
    case 'Caret':
    case 'Tilde':
    case 'Arrow':
      return SemanticTokenTypes.operator
    default:
      return undefined
  }
}

/**
 * The token type one resolved occurrence gives the identifier it covers. The role says how the name
 * was used and the resolved identity says what it names, so a function used as a value still reads
 * as a function while a parameter and a local binding stay distinct.
 */
const occurrenceTokenType = (
  snapshot: Analysis.FrontendSnapshot,
  occurrence: SemanticOccurrence.SemanticOccurrence,
): string => {
  const identity =
    occurrence.resolution._tag === 'Available' ? occurrence.resolution.identity : undefined
  switch (identity?._tag) {
    case 'TypeParameterIdentity':
      return SemanticTokenTypes.typeParameter
    case 'ParameterIdentity':
      return SemanticTokenTypes.parameter
    case 'BindingIdentity':
    case 'PatternBindingIdentity':
      return SemanticTokenTypes.variable
    case 'FieldIdentity':
      return SemanticTokenTypes.property
    case 'ServiceOperationIdentity':
    case 'IntrinsicOperationIdentity':
      return SemanticTokenTypes.method
    case 'ImportNamespaceIdentity':
    case 'IntrinsicActorIdentity':
      return SemanticTokenTypes.namespace
    case 'DeclarationIdentity': {
      const declaration = Analysis.declarationForIdentity(snapshot, identity)
      if (declaration?._tag === 'FunctionDeclaration') return SemanticTokenTypes.function
      if (declaration?._tag === 'ConstantDeclaration') return SemanticTokenTypes.variable
      return SemanticTokenTypes.type
    }
    default:
      break
  }
  // An unresolved name still has a role, which is enough to keep a type reading as a type.
  switch (occurrence.role) {
    case 'Type':
      return SemanticTokenTypes.type
    case 'Field':
      return SemanticTokenTypes.property
    case 'Actor':
      return SemanticTokenTypes.namespace
    case 'Operation':
      return SemanticTokenTypes.method
    default:
      return SemanticTokenTypes.variable
  }
}

/**
 * Colors the whole document from the compiler's own facts rather than from a regular expression.
 *
 * The lexer's token kinds carry keywords, comments, literals, and operators, and the semantic
 * occurrence index types every identifier by what it actually resolves to. Tokens are emitted in
 * source order and delta-encoded against the previous token as the protocol requires. A token
 * spanning more than one line is dropped rather than truncated, because the protocol's encoding
 * cannot express one, and a multi-line token in Silk is only ever a triple-quoted literal the
 * grammar already colors.
 */
export const semanticTokens = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
): SemanticTokens => {
  const syntax = Analysis.syntaxOf(snapshot, self.module)
  if (syntax === undefined) return { data: [] }
  const source = Analysis.sources(snapshot).get(self.module)
  const occurrences =
    source === undefined
      ? []
      : Option.match(SourceSpan.make(source, 0, SourceFile.length(source)), {
          onNone: () => [],
          onSome: (whole) => [...Analysis.semanticOccurrencesInRange(snapshot, self.module, whole)],
        })
  // Occurrences are start-sorted, so one advancing cursor pairs each identifier with its own.
  const byStart = new Map<number, SemanticOccurrence.SemanticOccurrence>()
  for (const occurrence of occurrences)
    if (!byStart.has(occurrence.span.start)) byStart.set(occurrence.span.start, occurrence)
  const data: Array<number> = []
  let previousLine = 0
  let previousCharacter = 0
  for (const token of SyntaxTree.tokens(syntax.root)) {
    const occurrence = byStart.get(token.span.start)
    const type =
      occurrence !== undefined && token.kind === 'Identifier'
        ? occurrenceTokenType(snapshot, occurrence)
        : lexicalTokenType(token.kind)
    if (type === undefined) continue
    const index = typeIndexes.get(type)
    if (index === undefined) continue
    const start = LineIndex.positionOf(self.index, token.span.start)
    const end = LineIndex.positionOf(self.index, token.span.end)
    if (end.line !== start.line) continue
    const length = end.character - start.character
    if (length <= 0) continue
    const deltaLine = start.line - previousLine
    data.push(
      deltaLine,
      deltaLine === 0 ? start.character - previousCharacter : start.character,
      length,
      index,
      0,
    )
    previousLine = start.line
    previousCharacter = start.character
  }
  return { data }
}

/** The syntax node kinds whose body an editor may fold, each folded from its own brace pair. */
const foldableKinds: ReadonlySet<SyntaxTree.NodeKind> = new Set([
  'Block',
  'StructDeclaration',
  'ServiceDeclaration',
  'InterfaceDeclaration',
  'ImplDeclaration',
  'MatchExpression',
  'ImportMemberList',
])

/**
 * Returns one folding range for each braced region and each run of comment lines.
 *
 * A region folds from the line holding its opening brace to the line holding its closing one, so
 * the collapsed line keeps the brace that opened it, which is what an editor shows. A region whose
 * braces sit on one line offers nothing to fold and is omitted. Comment runs fold as one range per
 * consecutive block of comment lines: Silk has no delimited block comment, so a run of `//`, `///`,
 * or `//!` lines is the block a reader means to collapse.
 */
export const foldingRanges = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
): ReadonlyArray<FoldingRange> => {
  const syntax = Analysis.syntaxOf(snapshot, self.module)
  if (syntax === undefined) return []
  const ranges: Array<FoldingRange> = []
  const visit = (node: SyntaxTree.Node): void => {
    if (foldableKinds.has(node.kind)) {
      const open = node.children.find(
        (child) => SyntaxTree.isToken(child) && child.kind === 'LeftBrace',
      )
      const close = node.children.findLast(
        (child) => SyntaxTree.isToken(child) && child.kind === 'RightBrace',
      )
      if (open !== undefined && close !== undefined) {
        const startLine = LineIndex.lineOf(self.index, SyntaxTree.span(open).start)
        const endLine = LineIndex.lineOf(self.index, SyntaxTree.span(close).start)
        if (endLine > startLine) ranges.push({ startLine, endLine })
      }
    }
    for (const child of node.children) if (SyntaxTree.isNode(child)) visit(child)
  }
  visit(syntax.root)
  // One run of adjacent comment lines folds as a single block, in source order with the rest.
  let runStart: number | undefined
  let runEnd = 0
  const flush = (): void => {
    if (runStart !== undefined && runEnd > runStart)
      ranges.push({ startLine: runStart, endLine: runEnd, kind: FoldingRangeKind.Comment })
    runStart = undefined
  }
  for (const token of SyntaxTree.tokens(syntax.root)) {
    if (
      token.kind !== 'LineComment' &&
      token.kind !== 'DocComment' &&
      token.kind !== 'ModuleDocComment'
    )
      continue
    const line = LineIndex.lineOf(self.index, token.span.start)
    if (runStart !== undefined && line === runEnd + 1) runEnd = line
    else {
      flush()
      runStart = line
      runEnd = line
    }
  }
  flush()
  return Object.freeze(
    ranges.sort((left, right) =>
      left.startLine !== right.startLine
        ? left.startLine - right.startLine
        : left.endLine - right.endLine,
    ),
  )
}

/** Builds the protocol item naming one source-backed declaration of the analyzed project. */
const callHierarchyItem = (
  declaration: DeclarationIndex.MemberFact,
  indexOf: (module: string) => LineIndex.LineIndex | undefined,
  uriOf: (module: string) => string | undefined,
): CallHierarchyItem | undefined => {
  if (declaration.name._tag !== 'Present') return undefined
  const module = declaration.name.token.span.sourceId
  const uri = uriOf(module)
  const index = indexOf(module)
  if (uri === undefined || index === undefined) return undefined
  return {
    name: declaration.name.spelling,
    kind: declaration._tag === 'FunctionDeclaration' ? SymbolKind.Function : SymbolKind.Constant,
    ...(declaration._tag === 'FunctionDeclaration'
      ? { detail: Presentation.functionDeclaration(declaration).text }
      : {}),
    uri,
    range: LineIndex.rangeOf(index, SyntaxTree.span(declaration.syntax)),
    selectionRange: LineIndex.rangeOf(index, declaration.name.token.span),
    data: SemanticOccurrence.identityKey(
      Object.freeze({
        _tag: 'DeclarationIdentity',
        id: declaration.canonical._tag === 'Canonical' ? declaration.canonical.id : declaration.id,
      }),
    ),
  }
}

/** Every function declaration of the analyzed project, in canonical module and source order. */
const functionDeclarations = (
  snapshot: Analysis.FrontendSnapshot,
): ReadonlyArray<DeclarationIndex.MemberFact> =>
  Analysis.declarationIndex(snapshot)
    .modules.flatMap((module) => module.members)
    .filter((member) => member._tag === 'FunctionDeclaration')

/** The declaration whose body encloses one span, which is the function a call is written inside. */
const enclosingDeclaration = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  span: SourceSpan.SourceSpan,
): DeclarationIndex.MemberFact | undefined =>
  functionDeclarations(snapshot).find((declaration) => {
    const declarationSpan = SyntaxTree.span(declaration.syntax)
    return (
      declarationSpan.sourceId === module &&
      declarationSpan.start <= span.start &&
      span.end <= declarationSpan.end
    )
  })

/**
 * Names the function the cursor selects, which anchors both call directions.
 *
 * The selection reads the semantic occurrence index, so a call site, a use as a value, and the
 * declaration's own name all prepare the same declaration. A position naming no source-backed
 * function prepares nothing rather than the file it sits in.
 */
export const prepareCallHierarchy = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  position: Position,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<CallHierarchyItem> => {
  const occurrence = Analysis.semanticOccurrenceAt(
    snapshot,
    self.module,
    LineIndex.offsetOf(self.index, position),
  )
  if (occurrence?.resolution._tag !== 'Available') return []
  const identity = occurrence.resolution.identity
  if (identity._tag !== 'DeclarationIdentity') return []
  const declaration = Analysis.declarationForIdentity(snapshot, identity)
  if (declaration?._tag !== 'FunctionDeclaration') return []
  const item = callHierarchyItem(declaration, lineIndexes(self, snapshot), (module) =>
    module === self.module ? self.uri : uriOf(module),
  )
  return item === undefined ? [] : Object.freeze([item])
}

/** Resolves the declaration one prepared item was built from, by the identity key it carries. */
const declarationOfItem = (
  snapshot: Analysis.FrontendSnapshot,
  item: CallHierarchyItem,
): DeclarationIndex.MemberFact | undefined =>
  functionDeclarations(snapshot).find(
    (declaration) =>
      SemanticOccurrence.identityKey(
        Object.freeze({
          _tag: 'DeclarationIdentity',
          id:
            declaration.canonical._tag === 'Canonical' ? declaration.canonical.id : declaration.id,
        }),
      ) === item.data,
  )

/**
 * Lists every function that calls the selected one, across every module of the analyzed project.
 *
 * Each use of the selected declaration is attributed to the declaration whose body encloses it, so
 * two calls from one caller collapse into one entry carrying both ranges, and a use written outside
 * any function body — an import clause, the declaration's own name — contributes no caller.
 */
export const incomingCalls = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  item: CallHierarchyItem,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<CallHierarchyIncomingCall> => {
  const target = declarationOfItem(snapshot, item)
  if (target === undefined) return []
  const identity = Object.freeze({
    _tag: 'DeclarationIdentity' as const,
    id: target.canonical._tag === 'Canonical' ? target.canonical.id : target.id,
  })
  const indexOf = lineIndexes(self, snapshot)
  const uriOfModule = (module: string): string | undefined =>
    module === self.module ? self.uri : uriOf(module)
  const callers = new Map<string, { item: CallHierarchyItem; ranges: Array<Range> }>()
  for (const match of matchesOfIdentity(snapshot, identity)) {
    if (match.occurrence.role === 'Declaration' || match.occurrence.role === 'Import') continue
    const caller = enclosingDeclaration(snapshot, match.module, match.occurrence.span)
    if (caller === undefined) continue
    const index = indexOf(match.module)
    if (index === undefined) continue
    const key = SemanticOccurrence.identityKey(
      Object.freeze({
        _tag: 'DeclarationIdentity',
        id: caller.canonical._tag === 'Canonical' ? caller.canonical.id : caller.id,
      }),
    )
    const existing = callers.get(key)
    const range = LineIndex.rangeOf(index, match.occurrence.span)
    if (existing !== undefined) {
      existing.ranges.push(range)
      continue
    }
    const from = callHierarchyItem(caller, indexOf, uriOfModule)
    if (from === undefined) continue
    callers.set(key, { item: from, ranges: [range] })
  }
  return Object.freeze(
    [...callers.values()].map(({ item: from, ranges }) => ({ from, fromRanges: ranges })),
  )
}

/**
 * Lists every function the selected one calls, in the order the calls are written.
 *
 * The selected declaration's own body is scanned through the same occurrence index, so a callee
 * reached through a qualified name is found exactly as a bare one is, and repeated calls to one
 * callee collapse into a single entry carrying every call's range.
 */
export const outgoingCalls = (
  self: Document,
  snapshot: Analysis.FrontendSnapshot,
  item: CallHierarchyItem,
  uriOf: (module: string) => string | undefined,
): ReadonlyArray<CallHierarchyOutgoingCall> => {
  const caller = declarationOfItem(snapshot, item)
  if (caller === undefined) return []
  const body = SyntaxTree.span(caller.syntax)
  const module = body.sourceId
  const indexOf = lineIndexes(self, snapshot)
  const callerIndex = indexOf(module)
  if (callerIndex === undefined) return []
  const uriOfModule = (candidate: string): string | undefined =>
    candidate === self.module ? self.uri : uriOf(candidate)
  const callees = new Map<string, { item: CallHierarchyItem; ranges: Array<Range> }>()
  for (const occurrence of Analysis.semanticOccurrencesInRange(snapshot, module, body)) {
    if (occurrence.role === 'Declaration' || occurrence.resolution._tag !== 'Available') continue
    const identity = occurrence.resolution.identity
    if (identity._tag !== 'DeclarationIdentity') continue
    const callee = Analysis.declarationForIdentity(snapshot, identity)
    if (callee?._tag !== 'FunctionDeclaration') continue
    const key = SemanticOccurrence.identityKey(identity)
    const range = LineIndex.rangeOf(callerIndex, occurrence.span)
    const existing = callees.get(key)
    if (existing !== undefined) {
      existing.ranges.push(range)
      continue
    }
    const to = callHierarchyItem(callee, indexOf, uriOfModule)
    if (to === undefined) continue
    callees.set(key, { item: to, ranges: [range] })
  }
  return Object.freeze(
    [...callees.values()].map(({ item: to, ranges }) => ({ to, fromRanges: ranges })),
  )
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
