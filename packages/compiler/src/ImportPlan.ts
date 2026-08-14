import * as Option from 'effect/Option'
import * as SourceAction from './SourceAction.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'

export interface Request {
  readonly syntax: SyntaxFile.SyntaxFile
  readonly module: string
  readonly spelling: string
}

const text = (source: SourceFile.SourceFile, element: SyntaxTree.Element): string | undefined =>
  Option.getOrUndefined(SourceFile.spelling(source, element.span))

const pathOf = (
  source: SourceFile.SourceFile,
  declaration: SyntaxTree.Node,
): string | undefined => {
  const path = SyntaxTree.directNode(declaration, 'ImportPath')
  if (path === undefined || !SyntaxTree.isAvailableSyntax(path)) return undefined
  const names = SyntaxTree.tokens(path).filter((token) => token.kind === 'Identifier')
  if (names.length === 0) return undefined
  return names.map((name) => text(source, name)).join('/')
}

const importedSpellings = (
  source: SourceFile.SourceFile,
  list: SyntaxTree.Node,
): ReadonlyArray<string> =>
  SyntaxTree.directNodes(list, 'ImportMember').flatMap((member): ReadonlyArray<string> => {
    if (!SyntaxTree.isAvailableSyntax(member)) return []
    const names = SyntaxTree.tokens(member).filter((token) => token.kind === 'Identifier')
    return names.flatMap((name) => {
      const value = text(source, name)
      return value === undefined ? [] : [value]
    })
  })

const newlineOf = (source: SourceFile.SourceFile): string => {
  for (let index = 0; index < source.bytes.length; index += 1)
    if (source.bytes[index] === 0x0a) return source.bytes[index - 1] === 0x0d ? '\r\n' : '\n'
  return '\n'
}

const indentationAt = (source: SourceFile.SourceFile, offset: number): string => {
  let start = offset
  while (start > 0 && source.bytes[start - 1] !== 0x0a && source.bytes[start - 1] !== 0x0d)
    start -= 1
  let end = start
  while (source.bytes[end] === 0x20 || source.bytes[end] === 0x09) end += 1
  return Array.from(source.bytes.slice(start, end), (byte) => String.fromCharCode(byte)).join('')
}

const containsNewline = (source: SourceFile.SourceFile, start: number, end: number): boolean =>
  source.bytes.slice(start, end).some((byte) => byte === 0x0a || byte === 0x0d)

const insertion = (
  source: SourceFile.SourceFile,
  offset: number,
  replacement: string,
): SourceAction.Edit | undefined => {
  const span = Option.getOrUndefined(SourceSpan.make(source, offset, offset))
  return span === undefined ? undefined : SourceAction.edit(span, replacement)
}

const extendMemberList = (
  source: SourceFile.SourceFile,
  list: SyntaxTree.Node,
  spelling: string,
): SourceAction.Edit | undefined => {
  if (!SyntaxTree.isAvailableSyntax(list)) return undefined
  if (importedSpellings(source, list).includes(spelling)) return undefined
  const members = SyntaxTree.directNodes(list, 'ImportMember')
  const last = members.at(-1)
  const right = SyntaxTree.directToken(list, 'RightBrace')
  if (last === undefined || right === undefined || !SyntaxTree.isAvailableSyntax(last))
    return undefined
  const tokensAfterLast = SyntaxTree.tokens(list).filter(
    (token) => token.span.start >= last.span.end && token.span.end <= right.span.start,
  )
  const trailingComma = tokensAfterLast.some((token) => token.kind === 'Comma')
  const multiline = containsNewline(source, list.span.start, list.span.end)
  if (!multiline)
    return trailingComma
      ? insertion(source, right.span.start, `${spelling}, `)
      : insertion(source, last.span.end, `, ${spelling}`)
  const newline = newlineOf(source)
  const firstMember = members.at(0)
  const firstName =
    firstMember === undefined ? undefined : SyntaxTree.directToken(firstMember, 'Identifier')
  const indent = indentationAt(source, firstName?.span.start ?? last.span.start)
  return trailingComma
    ? insertion(source, right.span.start, `${indent}${spelling},${newline}`)
    : insertion(source, last.span.end, `,${newline}${indent}${spelling}`)
}

const extendNamespaceImport = (
  source: SourceFile.SourceFile,
  declaration: SyntaxTree.Node,
  spelling: string,
): SourceAction.Edit | undefined =>
  SyntaxTree.isAvailableSyntax(declaration)
    ? insertion(source, declaration.span.end, ` { ${spelling} }`)
    : undefined

const newImport = (
  syntax: SyntaxFile.SyntaxFile,
  module: string,
  spelling: string,
): SourceAction.Edit | undefined => {
  const imports = SyntaxTree.directNodes(syntax.root, 'ImportDeclaration')
  const newline = newlineOf(syntax.source)
  const last = imports.at(-1)
  let leadingModuleDocumentationEnd = 0
  let sawModuleDocumentation = false
  for (const token of syntax.tokens) {
    if (token.kind === 'ModuleDocComment') {
      sawModuleDocumentation = true
      leadingModuleDocumentationEnd = token.span.end
      continue
    }
    if (sawModuleDocumentation && token.kind === 'Whitespace') {
      leadingModuleDocumentationEnd = token.span.end
      continue
    }
    break
  }
  const offset =
    last?.span.end ??
    (sawModuleDocumentation
      ? leadingModuleDocumentationEnd
      : (syntax.root.children.at(0)?.span.start ?? 0))
  const sourceModule = module.split('/').join('.')
  const replacement =
    last === undefined
      ? `import ${sourceModule} { ${spelling} }${newline}`
      : `${newline}import ${sourceModule} { ${spelling} }`
  return insertion(syntax.source, offset, replacement)
}

/** Plans one selected-member import without rewriting unrelated syntax or trivia. */
export const make = (request: Request): Option.Option<SourceAction.ChangePlan> => {
  const imports = SyntaxTree.directNodes(request.syntax.root, 'ImportDeclaration')
  const matching = imports.filter(
    (declaration) => pathOf(request.syntax.source, declaration) === request.module,
  )
  if (matching.length > 1) return Option.none()
  const declaration = matching.at(0)
  let edit: SourceAction.Edit | undefined
  if (declaration === undefined) edit = newImport(request.syntax, request.module, request.spelling)
  else {
    const list = SyntaxTree.directNode(declaration, 'ImportMemberList')
    edit =
      list === undefined
        ? extendNamespaceImport(request.syntax.source, declaration, request.spelling)
        : extendMemberList(request.syntax.source, list, request.spelling)
  }
  if (edit === undefined) return Option.none()
  return SourceAction.changePlan({
    preconditions: [SourceAction.precondition(request.syntax.source)],
    changes: [[request.syntax.source.id, [edit]]],
  })
}
