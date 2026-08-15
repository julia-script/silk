import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import type * as Diagnostic from './Diagnostic.js'
import * as FormattedDocument from './FormattedDocument.js'
import * as FormatDocument from './internal/FormatDocument.js'
import * as LiteralForm from './LiteralForm.js'
import * as SourceFile from './SourceFile.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** Expected refusal when source recovery prevents semantics-preserving formatting. */
export class FormatterError extends Data.TaggedError('FormatterError')<{
  readonly operation: 'Formatter.format'
  readonly sourceId: string
  readonly message: string
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly reason: { readonly _tag: 'DamagedSyntax' }
}> {}

class FormatterImplementationError extends Error {}

const isTrivia = (kind: Token.TokenKind): boolean =>
  kind === 'Whitespace' ||
  kind === 'LineComment' ||
  kind === 'DocComment' ||
  kind === 'ModuleDocComment'

interface Gap {
  readonly previous: Token.Token | undefined
  readonly trivia: ReadonlyArray<Token.Token>
}

interface Context {
  readonly syntax: SyntaxFile.SyntaxFile
  readonly gaps: ReadonlyMap<Token.Token, Gap>
}

const makeContext = (syntax: SyntaxFile.SyntaxFile): Context => {
  const gaps = new Map<Token.Token, Gap>()
  let previous: Token.Token | undefined
  let trivia: Array<Token.Token> = []
  for (const token of syntax.tokens) {
    if (isTrivia(token.kind)) {
      trivia.push(token)
      continue
    }
    gaps.set(token, { previous, trivia: Object.freeze(trivia) })
    previous = token
    trivia = []
  }
  return Object.freeze({ syntax, gaps })
}

const bytes = (context: Context, token: Token.Token): Uint8Array =>
  Option.getOrThrowWith(
    SourceFile.slice(context.syntax.source, token.span),
    () => new FormatterImplementationError(`Token span is outside ${context.syntax.source.id}`),
  )

const lineBreaks = (context: Context, tokens: ReadonlyArray<Token.Token>): number => {
  let count = 0
  for (const token of tokens) {
    if (token.kind !== 'Whitespace') continue
    const whitespace = bytes(context, token)
    for (let index = 0; index < whitespace.length; index += 1) {
      const byte = whitespace[index]
      if (byte === 0x0a) count += 1
      else if (byte === 0x0d) {
        count += 1
        if (whitespace[index + 1] === 0x0a) index += 1
      }
    }
  }
  return count
}

const gapOf = (context: Context, token: Token.Token): Gap =>
  context.gaps.get(token) ?? { previous: undefined, trivia: Object.freeze([]) }

const commentCount = (gap: Gap): number =>
  gap.trivia.filter(
    (token) =>
      token.kind === 'LineComment' ||
      token.kind === 'DocComment' ||
      token.kind === 'ModuleDocComment',
  ).length

const hasComments = (context: Context, token: Token.Token): boolean =>
  commentCount(gapOf(context, token)) > 0

const commentLeading = (
  context: Context,
  token: Token.Token,
  prefix: FormatDocument.Document,
  preserveBlank: boolean,
): FormatDocument.Document => {
  const gap = gapOf(context, token)
  const comments = gap.trivia.filter(
    (trivia) =>
      trivia.kind === 'LineComment' ||
      trivia.kind === 'DocComment' ||
      trivia.kind === 'ModuleDocComment',
  )
  if (comments.length === 0) {
    return preserveBlank && lineBreaks(context, gap.trivia) >= 2
      ? FormatDocument.concat(prefix, FormatDocument.hardLine)
      : prefix
  }

  const documents: Array<FormatDocument.Document> = []
  let triviaStart = 0
  for (const [index, comment] of comments.entries()) {
    const triviaIndex = gap.trivia.indexOf(comment, triviaStart)
    const before = triviaIndex < 0 ? [] : gap.trivia.slice(triviaStart, triviaIndex)
    const breaks = lineBreaks(context, before)
    if (index === 0) {
      if (gap.previous !== undefined && breaks === 0) {
        documents.push(gap.previous.kind === 'Comma' ? prefix : FormatDocument.text(' '))
      } else {
        documents.push(prefix)
        if (preserveBlank && breaks >= 2) documents.push(FormatDocument.hardLine)
      }
    } else {
      documents.push(FormatDocument.hardLine)
      if (breaks >= 2) documents.push(FormatDocument.hardLine)
    }
    documents.push(FormatDocument.text(bytes(context, comment)))
    triviaStart = triviaIndex < 0 ? gap.trivia.length : triviaIndex + 1
  }

  const after = gap.trivia.slice(triviaStart)
  documents.push(FormatDocument.hardLine)
  if (lineBreaks(context, after) >= 2) documents.push(FormatDocument.hardLine)
  return FormatDocument.concat(...documents)
}

const printToken = (
  context: Context,
  token: Token.Token,
  prefix: FormatDocument.Document = FormatDocument.empty,
  preserveBlank = false,
): FormatDocument.Document => {
  const spelling = bytes(context, token)
  const form =
    token.kind === 'TextLiteral' || token.kind === 'ByteStringLiteral'
      ? LiteralForm.recognize(spelling)
      : undefined
  return FormatDocument.concat(
    commentLeading(context, token, prefix, preserveBlank),
    form?.delimiterWidth === 3
      ? FormatDocument.verbatimMultiline(spelling)
      : FormatDocument.text(spelling),
  )
}

const directTokens = (node: SyntaxTree.Node): ReadonlyArray<Token.Token> =>
  node.children.filter(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && !isTrivia(element.kind) && element.kind !== 'EndOfFile',
  )

const directNodes = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.filter(SyntaxTree.isNode)

const tokenOf = (node: SyntaxTree.Node, kind: Token.TokenKind, occurrence = 0): Token.Token => {
  let found = 0
  for (const token of directTokens(node)) {
    if (token.kind !== kind) continue
    if (found === occurrence) return token
    found += 1
  }
  throw new FormatterImplementationError(`${node.kind} has no ${kind} token ${occurrence}`)
}

const nodeOf = (
  node: SyntaxTree.Node,
  kind: SyntaxTree.NodeKind,
  occurrence = 0,
): SyntaxTree.Node => {
  let found = 0
  for (const child of directNodes(node)) {
    if (child.kind !== kind) continue
    if (found === occurrence) return child
    found += 1
  }
  throw new FormatterImplementationError(`${node.kind} has no ${kind} node ${occurrence}`)
}

const commaTokens = (node: SyntaxTree.Node): ReadonlyArray<Token.Token> =>
  directTokens(node).filter((token) => token.kind === 'Comma')

const beginsWithTrailingComment = (context: Context, node: SyntaxTree.Node): boolean => {
  const first = SyntaxTree.tokens(node).find(
    (token) => !isTrivia(token.kind) && token.kind !== 'EndOfFile',
  )
  if (first === undefined) return false
  const gap = gapOf(context, first)
  const commentIndex = gap.trivia.findIndex(
    (token) =>
      token.kind === 'LineComment' ||
      token.kind === 'DocComment' ||
      token.kind === 'ModuleDocComment',
  )
  return (
    gap.previous?.kind === 'Comma' &&
    commentIndex >= 0 &&
    lineBreaks(context, gap.trivia.slice(0, commentIndex)) === 0
  )
}

const trailingComma = (
  context: Context,
  token: Token.Token | undefined,
): FormatDocument.Document => {
  if (token === undefined) return FormatDocument.ifBreak(FormatDocument.text(','))
  return hasComments(context, token)
    ? printToken(context, token)
    : FormatDocument.ifBreak(FormatDocument.text(','))
}

const printDelimited = (
  context: Context,
  open: Token.Token,
  items: ReadonlyArray<SyntaxTree.Node>,
  commas: ReadonlyArray<Token.Token>,
  close: Token.Token,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const openDocument = printToken(context, open, prefix)
  if (items.length === 0) return FormatDocument.concat(openDocument, printToken(context, close))

  const breakLine = FormatDocument.ifBreak(FormatDocument.hardLine)
  const itemDocuments = items.map((item, index) => {
    if (index === 0) return printNode(context, item, breakLine)
    const separator = commas.at(index - 1)
    const comma =
      separator === undefined ? FormatDocument.text(',') : printToken(context, separator)
    return printNode(
      context,
      item,
      FormatDocument.concat(
        comma,
        beginsWithTrailingComment(context, item)
          ? FormatDocument.text(' ')
          : FormatDocument.softLine,
      ),
    )
  })
  const originalTrailingComma = commas.at(items.length - 1)
  return FormatDocument.group(
    FormatDocument.concat(
      openDocument,
      FormatDocument.indent(
        FormatDocument.concat(...itemDocuments, trailingComma(context, originalTrailingComma)),
      ),
      printToken(context, close, breakLine),
    ),
  )
}

const printTokenSequence = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
  separator: FormatDocument.Document,
  preserveBlank = false,
): FormatDocument.Document => {
  const tokens = directTokens(node)
  return FormatDocument.concat(
    ...tokens.map((token, index) =>
      printToken(context, token, index === 0 ? prefix : separator, index === 0 && preserveBlank),
    ),
  )
}

const printBlock = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const open = tokenOf(node, 'LeftBrace')
  const close = tokenOf(node, 'RightBrace')
  const statements = directNodes(node)
  if (statements.length === 0) {
    return FormatDocument.concat(printToken(context, open, prefix), printToken(context, close))
  }
  return FormatDocument.concat(
    printToken(context, open, prefix),
    FormatDocument.indent(
      FormatDocument.concat(
        ...statements.map((statement) =>
          printNode(context, statement, FormatDocument.hardLine, true),
        ),
      ),
    ),
    printToken(context, close, FormatDocument.hardLine),
  )
}

const printStructDeclaration = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const fields = directNodes(node).filter((child) => child.kind === 'StructField')
  const typeParameters = directNodes(node).find((child) => child.kind === 'TypeParameterList')
  const publicKeyword = directTokens(node).find((token) => token.kind === 'PubKeyword')
  const head = FormatDocument.concat(
    ...(publicKeyword === undefined
      ? []
      : [printToken(context, publicKeyword, prefix), FormatDocument.text(' ')]),
    printToken(
      context,
      tokenOf(node, 'StructKeyword'),
      publicKeyword === undefined ? prefix : FormatDocument.empty,
    ),
    printToken(context, tokenOf(node, 'Identifier'), FormatDocument.text(' ')),
    ...(typeParameters === undefined ? [] : [printNode(context, typeParameters)]),
  )
  const open = tokenOf(node, 'LeftBrace')
  const close = tokenOf(node, 'RightBrace')
  if (fields.length === 0) {
    return FormatDocument.concat(
      head,
      printToken(context, open, FormatDocument.text(' ')),
      printToken(context, close),
    )
  }
  return FormatDocument.concat(
    head,
    printToken(context, open, FormatDocument.text(' ')),
    FormatDocument.indent(
      FormatDocument.concat(
        ...fields.map((field) => printNode(context, field, FormatDocument.hardLine, true)),
      ),
    ),
    printToken(context, close, FormatDocument.hardLine),
  )
}

const printServiceOperation = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const effectKeyword = directTokens(node).find((token) => token.kind === 'EffectKeyword')
  const typeParameters = directNodes(node).find((child) => child.kind === 'TypeParameterList')
  const failureRow = directNodes(node).find((child) => child.kind === 'FailureRow')
  const requirementRow = directNodes(node).find((child) => child.kind === 'RequirementRow')
  const body = directNodes(node).find((child) => child.kind === 'Block')
  return FormatDocument.concat(
    ...(effectKeyword === undefined
      ? []
      : [printToken(context, effectKeyword, prefix), FormatDocument.text(' ')]),
    printToken(
      context,
      tokenOf(node, 'FnKeyword'),
      effectKeyword === undefined ? prefix : FormatDocument.empty,
    ),
    printToken(context, tokenOf(node, 'Identifier'), FormatDocument.text(' ')),
    ...(typeParameters === undefined ? [] : [printNode(context, typeParameters)]),
    printNode(context, nodeOf(node, 'ParameterList')),
    ...(directNodes(node).find((child) => child.kind === 'ReturnType') === undefined
      ? []
      : [printNode(context, nodeOf(node, 'ReturnType'), FormatDocument.text(' '))]),
    ...(failureRow === undefined ? [] : [printNode(context, failureRow, FormatDocument.text(' '))]),
    ...(requirementRow === undefined
      ? []
      : [printNode(context, requirementRow, FormatDocument.text(' '))]),
    ...(body === undefined ? [] : [printNode(context, body, FormatDocument.text(' '))]),
  )
}

const printServiceDeclaration = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const publicKeyword = directTokens(node).find((token) => token.kind === 'PubKeyword')
  const typeParameters = directNodes(node).find((child) => child.kind === 'TypeParameterList')
  const operations = directNodes(node).filter((child) => child.kind === 'ServiceOperation')
  const head = FormatDocument.concat(
    ...(publicKeyword === undefined
      ? []
      : [printToken(context, publicKeyword, prefix), FormatDocument.text(' ')]),
    printToken(
      context,
      tokenOf(node, node.kind === 'InterfaceDeclaration' ? 'InterfaceKeyword' : 'ServiceKeyword'),
      publicKeyword === undefined ? prefix : FormatDocument.empty,
    ),
    printToken(context, tokenOf(node, 'Identifier'), FormatDocument.text(' ')),
    ...(typeParameters === undefined ? [] : [printNode(context, typeParameters)]),
    printToken(context, tokenOf(node, 'LeftBrace'), FormatDocument.text(' ')),
  )
  if (operations.length === 0)
    return FormatDocument.concat(head, printToken(context, tokenOf(node, 'RightBrace')))
  return FormatDocument.concat(
    head,
    FormatDocument.indent(
      FormatDocument.concat(
        ...operations.map((operation) =>
          printServiceOperation(context, operation, FormatDocument.hardLine),
        ),
      ),
    ),
    printToken(context, tokenOf(node, 'RightBrace'), FormatDocument.hardLine),
  )
}

const printConstantDeclaration = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const publicKeyword = directTokens(node).find((token) => token.kind === 'PubKeyword')
  const value = directNodes(node).at(-1) ?? nodeOf(node, 'IntegerLiteralExpression')
  const type = directNodes(node).at(0) ?? nodeOf(node, 'TypePath')
  return FormatDocument.concat(
    ...(publicKeyword === undefined
      ? []
      : [printToken(context, publicKeyword, prefix), FormatDocument.text(' ')]),
    printToken(
      context,
      tokenOf(node, 'ConstKeyword'),
      publicKeyword === undefined ? prefix : FormatDocument.empty,
    ),
    printToken(context, tokenOf(node, 'Identifier'), FormatDocument.text(' ')),
    printToken(context, tokenOf(node, 'Colon')),
    printNode(context, type, FormatDocument.text(' ')),
    printToken(context, tokenOf(node, 'Equals'), FormatDocument.text(' ')),
    printNode(context, value, FormatDocument.text(' ')),
  )
}

const printFunctionDeclaration = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const publicKeyword = directTokens(node).find((token) => token.kind === 'PubKeyword')
  const effectKeyword = directTokens(node).find((token) => token.kind === 'EffectKeyword')
  const typeParameters = directNodes(node).find((child) => child.kind === 'TypeParameterList')
  const failureRow = directNodes(node).find((child) => child.kind === 'FailureRow')
  const requirementRow = directNodes(node).find((child) => child.kind === 'RequirementRow')
  const name =
    directTokens(node).find((token) => token.kind === 'Identifier') ??
    directTokens(node).find((token) => token.kind === 'DropKeyword')
  if (name === undefined)
    throw new FormatterImplementationError('FunctionDeclaration has no function name')
  return FormatDocument.concat(
    ...(publicKeyword === undefined
      ? []
      : [printToken(context, publicKeyword, prefix), FormatDocument.text(' ')]),
    ...(effectKeyword === undefined
      ? []
      : [
          printToken(
            context,
            effectKeyword,
            publicKeyword === undefined ? prefix : FormatDocument.empty,
          ),
          FormatDocument.text(' '),
        ]),
    printToken(
      context,
      tokenOf(node, 'FnKeyword'),
      publicKeyword === undefined && effectKeyword === undefined ? prefix : FormatDocument.empty,
    ),
    printToken(context, name, FormatDocument.text(' ')),
    ...(typeParameters === undefined ? [] : [printNode(context, typeParameters)]),
    printNode(context, nodeOf(node, 'ParameterList')),
    printNode(context, nodeOf(node, 'ReturnType'), FormatDocument.text(' ')),
    ...(failureRow === undefined ? [] : [printNode(context, failureRow, FormatDocument.hardLine)]),
    ...(requirementRow === undefined
      ? []
      : [printNode(context, requirementRow, FormatDocument.hardLine)]),
    printNode(context, nodeOf(node, 'Block'), FormatDocument.text(' ')),
  )
}

const printImplDeclaration = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const nodes = directNodes(node)
  const typeParameters = nodes[0]?.kind === 'TypeParameterList' ? nodes[0] : undefined
  const positional = typeParameters === undefined ? nodes : nodes.slice(1)
  const capability = positional[0] ?? nodeOf(node, 'TypePath')
  const target = positional[1] ?? nodeOf(node, 'TypePath', 1)
  const members = positional.slice(2)
  const open = tokenOf(node, 'LeftBrace')
  const close = tokenOf(node, 'RightBrace')
  const head = FormatDocument.concat(
    printToken(context, tokenOf(node, 'ImplKeyword'), prefix),
    ...(typeParameters === undefined ? [] : [printNode(context, typeParameters)]),
    printNode(context, capability, FormatDocument.text(' ')),
    printToken(context, tokenOf(node, 'ForKeyword'), FormatDocument.text(' ')),
    printNode(context, target, FormatDocument.text(' ')),
    printToken(context, open, FormatDocument.text(' ')),
  )
  if (members.length === 0) return FormatDocument.concat(head, printToken(context, close))
  return FormatDocument.concat(
    head,
    FormatDocument.indent(
      FormatDocument.concat(
        ...members.map((member) => printNode(context, member, FormatDocument.hardLine, true)),
      ),
    ),
    printToken(context, close, FormatDocument.hardLine),
  )
}

const printSourceFile = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document,
): FormatDocument.Document => {
  const declarations = directNodes(node)
  const declarationDocuments = declarations.map((declaration, index) =>
    printNode(
      context,
      declaration,
      index === 0
        ? prefix
        : FormatDocument.concat(FormatDocument.hardLine, FormatDocument.hardLine),
    ),
  )
  const endOfFile = node.children.find(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'EndOfFile',
  )
  return FormatDocument.concat(
    ...declarationDocuments,
    ...(endOfFile === undefined ? [] : [printToken(context, endOfFile, FormatDocument.hardLine)]),
  )
}

const printNode = (
  context: Context,
  node: SyntaxTree.Node,
  prefix: FormatDocument.Document = FormatDocument.empty,
  preserveBlank = false,
): FormatDocument.Document => {
  switch (node.kind) {
    case 'SourceFile':
      return printSourceFile(context, node, prefix)
    case 'ImportDeclaration': {
      const keyword = tokenOf(node, 'ImportKeyword')
      const path = nodeOf(node, 'ImportPath')
      const alias = directNodes(node).find((child) => child.kind === 'ImportAlias')
      const members = directNodes(node).find((child) => child.kind === 'ImportMemberList')
      return FormatDocument.concat(
        printToken(context, keyword, prefix, preserveBlank),
        printNode(context, path, FormatDocument.text(' ')),
        ...(alias === undefined ? [] : [printNode(context, alias, FormatDocument.text(' '))]),
        ...(members === undefined ? [] : [printNode(context, members, FormatDocument.text(' '))]),
      )
    }
    case 'ImportPath':
    case 'TypePath':
      return printTokenSequence(context, node, prefix, FormatDocument.empty, preserveBlank)
    case 'TypeArgumentList': {
      const nodes = directNodes(node)
      const failure = nodes.find((child) => child.kind === 'FailureRow')
      const requirements = nodes.find((child) => child.kind === 'RequirementRow')
      if (failure === undefined && requirements === undefined)
        return printDelimited(
          context,
          tokenOf(node, 'Less'),
          nodes,
          commaTokens(node),
          tokenOf(node, 'Greater'),
          prefix,
        )
      const arguments_ = nodes.filter(
        (child) => child.kind !== 'FailureRow' && child.kind !== 'RequirementRow',
      )
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Less'), prefix, preserveBlank),
        ...arguments_.map((argument, ordinal) =>
          printNode(
            context,
            argument,
            ordinal === 0 ? FormatDocument.empty : FormatDocument.text(', '),
          ),
        ),
        ...(failure === undefined ? [] : [printNode(context, failure, FormatDocument.text(' '))]),
        ...(requirements === undefined
          ? []
          : [printNode(context, requirements, FormatDocument.text(' '))]),
        printToken(context, tokenOf(node, 'Greater')),
      )
    }
    case 'TypeParameterList':
    case 'CallTypeArgumentList':
      return printDelimited(
        context,
        tokenOf(node, 'Less'),
        directNodes(node),
        commaTokens(node),
        tokenOf(node, 'Greater'),
        prefix,
      )
    case 'TypeParameter': {
      const marker = directTokens(node).find(
        (token) => token.kind === 'Bang' || token.kind === 'Question',
      )
      const colon = directTokens(node).find((token) => token.kind === 'Colon')
      const bound = directNodes(node).at(0)
      return FormatDocument.concat(
        ...(marker === undefined ? [] : [printToken(context, marker, prefix, preserveBlank)]),
        printToken(
          context,
          tokenOf(node, 'Identifier'),
          marker === undefined ? prefix : FormatDocument.empty,
          preserveBlank,
        ),
        ...(colon === undefined ? [] : [printToken(context, colon)]),
        ...(bound === undefined
          ? []
          : [printNode(context, bound, FormatDocument.text(' '), preserveBlank)]),
      )
    }
    case 'AppliedType': {
      const nodes = directNodes(node)
      const mut = directTokens(node).find((token) => token.kind === 'MutKeyword')
      const once = directTokens(node).find((token) => token.kind === 'OnceKeyword')
      return FormatDocument.concat(
        ...(mut === undefined
          ? []
          : [printToken(context, mut, prefix, preserveBlank), FormatDocument.text(' ')]),
        ...(once === undefined
          ? []
          : [printToken(context, once, prefix, preserveBlank), FormatDocument.text(' ')]),
        printNode(
          context,
          nodes[0] ?? nodeOf(node, 'TypePath'),
          mut === undefined && once === undefined ? prefix : FormatDocument.empty,
          preserveBlank,
        ),
        printNode(context, nodes[1] ?? nodeOf(node, 'TypeArgumentList')),
      )
    }
    case 'ImportAlias':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'AsKeyword'), prefix, preserveBlank),
        printToken(context, tokenOf(node, 'Identifier'), FormatDocument.text(' ')),
      )
    case 'ImportMemberList':
      return printDelimited(
        context,
        tokenOf(node, 'LeftBrace'),
        directNodes(node),
        commaTokens(node),
        tokenOf(node, 'RightBrace'),
        prefix,
      )
    case 'ImportMember': {
      const alias = directNodes(node).find((child) => child.kind === 'ImportAlias')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Identifier'), prefix, preserveBlank),
        ...(alias === undefined ? [] : [printNode(context, alias, FormatDocument.text(' '))]),
      )
    }
    case 'StructDeclaration':
      return printStructDeclaration(context, node, prefix)
    case 'ServiceDeclaration':
    case 'InterfaceDeclaration':
      return printServiceDeclaration(context, node, prefix)
    case 'ServiceOperation':
      return printServiceOperation(context, node, prefix)
    case 'ServiceInvalidMember':
      throw new FormatterImplementationError('Damaged service member reached the syntax printer')
    case 'ConstantDeclaration':
      return printConstantDeclaration(context, node, prefix)
    case 'ImplDeclaration':
      return printImplDeclaration(context, node, prefix)
    case 'ImplOperation':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Identifier'), prefix, preserveBlank),
        printToken(context, tokenOf(node, 'Colon')),
        printNode(context, nodeOf(node, 'TypePath'), FormatDocument.text(' ')),
      )
    case 'StructField': {
      const publicKeyword = directTokens(node).find((token) => token.kind === 'PubKeyword')
      return FormatDocument.concat(
        ...(publicKeyword === undefined
          ? []
          : [printToken(context, publicKeyword, prefix, preserveBlank), FormatDocument.text(' ')]),
        printToken(
          context,
          tokenOf(node, 'Identifier'),
          publicKeyword === undefined ? prefix : FormatDocument.empty,
          preserveBlank,
        ),
        printToken(context, tokenOf(node, 'Colon')),
        printNode(
          context,
          directNodes(node).at(-1) ?? nodeOf(node, 'TypePath'),
          FormatDocument.text(' '),
        ),
      )
    }
    case 'FixedArrayType':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'LeftBracket'), prefix, preserveBlank),
        printNode(context, directNodes(node)[0] ?? nodeOf(node, 'TypePath')),
        printToken(context, tokenOf(node, 'Semicolon')),
        printToken(context, tokenOf(node, 'DecimalInteger'), FormatDocument.text(' ')),
        printToken(context, tokenOf(node, 'RightBracket')),
      )
    case 'SliceType': {
      const mut = directTokens(node).find((token) => token.kind === 'MutKeyword')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Ampersand'), prefix, preserveBlank),
        ...(mut === undefined ? [] : [printToken(context, mut), FormatDocument.text(' ')]),
        printToken(context, tokenOf(node, 'LeftBracket')),
        printNode(context, directNodes(node)[0] ?? nodeOf(node, 'TypePath')),
        printToken(context, tokenOf(node, 'RightBracket')),
      )
    }
    case 'ReferenceType': {
      const mut = directTokens(node).find((token) => token.kind === 'MutKeyword')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Ampersand'), prefix, preserveBlank),
        ...(mut === undefined ? [] : [printToken(context, mut), FormatDocument.text(' ')]),
        printNode(context, directNodes(node)[0] ?? nodeOf(node, 'TypePath')),
      )
    }
    case 'CallableType': {
      const nodes = directNodes(node)
      const result = nodes.at(-1) ?? nodeOf(node, 'TypePath')
      const parameters = nodes.slice(0, -1)
      const mut = directTokens(node).find((token) => token.kind === 'MutKeyword')
      const once = directTokens(node).find((token) => token.kind === 'OnceKeyword')
      return FormatDocument.concat(
        ...(mut === undefined
          ? []
          : [printToken(context, mut, prefix, preserveBlank), FormatDocument.text(' ')]),
        ...(once === undefined
          ? []
          : [printToken(context, once, prefix, preserveBlank), FormatDocument.text(' ')]),
        printToken(
          context,
          tokenOf(node, 'FnKeyword'),
          mut === undefined && once === undefined ? prefix : FormatDocument.empty,
          preserveBlank,
        ),
        printDelimited(
          context,
          tokenOf(node, 'LeftParenthesis'),
          parameters,
          commaTokens(node),
          tokenOf(node, 'RightParenthesis'),
          FormatDocument.empty,
        ),
        printToken(context, tokenOf(node, 'Arrow'), FormatDocument.text(' ')),
        printNode(context, result, FormatDocument.text(' ')),
      )
    }
    case 'UnitType':
      return printTokenSequence(context, node, prefix, FormatDocument.empty, preserveBlank)
    case 'ParenthesizedType':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'LeftParenthesis'), prefix, preserveBlank),
        printNode(context, directNodes(node)[0] ?? nodeOf(node, 'TypePath')),
        printToken(context, tokenOf(node, 'RightParenthesis')),
      )
    case 'UnionType': {
      const members = directNodes(node)
      const separators = directTokens(node).filter((token) => token.kind === 'Pipe')
      const first = members.at(0)
      if (first === undefined) throw new FormatterImplementationError('UnionType has no members')
      const documents: Array<FormatDocument.Document> = [
        printNode(context, first, prefix, preserveBlank),
      ]
      for (const [index, separator] of separators.entries()) {
        const member = members.at(index + 1)
        if (member === undefined)
          throw new FormatterImplementationError('UnionType has no member after separator')
        documents.push(
          printToken(context, separator, FormatDocument.text(' ')),
          printNode(context, member, FormatDocument.text(' ')),
        )
      }
      return FormatDocument.concat(...documents)
    }
    case 'FunctionDeclaration':
      return printFunctionDeclaration(context, node, prefix)
    case 'ParameterList':
      return printDelimited(
        context,
        tokenOf(node, 'LeftParenthesis'),
        directNodes(node),
        commaTokens(node),
        tokenOf(node, 'RightParenthesis'),
        prefix,
      )
    case 'ParameterDeclaration':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Identifier'), prefix, preserveBlank),
        printToken(context, tokenOf(node, 'Colon')),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'TypePath'),
          FormatDocument.text(' '),
        ),
      )
    case 'ReturnType':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Arrow'), prefix, preserveBlank),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'TypePath'),
          FormatDocument.text(' '),
        ),
      )
    case 'FailureRow':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Bang'), prefix, preserveBlank),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'TypePath'),
          FormatDocument.text(' '),
        ),
      )
    case 'RequirementRow': {
      const members = directNodes(node).filter(
        (child) => child.kind === 'Requirement' || child.kind === 'TypePath',
      )
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Question'), prefix, preserveBlank),
        ...members.map((member, ordinal) =>
          printNode(
            context,
            member,
            ordinal === 0 ? FormatDocument.text(' ') : FormatDocument.text(' | '),
          ),
        ),
      )
    }
    case 'Requirement': {
      const mut = directTokens(node).find((token) => token.kind === 'MutKeyword')
      const at = directTokens(node).find((token) => token.kind === 'At')
      const role = directTokens(node).find((token) => token.kind === 'Identifier')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Ampersand'), prefix, preserveBlank),
        ...(mut === undefined ? [] : [printToken(context, mut), FormatDocument.text(' ')]),
        printNode(context, nodeOf(node, 'TypePath')),
        ...(at === undefined ? [] : [printToken(context, at)]),
        ...(role === undefined ? [] : [printToken(context, role)]),
      )
    }
    case 'Block':
      return printBlock(context, node, prefix)
    case 'UnsafeStatement':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'UnsafeKeyword'), prefix, preserveBlank),
        printNode(context, nodeOf(node, 'Block'), FormatDocument.text(' ')),
      )
    case 'ExpressionStatement':
      return printNode(
        context,
        directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
        prefix,
        preserveBlank,
      )
    case 'BindingStatement': {
      const mutableKeyword = directTokens(node).find((token) => token.kind === 'MutKeyword')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'LetKeyword'), prefix, preserveBlank),
        ...(mutableKeyword === undefined
          ? []
          : [printToken(context, mutableKeyword, FormatDocument.text(' '))]),
        printToken(context, tokenOf(node, 'Identifier'), FormatDocument.text(' ')),
        printToken(context, tokenOf(node, 'Equals'), FormatDocument.text(' ')),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
      )
    }
    case 'AssignmentStatement': {
      const nodes = directNodes(node)
      return FormatDocument.concat(
        printNode(context, nodes[0] ?? nodeOf(node, 'IdentifierExpression'), prefix, preserveBlank),
        printToken(context, tokenOf(node, 'Equals'), FormatDocument.text(' ')),
        printNode(
          context,
          nodes[1] ?? nodeOf(node, 'IdentifierExpression', 1),
          FormatDocument.text(' '),
        ),
      )
    }
    case 'ConditionalStatement': {
      const nodes = directNodes(node)
      const elseKeyword = directTokens(node).find((token) => token.kind === 'ElseKeyword')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'IfKeyword'), prefix, preserveBlank),
        printNode(
          context,
          nodes[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
        printNode(context, nodes[1] ?? nodeOf(node, 'Block'), FormatDocument.text(' ')),
        ...(elseKeyword === undefined
          ? []
          : [
              printToken(context, elseKeyword, FormatDocument.text(' ')),
              printNode(context, nodes[2] ?? nodeOf(node, 'Block', 1), FormatDocument.text(' ')),
            ]),
      )
    }
    case 'WhileStatement': {
      const nodes = directNodes(node)
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'WhileKeyword'), prefix, preserveBlank),
        printNode(
          context,
          nodes[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
        printNode(context, nodes[1] ?? nodeOf(node, 'Block'), FormatDocument.text(' ')),
      )
    }
    case 'BreakStatement':
      return printToken(context, tokenOf(node, 'BreakKeyword'), prefix, preserveBlank)
    case 'ContinueStatement':
      return printToken(context, tokenOf(node, 'ContinueKeyword'), prefix, preserveBlank)
    case 'ReturnStatement':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'ReturnKeyword'), prefix, preserveBlank),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IntegerLiteralExpression'),
          FormatDocument.text(' '),
        ),
      )
    case 'FailStatement': {
      const move = directTokens(node).find((token) => token.kind === 'MoveKeyword')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'FailKeyword'), prefix, preserveBlank),
        ...(move === undefined ? [] : [printToken(context, move, FormatDocument.text(' '))]),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
      )
    }
    case 'DropStatement':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'DropKeyword'), prefix, preserveBlank),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
      )
    case 'RoleExpression':
      return printTokenSequence(context, node, prefix, FormatDocument.empty, preserveBlank)
    case 'UnitExpression':
    case 'IntegerLiteralExpression':
    case 'FloatingLiteralExpression':
    case 'StaticTextLiteralExpression':
    case 'CharacterLiteralExpression':
    case 'BooleanLiteralExpression':
    case 'IdentifierExpression':
      return printTokenSequence(context, node, prefix, FormatDocument.empty, preserveBlank)
    case 'MoveExpression':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'MoveKeyword'), prefix, preserveBlank),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
      )
    case 'EffectExpression':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'EffectKeyword'), prefix, preserveBlank),
        printNode(context, nodeOf(node, 'Block'), FormatDocument.text(' ')),
      )
    case 'RunExpression':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'RunKeyword'), prefix, preserveBlank),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
      )
    case 'BorrowExpression': {
      const mut = directTokens(node).find((token) => token.kind === 'MutKeyword')
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Ampersand'), prefix, preserveBlank),
        ...(mut === undefined ? [] : [printToken(context, mut), FormatDocument.text(' ')]),
        printNode(context, directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression')),
      )
    }
    case 'MatchExpression': {
      const nodes = directNodes(node)
      const access = nodes[0] ?? nodeOf(node, 'MatchAccess')
      const scrutinee = nodes[1] ?? nodeOf(node, 'IdentifierExpression')
      const arms = nodes.slice(2)
      const accessTokens = directTokens(access)
      const head = FormatDocument.concat(
        printToken(context, tokenOf(node, 'MatchKeyword'), prefix, preserveBlank),
        ...(accessTokens.length === 0
          ? []
          : [printNode(context, access, FormatDocument.text(' '))]),
        printNode(context, scrutinee, FormatDocument.text(' ')),
        printToken(context, tokenOf(node, 'LeftBrace'), FormatDocument.text(' ')),
      )
      if (arms.length === 0) {
        return FormatDocument.concat(head, printToken(context, tokenOf(node, 'RightBrace')))
      }
      return FormatDocument.concat(
        head,
        FormatDocument.indent(
          FormatDocument.concat(
            ...arms.map((arm) => printNode(context, arm, FormatDocument.hardLine, true)),
          ),
        ),
        printToken(context, tokenOf(node, 'RightBrace'), FormatDocument.hardLine),
      )
    }
    case 'MatchAccess':
      return printTokenSequence(context, node, prefix, FormatDocument.empty, preserveBlank)
    case 'MatchArm': {
      const nodes = directNodes(node)
      const pattern = nodes[0] ?? nodeOf(node, 'UniversalPattern')
      const guardKeyword = directTokens(node).find((token) => token.kind === 'IfKeyword')
      const result = nodes.at(-1) ?? nodeOf(node, 'IdentifierExpression')
      const guard = guardKeyword === undefined ? undefined : nodes[1]
      return FormatDocument.concat(
        printNode(context, pattern, prefix, preserveBlank),
        ...(guardKeyword === undefined || guard === undefined
          ? []
          : [
              printToken(context, guardKeyword, FormatDocument.text(' ')),
              printNode(context, guard, FormatDocument.text(' ')),
            ]),
        printToken(context, tokenOf(node, 'FatArrow'), FormatDocument.text(' ')),
        printNode(context, result, FormatDocument.text(' ')),
      )
    }
    case 'BindingPattern': {
      const nodes = directNodes(node)
      return FormatDocument.concat(
        printNode(context, nodes[0] ?? nodeOf(node, 'TypePath'), prefix, preserveBlank),
        printToken(context, tokenOf(node, 'Identifier'), FormatDocument.text(' ')),
      )
    }
    case 'NominalPattern': {
      const nodes = directNodes(node)
      return FormatDocument.concat(
        printNode(context, nodes[0] ?? nodeOf(node, 'TypePath'), prefix, preserveBlank),
        printDelimited(
          context,
          tokenOf(node, 'LeftBrace'),
          nodes.slice(1),
          commaTokens(node),
          tokenOf(node, 'RightBrace'),
          FormatDocument.text(' '),
        ),
      )
    }
    case 'UniversalPattern':
    case 'RestPattern':
      return printTokenSequence(context, node, prefix, FormatDocument.empty, preserveBlank)
    case 'PatternField': {
      const identifiers = directTokens(node).filter((token) => token.kind === 'Identifier')
      const nested = directNodes(node).find((child) => child.kind === 'NominalPattern')
      const colon = directTokens(node).find((token) => token.kind === 'Colon')
      const name = identifiers[0] ?? tokenOf(node, 'Identifier')
      if (colon === undefined) return printToken(context, name, prefix, preserveBlank)
      return FormatDocument.concat(
        printToken(context, name, prefix, preserveBlank),
        printToken(context, colon),
        nested === undefined
          ? printToken(
              context,
              identifiers[1] ?? tokenOf(node, 'Identifier', 1),
              FormatDocument.text(' '),
            )
          : printNode(context, nested, FormatDocument.text(' ')),
      )
    }
    case 'StructLiteralExpression': {
      const nodes = directNodes(node)
      const target = nodes[0] ?? nodeOf(node, 'TypePath')
      const fields = nodes.slice(1)
      return FormatDocument.concat(
        printNode(context, target, prefix),
        printDelimited(
          context,
          tokenOf(node, 'LeftBrace'),
          fields,
          commaTokens(node),
          tokenOf(node, 'RightBrace'),
          FormatDocument.text(' '),
        ),
      )
    }
    case 'ArrayLiteralExpression':
      return printDelimited(
        context,
        tokenOf(node, 'LeftBracket'),
        directNodes(node),
        commaTokens(node),
        tokenOf(node, 'RightBracket'),
        prefix,
      )
    case 'StructFieldInitializer':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'Identifier'), prefix, preserveBlank),
        printToken(context, tokenOf(node, 'Colon')),
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          FormatDocument.text(' '),
        ),
      )
    case 'FieldProjectionExpression':
      return FormatDocument.concat(
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          prefix,
          preserveBlank,
        ),
        printToken(context, tokenOf(node, 'Dot')),
        printToken(context, tokenOf(node, 'Identifier')),
      )
    case 'IndexProjectionExpression':
      return FormatDocument.concat(
        printNode(
          context,
          directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression'),
          prefix,
          preserveBlank,
        ),
        printToken(context, tokenOf(node, 'LeftBracket')),
        printNode(context, directNodes(node)[1] ?? nodeOf(node, 'IdentifierExpression', 1)),
        printToken(context, tokenOf(node, 'RightBracket')),
      )
    case 'CallExpression': {
      const nodes = directNodes(node)
      const callee = nodes[0] ?? nodeOf(node, 'IdentifierExpression')
      const typeArguments = nodes.find((child) => child.kind === 'CallTypeArgumentList')
      const argumentsList = nodes.find((child) => child.kind === 'ArgumentList')
      return FormatDocument.concat(
        printNode(context, callee, prefix, preserveBlank),
        ...(typeArguments === undefined ? [] : [printNode(context, typeArguments)]),
        printNode(context, argumentsList ?? nodeOf(node, 'ArgumentList')),
      )
    }
    case 'GroupedExpression':
      return FormatDocument.concat(
        printToken(context, tokenOf(node, 'LeftParenthesis'), prefix, preserveBlank),
        printNode(context, directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression')),
        printToken(context, tokenOf(node, 'RightParenthesis')),
      )
    case 'PrefixExpression': {
      const operator = directTokens(node)[0]
      if (operator === undefined)
        throw new FormatterImplementationError('PrefixExpression has no operator')
      return FormatDocument.concat(
        printToken(context, operator, prefix, preserveBlank),
        printNode(context, directNodes(node)[0] ?? nodeOf(node, 'IdentifierExpression')),
      )
    }
    case 'InfixExpression': {
      const nodes = directNodes(node)
      const operator = directTokens(node)[0]
      if (operator === undefined)
        throw new FormatterImplementationError('InfixExpression has no operator')
      return FormatDocument.concat(
        printNode(context, nodes[0] ?? nodeOf(node, 'IdentifierExpression'), prefix, preserveBlank),
        printToken(context, operator, FormatDocument.text(' ')),
        printNode(
          context,
          nodes[1] ?? nodeOf(node, 'IdentifierExpression', 1),
          FormatDocument.text(' '),
        ),
      )
    }
    case 'PipelineExpression': {
      const nodes = directNodes(node)
      return FormatDocument.concat(
        printNode(context, nodes[0] ?? nodeOf(node, 'IdentifierExpression'), prefix, preserveBlank),
        FormatDocument.indent(
          FormatDocument.concat(
            printToken(context, tokenOf(node, 'PipeGreater'), FormatDocument.hardLine),
            printNode(
              context,
              nodes[1] ?? nodeOf(node, 'IdentifierExpression'),
              FormatDocument.text(' '),
            ),
          ),
        ),
      )
    }
    case 'ArgumentList':
      return printDelimited(
        context,
        tokenOf(node, 'LeftParenthesis'),
        directNodes(node),
        commaTokens(node),
        tokenOf(node, 'RightParenthesis'),
        prefix,
      )
    case 'ErrorStatement':
    case 'Error':
      throw new FormatterImplementationError('Damaged Error node reached the syntax printer')
  }
}

const changed = (source: ReadonlyArray<number>, formatted: Uint8Array): boolean =>
  source.length !== formatted.length || source.some((byte, index) => byte !== formatted[index])

/** Formats one complete lossless syntax artifact with Silk's canonical public policy. */
export const format = Effect.fn('Formatter.format')(function* (
  syntax: SyntaxFile.SyntaxFile,
): Effect.fn.Return<FormattedDocument.FormattedDocument, FormatterError> {
  if (
    syntax.lexicalDiagnostics.length > 0 ||
    syntax.parserDiagnostics.length > 0 ||
    !SyntaxTree.isAvailableSyntax(syntax.root)
  ) {
    const diagnostics = Object.freeze([...syntax.lexicalDiagnostics, ...syntax.parserDiagnostics])
    return yield* new FormatterError({
      operation: 'Formatter.format',
      sourceId: syntax.source.id,
      message: `Cannot format damaged Silk source ${syntax.source.id}`,
      diagnostics,
      reason: { _tag: 'DamagedSyntax' },
    })
  }

  const formatted = FormatDocument.render(printNode(makeContext(syntax), syntax.root))
  return FormattedDocument.make(formatted, changed(syntax.source.bytes, formatted))
})
