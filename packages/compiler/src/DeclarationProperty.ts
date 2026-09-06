import * as Option from 'effect/Option'
import * as LiteralForm from './LiteralForm.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import * as StaticText from './StaticText.js'
import * as SyntaxTree from './SyntaxTree.js'

/** Reads one property token's source spelling without evaluating source expressions. */
export const spelling = (source: SourceFile.SourceFile, span: SourceSpan.SourceSpan): string =>
  Option.getOrElse(SourceFile.spelling(source, span), () => '')

/** Returns a sealed clause's namespace and operation spelling. */
export const owner = (source: SourceFile.SourceFile, clause: SyntaxTree.Node): string => {
  const names = clause.children
    .filter(SyntaxTree.isToken)
    .filter((token) => token.kind === 'Identifier')
    .map((token) => spelling(source, token.span))
  return `${names[1] ?? ''}.${names[2] ?? ''}`
}

/** Selects all direct sealed clauses, retaining their exact authored order and spans. */
export const clauses = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.filter(SyntaxTree.isNode).filter((child) => child.kind === 'FunctionPropertyClause')

/** Decodes a literal text operand; interpolated or computed expressions are unavailable. */
export const text = (source: SourceFile.SourceFile, node: SyntaxTree.Node): string | undefined => {
  const token = SyntaxTree.directToken(node, 'TextLiteral')
  if (token === undefined) return undefined
  const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
  const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
  if (bytes === undefined || form === undefined) return undefined
  const decoded = StaticText.decode(Array.from(bytes), form)
  return decoded._tag === 'Decoded'
    ? new TextDecoder().decode(Uint8Array.from(decoded.data.bytes))
    : undefined
}
