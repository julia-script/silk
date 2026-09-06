import * as ConfigurationError from './ConfigurationError.js'
import * as ConfigurationOrigin from './ConfigurationOrigin.js'
import * as DeclarationProperty from './DeclarationProperty.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as Diagnostic from './Diagnostic.js'
import type * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'

/** A compiler-work-free entry body. Both guarantees are inseparable in this initial subset. */
export interface MachineFunction {
  readonly naked: true
  readonly noReturn: true
  readonly span: SourceSpan.SourceSpan
}

const diagnostic = (detail: string, span: SourceSpan.SourceSpan): Diagnostic.Diagnostic =>
  Diagnostic.invalidConfiguration(
    ConfigurationError.make('MachineFunction.validate', 'InvalidInput', detail, [
      { ...ConfigurationOrigin.literal(span.sourceId), span },
    ]),
    span,
  )

/** Validates the sealed property clause independently of declaration spelling or library ownership. */
export const analyze = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): {
  readonly properties?: MachineFunction
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const clauses = DeclarationProperty.clauses(node).filter(
    (clause) => DeclarationProperty.owner(source, clause) === 'Intrinsic.machine',
  )
  const clause = clauses[0]
  if (clause === undefined) return { diagnostics: [] }
  const properties = new Map<string, boolean>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  if (clauses.length !== 1)
    diagnostics.push(
      ...clauses.map((entry) => diagnostic('duplicate machine function clause', entry.span)),
    )
  for (const property of clause.children
    .filter(SyntaxTree.isNode)
    .filter((child) => child.kind === 'FunctionProperty')) {
    const name = SyntaxTree.directToken(property, 'Identifier')
    const expression = property.children.find(SyntaxTree.isNode)
    const spelling = name === undefined ? '' : DeclarationProperty.spelling(source, name.span)
    const token =
      expression === undefined
        ? undefined
        : SyntaxTree.tokens(expression).filter(
            (token) =>
              !['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment'].includes(token.kind),
          )[0]
    if (
      (spelling !== 'naked' && spelling !== 'noReturn') ||
      properties.has(spelling) ||
      token?.kind !== 'TrueKeyword' ||
      expression === undefined ||
      SyntaxTree.tokens(expression).filter(
        (token) =>
          !['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment'].includes(token.kind),
      ).length !== 1
    )
      diagnostics.push(
        diagnostic('machine properties require naked: true and noReturn: true', property.span),
      )
    properties.set(spelling, true)
  }
  if (!properties.has('naked') || !properties.has('noReturn'))
    diagnostics.push(diagnostic('machine properties require naked and noReturn', clause.span))
  return diagnostics.length > 0
    ? { diagnostics }
    : {
        properties: Object.freeze({ naked: true, noReturn: true, span: clause.span }),
        diagnostics: [],
      }
}

const terminalCall = (node: SyntaxTree.Node): SyntaxTree.Node | undefined => {
  if (node.kind === 'CallExpression') return node
  if (
    ![
      'Block',
      'ReturnStatement',
      'UnsafeStatement',
      'UnsafeExpression',
      'ExpressionStatement',
    ].includes(node.kind)
  )
    return undefined
  const children = node.children.filter(SyntaxTree.isNode)
  const child = children[0]
  return children.length === 1 && child !== undefined ? terminalCall(child) : undefined
}

/** Rejects bodies that could require compiler-created stack state before any lowering occurs. */
export const bodyDiagnostics = (
  source: SourceFile.SourceFile,
  declaration: DeclarationFacts.DeclarationFact,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const properties = declaration.machine
  if (properties === undefined) return []
  const rejected = (detail: string) => [diagnostic(detail, properties.span)]
  if (
    !declaration.unsafe ||
    declaration.phase !== 'Runtime' ||
    declaration.functionKind !== 'Ordinary' ||
    declaration.parameters.length !== 0 ||
    declaration.typeParameters.length !== 0 ||
    declaration.returnType._tag !== 'Resolved' ||
    !Type.equals(declaration.returnType.type, Type.unit)
  )
    return rejected('naked functions require an unsafe monomorphic zero-argument unit signature')
  const body = declaration.syntax.children
    .filter(SyntaxTree.isNode)
    .find((child) => child.kind === 'Block')
  const call = body === undefined ? undefined : terminalCall(body)
  if (call === undefined)
    return rejected('naked bodies require one terminal operand-free assembly invocation')
  const tokens = SyntaxTree.tokens(call)
    .filter(
      (token) =>
        !['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment'].includes(token.kind),
    )
    .slice(0, 3)
    .map((token) => DeclarationProperty.spelling(source, token.span))
  if (tokens.join('') !== 'Intrinsic.assembly')
    return rejected('naked bodies admit only Intrinsic.assembly')
  return []
}
