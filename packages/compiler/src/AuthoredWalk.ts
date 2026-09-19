import type * as AuthoredHir from './AuthoredHir.js'
import * as AuthoredIdentity from './AuthoredIdentity.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as SemanticContext from './SemanticContext.js'

/**
 * Traversal of one authored body in document order.
 *
 * Body analysis needs three things syntax used to provide by walking children: the block a
 * declaration's body owns, every statement below it, and the anchors of every node in preorder.
 * Authored HIR exposes each construct's parts as named fields, so traversal is a closed dispatch
 * over the vocabulary rather than an untyped child scan.
 */

const emptyBlock = (anchor: AuthoredHir.Anchor): AuthoredHir.Block =>
  Object.freeze({
    _tag: 'Block',
    anchor,
    origin: Object.freeze({ _tag: 'Authored' }),
    causes: Object.freeze([]),
    statements: Object.freeze([]),
  })

/** The authored declaration one owner identity names, at any nesting depth of the module. */
export const declarationOf = (
  module: AuthoredHir.Module,
  owner: AuthoredIdentity.Identity,
): AuthoredHir.Declaration | undefined => {
  const pending = [...module.declarations]
  while (pending.length > 0) {
    const declaration = pending.pop()
    if (declaration === undefined) break
    if (AuthoredIdentity.equals(declaration.owner, owner)) return declaration
    if (declaration.body._tag === 'MembersBody') pending.push(...declaration.body.members)
    else if (declaration.body._tag === 'ConditionalBody') {
      pending.push(declaration.body.thenBranch)
      if (declaration.body.elseBranch !== undefined) pending.push(declaration.body.elseBranch)
    }
  }
  return undefined
}

/** The authored block a callable declaration elaborates, empty when the body is absent. */
export const bodyBlock = (
  context: SemanticContext.SemanticContext,
  declaration: DeclarationFacts.DeclarationFact,
): AuthoredHir.Block => {
  const found = declarationOf(context.module, declaration.owner)
  return found?.body._tag === 'CallableBody' && found.body.block !== undefined
    ? found.body.block
    : emptyBlock(declaration.anchor)
}

const blocksOf = (
  statement: AuthoredHir.Statement,
): ReadonlyArray<AuthoredHir.Block | AuthoredHir.Conditional> => {
  switch (statement._tag) {
    case 'ConditionalStatement':
    case 'PatternConditionalStatement':
    case 'StaticConditionalStatement':
      return statement.elseBranch === undefined
        ? [statement.thenBranch]
        : [statement.thenBranch, statement.elseBranch]
    case 'StaticForStatement':
    case 'WhileStatement':
    case 'UnsafeStatement':
      return [statement.body]
    default:
      return []
  }
}

/** Every statement below one block, including nested arms and loop bodies, in document order. */
export const statements = (block: AuthoredHir.Block): ReadonlyArray<AuthoredHir.Statement> => {
  const found: Array<AuthoredHir.Statement> = []
  const visitBlock = (current: AuthoredHir.Block | AuthoredHir.Conditional): void => {
    if (current._tag !== 'Block') {
      visitStatement(current)
      return
    }
    for (const statement of current.statements) visitStatement(statement)
  }
  const visitStatement = (statement: AuthoredHir.Statement): void => {
    found.push(statement)
    for (const nested of blocksOf(statement)) visitBlock(nested)
  }
  visitBlock(block)
  return Object.freeze(found)
}

/**
 * Preorder anchors of one authored body, assigning the stable finite lifetime domain.
 *
 * Only the positions body analysis can request a region for need a point, so statements and the
 * expressions they own are enumerated; annotation-only positions inherit their statement's point.
 */
export const anchors = (block: AuthoredHir.Block): ReadonlyArray<AuthoredHir.Anchor> => {
  const found: Array<AuthoredHir.Anchor> = [block.anchor]
  const visitExpression = (expression: AuthoredHir.Expression): void => {
    found.push(expression.anchor)
    for (const child of expressionChildren(expression)) visitExpression(child)
  }
  for (const statement of statements(block)) {
    found.push(statement.anchor)
    for (const expression of statementExpressions(statement)) visitExpression(expression)
  }
  return Object.freeze(found)
}

/** The expressions one statement owns directly, in evaluation order. */
export const statementExpressions = (
  statement: AuthoredHir.Statement,
): ReadonlyArray<AuthoredHir.Expression> => {
  switch (statement._tag) {
    case 'ExpressionStatement':
      return [statement.expression]
    case 'BindingStatement':
      return [statement.initializer]
    case 'PatternBindingStatement':
      return [statement.initializer]
    case 'AssignmentStatement':
      return [statement.target, statement.value]
    case 'StaticForStatement':
      return [statement.iterable]
    case 'WhileStatement':
      return [statement.condition]
    case 'ConditionalStatement':
    case 'StaticConditionalStatement':
      return [statement.condition]
    case 'PatternConditionalStatement':
      return [statement.subject]
    case 'ReturnStatement':
      return statement.value === undefined ? [] : [statement.value]
    case 'FailStatement':
      return [statement.value]
    case 'DropStatement':
      return [statement.value]
    case 'InvalidStatement':
      return statement.retained.filter(
        (retained): retained is AuthoredHir.Expression =>
          retained._tag !== 'MissingStatement' && !('statements' in retained),
      )
    default:
      return []
  }
}

/** The expressions one expression owns directly, in evaluation order. */
export const expressionChildren = (
  expression: AuthoredHir.Expression,
): ReadonlyArray<AuthoredHir.Expression> => {
  switch (expression._tag) {
    case 'MoveExpression':
    case 'BorrowExpression':
    case 'RunExpression':
    case 'UnsafeExpression':
      return [expression.operand]
    case 'CompileErrorExpression':
      return [expression.message]
    case 'MatchExpression':
      return [expression.subject]
    case 'StructExpression':
      return expression.fields.map((field) => field.value)
    case 'RecordExpression':
      return expression.fields.map((field) => field.value)
    case 'TupleExpression':
    case 'ArrayExpression':
      return expression.elements
    case 'MemberExpression':
      return expression.fields === undefined ? [] : expression.fields.map((field) => field.value)
    case 'FieldExpression':
    case 'ReferentExpression':
      return [expression.subject]
    case 'OrdinalExpression':
      return [expression.subject]
    case 'IndexExpression':
      return [expression.subject, expression.index]
    case 'CallExpression':
      return [expression.callee, ...expression.arguments]
    case 'PrefixExpression':
      return [expression.operand]
    case 'InfixExpression':
      return [expression.left, expression.right]
    case 'PipelineExpression':
      return [expression.input, expression.target]
    case 'InvalidExpression':
      return expression.retained
    default:
      return []
  }
}

/** Anonymous callables below one body, in traversal order; their ordinal names `$callable$N`. */
export const callableExpressions = (
  block: AuthoredHir.Block,
): ReadonlyArray<Extract<AuthoredHir.Expression, { readonly _tag: 'CallableExpression' }>> => {
  const found: Array<Extract<AuthoredHir.Expression, { readonly _tag: 'CallableExpression' }>> = []
  const visit = (expression: AuthoredHir.Expression): void => {
    if (expression._tag === 'CallableExpression') {
      found.push(expression)
      // A nested callable belongs to its own declaration's counter, not this one.
      return
    }
    for (const child of expressionChildren(expression)) visit(child)
  }
  for (const statement of statements(block))
    for (const expression of statementExpressions(statement)) visit(expression)
  return Object.freeze(found)
}

/** True when an authored node is not a lexical-recovery placeholder. */
export const isAvailable = (
  node: AuthoredHir.Expression | AuthoredHir.Pattern | AuthoredHir.Type,
): boolean =>
  node._tag !== 'MissingExpression' &&
  node._tag !== 'InvalidExpression' &&
  node._tag !== 'MissingPattern' &&
  node._tag !== 'InvalidPattern' &&
  node._tag !== 'MissingType' &&
  node._tag !== 'InvalidType'
