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
  if (found !== undefined)
    return found.body._tag === 'CallableBody' && found.body.block !== undefined
      ? found.body.block
      : emptyBlock(declaration.anchor)
  // Hidden declarations belong to anonymous callables, which are nested owners inside a body.
  return callableOf(context.module, declaration.owner)?.body ?? emptyBlock(declaration.anchor)
}

/** Every callable body block a module owns directly, at any declaration nesting depth. */
const callableBlocks = (module: AuthoredHir.Module): ReadonlyArray<AuthoredHir.Block> => {
  const found: Array<AuthoredHir.Block> = []
  const pending = [...module.declarations]
  while (pending.length > 0) {
    const declaration = pending.pop()
    if (declaration === undefined) break
    if (declaration.body._tag === 'CallableBody') {
      if (declaration.body.block !== undefined) found.push(declaration.body.block)
    } else if (declaration.body._tag === 'MembersBody') pending.push(...declaration.body.members)
    else if (declaration.body._tag === 'ConditionalBody') {
      pending.push(declaration.body.thenBranch)
      if (declaration.body.elseBranch !== undefined) pending.push(declaration.body.elseBranch)
    }
  }
  return found
}

/** The anonymous callable one nested owner identity names, at any nesting depth of the module. */
export const callableOf = (
  module: AuthoredHir.Module,
  owner: AuthoredIdentity.Identity,
): Extract<AuthoredHir.Expression, { readonly _tag: 'CallableExpression' }> | undefined => {
  const search = (
    block: AuthoredHir.Block,
  ): Extract<AuthoredHir.Expression, { readonly _tag: 'CallableExpression' }> | undefined => {
    for (const callable of callableExpressions(block)) {
      if (AuthoredIdentity.equals(callable.anchor.owner, owner)) return callable
      const nested = search(callable.body)
      if (nested !== undefined) return nested
    }
    return undefined
  }
  for (const block of callableBlocks(module)) {
    const found = search(block)
    if (found !== undefined) return found
  }
  return undefined
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

/**
 * Blocks an expression owns within the same authored owner: effect bodies and block-valued match
 * arms. Anonymous callable bodies are nested owners and are deliberately excluded.
 */
const expressionBlocks = (expression: AuthoredHir.Expression): ReadonlyArray<AuthoredHir.Block> => {
  const found: Array<AuthoredHir.Block> = []
  const visit = (current: AuthoredHir.Expression): void => {
    if (current._tag === 'EffectExpression') found.push(current.body)
    if (current._tag === 'MatchExpression')
      for (const arm of current.arms) if (arm.result._tag === 'Block') found.push(arm.result)
    for (const child of expressionChildren(current)) visit(child)
  }
  visit(expression)
  return found
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
    for (const expression of statementExpressions(statement))
      for (const nested of expressionBlocks(expression)) visitBlock(nested)
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
      return [
        expression.subject,
        ...expression.arms.flatMap((arm): ReadonlyArray<AuthoredHir.Expression> => [
          ...(arm.guard === undefined ? [] : [arm.guard]),
          ...(arm.result._tag === 'Block' ? [] : [arm.result]),
        ]),
      ]
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

/** The types one pattern writes, including the annotations its nested fields write. */
const patternTypes = (pattern: AuthoredHir.Pattern): ReadonlyArray<AuthoredHir.Type> => {
  const found: Array<AuthoredHir.Type> = []
  const visit = (current: AuthoredHir.Pattern): void => {
    switch (current._tag) {
      case 'NominalPattern':
        found.push(current.type)
        for (const field of current.fields)
          if (field._tag === 'PatternField') visit(field.pattern)
        return
      case 'BindingPattern':
        if (current.type !== undefined) found.push(current.type)
        return
      case 'VariantPattern':
        found.push(current.selector.subject)
        for (const field of current.fields ?? [])
          if (field._tag === 'PatternField') visit(field.pattern)
        return
      case 'InvalidPattern':
        for (const retained of current.retained)
          if (retained._tag !== 'Name' && 'statements' in retained === false) {
            if (isPattern(retained)) visit(retained)
            else if (isType(retained)) found.push(retained)
          }
        return
      default:
        return
    }
  }
  visit(pattern)
  return Object.freeze(found)
}

const patternTags: ReadonlySet<string> = new Set([
  'EnumPattern',
  'IntegerPattern',
  'NominalPattern',
  'BindingPattern',
  'UniversalPattern',
  'VariantPattern',
  'MissingPattern',
  'InvalidPattern',
])

const isPattern = (node: {
  readonly _tag: string
}): node is AuthoredHir.Pattern => patternTags.has(node._tag)

const isType = (node: { readonly _tag: string }): node is AuthoredHir.Type =>
  !patternTags.has(node._tag) && node._tag !== 'Name'

/**
 * Every type one authored body writes, in document order.
 *
 * Lifetime elaboration must reach the annotations a body writes — a call's explicit type
 * arguments, a binding's declared type, a pattern's named type — because each one can elide a
 * region the body owns. Header traversal alone leaves those anchors without a region.
 */
export const bodyTypes = (block: AuthoredHir.Block): ReadonlyArray<AuthoredHir.Type> => {
  const found: Array<AuthoredHir.Type> = []
  const visitExpression = (expression: AuthoredHir.Expression): void => {
    if (expression._tag === 'CallExpression' && expression.generics !== undefined)
      for (const argument of expression.generics.arguments)
        if (argument._tag === 'RequirementSelector') found.push(argument.subject)
        else if (argument._tag !== 'Lifetime') found.push(argument)
    if (expression._tag === 'StructExpression') found.push(expression.type)
    if (expression._tag === 'MemberExpression') found.push(expression.selector.subject)
    if (expression._tag === 'MatchExpression')
      for (const arm of expression.arms) found.push(...patternTypes(arm.pattern))
    for (const child of expressionChildren(expression)) visitExpression(child)
  }
  for (const statement of statements(block)) {
    if (statement._tag === 'BindingStatement' && statement.type !== undefined)
      found.push(statement.type)
    if (statement._tag === 'PatternBindingStatement') found.push(...patternTypes(statement.pattern))
    if (statement._tag === 'PatternConditionalStatement')
      found.push(...patternTypes(statement.pattern))
    for (const expression of statementExpressions(statement)) visitExpression(expression)
  }
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
