import * as Option from 'effect/Option'
import type {
  ConstantLiteralFact,
  DeclaredName,
  Index,
  RequirementRoleFact,
  TypePathFact,
} from './DeclarationFacts.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as TypeInference from './internal/TypeInference.js'
import * as LiteralForm from './LiteralForm.js'
import * as RequirementRow from './RequirementRow.js'
import * as SourceFile from './SourceFile.js'
import * as StaticText from './StaticText.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as TargetConstant from './TargetConstant.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

export const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Header token span does not belong to source ${source.id}`),
  )

const retainedTypePath = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
): TypePathFact | undefined => {
  const segments = SyntaxTree.tokens(syntax)
    .filter((token) => token.kind === 'Identifier')
    .map((token) => Object.freeze({ spelling: spelling(source, token), token }))
  return segments.length === 0
    ? undefined
    : Object.freeze({
        _tag: 'TypePath',
        spelling: segments.map((segment) => segment.spelling).join('.'),
        segments: Object.freeze(segments),
        syntax,
      })
}

export const collectedRequirementRole = (
  source: SourceFile.SourceFile,
  requirement: SyntaxTree.Node,
): RequirementRoleFact => {
  const at = SyntaxTree.directToken(requirement, 'Identifier')
  const roleSyntax =
    at === undefined ? undefined : SyntaxTree.directNodes(requirement, 'TypePath').at(-1)
  const path = roleSyntax?.kind === 'TypePath' ? retainedTypePath(source, roleSyntax) : undefined
  return path === undefined
    ? Object.freeze({ _tag: 'DefaultRole' })
    : Object.freeze({ _tag: 'UnresolvedRole', path })
}

export const requirementRoleIdentity = (
  role: RequirementRoleFact,
): Type.Requirement['role'] | undefined =>
  role._tag === 'DefaultRole'
    ? RequirementRow.defaultRole
    : role._tag === 'ResolvedRole'
      ? role.role
      : undefined

export const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined)
    throw new RangeError(`Header collection expected ${kind} below ${parent.kind}`)
  return child
}

export const isDeclaredTypeNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) &&
  (element.kind === 'TypePath' ||
    element.kind === 'AppliedType' ||
    element.kind === 'FixedArrayType' ||
    element.kind === 'SliceType' ||
    element.kind === 'ReferenceType' ||
    element.kind === 'CallableType' ||
    element.kind === 'UnitType' ||
    element.kind === 'ParenthesizedType' ||
    element.kind === 'ExactRepresentationType' ||
    element.kind === 'OpaqueResultType' ||
    element.kind === 'UnionType')

export const declaredTypeNode = (parent: SyntaxTree.Node): SyntaxTree.Node => {
  const child = parent.children.find((element): element is SyntaxTree.Node =>
    isDeclaredTypeNode(element),
  )
  if (child === undefined) throw new RangeError(`Header collection expected a declared type`)
  return child
}

export const presentName = (source: SourceFile.SourceFile, node: SyntaxTree.Node): DeclaredName => {
  const token = SyntaxTree.directToken(node, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(node, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })
}

export const constantLiteral = (
  source: SourceFile.SourceFile,
  initializer: SyntaxTree.Node,
): ConstantLiteralFact => {
  if (initializer.kind === 'BooleanLiteralExpression') {
    const token =
      SyntaxTree.directToken(initializer, 'TrueKeyword') ??
      SyntaxTree.directToken(initializer, 'FalseKeyword')
    return token === undefined
      ? Object.freeze({ _tag: 'Unavailable', syntax: initializer })
      : Object.freeze({ _tag: 'BooleanLiteral', value: token.kind === 'TrueKeyword', token })
  }
  if (initializer.kind === 'CharacterLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'CharLiteral')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    if (bytes === undefined || form === undefined)
      return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const decoded = StaticText.decodeScalar(Array.from(bytes), form)
    return decoded._tag === 'Scalar'
      ? Object.freeze({ _tag: 'CharacterLiteral', value: decoded.value, token })
      : Object.freeze({ _tag: 'Malformed', detail: decoded.detail, syntax: initializer })
  }
  if (initializer.kind === 'IntegerLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'DecimalInteger')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const digits = spelling(source, token)
    const negative = SyntaxTree.directToken(initializer, 'Minus') !== undefined
    const magnitude = IntegerLiteral.magnitude(digits)
    return Object.freeze({
      _tag: 'IntegerLiteral',
      value: negative ? -magnitude : magnitude,
      spelling: `${negative ? '-' : ''}${digits}`,
      token,
    })
  }
  if (initializer.kind === 'FloatingLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'DecimalFloat')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const literal = DigitSeparator.strip(spelling(source, token))
    return Object.freeze({
      _tag: 'FloatingLiteral',
      spelling: `${SyntaxTree.directToken(initializer, 'Minus') === undefined ? '' : '-'}${literal}`,
      token,
    })
  }
  if (initializer.kind === 'StaticTextLiteralExpression') {
    const token =
      SyntaxTree.directToken(initializer, 'TextLiteral') ??
      SyntaxTree.directToken(initializer, 'ByteStringLiteral')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    if (bytes === undefined || form === undefined)
      return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    // The header decodes once so every reference — in this module or an importing one — shares
    // the exact bytes the equivalent `let` binding would produce.
    const decoded = StaticText.decode(Array.from(bytes), form)
    return decoded._tag === 'Decoded'
      ? Object.freeze({ _tag: 'StringLiteral', data: decoded.data, token })
      : Object.freeze({ _tag: 'Malformed', detail: decoded.detail, syntax: initializer })
  }
  const target = targetConstant(source, initializer)
  if (target !== undefined) return target
  return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
}

/**
 * Recognizes `Target.<fact>`, the one initializer form that is not a source literal. The projection
 * is matched on syntax alone — `Target` names no declaration and resolves to nothing — so a form
 * that is already rejected today is the only form whose meaning changes.
 */
const targetConstant = (
  source: SourceFile.SourceFile,
  initializer: SyntaxTree.Node,
): ConstantLiteralFact | undefined => {
  if (initializer.kind !== 'FieldProjectionExpression') return undefined
  const base = SyntaxTree.directNode(initializer, 'IdentifierExpression')
  const baseToken = base === undefined ? undefined : SyntaxTree.directToken(base, 'Identifier')
  if (baseToken === undefined || spelling(source, baseToken) !== TargetConstant.root)
    return undefined
  const member = SyntaxTree.directToken(initializer, 'Identifier')
  if (member === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
  const memberSpelling = spelling(source, member)
  const selector = TargetConstant.find(memberSpelling)
  return selector === undefined
    ? Object.freeze({
        _tag: 'Malformed',
        detail: `${TargetConstant.root}.${memberSpelling} names no target fact; the target facts are ${TargetConstant.all.map((candidate) => `${TargetConstant.root}.${candidate}`).join(', ')}`,
        syntax: initializer,
      })
    : Object.freeze({ _tag: 'TargetConstant', selector, token: member })
}

import { copyProof } from './ConformanceProof.js'

/** Tests whether every value of this concrete type copies freely (no affine obligation). */
export const copyType = (
  self: Index,
  type: Type.Type,
  assumptions: ReadonlySet<string> = new Set(),
): boolean => copyProof(self, type, assumptions)._tag === 'Copy'

/** Tests whether a value of this type can retain lexical storage through its fields. */
export const containsLexicalBorrow = (
  self: Index,
  type: Type.Type,
  seen: ReadonlySet<string> = new Set(),
): boolean => {
  if (Type.isString(type) || Type.isSlice(type) || Type.isReference(type)) return true
  if (Type.isFixedArray(type)) return containsLexicalBorrow(self, type.element, seen)
  if (Type.isUnion(type))
    return type.members.some((member) => containsLexicalBorrow(self, member, seen))
  if (Type.isEffect(type))
    return (
      containsLexicalBorrow(self, type.success, seen) ||
      Type.failureMembers(type).some((failure) => containsLexicalBorrow(self, failure, seen))
    )
  if (!Type.isNominal(type)) return false
  const key = Type.key(type)
  if (seen.has(key)) return false
  const declaration = DeclarationFacts.byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration')
    return type.arguments
      .filter(Type.isTypeArgument)
      .some((argument) => containsLexicalBorrow(self, argument, seen))
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(seen).add(key)
  return declaration.fields.some(
    (field) =>
      field.declaredType._tag === 'Resolved' &&
      containsLexicalBorrow(self, Type.substitute(field.declaredType.type, substitution), next),
  )
}

/** One stored bare-callable occurrence that denies an aggregate type a target layout. */
export interface StoredCallable {
  /** Field names from the aggregate down to the callable-typed position; empty for elements. */
  readonly path: ReadonlyArray<string>
  readonly callable: Type.Callable
}

/**
 * Finds the first stored position at which a concrete type keeps a bare callable value.
 *
 * The walk mirrors what nominal layout planning can actually see: struct fields after
 * substitution, fixed-array and slice elements, and union members. It deliberately does not
 * descend through references (their layout is an address regardless of the target), intrinsic
 * nominals (their layouts never depend on their type arguments), or Effect types (their hidden
 * environments plan callables from concrete identities). A position this finds is exactly one
 * `Layout.layoutType` would refuse with `callable environment layout is planned from its hidden
 * concrete identity`, so a construction of the enclosing aggregate cannot receive a layout.
 */
export const storedCallable = (
  self: Index,
  type: Type.Type,
  seen: ReadonlySet<string> = new Set(),
): StoredCallable | undefined => {
  if (Type.isCallable(type)) return Object.freeze({ path: Object.freeze([]), callable: type })
  if (Type.isFixedArray(type) || Type.isSlice(type)) return storedCallable(self, type.element, seen)
  if (Type.isUnion(type)) {
    for (const member of type.members) {
      const found = storedCallable(self, member, seen)
      if (found !== undefined) return found
    }
    return undefined
  }
  if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return undefined
  const key = Type.key(type)
  if (seen.has(key)) return undefined
  const declaration = DeclarationFacts.byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return undefined
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(seen).add(key)
  for (const field of declaration.fields) {
    if (field.declaredType._tag !== 'Resolved' || field.name._tag !== 'Present') continue
    const found = storedCallable(self, Type.substitute(field.declaredType.type, substitution), next)
    if (found !== undefined)
      return Object.freeze({
        path: Object.freeze([field.name.spelling, ...found.path]),
        callable: found.callable,
      })
  }
  return undefined
}
