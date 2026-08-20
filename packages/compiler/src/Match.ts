import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/** The lexical way a match observes or takes its scrutinee. */
export type Access = 'Copy' | 'Move' | 'Shared' | 'Exclusive'

/** Stable source identity for one match expression. */
export interface MatchId {
  readonly _tag: 'MatchId'
  readonly function: DeclarationIndex.DeclarationId
  readonly span: SourceSpan.SourceSpan
}

/** Stable source-order identity for one arm. */
export interface ArmId {
  readonly _tag: 'MatchArmId'
  readonly match: MatchId
  readonly ordinal: number
}

/** Stable preorder identity for one pattern below an arm. */
export interface PatternId {
  readonly _tag: 'PatternId'
  readonly arm: ArmId
  readonly ordinal: number
}

/** Stable source-order identity for one leaf binding below an arm. */
export interface BindingId {
  readonly _tag: 'PatternBindingId'
  readonly arm: ArmId
  readonly ordinal: number
}

/** One source decision reduced to the facts that affect coverage. */
export interface Decision<Member extends Type.Type = Type.Type> {
  readonly member?: Member
  readonly universal: boolean
  readonly guarded: boolean
}

/** One arm's immutable canonical coverage transition. */
export interface CoverageTransition<Member extends Type.Type = Type.Type> {
  readonly before: ReadonlyArray<Member>
  readonly after: ReadonlyArray<Member>
  readonly reachable: boolean
}

/** Complete ordered coverage result for one nominal or structural-union scrutinee. */
export interface Coverage<Member extends Type.Type = Type.Type> {
  readonly initial: ReadonlyArray<Member>
  readonly transitions: ReadonlyArray<CoverageTransition<Member>>
  readonly missing: ReadonlyArray<Member>
  readonly exhaustive: boolean
}

const contains = <Member extends Type.Type>(
  members: ReadonlyArray<Member>,
  member: Member,
): boolean => members.some((candidate) => Type.equals(candidate, member))

/** Returns the canonical matchable member set, or `undefined` for a non-nominal type. */
export const membersOf = (type: Type.Type): ReadonlyArray<Type.Nominal> | undefined =>
  Type.isNominal(type)
    ? Object.freeze([type])
    : Type.isUnion(type) && type.members.every(Type.isNominal)
      ? type.members
      : undefined

/** Folds source decisions over one canonical remaining-member set. */
export const cover = <Member extends Type.Type>(
  initial: ReadonlyArray<Member>,
  decisions: ReadonlyArray<Decision<Member>>,
): Coverage<Member> => {
  let remaining = Object.freeze([...initial]) as ReadonlyArray<Member>
  const transitions: Array<CoverageTransition<Member>> = []
  for (const decision of decisions) {
    const before = remaining
    const reachable = decision.universal
      ? before.length > 0
      : decision.member !== undefined &&
        contains(initial, decision.member) &&
        contains(before, decision.member)
    if (reachable && !decision.guarded) {
      remaining = decision.universal
        ? Object.freeze([])
        : Object.freeze(
            before.filter(
              (candidate) =>
                decision.member === undefined || !Type.equals(candidate, decision.member),
            ),
          )
    }
    transitions.push(Object.freeze({ before, after: remaining, reachable }))
  }
  return Object.freeze({
    initial: Object.freeze([...initial]),
    transitions: Object.freeze(transitions),
    missing: remaining,
    exhaustive: remaining.length === 0,
  })
}

export type Join =
  | { readonly _tag: 'Joined'; readonly type: Type.Type }
  | { readonly _tag: 'Incompatible'; readonly types: ReadonlyArray<Type.Type> }

/** Joins reachable arm results using the language's one canonical match-result rule. */
export const join = (inputs: ReadonlyArray<Type.Type>): Join => {
  const contributing = inputs.filter((type) => !Type.isNever(type))
  const first = contributing.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Joined', type: 'never' })
  if (contributing.every((type) => Type.equals(type, first))) {
    return Object.freeze({ _tag: 'Joined', type: first })
  }
  for (const [leftOrdinal, left] of contributing.entries()) {
    for (const right of contributing.slice(leftOrdinal + 1)) {
      if (Type.firstRepresentationDivergence(left, right) !== undefined)
        return Object.freeze({ _tag: 'Incompatible', types: Object.freeze([...contributing]) })
    }
  }
  const normalized = Type.union(contributing)
  return normalized._tag === 'Normalized'
    ? Object.freeze({ _tag: 'Joined', type: normalized.type })
    : Object.freeze({ _tag: 'Incompatible', types: Object.freeze([...contributing]) })
}
