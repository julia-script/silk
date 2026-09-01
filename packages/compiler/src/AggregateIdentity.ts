import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/** The source-visible or occurrence-generated identity of one nominal aggregate. */
export type AggregateIdentity =
  | {
      readonly _tag: 'SourceAggregateIdentity'
      readonly module: string
      readonly name: string
      readonly kind: 'Named' | 'Positional'
    }
  | {
      readonly _tag: 'AnonymousAggregateIdentity'
      readonly module: string
      readonly occurrence: SourceSpan.SourceSpan
      readonly kind: 'AnonymousNamed' | 'AnonymousPositional'
    }

/** The closed source-independent identity of one aggregate member. */
export type MemberIdentity =
  | { readonly _tag: 'LabeledAggregateMember'; readonly label: string }
  | { readonly _tag: 'OrdinalAggregateMember'; readonly ordinal: number }

export const source = (
  module: string,
  name: string,
  kind: Extract<AggregateIdentity, { readonly _tag: 'SourceAggregateIdentity' }>['kind'],
): AggregateIdentity => Object.freeze({ _tag: 'SourceAggregateIdentity', module, name, kind })

export const anonymous = (
  module: string,
  occurrence: SourceSpan.SourceSpan,
  kind: Extract<AggregateIdentity, { readonly _tag: 'AnonymousAggregateIdentity' }>['kind'],
): AggregateIdentity =>
  Object.freeze({ _tag: 'AnonymousAggregateIdentity', module, occurrence, kind })

export const labeled = (label: string): MemberIdentity =>
  Object.freeze({ _tag: 'LabeledAggregateMember', label })

export const ordinal = (value: number): MemberIdentity =>
  Object.freeze({ _tag: 'OrdinalAggregateMember', ordinal: value })

/** Compiler-private spelling for generated declarations; it never enters lexical lookup. */
export const internalName = (self: AggregateIdentity): string =>
  self._tag === 'SourceAggregateIdentity'
    ? self.name
    : `@${self.kind}:${self.occurrence.start}:${self.occurrence.end}`

export const nominal = (self: AggregateIdentity): Type.Nominal =>
  Type.nominal(self.module, internalName(self))

/** Human-facing provenance used where generated identities must not pretend to be declarations. */
export const display = (self: AggregateIdentity): string => {
  if (self._tag === 'SourceAggregateIdentity') return `${self.module}.${self.name}`
  return self.kind === 'AnonymousPositional' ? 'anonymous tuple' : 'anonymous record'
}

export const memberKey = (self: MemberIdentity): string =>
  self._tag === 'LabeledAggregateMember' ? `label:${self.label}` : `ordinal:${self.ordinal}`
