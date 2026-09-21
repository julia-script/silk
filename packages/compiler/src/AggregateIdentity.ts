import * as AuthoredIdentity from './AuthoredIdentity.js'
import type * as Tir from './Tir.js'
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
      readonly node: Tir.NodeRef
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
): AggregateIdentity => ({ _tag: 'SourceAggregateIdentity', module, name, kind })

export const anonymous = (
  module: string,
  node: Tir.NodeRef,
  kind: Extract<AggregateIdentity, { readonly _tag: 'AnonymousAggregateIdentity' }>['kind'],
): AggregateIdentity => ({ _tag: 'AnonymousAggregateIdentity', module, node, kind })

export const labeled = (label: string): MemberIdentity => ({
  _tag: 'LabeledAggregateMember',
  label,
})

export const ordinal = (value: number): MemberIdentity => ({
  _tag: 'OrdinalAggregateMember',
  ordinal: value,
})

/** Compiler-private spelling for generated declarations; it never enters lexical lookup. */
export const internalName = (self: AggregateIdentity): string =>
  self._tag === 'SourceAggregateIdentity'
    ? self.name
    : `@${self.kind}:${artifactText(self.node.artifact)}:n${self.node.node.ordinal}`

const artifactKey = (self: Tir.ArtifactId): string =>
  JSON.stringify([
    AuthoredIdentity.key(self.owner),
    self.request._tag === 'Check' ? null : self.request.application,
    self.parent === undefined ? null : artifactKey(self.parent),
  ])

const artifactText = (self: Tir.ArtifactId): string =>
  Array.from(new TextEncoder().encode(artifactKey(self)), (byte) =>
    byte.toString(16).padStart(2, '0'),
  ).join('')

export const nominal = (self: AggregateIdentity): Type.Nominal =>
  Type.nominal(self.module, internalName(self))

/** Human-facing provenance used where generated identities must not pretend to be declarations. */
export const display = (self: AggregateIdentity): string => {
  if (self._tag === 'SourceAggregateIdentity') return `${self.module}.${self.name}`
  return self.kind === 'AnonymousPositional' ? 'anonymous tuple' : 'anonymous record'
}

export const memberKey = (self: MemberIdentity): string =>
  self._tag === 'LabeledAggregateMember' ? `label:${self.label}` : `ordinal:${self.ordinal}`
