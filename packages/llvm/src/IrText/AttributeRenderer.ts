import * as ByteString from '../ByteString.js'
import type * as AttributeDescription from '../internal/AttributeDescription.js'
import type * as BuilderState from '../internal/BuilderState.js'
import { quoted, rawBytes } from './shared.js'
import { renderType } from './TypeRenderer.js'

/** @internal */
export const attributeAt = (
  state: BuilderState.Snapshot,
  index: number,
): AttributeDescription.Description => {
  const description = state.attributes[index]
  if (description === undefined) throw new Error(`missing attribute ${index}`)
  return description
}

/** @internal */
export const renderAttribute = (state: BuilderState.Snapshot, index: number): string => {
  const description = attributeAt(state, index)
  const name = rawBytes(description.name)
  switch (description._tag) {
    case 'Flag':
      return name
    case 'Integer':
      if (name === 'captures' && description.value === 0n) return 'captures(none)'
      if (name === 'align') return `align ${description.value}`
      if (name === 'memory') {
        const effects = ['none', 'read', 'write', 'readwrite']
        const values = [
          ['argmem', Number(description.value & 3n)],
          ['inaccessiblemem', Number((description.value >> 2n) & 3n)],
          ['other', Number((description.value >> 4n) & 3n)],
        ] as const
        const rendered = values
          .filter(([, effect]) => effect !== 0)
          .map(([location, effect]) => `${location}: ${effects[effect] ?? 'none'}`)
        return rendered.length === 0 ? 'memory(none)' : `memory(${rendered.join(', ')})`
      }
      return `${name}(${description.value})`
    case 'Type':
      return `${name}(${renderType(state, description.type)})`
    case 'String':
      return `${quoted(description.name)}${ByteString.isEmpty(description.value) ? '' : `=${quoted(description.value)}`}`
    case 'IntegerList':
      return `${name}(${description.values.join(',')})`
  }
}

/** @internal */
export const renderAttributeSet = (state: BuilderState.Snapshot, index: number): string => {
  const attributes = state.attributeSets[index]
  return attributes === undefined
    ? ''
    : attributes.map((attribute) => renderAttribute(state, attribute)).join(' ')
}
