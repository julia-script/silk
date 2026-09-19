/** A deterministic indented rendering of authored records for goldens and assertion messages. */
export const render = (value: unknown, indent = ''): string => {
  if (typeof value === 'bigint') return `${value}n`
  if (typeof value === 'string')
    return `"${value.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/\n/g, '\\n')}"`
  if (value === undefined) return 'undefined'
  if (value === null) return 'null'
  if (typeof value === 'number' || typeof value === 'boolean') return `${value}`
  if (typeof value !== 'object') return typeof value
  const inner = `${indent}  `
  if (Array.isArray(value)) {
    if (value.length === 0) return '[]'
    return `[\n${value.map((item) => `${inner}${render(item, inner)}`).join('\n')}\n${indent}]`
  }
  const entries = Object.entries(value)
  if (entries.length === 0) return '{}'
  return `{\n${entries.map(([key, item]) => `${inner}${key}: ${render(item, inner)}`).join('\n')}\n${indent}}`
}
