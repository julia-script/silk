import { createHash } from 'node:crypto'

export const sourceNames = ['Builder.zig', 'bitcode_writer.zig', 'ir.zig']

export const sourceUrl = (commit, name) =>
  `https://codeberg.org/ziglang/zig/raw/commit/${commit}/lib/std/zig/llvm/${name}`

export const sha256 = (value) => createHash('sha256').update(value).digest('hex')

export const fetchSources = async (commit) => {
  const entries = await Promise.all(
    sourceNames.map(async (name) => {
      const response = await fetch(sourceUrl(commit, name))
      if (!response.ok) throw new Error(`Unable to fetch ${name}: HTTP ${response.status}`)
      return [name, await response.text()]
    }),
  )
  return Object.fromEntries(entries)
}

const lineNumber = (source, offset) => source.slice(0, offset).split('\n').length

const matchingBrace = (source, open) => {
  let depth = 0
  let string
  let lineComment = false
  for (let index = open; index < source.length; index += 1) {
    const character = source[index]
    const next = source[index + 1]
    if (lineComment) {
      if (character === '\n') lineComment = false
      continue
    }
    if (string !== undefined) {
      if (character === '\\') index += 1
      else if (character === string) string = undefined
      continue
    }
    if (character === '/' && next === '/') {
      lineComment = true
      index += 1
      continue
    }
    if (character === '"' || character === "'") {
      string = character
      continue
    }
    if (character === '{') depth += 1
    if (character === '}') {
      depth -= 1
      if (depth === 0) return index
    }
  }
  throw new Error(`Unmatched source brace at byte ${open}`)
}

const enums = (name, source) => {
  const values = []
  const pattern = /(?:pub\s+)?const\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*enum(?:\([^)]*\))?\s*\{/g
  for (const match of source.matchAll(pattern)) {
    const enumName = match[1]
    const start = (match.index ?? 0) + match[0].lastIndexOf('{')
    const end = matchingBrace(source, start)
    const declarationLine = lineNumber(source, match.index ?? 0)
    let depth = 0
    const body = source.slice(start + 1, end)
    for (const [offset, line] of body.split('\n').entries()) {
      const trimmed = line.replace(/\/\/.*$/, '').trim()
      if (depth === 0) {
        const item = /^(?:@"([^"]+)"|([A-Za-z_][A-Za-z0-9_]*))\s*(?:=[^,]+)?[,]?$/.exec(trimmed)
        const caseName = item?.[1] ?? item?.[2]
        if (caseName !== undefined && caseName !== '_') {
          values.push({
            id: `${name}:enum:${enumName}@${declarationLine}:${caseName}`,
            source: name,
            enum: enumName,
            case: caseName,
            line: declarationLine + offset + 1,
          })
        }
      }
      for (const character of trimmed) {
        if (character === '{') depth += 1
        if (character === '}') depth -= 1
      }
    }
  }
  return values
}

const publicOperations = (name, source) =>
  [...source.matchAll(/^\s*pub fn\s+([A-Za-z_][A-Za-z0-9_]*)/gm)].map((match) => {
    const line = lineNumber(source, match.index ?? 0)
    return {
      id: `${name}:public:${match[1]}@${line}`,
      source: name,
      name: match[1],
      line,
    }
  })

const abbreviationRecords = (name, source) => {
  const records = []
  const pattern = /pub const abbrevs\s*=\s*\[_\]type\s*\{/g
  for (const match of source.matchAll(pattern)) {
    const start = (match.index ?? 0) + match[0].lastIndexOf('{')
    const end = matchingBrace(source, start)
    const declarationLine = lineNumber(source, match.index ?? 0)
    for (const [offset, line] of source
      .slice(start + 1, end)
      .split('\n')
      .entries()) {
      const record = line
        .replace(/\/\/.*$/, '')
        .trim()
        .replace(/,$/, '')
      if (/^[A-Za-z_][A-Za-z0-9_.]*$/.test(record)) {
        records.push({
          id: `${name}:abbrev:${declarationLine}:${record}`,
          source: name,
          record,
          line: declarationLine + offset + 1,
        })
      }
    }
  }
  return records
}

const sourceMarkers = (name, source, pattern, kind) =>
  source.split('\n').flatMap((line, offset) =>
    pattern.test(line)
      ? [
          {
            id: `${name}:${kind}:${offset + 1}`,
            source: name,
            line: offset + 1,
            text: line.trim(),
          },
        ]
      : [],
  )

const unique = (values) => {
  const seen = new Set()
  return values.filter((value) => {
    if (seen.has(value.id)) return false
    seen.add(value.id)
    return true
  })
}

export const extractInventory = (commit, sources) => {
  const enumCases = unique(sourceNames.flatMap((name) => enums(name, sources[name] ?? '')))
  const intrinsicEntries = enumCases.filter(
    (entry) => entry.source === 'Builder.zig' && entry.enum === 'Intrinsic',
  )
  const blockRecords = enumCases.filter(
    (entry) => entry.source === 'ir.zig' && entry.enum === 'Code',
  )
  return {
    schemaVersion: 1,
    commit,
    sources: Object.fromEntries(
      sourceNames.map((name) => {
        const source = sources[name] ?? ''
        return [name, { sha256: sha256(source), lines: source.split('\n').length }]
      }),
    ),
    publicOperations: unique(
      sourceNames.flatMap((name) => publicOperations(name, sources[name] ?? '')),
    ),
    enumCases: enumCases.filter(
      (entry) =>
        !intrinsicEntries.some((intrinsic) => intrinsic.id === entry.id) &&
        !blockRecords.some((record) => record.id === entry.id),
    ),
    intrinsicEntries,
    blockRecords,
    abbreviationRecords: unique(
      sourceNames.flatMap((name) => abbreviationRecords(name, sources[name] ?? '')),
    ),
    todos: unique(
      sourceNames.flatMap((name) => sourceMarkers(name, sources[name] ?? '', /\bTODO\b/, 'todo')),
    ),
    panicPaths: unique(
      sourceNames.flatMap((name) =>
        sourceMarkers(
          name,
          sources[name] ?? '',
          /\bunreachable\b|@panic\s*\(|assert\s*\(false\)/,
          'panic',
        ),
      ),
    ),
  }
}

export const inventoryKinds = [
  'publicOperations',
  'enumCases',
  'intrinsicEntries',
  'blockRecords',
  'abbreviationRecords',
  'todos',
  'panicPaths',
]
