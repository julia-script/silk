// Regenerates packages/language/docs/stdlib/ and diagnostics.md.
//
// The standard library page comes from the same documentation model `silk doc` emits, so the
// rendered signatures and prose are exactly the `///` comments in stdlib/silk/*.silk. The
// diagnostic index comes from the code constants and message templates in src/Diagnostic.ts.
//
// Run with: pnpm --filter @silk-effect/compiler documentation:generate

import { existsSync, mkdirSync, readdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import * as Effect from 'effect/Effect'
import * as DocumentationProject from '../../documentation/dist/Project.js'
import * as DocumentationReference from '../../documentation/dist/Reference.js'
import * as Analysis from '../dist/Analysis.js'
import * as Stdlib from '../dist/Stdlib.js'

const documentationRoot = fileURLToPath(new URL('../../language/docs/', import.meta.url))
const stdlibRoot = fileURLToPath(new URL('../../language/docs/stdlib/', import.meta.url))
const diagnosticSource = fileURLToPath(new URL('../src/Diagnostic.ts', import.meta.url))

const stdlibTree = async () => {
  const modules = []
  for (const module of Stdlib.manifest) {
    // Read the canonical source rather than the compiler's generated source map. Documentation is
    // commonly regenerated immediately after editing a .silk file, before compiler dist has been
    // rebuilt; using the embedded map there would quietly emit yesterday's prose.
    const bytes = Uint8Array.from(
      readFileSync(new URL(`../stdlib/${module.path}`, import.meta.url)),
    )
    const snapshot = await Effect.runPromise(Analysis.ofSource(module.module, bytes))
    const project = DocumentationProject.make(snapshot)
    const documented = project.modules.find((entry) => entry.name === module.module)
    if (documented !== undefined) modules.push({ manifest: module, documented })
  }

  const project = Object.freeze({
    schema: 'silk-documentation',
    experimental: true,
    modules: Object.freeze(modules.map((entry) => entry.documented)),
  })
  const rendered = DocumentationReference.make(Stdlib.manifest, project)
  if (rendered._tag === 'Failure') {
    for (const error of rendered.errors) console.error('Stdlib reference generation failed:', error)
    process.exit(1)
  }
  return rendered.reference.files
}

const phases = {
  LEX: 'Lexical',
  PAR: 'Parser',
  MOD: 'Module',
  SEM: 'Semantic',
  OWN: 'Ownership',
  LAY: 'Layout',
}

// The diagnostic message scanners. Message expressions are walked character by character with
// string, template, and bracket state, because a regular expression cannot see where a template
// hole or a ternary branch ends once literals nest inside interpolations.

/** Index just past the string or template literal opening at `index`. */
const literalEnd = (text, index) => {
  const quote = text[index]
  let cursor = index + 1
  while (cursor < text.length) {
    const character = text[cursor]
    if (character === '\\') cursor += 2
    else if (character === quote) return cursor + 1
    else if (quote === '`' && character === '$' && text[cursor + 1] === '{')
      cursor = expressionEnd(text, cursor + 2, '}') + 1
    else cursor += 1
  }
  return cursor
}

/** Index of the first occurrence of `terminator` at bracket depth zero, outside literals. */
const expressionEnd = (text, index, terminator) => {
  let depth = 0
  let cursor = index
  while (cursor < text.length) {
    const character = text[cursor]
    if (character === "'" || character === '"' || character === '`') {
      cursor = literalEnd(text, cursor)
      continue
    }
    if (depth === 0 && character === terminator) return cursor
    if (character === '(' || character === '[' || character === '{') depth += 1
    else if (character === ')' || character === ']' || character === '}') depth -= 1
    cursor += 1
  }
  return cursor
}

/**
 * Index where the expression starting at `index` ends: the first unnested line break whose next
 * line does not continue it, or an unnested closing bracket. This is how far a `return` reaches
 * in the semicolon-free house style, where a wrapped conditional opens its lines with `?` or `:`.
 */
const statementEnd = (text, index) => {
  let depth = 0
  let cursor = index
  while (cursor < text.length) {
    const character = text[cursor]
    if (character === "'" || character === '"' || character === '`') {
      cursor = literalEnd(text, cursor)
      continue
    }
    if (character === '(' || character === '[' || character === '{') depth += 1
    else if (character === ')' || character === ']' || character === '}') {
      if (depth === 0) return cursor
      depth -= 1
    } else if (character === '\n' && depth === 0) {
      if (!/^\s*(?:[?:.]|\|\||&&|\?\?)/.test(text.slice(cursor + 1, cursor + 12))) return cursor
    }
    cursor += 1
  }
  return cursor
}

/** The template's text with each interpolation hole replaced by a `<placeholder>` name. */
const templateOf = (content) => {
  let rendered = ''
  let cursor = 0
  while (cursor < content.length) {
    if (content[cursor] === '$' && content[cursor + 1] === '{') {
      const end = expressionEnd(content, cursor + 2, '}')
      const expression = content.slice(cursor + 2, end)
      const tail = expression.trim().split('.').pop() ?? expression
      rendered += `<${tail.replace(/[^A-Za-z0-9]/g, '')}>`
      cursor = end + 1
    } else {
      rendered += content[cursor]
      cursor += 1
    }
  }
  return rendered
}

/** The two branches of the expression's outermost conditional, or undefined without one. */
const branchesOf = (text) => {
  let depth = 0
  let question = -1
  let pending = 0
  let cursor = 0
  while (cursor < text.length) {
    const character = text[cursor]
    if (character === "'" || character === '"' || character === '`') {
      cursor = literalEnd(text, cursor)
      continue
    }
    if (character === '(' || character === '[' || character === '{') depth += 1
    else if (character === ')' || character === ']' || character === '}') depth -= 1
    else if (
      depth === 0 &&
      character === '?' &&
      text[cursor + 1] !== '?' &&
      text[cursor + 1] !== '.' &&
      text[cursor - 1] !== '?'
    ) {
      if (question === -1) question = cursor
      else pending += 1
    } else if (depth === 0 && character === ':' && question !== -1) {
      if (pending === 0)
        return Object.freeze([text.slice(question + 1, cursor), text.slice(cursor + 1)])
      pending -= 1
    }
    cursor += 1
  }
  return undefined
}

/** The unnested string and template literals of the expression, holes already renamed. */
const literalsOf = (text) => {
  const literals = []
  let depth = 0
  let cursor = 0
  while (cursor < text.length) {
    const character = text[cursor]
    if (character === "'" || character === '"' || character === '`') {
      const end = literalEnd(text, cursor)
      if (depth === 0) literals.push(templateOf(text.slice(cursor + 1, end - 1)))
      cursor = end
      continue
    }
    if (character === '(' || character === '[' || character === '{') depth += 1
    else if (character === ')' || character === ']' || character === '}') depth -= 1
    cursor += 1
  }
  return literals
}

/**
 * Every complete template the message expression can produce, in source order.
 *
 * A conditional contributes both branches and drops its condition, whose literals are
 * discriminants rather than wording. An expression with no literal of its own must be a call to a
 * message helper in the same file; its templates are the ones its `return` statements produce.
 */
const templatesOf = (source, expression) => {
  const text = expression.trim()
  const branches = branchesOf(text)
  if (branches !== undefined)
    return [...templatesOf(source, branches[0]), ...templatesOf(source, branches[1])]
  const literals = literalsOf(text)
  if (literals.length > 0) return literals
  const call = /^(\w+)\(/.exec(text)
  if (call === null) return []
  const opening = source.indexOf(`const ${call[1]} = `)
  if (opening === -1) return []
  const body = source.slice(source.indexOf('=> {', opening), statementEnd(source, opening))
  const templates = []
  for (const returned of body.matchAll(/\breturn\s/g)) {
    const start = (returned.index ?? 0) + returned[0].length
    templates.push(...templatesOf(source, body.slice(start, statementEnd(body, start))))
  }
  return templates
}

const diagnosticsPage = () => {
  const source = readFileSync(diagnosticSource, 'utf8')

  const documented = new Map()
  const collisions = new Map()
  // `[^*]` in the comment body keeps the match from reaching back past an earlier `*/`.
  const declaration =
    /(?:\/\*\*((?:[^*]|\*(?!\/))*)\*\/\s*)?export const (\w+)Code = '([A-Z]{3}[0-9]{4})' as const/g
  for (const match of source.matchAll(declaration)) {
    const comment = (match[1] ?? '')
      .replace(/^\s*\*\s?/gm, '')
      .replace(/\s+/g, ' ')
      .trim()
    // Keyed by code, so a second constant claiming a taken code would silently displace the first
    // and this page would document one meaning while the compiler reports two. Refuse instead: a
    // published index that resolves a collision by dropping a diagnostic is worse than no index.
    const taken = documented.get(match[3])
    if (taken !== undefined)
      collisions.set(match[3], [
        ...(collisions.get(match[3]) ?? [`${taken.name}Code`]),
        `${match[2]}Code`,
      ])
    documented.set(match[3], { name: match[2], comment })
  }

  if (collisions.size > 0) {
    console.error('Duplicate stable diagnostic codes in src/Diagnostic.ts:')
    for (const [code, names] of collisions)
      console.error(`  ${code} is held by ${names.join(' and ')}`)
    console.error('A stable code identifies one condition. Renumber the newer constant.')
    process.exit(1)
  }

  // The constructor bodies carry the user-visible message templates, which are the most precise
  // one-line meaning available for the codes whose constant carries no comment. A message is not
  // always one literal: some factories choose between complete templates with a conditional, and
  // one delegates to a helper whose switch returns a template per problem. `templatesOf` below
  // walks the message expression by nesting rather than by pattern, so every complete wording a
  // code can report lands in this catalog — and is thereby gated against drift.
  const messages = new Map()
  for (const match of source.matchAll(/code: (\w+)Code,[\s\S]{0,400}?\bmessage:/g)) {
    const start = (match.index ?? 0) + match[0].length
    const variants = templatesOf(source, source.slice(start, expressionEnd(source, start, ',')))
    if (variants.length > 0 && !messages.has(match[1])) messages.set(match[1], variants)
  }
  const byName = new Map(
    [...documented.entries()].map(([code, entry]) => [entry.name, { code, ...entry }]),
  )
  for (const [name, templates] of messages) {
    const entry = byName.get(name)
    if (entry !== undefined) entry.messages = templates
  }

  const codes = [...documented.entries()]
    .map(([code, entry]) => ({ code, ...entry, ...(byName.get(entry.name) ?? {}) }))
    .sort((left, right) => left.code.localeCompare(right.code))

  const missingMessages = codes.filter(
    (entry) =>
      entry.messages === undefined ||
      entry.messages.length === 0 ||
      entry.messages.some((message) => message.trim().length === 0),
  )
  if (missingMessages.length > 0) {
    console.error('Diagnostic codes without a discoverable non-empty message template:')
    for (const entry of missingMessages) console.error(`  ${entry.code} (${entry.name}Code)`)
    console.error('Keep each code and complete message template together in a diagnostic factory.')
    process.exit(1)
  }

  const lines = [
    '# Silk diagnostic index',
    '',
    `Every diagnostic code the Silk compiler can report, with its meaning.`,
    '',
    '<!-- Generated by scripts/generate-documentation.mjs. Do not edit by hand. -->',
    '',
    'Each code is stable: it identifies one condition and never changes meaning between releases.',
    'The prefix names the compiler phase that reports it. Regenerate this page with:',
    '',
    '```console',
    '$ pnpm --filter @silk-effect/compiler documentation:generate',
    '```',
    '',
    '| Prefix | Phase | Codes |',
    '| --- | --- | --- |',
  ]

  for (const [prefix, phase] of Object.entries(phases)) {
    const count = codes.filter((entry) => entry.code.startsWith(prefix)).length
    if (count > 0) lines.push(`| \`${prefix}\` | ${phase} | ${count} |`)
  }
  lines.push('', `There are ${codes.length} codes in total.`, '')

  for (const [prefix, phase] of Object.entries(phases)) {
    const group = codes.filter((entry) => entry.code.startsWith(prefix))
    if (group.length === 0) continue
    lines.push(
      `## ${phase} (\`${prefix}\`)`,
      '',
      '| Code | Meaning | Reported as |',
      '| --- | --- | --- |',
    )
    for (const entry of group) {
      const meaning = entry.comment.length > 0 ? entry.comment : ''
      // A code with several complete templates lists each: they are all wording this page gates.
      // A template that itself contains a backtick needs the padded double-backtick span form.
      const reported = (entry.messages ?? [])
        .map((template) => template.replace(/\|/g, '\\|'))
        .map((template) => (template.includes('`') ? `\`\` ${template} \`\`` : `\`${template}\``))
        .join('<br>')
      lines.push(`| \`${entry.code}\` | ${meaning.replace(/\|/g, '\\|')} | ${reported} |`)
    }
    lines.push('')
  }

  lines.push(
    '## See also',
    '',
    '- [Tutorial](./tutorial.md)',
    '- [Language reference](./reference.md)',
    '- [Standard library](./stdlib/)',
    '',
  )
  return lines.join('\n')
}

const check = process.argv.includes('--check')

const write = (name, contents) => {
  const destination = `${documentationRoot}${name}`
  if (check) {
    if (!existsSync(destination)) {
      console.error(
        `${name} is missing. Run pnpm --filter @silk-effect/compiler documentation:generate`,
      )
      process.exitCode = 1
      return
    }
    const existing = readFileSync(destination, 'utf8')
    if (existing !== contents) {
      console.error(
        `${name} is stale. Run pnpm --filter @silk-effect/compiler documentation:generate`,
      )
      process.exitCode = 1
    }
    return
  }
  writeFileSync(destination, contents)
}

const writeStdlib = (files) => {
  const expected = new Map(files.map((file) => [file.path, file.contents]))
  const actual = existsSync(stdlibRoot)
    ? readdirSync(stdlibRoot).filter((entry) => entry.endsWith('.md'))
    : []
  if (check) {
    for (const path of expected.keys())
      if (!actual.includes(path)) {
        console.error(
          `stdlib/${path} is missing. Run pnpm --filter @silk-effect/compiler documentation:generate`,
        )
        process.exitCode = 1
      }
    for (const path of actual)
      if (!expected.has(path)) {
        console.error(
          `stdlib/${path} is extra or renamed. Run pnpm --filter @silk-effect/compiler documentation:generate`,
        )
        process.exitCode = 1
      }
    for (const [path, contents] of expected) {
      const destination = `${stdlibRoot}${path}`
      if (!existsSync(destination)) continue
      if (readFileSync(destination, 'utf8') !== contents) {
        console.error(
          `stdlib/${path} is stale. Run pnpm --filter @silk-effect/compiler documentation:generate`,
        )
        process.exitCode = 1
      }
    }
    return
  }
  mkdirSync(stdlibRoot, { recursive: true })
  for (const path of actual) if (!expected.has(path)) rmSync(`${stdlibRoot}${path}`)
  for (const [path, contents] of expected) writeFileSync(`${stdlibRoot}${path}`, contents)
}

// The complete tree and diagnostic page are rendered before anything is written, so a collision
// or rejected diagnostic index leaves generated documentation untouched.
const stdlib = await stdlibTree()
const diagnostics = diagnosticsPage()

writeStdlib(stdlib)
write('diagnostics.md', diagnostics)
