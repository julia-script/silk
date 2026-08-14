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
const obsoleteStdlibPage = fileURLToPath(new URL('../../language/docs/stdlib.md', import.meta.url))
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

  // The constructor bodies carry the user-visible message template, which is the most precise
  // one-line meaning available for the codes whose constant carries no comment.
  const messages = new Map()
  // `message:` may wrap onto its own line before the string literal.
  const factory = /code: (\w+)Code,[\s\S]{0,400}?message:\s*([`'"])((?:\\.|(?!\2)[\s\S])*)\2/g
  for (const match of source.matchAll(factory)) {
    const template = match[3].replace(/\$\{([^}]*)\}/g, (_, expression) => {
      const tail = expression.trim().split('.').pop() ?? expression
      return `<${tail.replace(/[^A-Za-z0-9]/g, '')}>`
    })
    if (!messages.has(match[1])) messages.set(match[1], template)
  }
  const byName = new Map(
    [...documented.entries()].map(([code, entry]) => [entry.name, { code, ...entry }]),
  )
  for (const [name, template] of messages) {
    const entry = byName.get(name)
    if (entry !== undefined) entry.message = template
  }

  const codes = [...documented.entries()]
    .map(([code, entry]) => ({ code, ...entry, ...(byName.get(entry.name) ?? {}) }))
    .sort((left, right) => left.code.localeCompare(right.code))

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
      const reported =
        entry.message === undefined ? '' : `\`${entry.message.replace(/\|/g, '\\|')}\``
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
    if (existsSync(obsoleteStdlibPage)) {
      console.error(
        'stdlib.md is obsolete. Run pnpm --filter @silk-effect/compiler documentation:generate',
      )
      process.exitCode = 1
    }
    return
  }
  mkdirSync(stdlibRoot, { recursive: true })
  for (const path of actual) if (!expected.has(path)) rmSync(`${stdlibRoot}${path}`)
  for (const [path, contents] of expected) writeFileSync(`${stdlibRoot}${path}`, contents)
  if (existsSync(obsoleteStdlibPage)) rmSync(obsoleteStdlibPage)
}

// The complete tree and diagnostic page are rendered before anything is written, so a collision
// or rejected diagnostic index leaves generated documentation untouched.
const stdlib = await stdlibTree()
const diagnostics = diagnosticsPage()

writeStdlib(stdlib)
write('diagnostics.md', diagnostics)
