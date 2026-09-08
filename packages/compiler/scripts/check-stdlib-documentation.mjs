// Checks maintained documentation policy across the canonical standard-library manifest.
//
// Run after building the compiler and documentation packages:
//   pnpm --filter @silklang/compiler documentation:policy

import { readFileSync } from 'node:fs'
import * as Data from 'effect/Data'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as DocumentationPolicy from '../../docgen/dist/Policy.js'
import * as DocumentationProject from '../../docgen/dist/Project.js'
import * as ProjectAnalysis from '../dist/ProjectAnalysis.js'
import * as SourceFile from '../dist/SourceFile.js'
import * as SourceResolver from '../dist/SourceResolver.js'
import * as CompilerStdlib from '../dist/Stdlib.js'
import { documentationProfiles } from './documentation-profiles.mjs'

class DocumentationPolicyError extends Data.TaggedError('DocumentationPolicyError') {}

const log = (...values) => Effect.runSync(Console.log(...values))
const logError = (...values) => Effect.runSync(Console.error(...values))

const lineAt = (bytes, offset) => {
  const limit = Math.max(0, Math.min(offset, bytes.length))
  let line = 1
  for (let index = 0; index < limit; index += 1) if (bytes[index] === 0x0a) line += 1
  return line
}

const program = Effect.gen(function* () {
  const analyzed = []
  for (const manifest of CompilerStdlib.manifest) {
    const bytes = yield* Effect.try({
      try: () =>
        Uint8Array.from(readFileSync(new URL(`../stdlib/${manifest.path}`, import.meta.url))),
      catch: (cause) =>
        new DocumentationPolicyError({
          message: `Missing stdlib source: ${manifest.module}`,
          cause,
        }),
    })
    analyzed.push({ manifest, bytes, root: SourceFile.make(manifest.module, bytes) })
  }
  const seen = new Set()
  const violations = new Map()
  for (const selected of documentationProfiles) {
    const analysis = yield* ProjectAnalysis.make(
      analyzed.map((entry) => entry.root),
      { configuration: { profile: selected.profile } },
    ).pipe(Effect.provide(SourceResolver.empty))
    const project = DocumentationProject.fromProjectAnalysis(analysis)
    for (const entry of analyzed) {
      const documented = project.modules.find((module) => module.name === entry.manifest.module)
      const snapshot = ProjectAnalysis.view(analysis, entry.manifest.module)
      if (documented === undefined || snapshot === undefined)
        return yield* Effect.fail(`Missing documentation model: ${entry.manifest.module}`)
      if (documented.documentation === undefined && documented.items.length === 0) continue
      seen.add(entry.manifest.module)
      for (const violation of DocumentationPolicy.check(documented, snapshot, project)) {
        const key = `${violation.code}:${violation.identity}:${violation.source.start}`
        violations.set(key, {
          violation,
          path: entry.manifest.path,
          line: lineAt(entry.bytes, violation.source.start),
        })
      }
    }
  }
  for (const entry of analyzed) {
    if (!seen.has(entry.manifest.module))
      return yield* new DocumentationPolicyError({
        message: `No admitted documentation profile exposes ${entry.manifest.module}`,
      })
  }
  return [...violations.values()]
})

const moduleFlag = process.argv.indexOf('--module')
const selectedModule = moduleFlag === -1 ? undefined : process.argv[moduleFlag + 1]
const allViolations = await Effect.runPromise(program)
const violations =
  selectedModule === undefined
    ? allViolations
    : allViolations.filter((entry) => entry.violation.source.sourceId === selectedModule)
const checkedModules = selectedModule === undefined ? CompilerStdlib.manifest.length : 1
if (!process.argv.includes('--summary'))
  for (const { violation, path, line } of violations)
    logError(`${path}:${line}: [${violation.code}] ${violation.identity}: ${violation.message}`)
log(
  violations.length === 0
    ? `Stdlib documentation policy: ${checkedModules} modules checked, no violations.`
    : `Stdlib documentation policy: ${checkedModules} modules checked, ${violations.length} violations.`,
)
if (violations.length > 0) process.exitCode = 1
