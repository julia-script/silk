// Checks maintained documentation policy across the canonical standard-library manifest.
//
// Run after building the compiler and documentation packages:
//   pnpm --filter @silk-lang/compiler documentation:policy

import { readFileSync } from 'node:fs'
import * as Effect from 'effect/Effect'
import * as DocumentationPolicy from '../../documentation/dist/Policy.js'
import * as DocumentationProject from '../../documentation/dist/Project.js'
import * as ProjectAnalysis from '../dist/ProjectAnalysis.js'
import * as SourceFile from '../dist/SourceFile.js'
import * as SourceResolver from '../dist/SourceResolver.js'
import * as CompilerStdlib from '../dist/Stdlib.js'

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
      catch: (cause) => new Error(`Missing stdlib source: ${manifest.module}`, { cause }),
    })
    analyzed.push({ manifest, bytes, root: SourceFile.make(manifest.module, bytes) })
  }
  const analysis = yield* ProjectAnalysis.make(analyzed.map((entry) => entry.root)).pipe(
    Effect.provide(SourceResolver.empty),
  )
  const project = DocumentationProject.fromProjectAnalysis(analysis)
  const checked = []
  for (const entry of analyzed) {
    const documented = project.modules.find((module) => module.name === entry.manifest.module)
    const snapshot = ProjectAnalysis.view(analysis, entry.manifest.module)
    if (documented === undefined || snapshot === undefined)
      return yield* Effect.fail(`Missing documentation model: ${entry.manifest.module}`)
    checked.push({ ...entry, documented, snapshot })
  }
  return checked.flatMap((entry) =>
    DocumentationPolicy.check(entry.documented, entry.snapshot, project).map((violation) => ({
      violation,
      path: entry.manifest.path,
      line: lineAt(entry.bytes, violation.source.start),
    })),
  )
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
    console.error(
      `${path}:${line}: [${violation.code}] ${violation.identity}: ${violation.message}`,
    )
console.log(
  violations.length === 0
    ? `Stdlib documentation policy: ${checkedModules} modules checked, no violations.`
    : `Stdlib documentation policy: ${checkedModules} modules checked, ${violations.length} violations.`,
)
if (violations.length > 0) process.exitCode = 1
