import { readdirSync, readFileSync } from 'node:fs'
import { dirname, join, relative } from 'node:path'
import { fileURLToPath } from 'node:url'
import { expect, it } from 'vitest'

const sourceRoot = join(dirname(fileURLToPath(import.meta.url)), '..', 'src')

const phaseModules = [
  'Backend',
  'Lexer',
  'Parser',
  'ModuleClosure',
  'DeclarationIndex',
  'NameResolution',
  'Elaboration',
  'BootstrapEvaluation',
]

const sourceFiles = (directory: string): ReadonlyArray<string> =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name)
    if (entry.isDirectory()) return sourceFiles(path)
    return /\.ts$/.test(entry.name) ? [path] : []
  })

const valueImportedNames = (source: string): ReadonlyArray<string> => {
  const names: Array<string> = []
  const imports = source.matchAll(
    /import\s+(type\s+)?\{([^}]*)\}\s+from\s+'@silk-lang\/compiler'/g,
  )
  for (const match of imports) {
    if (match[1] !== undefined) continue
    for (const specifier of (match[2] ?? '').split(',')) {
      const trimmed = specifier.trim()
      if (trimmed.length === 0 || trimmed.startsWith('type ')) continue
      names.push(trimmed.split(' ')[0] ?? trimmed)
    }
  }
  return names
}

it('inspector projections value-import no compiler phase module', () => {
  const offenders: Array<string> = []
  for (const file of sourceFiles(sourceRoot)) {
    const names = valueImportedNames(readFileSync(file, 'utf8'))
    for (const name of names) {
      if (phaseModules.includes(name)) {
        offenders.push(`${relative(sourceRoot, file)}: ${name}`)
      }
    }
  }
  expect(offenders).toEqual([])
})
