import * as Fs from 'node:fs'
import * as Path from 'node:path'
import { assert, it } from '@effect/vitest'

const source = Path.join(import.meta.dirname, '..', 'src')

it('has no public re-analysis surface or legacy lowering module', () => {
  const files = Fs.readdirSync(source, { recursive: true, encoding: 'utf8' })
  assert.notInclude(files, 'TirLowering.ts')
  assert.notInclude(files, 'BodyConstruction.ts')
  const legacyNames = [
    /\bFunctionFact\b/,
    /\bStatementFact\b/,
    /\bExpressionFact\b/,
    /\bTirLowering\b/,
    /Elaboration\.records\b/,
  ]
  for (const file of files.filter((candidate) => candidate.endsWith('.ts'))) {
    const text = Fs.readFileSync(Path.join(source, file), 'utf8')
    assert.notMatch(text, /Elaboration\.(records|executableFunctions)\b/, file)
    for (const legacyName of legacyNames) assert.notMatch(text, legacyName, file)
  }
})

it('has no rebinding of cached bodies', () => {
  const files = Fs.readdirSync(source, { recursive: true, encoding: 'utf8' })
  assert.notInclude(files, 'SemanticRebinding.ts')
  for (const file of files.filter((candidate) => candidate.endsWith('.ts')))
    assert.notMatch(Fs.readFileSync(Path.join(source, file), 'utf8'), /SemanticRebinding/, file)
})
