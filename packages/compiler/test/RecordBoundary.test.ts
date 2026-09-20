import * as Fs from 'node:fs'
import * as Path from 'node:path'
import { assert, it } from '@effect/vitest'

const source = Path.join(import.meta.dirname, '..', 'src')

/** Working records: what analysis builds a body from before it publishes nodes and tables. */
const record =
  /\b(FunctionFact|StatementFact|ExpressionFact)\b|\bvisit(Statement|Expression)Facts?\b|Elaboration\.(records|recordOf|executableFunctions)\b/

/**
 * Construction, which builds records; the table builders it calls; the two coordinators that run
 * construction for an application or a module condition; and the inspection surface, which shows
 * construction itself. Every other module reads a checked body: nodes and tables.
 */
const construction = [
  'BodyControlFlow.ts',
  'CallResolution.ts',
  'Elaboration.ts',
  'ExpressionAnalysis.ts',
  'LifetimeFlow.ts',
  'NativeAssembly.ts',
  'OpaqueRealization.ts',
  'SemanticOccurrence.ts',
  'StatementAnalysis.ts',
  'TirLowering.ts',
  'TypeHint.ts',
  'ModuleSelection.ts',
  'Residualization.ts',
]
const inspection = ['Analysis.ts', 'InspectorFlowModel.ts', 'ModuleTooling.ts']

it('keeps working records inside construction and its inspection surface', () => {
  const readers = Fs.readdirSync(source, { recursive: true, encoding: 'utf8' })
    .filter((file) => file.endsWith('.ts'))
    .filter((file) => record.test(Fs.readFileSync(Path.join(source, file), 'utf8')))
    .sort()
  assert.deepEqual(readers, [...construction, ...inspection].sort())
})

it('has no rebinding of cached bodies', () => {
  const files = Fs.readdirSync(source, { recursive: true, encoding: 'utf8' })
  assert.notInclude(files, 'SemanticRebinding.ts')
  for (const file of files.filter((candidate) => candidate.endsWith('.ts')))
    assert.notMatch(Fs.readFileSync(Path.join(source, file), 'utf8'), /SemanticRebinding/, file)
})
