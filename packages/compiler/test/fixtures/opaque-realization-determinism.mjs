import * as Effect from 'effect/Effect'
import * as OpaqueRealization from '../../dist/OpaqueRealization.js'
import * as ProjectAnalysis from '../../dist/ProjectAnalysis.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SourceResolver from '../../dist/SourceResolver.js'
import * as Type from '../../dist/Type.js'

const encoder = new TextEncoder()
const library = 'fixture/opaque_library'
const application = 'fixture/opaque_application'
const producer = (captured) => `fn keep<T>(value: T, enabled: bool) -> T { return move value }
pub fn make<T>(enabled: bool) -> some<F: fn(T) -> T> F { return keep<T>(${captured}) }`
const importer = `import fixture.opaque_library { make }
pub fn use() -> i32 { let parser = make<i32>(true) return parser(1) }`

const analyze = async (librarySource, previous) => {
  const sources = new Map([
    [library, encoder.encode(librarySource)],
    [application, encoder.encode(importer)],
  ])
  const root = SourceFile.make(application, sources.get(application))
  const effect =
    previous === undefined ? ProjectAnalysis.make([root]) : ProjectAnalysis.revise(previous, [root])
  return Effect.runPromise(effect.pipe(Effect.provide(SourceResolver.memory(sources))))
}

const baseline = await analyze(producer('enabled'))
const edited = await analyze(producer('!enabled'), baseline)
const view = ProjectAnalysis.view(baseline, application)
if (view === undefined) throw new Error('missing application view')
const declaration = view.index.modules
  .find((module) => module.module === library)
  ?.declarations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === 'make',
  )
if (declaration?.returnType._tag !== 'Resolved')
  throw new Error(`missing opaque return in ${view.index.modules.map((module) => module.module)}`)
const instance = Type.opaqueRepresentationArguments(declaration.returnType.type).at(0)
if (instance === undefined) throw new Error('missing opaque instance')
const definition = OpaqueRealization.catalogOf(view).definitions.get(
  Type.opaqueFamilyKey(instance.family),
)
if (definition === undefined) throw new Error('missing opaque definition')

process.stdout.write(
  JSON.stringify({
    family: Type.genericArgumentKey(instance),
    publicSignature: declaration.opaqueResult?.publicSignature,
    publicSurface: view.surfaces.get(library)?.canonical,
    privateDefinition: {
      key: Type.opaqueFamilyKey(definition.family),
      target: definition.targetFingerprint,
      body: definition.bodyFingerprint,
      layout: definition.layoutFingerprint,
      captures: definition.captures.map((capture) => ({
        ordinal: capture.ordinal,
        type: Type.key(capture.type),
        access: capture.access,
      })),
    },
    invalidation: edited.semanticInvalidation,
  }),
)
