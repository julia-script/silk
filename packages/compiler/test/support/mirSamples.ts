import type * as DeclarationIndex from '../../src/DeclarationIndex.js'
import type * as Instances from '../../src/Instances.js'
import * as Layout from '../../src/Layout.js'
import type { LocalId, Module, Provenance, RegionId, Type } from '../../src/Mir.js'
import * as SourceFile from '../../src/SourceFile.js'
import * as SourceSpan from '../../src/SourceSpan.js'
import * as Target from '../../src/Target.js'

const sampleSpan = (
  source: SourceFile.SourceFile,
  start: number,
  end: number,
): SourceSpan.SourceSpan => {
  const span = SourceSpan.make(source, start, end)
  if (span._tag === 'None') throw new RangeError('MIR sample produced an invalid span')
  return span.value
}
const local = (ordinal: number): LocalId => Object.freeze({ _tag: 'Local', ordinal })
const region = (ordinal: number): RegionId => Object.freeze({ _tag: 'Region', ordinal })
const i32: Type = Object.freeze({ _tag: 'i32' })
const bool: Type = Object.freeze({ _tag: 'bool' })
const canonical = (module: string, name: string): DeclarationIndex.CanonicalId =>
  Object.freeze({ _tag: 'CanonicalDeclarationId', module, name })
const instance = (declaration: DeclarationIndex.CanonicalId): Instances.InstanceKey =>
  Object.freeze({
    _tag: 'InstanceKey',
    declaration,
    typeArguments: Object.freeze([]),
    contractRow: Object.freeze([]),
  })

export const samples = (): ReadonlyArray<Module> => {
  const source = SourceFile.make(
    'sample://regions.silk',
    Uint8Array.from('pub fn answer() -> i32 { return 42 }', (char) => char.charCodeAt(0)),
  )
  const provenance = (start: number, end: number, generated = false): Provenance =>
    Object.freeze({ span: sampleSpan(source, start, end), generated })
  const straight: Module = Object.freeze({
    _tag: 'MirModule',
    module: source.id,
    intrinsics: Object.freeze([]),
    entry: Object.freeze({
      _tag: 'OrdinaryEntry',
      target: instance(canonical(source.id, 'answer')),
      machine: instance(canonical(source.id, 'answer')),
    }),
    layout: Layout.make(Target.aarch64AppleDarwin, ['i32']),
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical(source.id, 'answer'),
        instance: instance(canonical(source.id, 'answer')),
        parameterCount: 0,
        localTypes: Object.freeze([i32]),
        result: i32,
        entry: region(0),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(0),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Literal' as const,
                destination: local(0),
                type: i32,
                value: 42,
                provenance: provenance(32, 34),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: local(0),
              provenance: provenance(25, 34),
            }),
          }),
        ]),
      }),
    ]),
  })
  const conditional: Module = Object.freeze({
    _tag: 'MirModule',
    module: source.id,
    intrinsics: Object.freeze([]),
    entry: Object.freeze({
      _tag: 'OrdinaryEntry',
      target: instance(canonical(source.id, 'choose')),
      machine: instance(canonical(source.id, 'choose')),
    }),
    layout: Layout.make(Target.aarch64AppleDarwin, ['i32', 'bool']),
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical(source.id, 'choose'),
        instance: instance(canonical(source.id, 'choose')),
        parameterCount: 0,
        localTypes: Object.freeze([bool, i32]),
        result: i32,
        entry: region(0),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'ConditionalRegion' as const,
            id: region(0),
            condition: local(0),
            taken: region(1),
            otherwise: region(2),
            provenance: provenance(25, 34),
          }),
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(1),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Literal' as const,
                destination: local(1),
                type: i32,
                value: 1,
                provenance: provenance(32, 33),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: local(1),
              provenance: provenance(25, 34),
            }),
          }),
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(2),
            operations: Object.freeze([]),
            outcome: Object.freeze({
              _tag: 'Trap' as const,
              reason: 'otherwise',
              provenance: provenance(25, 34, true),
            }),
          }),
        ]),
      }),
    ]),
  })
  return Object.freeze([straight, conditional])
}
