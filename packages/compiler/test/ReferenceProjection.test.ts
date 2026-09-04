import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Residualization from '../src/Residualization.js'
import type * as StaticValue from '../src/StaticValue.js'
import { referenceProjectionAcceptance } from './support/corpus.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('retains failed referent facts and rejects affine borrowed reads', () =>
  Effect.gen(function* () {
    const invalid = yield* Analysis.ofSourceRealized(
      'reference-projection/non-reference-referent',
      ascii('fn invalid(value: i32) -> i32 { return value.* }'),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(invalid).map((diagnostic) => diagnostic.code),
      ['SEM0171'],
    )
    assert.strictEqual(
      Analysis.referentProjectionsOf(invalid, 'reference-projection/non-reference-referent').at(0)
        ?.state._tag,
      'Unavailable',
    )

    const affine = yield* Analysis.ofSourceRealized(
      'reference-projection/affine-referent',
      ascii(`struct Token { value: i32 }
fn invalid(value: &Token) -> Token { return value.* }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(affine).map((diagnostic) => diagnostic.code),
      ['OWN0012'],
    )
  }),
)

it.effect('rejects strengthening a shared value-reference reborrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/value-reborrow-strengthening',
      ascii(`struct Box { value: i32 }
fn mutate(box: &mut Box) -> () { box.value = 1 }
fn invalid(box: &Box) -> () { mutate(&mut box) }`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0056'],
    )
  }),
)

it.effect('retains zero-lane reads and nested reborrows while restoring the parent', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/surviving-runtime-structure',
      ascii(referenceProjectionAcceptance),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const encoded = Hir.encode(Analysis.rootAnalysis(snapshot).hir)
    assert.include(encoded, 'reborrow-value')
    assert.include(encoded, 'readEmpty')
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

it.effect('consumes a static field descriptor into an ordinary shared residual borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/static-descriptor',
      ascii(`struct Box { value: i32 }

fn borrowValue(
  owner: &Box,
  static field: Intrinsic.Field<Box, i32>,
) -> &i32 {
  return Intrinsic.borrowField<Box, i32>(owner, field)
}

pub fn main() -> i32 { return 0 }`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const module = snapshot.index.modules.find(
      (candidate) => candidate.module === 'reference-projection/static-descriptor',
    )
    const aggregate = module?.structs.find(
      (declaration) => declaration.name._tag === 'Present' && declaration.name.spelling === 'Box',
    )
    const function_ = module?.declarations.find(
      (declaration) =>
        declaration.name._tag === 'Present' && declaration.name.spelling === 'borrowValue',
    )
    assert.strictEqual(aggregate?._tag, 'StructDeclaration')
    assert.strictEqual(function_?._tag, 'FunctionDeclaration')
    if (
      aggregate?._tag !== 'StructDeclaration' ||
      aggregate.canonical._tag !== 'Canonical' ||
      function_?._tag !== 'FunctionDeclaration' ||
      function_.canonical._tag !== 'Canonical' ||
      snapshot.target._tag !== 'Resolved'
    )
      return
    const field = aggregate.fields.at(0)
    assert.isDefined(field)
    if (field === undefined || field.declaredType._tag !== 'Resolved') return
    const descriptor: StaticValue.FieldDescriptorValue = Object.freeze({
      _tag: 'FieldDescriptorValue',
      owner: Object.freeze({
        _tag: 'TypeDescriptorValue',
        owner: Object.freeze({
          _tag: 'NominalType',
          module: aggregate.canonical.id.module,
          name: aggregate.canonical.id.name,
          arguments: Object.freeze([]),
        }),
        kind: aggregate.aggregateKind,
      }),
      declarationOrdinal: field.id.ordinal,
      member: Object.freeze({ _tag: 'LabeledField', label: 'value' }),
      valueType: field.declaredType.type,
      authorization: function_.canonical.id,
      provenance: Object.freeze({
        sourceId: field.syntax.span.sourceId,
        start: field.syntax.span.start,
        end: field.syntax.span.end,
      }),
    })
    const residual = Residualization.residualize(
      Residualization.make(
        snapshot.target.target,
        snapshot.results,
        snapshot.resolution,
        snapshot.index,
      ),
      Object.freeze({
        declaration: function_.canonical.id,
        typeArguments: Object.freeze([]),
        evidence: Object.freeze([]),
        contractRow: Object.freeze([]),
        staticArguments: Object.freeze([descriptor]),
      }),
    )
    assert.strictEqual(residual._tag, 'ResidualBody')
    if (residual._tag !== 'ResidualBody') return
    assert.deepEqual(residual.diagnostics, [])
    assert.strictEqual(
      residual.function.contract._tag === 'Contract'
        ? residual.function.contract.parameters.length
        : 0,
      1,
    )
    const expressions = residual.function.statements.flatMap(Hir.statementExpressions)
    const tree = expressions.flatMap(Hir.expressionTree)
    const projection = tree.find(
      (expression): expression is Extract<Hir.Expression, { readonly _tag: 'ValueBorrow' }> =>
        expression._tag === 'ValueBorrow',
    )
    assert.isDefined(
      projection,
      Hir.encode(
        Object.freeze({
          _tag: 'HirModule',
          module: 'debug',
          functions: Object.freeze([residual.function]),
        }),
      ),
    )
    assert.strictEqual(projection?.access, 'Shared')
    assert.strictEqual(projection?.selectors.length, 1)
    assert.strictEqual(projection?.selectors.at(0)?._tag, 'Field')
    const encoded = Hir.encode(
      Object.freeze({
        _tag: 'HirModule',
        module: 'reference-projection/static-descriptor',
        functions: Object.freeze([residual.function]),
      }),
    )
    assert.isFalse(encoded.includes('Intrinsic.borrowField'))
    assert.isFalse(encoded.includes('Intrinsic.Field'))
    assert.deepEqual(
      Hir.verify(
        Object.freeze({
          _tag: 'HirModule',
          module: 'reference-projection/static-descriptor',
          functions: Object.freeze([residual.function]),
        }),
      ),
      [],
    )

    const unauthorized = Residualization.residualize(
      Residualization.make(
        snapshot.target.target,
        snapshot.results,
        snapshot.resolution,
        snapshot.index,
      ),
      Object.freeze({
        declaration: function_.canonical.id,
        typeArguments: Object.freeze([]),
        evidence: Object.freeze([]),
        contractRow: Object.freeze([]),
        staticArguments: Object.freeze([
          Object.freeze({
            ...descriptor,
            authorization: Object.freeze({
              _tag: 'CanonicalDeclarationId' as const,
              module: 'reference-projection/unauthorized',
              name: 'forged',
            }),
          }),
        ]),
      }),
    )
    assert.strictEqual(unauthorized._tag, 'ResidualBody')
    if (unauthorized._tag === 'ResidualBody') {
      assert.include(
        unauthorized.diagnostics.map((diagnostic) => diagnostic.code),
        'SEM0028',
      )
    }
  }),
)

it.effect('rejects invalid mixed field-projection calling shapes before residual HIR', () =>
  Effect.gen(function* () {
    const wrongOwner = yield* Analysis.ofSource(
      'reference-projection/static-descriptor-owner',
      ascii(`struct Left { value: i32 }
struct Right { value: i32 }
fn invalid(
  owner: &Right,
  static field: Intrinsic.Field<Left, i32>,
) -> &i32 {
  return Intrinsic.borrowField<Left, i32>(owner, field)
}`),
    )
    assert.include(
      Analysis.diagnostics(wrongOwner).map((diagnostic) => diagnostic.code),
      'SEM0100',
    )

    const runtimeDescriptor = yield* Analysis.ofSource(
      'reference-projection/runtime-descriptor',
      ascii(`struct Box { value: i32 }
fn invalid(owner: &Box, field: Intrinsic.Field<Box, i32>) -> &i32 {
  return Intrinsic.borrowField<Box, i32>(owner, field)
}`),
    )
    assert.include(
      Analysis.diagnostics(runtimeDescriptor).map((diagnostic) => diagnostic.code),
      'SEM0176',
    )

    const owned = yield* Analysis.ofSource(
      'reference-projection/owned-descriptor-projection',
      ascii(`struct Box { value: i32 }
fn invalid(owner: Box, static field: Intrinsic.Field<Box, i32>) -> &i32 {
  return Intrinsic.borrowField<Box, i32>(owner, field)
}`),
    )
    assert.include(
      Analysis.diagnostics(owned).map((diagnostic) => diagnostic.code),
      'SEM0012',
    )

    const exclusive = yield* Analysis.ofSource(
      'reference-projection/exclusive-descriptor-projection',
      ascii(`struct Box { value: i32 }
fn invalid(owner: &mut Box, static field: Intrinsic.Field<Box, i32>) -> &i32 {
  return Intrinsic.borrowField<Box, i32>(owner, field)
}`),
    )
    assert.include(
      Analysis.diagnostics(exclusive).map((diagnostic) => diagnostic.code),
      'SEM0012',
    )

    const escaping = yield* Analysis.ofSource(
      'reference-projection/escaping-descriptor-projection',
      ascii(`struct Box { value: i32 }
fn invalid(
  source: &Box,
  static field: Intrinsic.Field<Box, i32>,
) -> &i32 {
  let local = Box { value: 0 }
  return Intrinsic.borrowField<Box, i32>(&local, field)
}`),
    )
    assert.include(
      Analysis.diagnostics(escaping).map((diagnostic) => diagnostic.code),
      'SEM0092',
    )
  }),
)

it.effect('rejects forged consuming reads and writes through shared references', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/forged-shared-mir',
      ascii(`fn read(value: &i32) -> i32 { return value.* }
pub fn main() -> i32 {
  let value = 42
  return read(&value)
}`),
      'wasm32-unknown-unknown',
    )
    const mir = Analysis.loweredMir(snapshot)
    const functionIndex = mir.functions.findIndex((fn) => fn.id.name === 'read')
    const fn = mir.functions.at(functionIndex)
    if (fn === undefined) throw new RangeError('expected read MIR function')
    const read = MirVerification.operations(fn).find((operation) => operation._tag === 'ReadPlace')
    if (read === undefined) throw new RangeError('expected shared referent read')
    const rootType = fn.localTypes.at(read.root.ordinal)
    if (rootType === undefined) throw new RangeError('expected shared reference root type')

    const consuming: Mir.MirFunction = Object.freeze({
      ...fn,
      regions: Object.freeze(
        fn.regions.map((region) =>
          region._tag === 'OperationRegion'
            ? Object.freeze({
                ...region,
                operations: Object.freeze(
                  region.operations.map((operation): Mir.Operation =>
                    operation === read ? Object.freeze({ ...read, consume: true }) : operation,
                  ),
                ),
              })
            : region,
        ),
      ),
    })
    const consumingModule: Mir.Module = Object.freeze({
      ...mir,
      functions: Object.freeze([
        ...mir.functions.slice(0, functionIndex),
        consuming,
        ...mir.functions.slice(functionIndex + 1),
      ]),
    })
    assert.include(
      MirVerification.verify(consumingModule).map((violation) => violation.rule),
      'InvalidAggregateOperation',
    )

    const sharedWrite: Mir.MirFunction = Object.freeze({
      ...fn,
      regions: Object.freeze(
        fn.regions.map((region) =>
          region._tag === 'OperationRegion'
            ? Object.freeze({
                ...region,
                operations: Object.freeze(
                  region.operations.flatMap((operation): ReadonlyArray<Mir.Operation> =>
                    operation === read
                      ? [
                          read,
                          Object.freeze({
                            _tag: 'CheckPlace',
                            root: read.root,
                            selectors: read.selectors,
                            type: read.type,
                            provenance: read.provenance,
                          }),
                          Object.freeze({
                            _tag: 'WritePlace',
                            root: read.root,
                            selectors: read.selectors,
                            source: read.destination,
                            rootType,
                            type: read.type,
                            mutable: true,
                            replacement: 'Copy',
                            commit: 'AfterCleanup',
                            provenance: read.provenance,
                          }),
                        ]
                      : [operation],
                  ),
                ),
              })
            : region,
        ),
      ),
    })
    const sharedWriteModule: Mir.Module = Object.freeze({
      ...mir,
      functions: Object.freeze([
        ...mir.functions.slice(0, functionIndex),
        sharedWrite,
        ...mir.functions.slice(functionIndex + 1),
      ]),
    })
    assert.include(
      MirVerification.verify(sharedWriteModule).map((violation) => violation.rule),
      'InvalidWrite',
    )
  }),
)

it.effect('keeps reference projection inside the borrow contract', () =>
  Effect.gen(function* () {
    // Writing through a shared reference is not a writable place.
    const shared = yield* Analysis.ofSourceRealized(
      'reference-projection/shared-write',
      ascii(`struct Counter { value: i32 }
fn bump(self: &Counter) -> i32 {
  self.value = 1
  return self.value
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(shared).map((diagnostic) => diagnostic.code),
      ['SEM0036'],
    )

    // Consuming a field through a reference stays a partial move.
    const stolen = yield* Analysis.ofSourceRealized(
      'reference-projection/steal',
      ascii(`struct Token { value: i32 }
struct Holder { token: Token }
fn steal(self: &mut Holder) -> Token {
  return move self.token
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(stolen).map((diagnostic) => diagnostic.code),
      ['OWN0002'],
    )
  }),
)
