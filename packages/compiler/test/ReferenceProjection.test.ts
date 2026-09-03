import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Residualization from '../src/Residualization.js'
import type * as StaticValue from '../src/StaticValue.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('reads a Copy scalar through an explicit referent projection', () =>
  Effect.gen(function* () {
    const source = `fn read(value: &i32) -> i32 { return value.* }
pub fn main() -> i32 {
  let value = 42
  return read(&value)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/scalar-referent',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const [projection] = Analysis.referentProjectionsOf(
      snapshot,
      'reference-projection/scalar-referent',
    )
    assert.strictEqual(projection?.state._tag, 'Resolved')
    assert.strictEqual(projection?.borrowAccess, 'Shared')
    assert.strictEqual(projection?.type._tag, 'Available')
    if (projection?.type._tag === 'Available') assert.strictEqual(projection.type.type, 'i32')
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('reads a zero-lane Copy referent without inventing runtime state', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/zero-lane-referent',
      ascii(`struct Empty {}
impl Copy for Empty {}
fn read(value: &Empty) -> Empty { return value.* }
pub fn main() -> i32 {
  let value = Empty {}
  let copied = read(&value)
  drop copied
  return 42
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('reads and writes a runtime-indexed element through an explicit referent', () =>
  Effect.gen(function* () {
    const source = `import silk.usize as usize
struct Buffer { values: [i32; 3] }
fn update(buffer: &mut Buffer, index: usize) -> i32 {
  buffer.*.values[index] = 42
  return buffer.*.values[index]
}
pub fn main() -> i32 {
  let mut buffer = Buffer { values: [1, 2, 3] }
  return update(&mut buffer, usize.ONE)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/runtime-indexed-referent',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

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

it.effect('reborrows value-reference parameters for nested calls and restores the parent', () =>
  Effect.gen(function* () {
    const source = `struct Box { value: i32 }
fn increment(box: &mut Box) -> () { box.value = box.value + 1 }
fn observe(box: &Box) -> i32 { return box.value }
fn read(value: &i32) -> i32 { return value.* }
fn forwarded(value: &i32) -> i32 { return read(&value.*) }
fn twice(box: &mut Box) -> i32 {
  increment(&mut box)
  increment(&mut box)
  return observe(&box) + forwarded(&box.value)
}
pub fn main() -> i32 {
  let mut box = Box { value: 20 }
  return twice(&mut box)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/value-reborrow',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.include(Hir.encode(Analysis.rootAnalysis(snapshot).hir), 'reborrow-value')
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 44n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 44)
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

it.effect('replaces an exclusive referent with exact-once cleanup', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/referent-replacement',
      ascii(`struct Token { value: i32 }
fn replace(token: &mut Token) -> i32 {
  token.* = Token { value: 42 }
  return token.*.value
}
pub fn main() -> i32 {
  let mut token = Token { value: 1 }
  return replace(&mut token)
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'Replacement').length, 1)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const shared = yield* Analysis.ofSourceRealized(
      'reference-projection/shared-referent-replacement',
      ascii(`struct Token { value: i32 }
fn invalid(token: &Token) -> () { token.* = Token { value: 1 } }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(shared).map((diagnostic) => diagnostic.code),
      ['SEM0036'],
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

it.effect('reads and writes fields through nominal references on both targets', () =>
  Effect.gen(function* () {
    const source = `struct Counter { value: i32 }

fn bump(self: &mut Counter) -> i32 {
  self.value = self.value + 1
  return self.value
}

fn peek(self: &Counter) -> i32 {
  return self.value
}

pub fn main() -> i32 {
  let mut counter = Counter { value: 40 }
  let bumped = bump(&mut counter)
  let again = bump(&mut counter)
  return again + peek(&counter) - again
}`
    for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown']) {
      const snapshot = yield* Analysis.ofSourceRealized(
        'reference-projection/counter',
        ascii(source),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], target)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', target)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, 42n, target)
    }
    const wasm = yield* Analysis.codegenWasm(
      yield* Analysis.ofSourceRealized(
        'reference-projection/counter',
        ascii(source),
        'wasm32-unknown-unknown',
      ),
      { mode: 'release' },
    )
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
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

it.effect('returns an exclusive nominal reference through a pipeline', () =>
  Effect.gen(function* () {
    const source = `struct Counter {
  value: i32
}

fn increment(counter: &mut Counter) -> &mut Counter {
  counter.value = counter.value + 1
  return move counter
}

pub fn main() -> i32 {
  let mut counter = Counter { value: 0 }
  let result = &mut counter |> increment
  return result.value
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/returned-pipeline',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 1n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 1)
  }),
)

it.effect('mutates an owned parameter transferred through a pipeline', () =>
  Effect.gen(function* () {
    const source = `struct Counter {
  value: i32
}

fn increment(mut counter: Counter) -> Counter {
  counter.value = counter.value + 1
  return move counter
}

pub fn main() -> i32 {
  let counter = Counter { value: 0 }
  let result = move counter |> increment
  return result.value
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/mutable-owned-parameter',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 1n)
  }),
)

it.effect(
  'keeps mutable owned parameters out of sections and generic specialization identity',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'reference-projection/mutable-owned-callable-identity',
        ascii(`struct Counter { value: i32 }
fn adjust<T>(mut value: T, delta: i32) -> T { return move value }
fn increment(mut counter: Counter, delta: i32) -> Counter {
  counter.value = counter.value + delta
  return move counter
}
pub fn main() -> i32 {
  let callback = increment(2)
  let first = Counter { value: 40 }
  let updated = callback(move first)
  let specialized = adjust<Counter>(move updated, 0)
  return specialized.value
}`),
        'wasm32-unknown-unknown',
      )

      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    }),
)

it.effect('cleans and replaces mutable owned parameter storage exactly once', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
fn replace(mut token: Token) -> Token {
  token = Token { value: 42 }
  return move token
}
pub fn main() -> i32 {
  let token = Token { value: 1 }
  let result = replace(move token)
  return result.value
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/mutable-owned-replacement',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'Replacement').length, 1)
  }),
)
