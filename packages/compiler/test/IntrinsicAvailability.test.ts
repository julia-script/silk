import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CAbi from '../src/CAbi.js'
import type * as Diagnostic from '../src/Diagnostic.js'
import * as ForeignAvailability from '../src/ForeignAvailability.js'
import * as ForeignPlanning from '../src/ForeignPlanning.js'
import * as Instances from '../src/Instances.js'
import * as Mir from '../src/Mir.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as Target from '../src/Target.js'
import * as ToolchainIntegrity from '../src/ToolchainIntegrity.js'
import * as Type from '../src/Type.js'
import * as MirSamples from './support/mirSamples.js'
import { unreachable } from './support/raise.js'

const encoder = new TextEncoder()
const source = `fn nativeWrapper() -> i32 { return Intrinsic.i32Add(20, 22) }
pub fn main() -> i32 { return 0 }`

const snapshot = (text: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('availability/main', encoder.encode(text), target)

it.effect('retains exact canonical identities and call provenance only when reachable', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source.replace('return 0', 'return nativeWrapper()'))
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      self.instances.intrinsics.map((call) => ({
        operation: Intrinsic.operationText(call.operation),
        sourceId: call.span.sourceId,
      })),
      [{ operation: 'Intrinsic.i32Add', sourceId: 'availability/main' }],
    )
  }),
)

it.effect('does not grant compiler availability privilege to an ordinary declaration name', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn i32Add(left: i32, right: i32) -> i32 {
  return left
}
pub fn main() -> i32 { return i32Add(20, 22) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.isFalse(
      self.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.i32Add',
      ),
    )
  }),
)

it.effect('does not grant enum projection privilege to an ordinary value declaration', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn value(input: i32) -> i32 { return input }
pub fn main() -> i32 { return value(42) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.isFalse(
      self.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.enumValue',
      ),
    )
  }),
)

it('requires normalized target metadata in every sealed inventory entry', () => {
  assert.deepEqual(
    Intrinsic.runtimeTargets,
    Target.all.map((target) => target.id),
  )
  assert.isTrue(
    Intrinsic.inventory().every((entry) => {
      const expected =
        entry.phase === 'Runtime' ? Intrinsic.normalizeRuntimeTargets(entry.targets) : []
      return (
        JSON.stringify(entry.targets) === JSON.stringify(expected) &&
        (entry.phase !== 'Runtime' || entry.targets.length > 0)
      )
    }),
  )
})

it('rejects static-only intrinsic leakage at runtime availability and integrity seams', () => {
  const operation = Intrinsic.findOperation('Intrinsic', 'targetPointerBits')
  const span = SourceSpan.fromOffsets('availability/static-only', 0, 0)
  assert.isDefined(operation)
  assert.isDefined(span)
  if (operation === undefined || span === undefined) return
  const calls = [{ _tag: 'ReachableIntrinsicCall' as const, operation: operation.id, span }]
  for (const target of Target.all) {
    const availability = IntrinsicAvailability.select(calls, target)
    assert.strictEqual(availability._tag, 'Unavailable')
    if (availability._tag === 'Unavailable')
      assert.deepEqual(availability.operations, ['Intrinsic.targetPointerBits'])
    const integrity = ToolchainIntegrity.validateTarget(
      ToolchainIntegrity.installed(),
      target,
      calls,
      [],
    )
    assert.strictEqual(integrity._tag, 'UnsupportedTarget')
    if (integrity._tag === 'UnsupportedTarget')
      assert.deepEqual(integrity.operations, ['Intrinsic.targetPointerBits'])
  }
  assert.isFalse(
    ToolchainIntegrity.installed().components.some(
      (component) =>
        component.kind === 'RuntimeSupport' &&
        component.id.endsWith('/Intrinsic.targetPointerBits'),
    ),
  )
})

it.effect('does not admit targetPointerBits into runtime HIR', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'pub fn main() -> u32 { return Intrinsic.targetPointerBits() }',
      'wasm32-unknown-unknown',
    )
    assert.isAbove(Analysis.diagnostics(self).length, 0)
    assert.isFalse(
      self.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.targetPointerBits',
      ),
    )
  }),
)

it.effect('checks native-only intrinsics after reachability for LLVM-to-Wasm', () =>
  Effect.gen(function* () {
    const unused = yield* snapshot(
      `import silk.os_monotonic_clock { OsMonotonicClock }
pub fn main() -> i32 { return 42 }`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(unused), [])
    assert.strictEqual(
      ToolchainIntegrity.validateTarget(
        ToolchainIntegrity.installed(),
        Target.wasm32UnknownUnknown,
        unused.instances.intrinsics,
        unused.closure.sources.keys(),
      )._tag,
      'Matched',
    )
    const portable = yield* Analysis.codegen(unused, { mode: 'release' })
    assert.deepEqual(portable.nativeRuntimeSymbols, [])

    const reachable = yield* snapshot(
      `import silk.effect { Effect }
import silk.monotonic_clock { MonotonicClock }
import silk.os_monotonic_clock { OsMonotonicClock }
pub fn main() -> i32 {
  let mut provider = OsMonotonicClock.make()
  let resolution = run Effect.provideMut(MonotonicClock.getResolution(), &mut provider)
  if resolution > 0 { return 42 }
  return 0
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(reachable), [])
    const unavailable = yield* Effect.flip(Analysis.codegen(reachable, { mode: 'release' }))
    assert.strictEqual(unavailable._tag, 'CodegenUnavailable')
    if (unavailable._tag !== 'CodegenUnavailable') return
    const diagnostic = unavailable.diagnostics.find((candidate) => candidate.code === 'SEM0093')
    assert.strictEqual(diagnostic?.reason._tag, 'IntrinsicTargetUnavailable')
    if (diagnostic?.reason._tag === 'IntrinsicTargetUnavailable') {
      assert.strictEqual(diagnostic.reason.operation, 'Intrinsic.osMonotonicClockResolution')
      assert.strictEqual(diagnostic.reason.target, 'wasm32-unknown-unknown')
    }

    const standardOutput = yield* snapshot(
      `import silk.effect { Effect }
import silk.writer { Writer, WriterError }
effect fn writeMessage() -> () ! WriterError {
  let mut writer = Writer.stdoutWriterProvider()
  return run Writer.writeAll(b"Silk") |> Effect.provideMut<Writer>(&mut writer)
}
effect fn ignoreWriteFailure(error: WriterError) -> () { return () }
pub fn main() -> i32 {
  let completed = run Effect.catchAll(writeMessage(), ignoreWriteFailure)
  return 42
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(standardOutput), [])
    const outputUnavailable = yield* Effect.flip(
      Analysis.codegen(standardOutput, { mode: 'release' }),
    )
    assert.strictEqual(outputUnavailable._tag, 'CodegenUnavailable')
    if (outputUnavailable._tag === 'CodegenUnavailable')
      assert.isTrue(
        outputUnavailable.diagnostics.some(
          (candidate) =>
            candidate.reason._tag === 'IntrinsicTargetUnavailable' &&
            candidate.reason.operation === 'Intrinsic.standardStreamWrite' &&
            candidate.reason.target === 'wasm32-unknown-unknown',
        ),
      )
  }),
)

it.effect('validates only reachable runtime support and selected provider identities', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source.replace('return 0', 'return nativeWrapper()'))
    const installed = ToolchainIntegrity.installed()
    const withoutUnrelatedRuntime = ToolchainIntegrity.make(
      installed.components.filter(
        (component) => component.id !== 'runtime/aarch64-apple-darwin/Intrinsic.boolEquals',
      ),
    )
    assert.strictEqual(
      ToolchainIntegrity.validateTarget(
        withoutUnrelatedRuntime,
        Target.aarch64AppleDarwin,
        self.instances.intrinsics,
        [],
      )._tag,
      'Matched',
    )

    const withoutRequiredRuntime = ToolchainIntegrity.make(
      installed.components.filter(
        (component) => component.id !== 'runtime/aarch64-apple-darwin/Intrinsic.i32Add',
      ),
    )
    const missingRuntime = ToolchainIntegrity.validateTarget(
      withoutRequiredRuntime,
      Target.aarch64AppleDarwin,
      self.instances.intrinsics,
      [],
    )
    assert.strictEqual(missingRuntime._tag, 'Invalid')
    if (missingRuntime._tag === 'Invalid')
      assert.isTrue(
        missingRuntime.failures.some(
          (failure) =>
            failure.reason._tag === 'MissingComponent' &&
            failure.reason.id === 'runtime/aarch64-apple-darwin/Intrinsic.i32Add',
        ),
      )

    const withoutProvider = ToolchainIntegrity.make(
      installed.components.filter((component) => component.id !== 'provider/silk/os_filesystem'),
    )
    const osOperation = Intrinsic.findOperation('Intrinsic', 'osPathInspect')
    const osSpan = SourceSpan.fromOffsets('availability/os-provider', 0, 1)
    assert.isDefined(osOperation)
    assert.isDefined(osSpan)
    if (osOperation === undefined || osSpan === undefined) return
    const missingProvider = ToolchainIntegrity.validateTarget(
      withoutProvider,
      Target.aarch64AppleDarwin,
      [{ _tag: 'ReachableIntrinsicCall', operation: osOperation.id, span: osSpan }],
      ['silk/os_filesystem'],
    )
    assert.strictEqual(missingProvider._tag, 'Invalid')
  }),
)

const foreignEntry = (
  symbol: string,
  parameters: ReadonlyArray<CAbi.CAbiType>,
  sourceId: string,
  start: number,
): Instances.ForeignCall => {
  const span = SourceSpan.fromOffsets(sourceId, start, start + 3)
  if (span === undefined) throw new Error('expected a span')
  return Object.freeze({
    _tag: 'ReachableForeignCall',
    symbol,
    signature: Object.freeze({ parameters, result: Object.freeze({ _tag: 'Void' }) }),
    declaration: Object.freeze({ _tag: 'CanonicalDeclarationId', module: sourceId, name: symbol }),
    declarationSpan: span,
    callSpan: span,
  })
}

it('rejects foreign calls off native LLVM and conflicting signatures per pair', () => {
  const i32 = Object.freeze({
    _tag: 'Integer' as const,
    bits: 32 as const,
    signed: true,
    extension: 'None' as const,
  })
  const i64 = Object.freeze({
    _tag: 'Integer' as const,
    bits: 64 as const,
    signed: true,
    extension: 'None' as const,
  })
  const exit = foreignEntry('exit', [i32], 'availability/a', 40)
  const calls = [
    foreignEntry('abs', [i32], 'availability/a', 0),
    foreignEntry('abs', [i64], 'availability/b', 10),
    exit,
  ]
  const codes = (selected: Target.Target) =>
    ForeignAvailability.select(calls, selected).map((diagnostic) => [
      diagnostic.code,
      diagnostic.reason._tag === 'ForeignFunctionTargetUnavailable'
        ? `${diagnostic.reason.symbol}@${diagnostic.reason.surface}`
        : diagnostic.relatedSpans?.map((related) => related.span.sourceId).join(','),
    ])
  assert.deepEqual(codes(Target.aarch64AppleDarwin), [['SEM0192', 'availability/a']])
  assert.deepEqual(codes(Target.wasm32UnknownUnknown), [
    ['SEM0193', 'abs@wasm32-unknown-unknown'],
    ['SEM0193', 'exit@wasm32-unknown-unknown'],
    ['SEM0192', 'availability/a'],
  ])
  assert.deepEqual(ForeignAvailability.select([exit], Target.x8664UnknownLinuxGnu), [])
})

it.effect('classifies a reachable foreign call for the selected target', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`unsafe extern "C" fn count(value: usize) -> usize
fn size() -> usize { return unsafe count(1) }
pub fn main() -> i32 { let n = size()
  drop n
  return 0 }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      self.instances.foreignCalls.map((call) => ({
        symbol: call.symbol,
        signature: CAbi.signatureKey(call.signature),
        declaration: call.declaration.name,
        callSource: call.callSpan.sourceId,
      })),
      [
        {
          symbol: 'count',
          signature: '(u64)->u64',
          declaration: 'count',
          callSource: 'availability/main',
        },
      ],
    )
    const program = Analysis.loweredMir(self)
    assert.deepEqual(program.foreignCalls, self.instances.foreignCalls)
  }),
)

it.effect('keeps unreachable foreign declarations out of LLVM-to-Wasm planning', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      `unsafe extern "C" fn abs(value: i32) -> i32
unsafe extern "C" fn install(callback: extern "C" fn(i32) -> i32) -> ()
unsafe extern "C" static environment: *mut *mut u8 as "environ"
pub fn main() -> i32 { return 42 }`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(self.instances.foreignCalls, [])
    const program = Analysis.loweredMir(self)
    assert.deepEqual(program.foreignCalls, [])
    assert.deepEqual(program.foreignStatics, [])
    const artifact = yield* Analysis.codegen(self, { mode: 'release' })
    assert.deepEqual(artifact.foreignImports, [])
    assert.notInclude(artifact.ir, '@abs')
    assert.notInclude(artifact.ir, '@install')
    assert.notInclude(artifact.ir, '@environ')
  }),
)

const twoModules = (dependencyAbs: string) =>
  Analysis.makeRealized({
    root: SourceFile.make(
      'availability/foreign-root',
      encoder.encode(`import foreign_dep as Dep
unsafe extern "C" fn abs(value: i32) -> i32
pub fn main() -> i32 { return unsafe abs(-2) + Dep.viaDep() }`),
    ),
  }).pipe(
    Effect.provide(
      SourceResolver.memory(
        new Map([
          [
            'foreign_dep',
            encoder.encode(`${dependencyAbs}
pub fn viaDep() -> i32 { let wide = unsafe abs(-1)
  drop wide
  return 0 }`),
          ] as const,
        ]),
      ),
    ),
  )

it.effect('accepts agreeing redeclarations of one symbol across two modules', () =>
  Effect.gen(function* () {
    const self = yield* twoModules('unsafe extern "C" fn abs(value: i32) -> i32')
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      self.instances.foreignCalls.map((call) => [
        call.declaration.module,
        CAbi.signatureKey(call.signature),
      ]),
      [
        ['availability/foreign-root', '(i32)->i32'],
        ['foreign_dep', '(i32)->i32'],
      ],
    )
    if (self.target._tag !== 'Resolved') return assert.fail('expected a resolved target')
    assert.deepEqual(
      ForeignAvailability.select(self.instances.foreignCalls, self.target.target),
      [],
    )
    const artifact = yield* Analysis.codegen(self, { mode: 'release' })
    assert.deepEqual(artifact.foreignImports, [
      { symbol: 'abs', parameters: ['i32'], result: 'i32' },
    ])
  }),
)

it.effect('rejects a conflicting redeclaration relating both declarations', () =>
  Effect.gen(function* () {
    const self = yield* twoModules('unsafe extern "C" fn abs(value: i64) -> i64 as "abs"')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const failure = yield* Effect.flip(Analysis.codegen(self, { mode: 'release' }))
    assert.strictEqual(failure._tag, 'CodegenUnavailable')
    if (failure._tag !== 'CodegenUnavailable') return
    assert.deepEqual(
      failure.diagnostics.map((diagnostic) => [
        diagnostic.code,
        diagnostic.span.sourceId,
        diagnostic.relatedSpans?.map((related) => related.span.sourceId),
      ]),
      [['SEM0192', 'foreign_dep', ['availability/foreign-root']]],
    )
  }),
)

const exportSource = `export "C" fn silk_test_double_v1(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return 0 }`

const exportInventory = (self: Analysis.Snapshot) =>
  self.instances.foreignExports.map((record) => ({
    symbol: record.symbol,
    signature: CAbi.signatureKey(record.signature),
    key: Instances.keyText(record.key),
    declaration: record.declaration.name,
    declarationSource: record.declarationSpan.sourceId,
  }))

it.effect('seeds native discovery with an uncalled export and records it on MIR', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(exportSource)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      self.instances.instances.map((instance) => instance.key.declaration.name),
      ['main', 'silk_test_double_v1'],
    )
    assert.deepEqual(exportInventory(self), [
      {
        symbol: 'silk_test_double_v1',
        signature: '(i32)->i32',
        key: Instances.keyText(
          self.instances.instances.at(1)?.key ?? unreachable('expected the export instance'),
        ),
        declaration: 'silk_test_double_v1',
        declarationSource: 'availability/main',
      },
    ])
    assert.deepEqual(Analysis.loweredMir(self).foreignExports, self.instances.foreignExports)
    const again = yield* snapshot(exportSource)
    assert.deepEqual(
      again.instances.instances.map((instance) => Instances.keyText(instance.key)),
      self.instances.instances.map((instance) => Instances.keyText(instance.key)),
    )
    assert.deepEqual(exportInventory(again), exportInventory(self))
  }),
)

const exportModules = (root: string, dependency: string, main = 'Dep.viaDep()') =>
  Analysis.makeRealized({
    root: SourceFile.make(
      'availability/export-root',
      encoder.encode(`import export_dep as Dep
${root}
pub fn main() -> i32 { return ${main} }`),
    ),
  }).pipe(
    Effect.provide(
      SourceResolver.memory(
        new Map([
          [
            'export_dep',
            encoder.encode(`${dependency}
pub fn viaDep() -> i32 { return 0 }`),
          ] as const,
        ]),
      ),
    ),
  )

const planningFailure = (self: Analysis.Snapshot) =>
  Effect.map(Effect.flip(Analysis.codegen(self, { mode: 'release' })), (failure) => {
    assert.strictEqual(failure._tag, 'CodegenUnavailable')
    return failure._tag === 'CodegenUnavailable'
      ? failure.diagnostics.map((diagnostic) => [
          diagnostic.code,
          diagnostic.span.sourceId,
          diagnostic.relatedSpans?.map((related) => related.span.sourceId),
        ])
      : []
  })

it.effect('rejects two exports of one symbol relating both declarations', () =>
  Effect.gen(function* () {
    const self = yield* exportModules(
      'export "C" fn one(value: i32) -> i32 as "silk_test_v1" { return value }',
      'export "C" fn two(value: i32) -> i32 as "silk_test_v1" { return value }',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(yield* planningFailure(self), [
      ['SEM0192', 'export_dep', ['availability/export-root']],
    ])
  }),
)

it.effect('rejects an export whose symbol a reachable import claims', () =>
  Effect.gen(function* () {
    const self = yield* exportModules(
      'unsafe extern "C" fn abs(value: i32) -> i32',
      'export "C" fn abs(value: i32) -> i32 { return value }',
      'unsafe abs(-1) + Dep.viaDep()',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(yield* planningFailure(self), [
      ['SEM0192', 'export_dep', ['availability/export-root']],
    ])
  }),
)

it.effect('accepts distinct export symbols across modules in canonical order', () =>
  Effect.gen(function* () {
    const self = yield* exportModules(
      'export "C" fn silk_test_double_v1(value: i32) -> i32 { return value * 2 }',
      'export "C" fn silk_test_add_v1(left: i32, right: i32) -> i32 { return left + right }',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      exportInventory(self).map((record) => [record.symbol, record.signature]),
      [
        ['silk_test_double_v1', '(i32)->i32'],
        ['silk_test_add_v1', '(i32,i32)->i32'],
      ],
    )
    if (self.target._tag !== 'Resolved') return assert.fail('expected a resolved target')
    assert.deepEqual(ForeignPlanning.check(Analysis.loweredMir(self), self.target.target), [])
    const artifact = yield* Analysis.codegen(self, { mode: 'release' })
    assert.deepEqual(artifact.foreignExports, [
      { symbol: 'silk_test_add_v1', parameters: ['i32', 'i32'], result: 'i32' },
      { symbol: 'silk_test_double_v1', parameters: ['i32'], result: 'i32' },
    ])
  }),
)

it.effect('rejects an export whose body suspends and accepts a synchronous one', () =>
  Effect.gen(function* () {
    const suspending = yield* snapshot(`import silk.effect { Effect }
export "C" fn silk_test_wait_v1() -> i32 { return run Effect.suspend(effect { return 2 }) }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(Analysis.diagnostics(suspending), [])
    const failure = yield* Effect.flip(Analysis.codegen(suspending, { mode: 'release' }))
    assert.strictEqual(failure._tag, 'CodegenUnavailable')
    if (failure._tag === 'CodegenUnavailable')
      assert.deepEqual(
        failure.diagnostics.map((diagnostic) => [
          diagnostic.code,
          diagnostic.reason._tag,
          diagnostic.relatedSpans?.map((related) => related.label),
        ]),
        [['SEM0201', 'ExportSuspends', ['suspending call']]],
      )
    const synchronous = yield* snapshot(exportSource)
    if (synchronous.target._tag !== 'Resolved') return assert.fail('expected a resolved target')
    assert.deepEqual(
      ForeignPlanning.check(Analysis.loweredMir(synchronous), synchronous.target.target),
      [],
    )
  }),
)

it.effect('rejects a suspending export at its C callback conversion site', () =>
  Effect.gen(function* () {
    const callbackSource = `import silk.effect { Effect }
unsafe extern "C" fn install(callback: extern "C" fn() -> i32) -> ()
export "C" fn silk_test_wait_v1() -> i32 { return run Effect.suspend(effect { return 2 }) }
pub fn main() -> i32 { unsafe install(silk_test_wait_v1) return 0 }`
    const self = yield* snapshot(callbackSource)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const failure = yield* Effect.flip(Analysis.codegen(self, { mode: 'release' }))
    assert.strictEqual(failure._tag, 'CodegenUnavailable')
    if (failure._tag !== 'CodegenUnavailable') return
    assert.deepEqual(
      failure.diagnostics.map((diagnostic) => [diagnostic.code, diagnostic.reason._tag]),
      [
        ['SEM0201', 'ExportSuspends'],
        ['SEM0207', 'InvalidForeignCallback'],
      ],
    )
    const callback = failure.diagnostics.find((diagnostic) => diagnostic.code === 'SEM0207')
    assert.strictEqual(
      callback === undefined
        ? undefined
        : callbackSource.slice(callback.span.start, callback.span.end),
      'silk_test_wait_v1',
    )
  }),
)

it('plans exports over MIR: symbol map, non-native rejection, and suspension', () => {
  const sample = MirSamples.foreignCallSample(Target.aarch64AppleDarwin)
  const key = Mir.machineEntry(sample)
  const i32 = Object.freeze({
    _tag: 'Integer' as const,
    bits: 32 as const,
    signed: true,
    extension: 'None' as const,
  })
  const record = (symbol: string, sourceId: string, start: number): Instances.ForeignExport =>
    Object.freeze({
      _tag: 'ForeignExport',
      symbol,
      type: Type.foreignFunction(['i32'], 'i32'),
      signature: Object.freeze({ parameters: Object.freeze([i32]), result: i32 }),
      key,
      declaration: Object.freeze({
        _tag: 'CanonicalDeclarationId',
        module: sourceId,
        name: symbol,
      }),
      declarationSpan: foreignEntry(symbol, [], sourceId, start).declarationSpan,
    })
  const program: Mir.Module = Object.freeze({
    ...sample,
    foreignCalls: Object.freeze([foreignEntry('abs', [i32], 'planning/import', 0)]),
    foreignExports: Object.freeze([
      record('silk_test_v1', 'planning/a', 0),
      record('abs', 'planning/b', 0),
      record('silk_test_v1', 'planning/c', 0),
      record('silk_test_ok', 'planning/d', 0),
    ]),
  })
  const summarize = (diagnostics: ReadonlyArray<Diagnostic.Diagnostic>) =>
    diagnostics.map((diagnostic) => [
      diagnostic.code,
      diagnostic.span.sourceId,
      diagnostic.reason._tag === 'ForeignFunctionTargetUnavailable'
        ? diagnostic.reason.surface
        : diagnostic.relatedSpans?.map((related) => related.span.sourceId).join(','),
    ])
  assert.deepEqual(summarize(ForeignPlanning.check(program, Target.aarch64AppleDarwin)), [
    ['SEM0192', 'planning/b', 'planning/import'],
    ['SEM0192', 'planning/c', 'planning/a'],
  ])
  assert.deepEqual(
    summarize(
      ForeignPlanning.check(
        { ...program, foreignExports: program.foreignExports.slice(3) },
        Target.wasm32UnknownUnknown,
      ),
    ),
    [['SEM0193', 'planning/d', 'wasm32-unknown-unknown']],
  )
  const suspending: Mir.Module = {
    ...program,
    foreignExports: program.foreignExports.slice(3),
    functions: program.functions.map((fn) => ({
      ...fn,
      suspension: { classification: 'Suspendable' as const, regions: Object.freeze([]) },
    })),
  }
  assert.deepEqual(summarize(ForeignPlanning.check(suspending, Target.aarch64AppleDarwin)), [
    ['SEM0201', 'planning/d', undefined],
  ])
})
