import * as ToolchainIntegrity from '../src/ToolchainIntegrity.js'
import * as Schema from 'effect/Schema'
import { readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as NodeServices from '@effect/platform-node/NodeServices'
import * as DataLayout from '@silklang/llvm/DataLayout'
import * as ArtifactKind from '../src/ArtifactKind.js'
import * as CompilationProfile from '../src/CompilationProfile.js'
import * as ConfigurationOrigin from '../src/ConfigurationOrigin.js'
import * as ConfigurationValue from '../src/ConfigurationValue.js'
import * as HeapObservation from '../src/HeapObservation.js'
import * as NativeLinkInput from '../src/NativeLinkInput.js'
import * as Target from '../src/Target.js'
import { unreachable } from './support/raise.js'

it('keeps the compiler root import graph free of Node built-ins', () => {
  const sourceRoot = resolve(dirname(fileURLToPath(import.meta.url)), '../src')
  const pending = [resolve(sourceRoot, 'index.ts')]
  const visited = new Set<string>()
  while (pending.length > 0) {
    const file = pending.pop()
    if (file === undefined || visited.has(file)) continue
    visited.add(file)
    const source = readFileSync(file, 'utf8')
    assert.notMatch(source, /from ['"]node:/, file)
    for (const match of source.matchAll(/from ['"](\.\.?\/[^'"]+)\.js['"]/g)) {
      const relative = match[1]
      if (relative !== undefined) pending.push(resolve(dirname(file), `${relative}.ts`))
    }
  }
  assert.isAbove(visited.size, 1)
})

it.effect('provides browser heap observation only when the browser layer is explicit', () =>
  Effect.gen(function* () {
    const observation = yield* HeapObservation.HeapObservation
    assert.strictEqual(observation.heapBytes(), 0)
  }).pipe(Effect.provide(HeapObservation.layerBrowser)),
)

it('defines the four canonical profiles in deterministic order', () => {
  assert.deepEqual(
    Target.all.map((target) => target.id),
    [
      'aarch64-apple-darwin',
      'aarch64-unknown-linux-gnu',
      'wasm32-unknown-unknown',
      'x86_64-unknown-linux-gnu',
    ],
  )
  assert.deepEqual(
    Target.native.map((target) => target.id),
    ['aarch64-apple-darwin', 'aarch64-unknown-linux-gnu', 'x86_64-unknown-linux-gnu'],
  )
  assert.strictEqual(Target.wasm32UnknownUnknown.pointerSize, 4)
  assert.strictEqual(
    Target.native.every((target) => target.pointerSize === 8),
    true,
  )
})

it.effect('resolves every exact canonical identity', () =>
  Effect.gen(function* () {
    for (const target of Target.all) {
      assert.strictEqual(yield* Target.resolve(target.id), target)
      assert.strictEqual(Target.isCanonical(target), true)
    }
  }),
)

it.effect('resolves each supported native host', () =>
  Effect.gen(function* () {
    assert.strictEqual((yield* Target.fromHost('darwin', 'arm64')).id, 'aarch64-apple-darwin')
    assert.strictEqual((yield* Target.fromHost('linux', 'x64')).id, 'x86_64-unknown-linux-gnu')
    assert.strictEqual((yield* Target.fromHost('linux', 'arm64')).id, 'aarch64-unknown-linux-gnu')
  }),
)

it.effect('rejects unsupported requests and hosts as typed errors', () =>
  Effect.gen(function* () {
    const target = yield* Effect.flip(Target.resolve('mips-unknown-linux-gnu'))
    const host = yield* Effect.flip(Target.fromHost('win32', 'x64'))
    assert.strictEqual(target._tag, 'TargetError')
    assert.strictEqual(target.operation, 'Target.resolve')
    assert.strictEqual(host.operation, 'Target.host')
  }),
)

it('encodes target facts deterministically', () => {
  const target = Target.wasm32UnknownUnknown
  assert.strictEqual(Target.encode(target), Target.encode({ ...target }))
  assert.notStrictEqual(Target.encode(target), Target.encode(Target.aarch64AppleDarwin))
  assert.strictEqual(Target.isCanonical({ ...target, pointerAlignment: 8 }), false)
})

it.effect(
  'rejects incomplete and inconsistent machine descriptions before reading their facts',
  () =>
    Effect.gen(function* () {
      const target = Target.aarch64AppleDarwin
      for (const description of [
        { id: target.id },
        { ...target, primitives: { ...target.primitives, i64: { size: 8, alignment: 4 } } },
        { ...target, operatingSystem: 'linux' },
      ]) {
        const error = yield* Effect.flip(Target.validateDescription(description))
        assert.strictEqual(error.operation, 'Target.validateDescription')
      }
      assert.strictEqual(yield* Target.validateDescription({ ...target }), target)
    }),
)

it.effect('agrees with pinned LLVM and independently compiled C/object primitive witnesses', () =>
  Effect.gen(function* () {
    const fs = yield* FileSystem.FileSystem
    const root = resolve(dirname(fileURLToPath(import.meta.url)), 'fixtures/target-facts')
    const provenance = yield* Schema.decodeEffect(
      Schema.fromJsonString(
        Schema.Struct({
          sourceSha256: Schema.String,
        }),
      ),
    )(yield* fs.readFileString(resolve(root, 'provenance.json')))
    assert.strictEqual(
      ToolchainIntegrity.contentDigest(yield* fs.readFile(resolve(root, 'primitives.c'))),
      provenance.sourceSha256,
    )
    for (const target of Target.all) {
      const ir = yield* fs.readFileString(resolve(root, `${target.id}.ll`))
      const object = yield* fs.readFileString(resolve(root, `${target.id}.object.txt`))
      const layoutText =
        /target datalayout = "([^"]+)"/.exec(ir)?.[1] ?? unreachable('expected LLVM layout')
      // LLVM 22.1.8 LangRef defaults: AS0 pointers are 64/64; f32 and f64 align naturally.
      // These are LLVM layout defaults, not facts inferred from this test process host.
      const layout = yield* DataLayout.parse(layoutText)
      assert.strictEqual(layout.endian, target.endianness)
      assert.strictEqual(DataLayout.pointerSpec(layout)?.bitWidth ?? 64, target.pointerSize * 8)
      assert.strictEqual(
        DataLayout.pointerSpec(layout)?.abiAlignment.byteUnits ?? 8n,
        BigInt(target.pointerAlignment),
      )
      assert.strictEqual(layout.stackAlignment.byteUnits, BigInt(target.stackAlignment))
      for (const [bits, fact] of [
        [8, target.primitives.i8],
        [16, target.primitives.i16],
        [32, target.primitives.i32],
        [64, target.primitives.i64],
      ] as const) {
        assert.strictEqual(
          DataLayout.effectiveIntegerSpec(layout, bits).abiAlignment.byteUnits,
          BigInt(fact.alignment),
        )
      }
      for (const [bits, fact] of [
        [32, target.primitives.f32],
        [64, target.primitives.f64],
      ] as const)
        assert.strictEqual(
          DataLayout.floatSpec(layout, bits)?.abiAlignment.byteUnits ?? BigInt(bits / 8),
          BigInt(fact.alignment),
        )
      const witness =
        ir.split('\n').find((line) => line.startsWith('@silk_primitive_facts')) ??
        unreachable('expected C witness')
      const observed = [...witness.matchAll(/i32 (\d+)/g)].map((match) => Number(match[1]))
      assert.deepEqual(
        observed,
        [
          target.primitives.bool,
          target.primitives.cBool,
          target.primitives.i8,
          target.primitives.i16,
          target.primitives.i32,
          target.primitives.i64,
          target.primitives.f32,
          target.primitives.f64,
          { size: target.pointerSize, alignment: target.pointerAlignment },
          target.primitives.cLong,
        ].flatMap((fact) => [fact.size, fact.alignment]),
      )
      assert.include(object, `Arch: ${target.architecture}`)
      assert.include(object, 'silk_primitive_facts')
    }
  }).pipe(Effect.provide(NodeServices.layer)),
)

const profileOrigin = ConfigurationOrigin.literal('test profile')

it.effect(
  'canonicalizes independently constructed logical choices and retains no mutable inputs',
  () =>
    Effect.gen(function* () {
      const features = ['avx2', 'aes', 'avx2']
      const first = yield* CompilationProfile.normalize({
        target: Target.x8664UnknownLinuxGnu.id,
        cpu: { features },
        deployment: '6.1',
      })
      const second = yield* CompilationProfile.normalize({
        target: Target.x8664UnknownLinuxGnu.id,
        cpu: { features: ['sse2', 'aes', 'avx', 'avx2'] },
        deployment: '6.1.0',
      })
      assert.strictEqual(first.identity, second.identity)
      features.push('invalid')
      assert.deepEqual(first.cpu.features, ['aes', 'avx', 'avx2', 'sse2'])
      const identity = { package: 'example@1.0.0', module: 'config', parameter: 'enabled' }
      const disabled = yield* CompilationProfile.publish(first, [
        {
          ...identity,
          type: 'bool',
          value: { kind: 'boolean', value: false },
          origin: profileOrigin,
        },
      ])
      const enabled = yield* CompilationProfile.publish(first, [
        {
          ...identity,
          type: 'bool',
          value: { kind: 'boolean', value: true },
          origin: profileOrigin,
        },
      ])
      const equal = yield* CompilationProfile.publish(second, [
        {
          ...identity,
          type: 'bool',
          value: { kind: 'boolean', value: false },
          origin: ConfigurationOrigin.literal('/different/physical/manifest'),
        },
      ])
      assert.strictEqual(disabled.identity, equal.identity)
      assert.notStrictEqual(disabled.identity, enabled.identity)
      assert.notStrictEqual(first.identity, disabled.identity)
      assert.deepEqual(CompilationProfile.parameter(disabled, identity)?.value, {
        kind: 'boolean',
        value: false,
      })
      assert.strictEqual(Object.isFrozen(disabled.parameters), true)
      assert.strictEqual(Object.isFrozen(disabled.parameters[0]?.value), true)
      assert.notInclude(disabled.identity, profileOrigin.source)
    }),
)

it.effect('rejects unsupported combinations and physical or secret profile inputs', () =>
  Effect.gen(function* () {
    for (const input of [
      { target: Target.aarch64AppleDarwin.id, libc: 'gnu' },
      { target: Target.wasm32UnknownUnknown.id, relocation: 'pic' },
      { target: Target.x8664UnknownLinuxGnu.id, cpu: { features: ['neon'] } },
      {
        target: Target.x8664UnknownLinuxGnu.id,
        sanitizers: ['thread', 'address'],
        threading: 'multi',
      },
    ])
      assert.strictEqual(
        (yield* Effect.flip(CompilationProfile.decode(input))).code,
        'UnsupportedCombination',
      )
    assert.strictEqual(
      (yield* Effect.flip(
        CompilationProfile.decode({
          target: Target.aarch64AppleDarwin.id,
          sysroot: '/sdk',
        }),
      )).code,
      'InvalidInput',
    )
    const secret = yield* Effect.flip(
      CompilationProfile.decode('never expose me', {
        source: 'secret source',
        provenance: 'secret',
      }),
    )
    assert.strictEqual(secret.code, 'ForbiddenProvenance')
    assert.notInclude(
      yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))(secret),
      'never expose me',
    )
  }),
)

it.effect(
  'decodes all serializable kinds canonically without record-key or integer precision loss',
  () =>
    Effect.gen(function* () {
      const source = {
        kind: 'record',
        fields: Object.fromEntries([
          ['__proto__', { kind: 'integer', value: '18446744073709551615' }],
          [
            'choice',
            { kind: 'some', value: { kind: 'enum', type: 'p@1.0.0/config/Mode', member: 'Fast' } },
          ],
          [
            'items',
            {
              kind: 'array',
              values: [
                { kind: 'none' },
                { kind: 'boolean', value: true },
                { kind: 'string', value: '🍃' },
              ],
            },
          ],
        ]),
      }
      const value = yield* ConfigurationValue.decode(source, profileOrigin)
      const reordered = yield* ConfigurationValue.decode(
        { kind: 'record', fields: Object.fromEntries(Object.entries(source.fields).reverse()) },
        profileOrigin,
      )
      assert.strictEqual(ConfigurationValue.encode(value), ConfigurationValue.encode(reordered))
      assert.include(ConfigurationValue.encode(value), '18446744073709551615')
      assert.strictEqual(
        Object.hasOwn(value.kind === 'record' ? value.fields : {}, '__proto__'),
        true,
      )
      for (const invalid of [
        { kind: 'integer', value: 42 },
        { kind: 'integer', value: '01' },
        { kind: 'string', value: '\ud800' },
        { kind: 'none', callback: () => 1 },
      ])
        assert.strictEqual(
          (yield* Effect.flip(ConfigurationValue.decode(invalid, profileOrigin))).code,
          'InvalidInput',
        )
      const cycle: { kind: 'some'; value?: unknown } = { kind: 'some' }
      cycle.value = cycle
      assert.strictEqual(
        (yield* Effect.flip(ConfigurationValue.decode(cycle, profileOrigin))).code,
        'InvalidInput',
      )
      const translated = yield* ConfigurationValue.decode(
        { kind: 'boolean', value: true },
        {
          source: 'build tool',
          provenance: 'translated-public',
          translator: 'enable-feature-v1',
        },
      )
      assert.deepEqual(translated, { kind: 'boolean', value: true })
    }),
)

it('owns artifact compatibility and target-conventional filenames', () => {
  assert.strictEqual(ArtifactKind.fromManifest('executable'), 'NativeExecutable')
  assert.strictEqual(ArtifactKind.fromManifest('shared-library'), 'NativeSharedLibrary')
  assert.strictEqual(ArtifactKind.fromManifest('static-library'), 'NativeStaticLibrary')
  assert.strictEqual(ArtifactKind.fromManifest('shared'), undefined)
  assert.strictEqual(
    ArtifactKind.fileName('NativeSharedLibrary', 'math', Target.aarch64AppleDarwin),
    'libmath.dylib',
  )
  assert.strictEqual(
    ArtifactKind.fileName('NativeSharedLibrary', 'math', Target.x8664UnknownLinuxGnu),
    'libmath.so',
  )
  assert.strictEqual(
    ArtifactKind.fileName('NativeStaticLibrary', 'math', Target.aarch64UnknownLinuxGnu),
    'libmath.a',
  )
  assert.strictEqual(
    ArtifactKind.supports('NativeStaticLibrary', Target.wasm32UnknownUnknown),
    false,
  )
})

it('keeps native link inputs structured, immutable, ordered, and injectively encoded', () => {
  const inputs = Object.freeze([
    NativeLinkInput.searchPath('/sdk/lib'),
    NativeLinkInput.library('m', 'Dynamic'),
    NativeLinkInput.object('/tmp/a.o'),
    NativeLinkInput.staticArchive('/tmp/libb.a'),
    NativeLinkInput.framework('CoreFoundation'),
  ])
  assert.deepStrictEqual(
    inputs.map((input) => input._tag),
    ['SearchPath', 'Library', 'Object', 'StaticArchive', 'Framework'],
  )
  assert.deepStrictEqual(inputs.map(NativeLinkInput.path), [
    undefined,
    undefined,
    '/tmp/a.o',
    '/tmp/libb.a',
    undefined,
  ])
  assert.strictEqual(new Set(inputs.map(NativeLinkInput.encode)).size, inputs.length)
  assert.strictEqual(Object.isFrozen(inputs[0]), true)
})
