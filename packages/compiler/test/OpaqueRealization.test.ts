import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as OpaqueRealization from '../src/OpaqueRealization.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const snapshot = (module: string, source: string) =>
  Analysis.ofSource(module, encoder.encode(source))

const snapshotWithImports = (
  root: string,
  source: string,
  imports: Readonly<Record<string, string>>,
) =>
  Analysis.make({ root: SourceFile.make(root, encoder.encode(source)) }).pipe(
    Effect.provide(
      SourceResolver.memory(
        new Map(Object.entries(imports).map(([module, text]) => [module, encoder.encode(text)])),
      ),
    ),
  )

const opaqueArgument = (
  self: Analysis.FrontendSnapshot,
  module: string,
  name: string,
): Type.OpaqueRepresentationArgument => {
  const declaration = self.index.modules
    .find((candidate) => candidate.module === module)
    ?.declarations.find(
      (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === name,
    )
  const type = declaration?.returnType
  if (type?._tag !== 'Resolved') return assert.fail(`missing opaque result ${module}.${name}`)
  return (
    Type.opaqueRepresentationArguments(type.type).at(0) ??
    assert.fail(`missing opaque family ${module}.${name}`)
  )
}

it.effect('defines a stable private realization for one capturing opaque family', () =>
  Effect.gen(function* () {
    const module = 'opaque/realization'
    const self = yield* snapshot(
      module,
      `fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value) }`,
    )
    assert.deepEqual(
      self.diagnostics.map((diagnostic) => diagnostic.code),
      [],
    )
    const argument = opaqueArgument(self, module, 'make')
    const definition = OpaqueRealization.catalogOf(self).definitions.get(
      Type.opaqueFamilyKey(argument.family),
    )
    assert.isDefined(definition)
    assert.strictEqual(definition?.captures.length, 1)
    assert.strictEqual(definition?.captures.at(0)?.type, 'i32')
    assert.strictEqual(definition?.access, 'Shared')
    assert.strictEqual(definition?.cleanup, 'Trivial')
    assert.strictEqual(definition?.suspendable, false)
    assert.include(definition?.targetFingerprint ?? '', 'exact-representation:')
  }),
)

it.effect('keeps family identity stable across value edits and distinct across producers', () =>
  Effect.gen(function* () {
    const module = 'opaque/families'
    const before = yield* snapshot(
      module,
      `fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn first() -> some<F: fn(i32) -> i32> F { return add(1) }
pub fn second() -> some<G: fn(i32) -> i32> G { return add(1) }`,
    )
    const after = yield* snapshot(
      module,
      `fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn first() -> some<F: fn(i32) -> i32> F { return add(2) }
pub fn second() -> some<G: fn(i32) -> i32> G { return add(1) }`,
    )
    const beforeFirst = opaqueArgument(before, module, 'first')
    const afterFirst = opaqueArgument(after, module, 'first')
    const beforeSecond = opaqueArgument(before, module, 'second')
    assert.strictEqual(Type.genericArgumentKey(beforeFirst), Type.genericArgumentKey(afterFirst))
    assert.notStrictEqual(
      Type.opaqueFamilyKey(beforeFirst.family),
      Type.opaqueFamilyKey(beforeSecond.family),
    )
    const firstBefore = OpaqueRealization.catalogOf(before).definitions.get(
      Type.opaqueFamilyKey(beforeFirst.family),
    )
    const firstAfter = OpaqueRealization.catalogOf(after).definitions.get(
      Type.opaqueFamilyKey(afterFirst.family),
    )
    assert.strictEqual(firstBefore?.targetFingerprint, firstAfter?.targetFingerprint)
    assert.strictEqual(firstBefore?.layoutFingerprint, firstAfter?.layoutFingerprint)
    assert.notStrictEqual(firstBefore?.bodyFingerprint, firstAfter?.bodyFingerprint)
  }),
)

it.effect('specializes a generic opaque family over every enclosing argument', () =>
  Effect.gen(function* () {
    const module = 'opaque/generic'
    const self = yield* snapshot(
      module,
      `fn keep<T>(value: T, enabled: bool) -> T { return move value }
pub fn make<T>(enabled: bool) -> some<F: fn(T) -> T> F { return keep<T>(enabled) }`,
    )
    const open = opaqueArgument(self, module, 'make')
    const i32 = Type.opaqueRepresentationArgument(open.family, open.contract, ['i32'])
    const boolean = Type.opaqueRepresentationArgument(open.family, open.contract, ['bool'])
    assert.notStrictEqual(Type.genericArgumentKey(i32), Type.genericArgumentKey(boolean))
    assert.strictEqual(Type.genericArgumentKey(i32), Type.genericArgumentKey(i32))
  }),
)

it.effect('unifies every reachable nested return and reachable match arm', () =>
  Effect.gen(function* () {
    const module = 'opaque/reachable'
    const self = yield* snapshot(
      module,
      `struct Left {}
struct Right {}
fn identity(value: i32) -> i32 { return value }
pub fn make(choice: Left | Right, flag: bool) -> some<F: fn(i32) -> i32> F {
  if flag { unsafe { return identity } }
  while flag { return identity }
  return match move choice {
    Left {} => identity
    Right {} => identity
  }
}`,
    )
    assert.notInclude(
      self.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0113',
    )
    const argument = opaqueArgument(self, module, 'make')
    assert.strictEqual(
      OpaqueRealization.catalogOf(self).definitions.has(Type.opaqueFamilyKey(argument.family)),
      true,
    )
  }),
)

it.effect('rejects divergent reachable opaque returns', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'opaque/divergent',
      `fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }
pub fn make(flag: bool) -> some<F: fn(i32) -> i32> F {
  if flag { return decimal }
  return hexadecimal
}`,
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0113')
    assert.strictEqual(diagnostic?.reason._tag, 'DivergentOpaqueRealization')
  }),
)

it.effect('accepts recursion after local evidence and rejects realization-only recursion', () =>
  Effect.gen(function* () {
    const accepted = yield* snapshot(
      'opaque/recursive-base',
      `fn identity(value: i32) -> i32 { return value }
pub fn make(flag: bool) -> some<F: fn(i32) -> i32> F {
  if flag { return identity }
  return make(true)
}`,
    )
    assert.notInclude(
      accepted.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0114',
    )

    const rejected = yield* snapshot(
      'opaque/recursive-cycle',
      `pub fn make(flag: bool) -> some<F: fn(i32) -> i32> F { return make(flag) }`,
    )
    const diagnostic = rejected.diagnostics.find((candidate) => candidate.code === 'SEM0114')
    assert.strictEqual(diagnostic?.reason._tag, 'OpaqueRealizationCycle')
  }),
)

it.effect('rejects an inline capture of the family being realized', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'opaque/inline-cycle',
      `fn apply<F: fn(i32) -> i32>(value: i32, parser: F) -> i32 { return parser(value) }
pub fn make(flag: bool) -> some<F: fn(i32) -> i32> F {
  let previous = make(flag)
  return apply(move previous)
}`,
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0115')
    assert.strictEqual(diagnostic?.reason._tag, 'InlineOpaqueLayoutCycle')
  }),
)

it.effect('keeps cross-module realization details compiler-private and specializable', () =>
  Effect.gen(function* () {
    const library = 'opaque/library'
    const self = yield* snapshotWithImports(
      'opaque/application',
      `import opaque.library { make }
pub fn use() -> i32 { let parser = make<i32>(true) return parser(1) }`,
      {
        [library]: `fn keep<T>(value: T, enabled: bool) -> T { return move value }
pub fn make<T>(enabled: bool) -> some<F: fn(T) -> T> F { return keep<T>(enabled) }`,
      },
    )
    const open = opaqueArgument(self, library, 'make')
    const family = Type.opaqueFamilyKey(open.family)
    const privateDefinition = OpaqueRealization.catalogOf(self).definitions.get(family)
    assert.isDefined(privateDefinition)
    const publicSurface = self.surfaces.get(library)?.canonical ?? assert.fail('missing surface')
    assert.notInclude(publicSurface, 'exact-representation:')
    assert.notInclude(publicSurface, 'OpaqueCapture')
    const specialized = Type.opaqueRepresentationArgument(open.family, open.contract, ['i32'])
    const definition = OpaqueRealization.definitionOf(
      OpaqueRealization.catalogOf(self),
      specialized,
    )
    assert.isDefined(definition)
    assert.deepEqual(definition?.instance.arguments, ['i32'])
    assert.strictEqual(definition?.family.producer.module, library)
  }),
)

it.effect('keeps opaque identity out of runtime facts', () =>
  Effect.gen(function* () {
    const source = `fn add(left: i32, right: i32) -> i32 { return left + right }
fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value) }
pub fn main() -> i32 { let parser = make(40) return parser(2) }`
    const self = yield* Analysis.ofSourceRealized(
      'opaque/runtime-fence',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      self.diagnostics.map((diagnostic) => diagnostic.code),
      [],
    )
    assert.strictEqual(self.mir._tag, 'Available')
    if (self.mir._tag !== 'Available') return
    const encoded = Mir.encode(self.mir.value)
    assert.notInclude(encoded, 'OpaqueFamilyKey')
    assert.notInclude(encoded, 'OpaqueRepresentationArgument')
    assert.notInclude(encoded, 'Existential')
    assert.notInclude(encoded, 'CallIndirect')
    assert.notInclude(encoded, 'unavailable body')
    assert.notInclude(encoded, 'unavailable contract type')
    const operations = self.mir.value.functions.flatMap(Mir.operations)
    assert.strictEqual(
      operations.some((operation) => operation._tag === 'Allocate'),
      false,
    )
    assert.strictEqual(
      self.mir.value.functions.every((fn) =>
        Mir.operations(fn).every((operation) => {
          if (operation._tag !== 'ApplyCallable') return true
          if (operation.target !== undefined) return true
          if (operation.callable === undefined) return false
          return fn.localTypes.at(operation.callable.ordinal)?._tag === 'CallableValue'
        }),
      ),
      true,
    )
  }),
)

it.effect('lowers an opaque Effect through its private static runner', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'opaque/effect-runtime-fence',
      encoder.encode(`fn make(value: i32) -> some<F: Effect<i32>> F {
  return effect { return value }
}
pub fn main() -> i32 { return run make(42) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      self.diagnostics.map((diagnostic) => diagnostic.code),
      [],
    )
    assert.strictEqual(self.mir._tag, 'Available')
    if (self.mir._tag !== 'Available') return
    const encoded = Mir.encode(self.mir.value)
    assert.notInclude(encoded, 'unavailable body')
    assert.notInclude(encoded, 'unavailable contract type')
    assert.notInclude(encoded, 'OpaqueRepresentationArgument')
    assert.strictEqual(
      self.mir.value.functions
        .flatMap(Mir.operations)
        .some((operation) => operation._tag === 'Allocate'),
      false,
    )
  }),
)
