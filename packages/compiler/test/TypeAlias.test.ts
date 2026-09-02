import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'
import * as Json from './support/Json.js'
import * as WasmMain from './support/WasmMain.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (text: string, target?: string) =>
  Analysis.makeRealized({
    root: SourceFile.make('root', ascii(text)),
    ...(target === undefined ? {} : { target }),
  }).pipe(Effect.provide(SourceResolver.memory(new Map())))

const analyzeModules = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): Effect.Effect<Analysis.Snapshot> => {
  const rootText = entries.find(([name]) => name === rootModule)?.[1]
  if (rootText === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  const imports = new Map(
    entries
      .filter(([name]) => name !== rootModule)
      .map(([name, text]) => [name, ascii(text)] as const),
  )
  return Analysis.makeRealized({ root: SourceFile.make(rootModule, ascii(rootText)) }).pipe(
    Effect.provide(SourceResolver.memory(imports)),
  )
}

const codes = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)

const messages = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.message)

const evaluated = (self: Analysis.Snapshot): bigint => {
  const outcome = Analysis.evaluate(self)
  assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome))
  if (outcome._tag !== 'Completed') throw new Error('unreachable')
  return outcome.result.value as bigint
}

const memberType = (
  self: Analysis.Snapshot,
  module: string,
  spelling: string,
): string | undefined => {
  const lookup = Analysis.memberByName(self, module, spelling)
  if (lookup._tag !== 'Resolved') return undefined
  const member = lookup.declaration
  if (member._tag === 'AliasDeclaration')
    return member.target._tag === 'Resolved' ? Type.encode(member.target.type) : undefined
  if (member._tag === 'StructDeclaration') {
    const field = member.fields.at(0)
    return field?.declaredType._tag === 'Resolved'
      ? Type.encode(field.declaredType.type)
      : undefined
  }
  if (member._tag === 'FunctionDeclaration') {
    const parameter = member.parameters.at(0)
    return parameter?.declaredType._tag === 'Resolved'
      ? Type.encode(parameter.declaredType.type)
      : undefined
  }
  return undefined
}

const failureMembers = (
  self: Analysis.Snapshot,
  module: string,
  spelling: string,
): ReadonlyArray<string> => {
  const lookup = Analysis.memberByName(self, module, spelling)
  if (lookup._tag !== 'Resolved' || lookup.declaration._tag !== 'FunctionDeclaration') return []
  return lookup.declaration.failureRow.failures.map(Type.encode)
}

it.effect('treats two scalar aliases as one interchangeable type', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`type Meters = i32
type Kilograms = i32
fn takeMeters(value: Meters) -> i32 { return value }
pub fn main() -> i32 {
  let weight: Kilograms = 7
  return takeMeters(weight)
}`)
    assert.deepEqual(codes(self), [])
    assert.strictEqual(memberType(self, 'root', 'takeMeters'), 'i32')
    assert.strictEqual(memberType(self, 'root', 'Meters'), 'i32')
    assert.strictEqual(evaluated(self), 7n)
  }),
)

it.effect('erases an alias of a generic application', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Point<T> { x: T y: T }
type PointI32 = Point<i32>
struct Holder { at: PointI32 }
pub fn main() -> i32 {
  let holder = Holder { at: Point { x: 1, y: 2 } }
  return holder.at.y
}`)
    assert.deepEqual(codes(self), [])
    assert.strictEqual(memberType(self, 'root', 'Holder'), memberType(self, 'root', 'PointI32'))
    assert.include(memberType(self, 'root', 'Holder') ?? '', 'Point<i32>')
    assert.strictEqual(evaluated(self), 2n)
  }),
)

it.effect('erases a union alias and injects a member at the return boundary', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Circle {}
struct Square {}
type Shape = Circle | Square
fn make(flag: bool) -> Shape {
  if flag { return Circle {} }
  return Square {}
}
fn describe(shape: Shape) -> i32 {
  return match move shape {
    Circle {} => 1
    Square {} => 2
  }
}
pub fn main() -> i32 { return describe(make(false)) }`)
    assert.deepEqual(codes(self), [])
    assert.strictEqual(memberType(self, 'root', 'Shape'), memberType(self, 'root', 'describe'))
    assert.strictEqual(evaluated(self), 2n)
  }),
)

it.effect('presents the erased target and not the alias name in diagnostics', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Circle {}
struct Square {}
struct Triangle {}
type Shape = Circle | Square
fn take(shape: Shape) -> i32 { return 0 }
pub fn main() -> i32 { return take(Triangle {}) }`)
    const mismatch = messages(self).find((message) => message.includes('Circle'))
    assert.notStrictEqual(mismatch, undefined, Json.stringify(messages(self)))
    assert.include(mismatch ?? '', 'Square')
    assert.notInclude(mismatch ?? '', 'Shape')
  }),
)

it.effect('resolves an alias declared after its use and through another alias', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`type Outer = Inner
type Inner = i32
fn pass(value: Outer) -> i32 { return value }
pub fn main() -> i32 { return pass(3) }`)
    assert.deepEqual(codes(self), [])
    assert.strictEqual(memberType(self, 'root', 'Outer'), 'i32')
    assert.strictEqual(memberType(self, 'root', 'Inner'), 'i32')
    assert.strictEqual(evaluated(self), 3n)
  }),
)

it.effect('rejects a two-alias cycle once per declaration and keeps other aliases available', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`type A = B
type B = A
type Fine = i32
fn use(value: A) -> i32 { return 0 }
pub fn main() -> i32 { return 0 }`)
    const cyclic = Analysis.diagnostics(self).filter(
      (diagnostic) => diagnostic.code === Diagnostic.cyclicTypeAliasCode,
    )
    assert.strictEqual(cyclic.length, 2, Json.stringify(messages(self)))
    assert.deepEqual(
      cyclic.map((diagnostic) => diagnostic.relatedSpans?.length ?? 0),
      [1, 1],
    )
    assert.strictEqual(memberType(self, 'root', 'A'), undefined)
    assert.strictEqual(memberType(self, 'root', 'B'), undefined)
    assert.strictEqual(memberType(self, 'root', 'Fine'), 'i32')
  }),
)

it.effect('rejects a self-referential alias through a union', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Leaf {}
type Tree = Leaf | Tree
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(
      codes(self).filter((code) => code === Diagnostic.cyclicTypeAliasCode),
      [Diagnostic.cyclicTypeAliasCode],
    )
    assert.strictEqual(memberType(self, 'root', 'Tree'), undefined)
  }),
)

it.effect('rejects an alias name that collides with another declaration', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Token {}
type Token = i32
pub fn main() -> i32 { return 0 }`)
    assert.include(codes(self), Diagnostic.duplicateDeclarationNameCode)
  }),
)

it.effect('rejects a parameterized alias without reporting its parameters as unknown', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Point<T> { x: T y: T }
type Pair<T> = Point<T>
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(self), [Diagnostic.typeAliasParametersCode])
    assert.strictEqual(memberType(self, 'root', 'Pair'), undefined)
  }),
)

it.effect('accepts an applied target and rejects further arguments on the alias', () =>
  Effect.gen(function* () {
    const accepted = yield* analyze(`struct Point<T> { x: T y: T }
type Pair = Point<i32>
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(accepted), [])
    assert.include(memberType(accepted, 'root', 'Pair') ?? '', 'Point<i32>')
    const rejected = yield* analyze(`struct Point<T> { x: T y: T }
type Pair = Point<i32>
fn use(value: Pair<bool>) -> i32 { return 0 }
pub fn main() -> i32 { return 0 }`)
    assert.include(codes(rejected), Diagnostic.typeArgumentArityCode)
  }),
)

it.effect('rejects a public alias that exposes a private type', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Hidden {}
pub type Leaked = Hidden
pub fn main() -> i32 { return 0 }`)
    assert.include(codes(self), Diagnostic.privateTypeExposureCode)
    assert.strictEqual(memberType(self, 'root', 'Leaked'), undefined)
  }),
)

it.effect('refuses an alias in value position', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`type Count = i32
pub fn main() -> i32 { return Count }`)
    assert.notDeepEqual(codes(self), [])
    assert.notInclude(codes(self), Diagnostic.cyclicTypeAliasCode)
  }),
)

it.effect('flattens a union alias into the declared failure row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(
      `import silk.effect as Effect
struct HttpError {}
struct JsonError {}
struct Timeout {}
type FetchError = HttpError | JsonError
effect fn fetch(flag: bool) -> i32 ! FetchError {
  if flag { fail HttpError {} }
  fail JsonError {}
}
effect fn wide(flag: bool) -> i32 ! FetchError | Timeout {
  if flag { fail Timeout {} }
  return run fetch(flag)
}
effect fn recoverHttp(problem: HttpError) -> i32 { return 1 }
effect fn recoverFetch(problem: HttpError | JsonError) -> i32 { return 2 }
effect fn residual(flag: bool) -> i32 ! JsonError {
  return run Effect.catch<HttpError>(fetch(flag), recoverHttp)
}
effect fn selected(flag: bool) -> i32 ! Timeout {
  return run Effect.catch<FetchError>(wide(flag), recoverFetch)
}
effect fn recoverJson(problem: JsonError) -> i32 { return 8 }
effect fn recoverTimeout(problem: Timeout) -> i32 { return 16 }
effect fn done(flag: bool) -> i32 {
  let first = run Effect.catch<JsonError>(residual(flag), recoverJson)
  let second = run Effect.catch<Timeout>(selected(flag), recoverTimeout)
  return first + second
}
pub fn main() -> i32 { return run done(true) }`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(codes(self), [])
    assert.strictEqual(failureMembers(self, 'root', 'fetch').length, 2)
    assert.strictEqual(failureMembers(self, 'root', 'wide').length, 3)
    assert.strictEqual(failureMembers(self, 'root', 'residual').length, 1)
    assert.strictEqual(failureMembers(self, 'root', 'selected').length, 1)
    assert.strictEqual(evaluated(self), 17n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.strictEqual(yield* WasmMain.invoke(wasm.bytes, 'TypeAlias.flattenedRowWasm'), 17)
  }),
)

it.effect('keeps a nominal union atomic inside a failure row spelled through an alias', () =>
  Effect.gen(function* () {
    const program = (row: string) => `import silk.effect as Effect
union HttpError { NotFound, Timeout }
struct JsonError {}
type FetchError = HttpError | JsonError
effect fn fetch() -> i32 ! ${row} { fail HttpError.NotFound }
effect fn recover(problem: HttpError) -> i32 { return 4 }
effect fn residual() -> i32 ! JsonError {
  return run Effect.catch<HttpError>(fetch(), recover)
}
effect fn recoverJson(problem: JsonError) -> i32 { return 9 }
effect fn done() -> i32 { return run Effect.catch<JsonError>(residual(), recoverJson) }
pub fn main() -> i32 { return run done() }`
    const aliased = yield* analyze(program('FetchError'))
    const direct = yield* analyze(program('HttpError | JsonError'))
    assert.deepEqual(codes(aliased), [])
    assert.deepEqual(codes(direct), [])
    assert.deepEqual(
      failureMembers(aliased, 'root', 'fetch'),
      failureMembers(direct, 'root', 'fetch'),
    )
    assert.strictEqual(failureMembers(aliased, 'root', 'fetch').length, 2)
    assert.deepEqual(
      failureMembers(aliased, 'root', 'residual'),
      failureMembers(direct, 'root', 'residual'),
    )
    assert.strictEqual(failureMembers(aliased, 'root', 'residual').length, 1)
    // Lowering a catch over a nominal union beside a struct currently fails MIR verification for
    // the direct spelling too; the alias contract is that both spellings behave identically.
    assert.strictEqual(Analysis.evaluate(aliased)._tag, Analysis.evaluate(direct)._tag)
  }),
)

it.effect('resolves a public alias through selected and qualified imports', () =>
  Effect.gen(function* () {
    const lib = `pub struct HttpError {}
pub struct JsonError {}
pub type FetchError = HttpError | JsonError
type Secret = i32
pub effect fn fetch() -> i32 ! FetchError { fail HttpError {} }`
    const self = yield* analyzeModules('root', [
      [
        'root',
        `import lib as Lib
import lib { FetchError }
effect fn selected() -> i32 ! FetchError { return run Lib.fetch() }
effect fn qualified() -> i32 ! Lib.FetchError { return run Lib.fetch() }
pub fn main() -> i32 { return 0 }`,
      ],
      ['lib', lib],
    ])
    assert.deepEqual(codes(self), [])
    assert.deepEqual(failureMembers(self, 'root', 'selected'), failureMembers(self, 'lib', 'fetch'))
    assert.deepEqual(
      failureMembers(self, 'root', 'qualified'),
      failureMembers(self, 'lib', 'fetch'),
    )
    const hidden = yield* analyzeModules('root', [
      [
        'root',
        `import lib as Lib\nfn use(value: Lib.Secret) -> i32 { return 0 }\npub fn main() -> i32 { return 0 }`,
      ],
      ['lib', lib],
    ])
    assert.include(codes(hidden), Diagnostic.inaccessibleImportedMemberCode)
  }),
)
