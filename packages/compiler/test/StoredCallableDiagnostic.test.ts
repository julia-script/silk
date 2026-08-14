import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Stream from 'effect/Stream'
import { ChildProcess } from 'effect/unstable/process'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

/**
 * The frontend/MIR contract at a stored callable (#184).
 *
 * A direct callable value works because the compiler still holds its hidden concrete identity;
 * a struct field declared `fn(i32) -> i32` carries only the signature, so nominal layout planning
 * cannot size the callable's environment. Until nominal values can carry that identity, a
 * reachable construction that stores a bare callable is rejected with SEM0103 at the source site
 * instead of passing a clean frontend and dying in MIR validation as `InvalidMir`.
 *
 * The boundary matters as much as the rejection: programs that work today keep working. That is
 * why the check is scoped to reachable instances — a callable-bearing struct that is merely
 * declared, named in a signature, or constructed only in unreachable code compiles and runs now,
 * and stays accepted.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string) =>
  Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')

const codes = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

/** Evaluated scalars can be BigInt, which plain JSON serialization refuses. */
const describe = (outcome: unknown): string =>
  JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value))

const collectText = Stream.runFold(
  () => '',
  (text: string, chunk: string) => text + chunk,
)

const runExecutable = Effect.fnUntraced(function* (path: string) {
  const process = yield* ChildProcess.make(path, [], { stdin: 'ignore' })
  const [code, , stderr] = yield* Effect.all(
    [
      process.exitCode,
      process.stdout.pipe(Stream.decodeText(), collectText),
      process.stderr.pipe(Stream.decodeText(), collectText),
    ],
    { concurrency: 'unbounded' },
  )
  return { code, stderr }
})

/** The minimal reproducer from #184, repaired so declaration and semantic analysis accept it. */
const reproducer = `struct Parser<A> {
  decode: fn(i32) -> A
}

struct Nested<A> {
  parser: Parser<A>
}

fn make<A>(decoder: fn(i32) -> A) -> Parser<A> {
  return Parser<A> { decode: decoder }
}

fn nest<A>(parser: Parser<A>) -> Nested<A> {
  return Nested<A> { parser: move parser }
}

fn parse<A>(self: &Parser<A>, input: i32) -> A {
  return self.decode(input)
}

fn parseNested<A>(self: &Nested<A>, input: i32) -> A {
  return self.parser.decode(input)
}

pub fn main() -> i32 {
  let parser = make<i32>(i32.add(1))
  let nested = nest<i32>(move parser)
  let first = parseNested<i32>(&nested, 40)
  return parse<i32>(&nested.parser, first)
}`

it.effect('rejects the #184 reproducer at both construction sites', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed('stored-callable/reproducer', reproducer)
    assert.deepEqual(codes(snapshot), ['SEM0103', 'SEM0103'], messages(snapshot).join('\n'))
    // The direct field and the nested aggregate each name the exact stored position.
    assert.deepEqual(messages(snapshot), [
      'Cannot construct stored-callable/reproducer.Parser<i32>: field decode would store the callable fn(i32) -> i32, whose environment layout depends on a hidden concrete identity that stored-callable/reproducer.Parser<i32> does not carry',
      'Cannot construct stored-callable/reproducer.Nested<i32>: field parser.decode would store the callable fn(i32) -> i32, whose environment layout depends on a hidden concrete identity that stored-callable/reproducer.Nested<i32> does not carry',
    ])
  }),
)

it.effect('fences realization: SEM0103 leaves layout and MIR unavailable, not InvalidMir', () =>
  Effect.gen(function* () {
    // The diagnostic is only a real fence if nothing downstream of it is built: a snapshot that
    // still carried a layout and MIR would reproduce the InvalidMir failure the diagnostic exists
    // to replace.
    const snapshot = yield* analyzed('stored-callable/fence', reproducer)
    assert.include(codes(snapshot), 'SEM0103')
    assert.strictEqual(snapshot.layoutCatalog._tag, 'Unavailable')
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('rejects a monomorphic construction that stores a partial application', () =>
  Effect.gen(function* () {
    const source = `struct Parser { decode: fn(i32) -> i32 }
pub fn main() -> i32 {
  let parser = Parser { decode: i32.add(1) }
  return parser.decode(41)
}`
    const snapshot = yield* analyzed('stored-callable/monomorphic', source)
    assert.deepEqual(codes(snapshot), ['SEM0103'], messages(snapshot).join('\n'))
  }),
)

it.effect('points a generic wrapper violation at the specializing call site', () =>
  Effect.gen(function* () {
    // The declared field type is `T`; only instance substitution makes it a callable. The concrete
    // callable was written at the call, so that is the primary origin, and the generic body's
    // construction is retained as related provenance.
    const source = `struct Holder<T> { value: T }
fn wrap<T>(value: T) -> Holder<T> { return Holder<T> { value: move value } }
pub fn main() -> i32 {
  let held = wrap(i32.add(1))
  return 42
}`
    const snapshot = yield* analyzed('stored-callable/generic-inferred', source)
    assert.deepEqual(codes(snapshot), ['SEM0103'], messages(snapshot).join('\n'))
    const diagnostic = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(diagnostic?.span.sourceId, 'stored-callable/generic-inferred')
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'wrap(i32.add(1))',
    )
    assert.deepEqual(
      diagnostic?.relatedSpans?.map((related) => [
        related.label,
        source.slice(related.span.start, related.span.end).trim(),
      ]),
      [['constructed here', 'Holder<T> { value: move value }']],
    )
  }),
)

it.effect('rejects a callable stored through an array field and a bare array literal', () =>
  Effect.gen(function* () {
    const throughField = `struct Parser { decode: [fn(i32) -> i32; 2] }
pub fn main() -> i32 {
  let parser = Parser { decode: [i32.add(1), i32.add(2)] }
  return 42
}`
    const fieldSnapshot = yield* analyzed('stored-callable/array-field', throughField)
    // Both the array literal and the enclosing struct construction are stored-callable sites.
    assert.deepEqual(
      codes(fieldSnapshot),
      ['SEM0103', 'SEM0103'],
      messages(fieldSnapshot).join('\n'),
    )

    const bare = `pub fn main() -> i32 {
  let transforms = [i32.add(1), i32.add(2)]
  return 42
}`
    const bareSnapshot = yield* analyzed('stored-callable/bare-array', bare)
    assert.deepEqual(codes(bareSnapshot), ['SEM0103'], messages(bareSnapshot).join('\n'))
  }),
)

it.effect('rejects a capturing closure stored in a nominal field', () =>
  Effect.gen(function* () {
    const source = `struct Parser { decode: fn(i32) -> i32 }
pub fn main() -> i32 {
  let offset = 1
  let parser = Parser { decode: i32.add(offset) }
  return 42
}`
    const snapshot = yield* analyzed('stored-callable/capturing', source)
    assert.deepEqual(codes(snapshot), ['SEM0103'], messages(snapshot).join('\n'))
  }),
)

it.effect('reports the stored callable alongside ownership findings for a once field', () =>
  Effect.gen(function* () {
    const source = `struct Parser { decode: once fn(i32) -> i32 }
pub fn main() -> i32 {
  let parser = Parser { decode: i32.add(1) }
  return parser.decode(41)
}`
    const snapshot = yield* analyzed('stored-callable/once-field', source)
    assert.include(codes(snapshot), 'SEM0103', messages(snapshot).join('\n'))
  }),
)

it.effect('points a stdlib construction reached through inference at the user call', () =>
  Effect.gen(function* () {
    // `Option.some(i32.add(1))` specializes `Some<T>` with a callable argument. The construction
    // that cannot receive a layout lives inside silk/option, but the callable was written at the
    // user's call, so the primary span is the user source and the stdlib construction is related
    // provenance.
    const source = `pub fn main() -> i32 {
  let optional = Option.some(i32.add(1))
  return 42
}`
    const snapshot = yield* analyzed('stored-callable/stdlib-inference', source)
    assert.deepEqual(codes(snapshot), ['SEM0103'], messages(snapshot).join('\n'))
    const diagnostic = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(diagnostic?.span.sourceId, 'stored-callable/stdlib-inference')
    assert.deepEqual(
      diagnostic?.relatedSpans?.map((related) => [related.label, related.span.sourceId]),
      [['constructed here', 'silk/option']],
    )
  }),
)

/**
 * Callable-bearing declarations that never reach a live construction keep compiling, and the
 * asserted value — not compilation success — is the evidence on all three engines.
 */
const accepted = `struct Parser { decode: fn(i32) -> i32 }
struct Nested { parser: Parser }

fn size(self: &Parser) -> i32 { return 1 }

fn unreachableConstruction() -> i32 {
  let parser = Parser { decode: i32.add(1) }
  return parser.decode(41)
}

pub fn main() -> i32 { return 42 }`

it.effect(
  'keeps declaration-only and unreachable callable fields compiling on all three engines',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const path = yield* Path.Path
      const directory = yield* fileSystem.makeTempDirectoryScoped()
      const snapshot = yield* analyzed('stored-callable/accepted', accepted)
      assert.deepEqual(messages(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42, 'evaluator')

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42, 'wasm')

      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('stored-callable/accepted', ascii(accepted)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: path.join(directory, 'accepted'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = yield* runExecutable(compiled.path)
      assert.strictEqual(run.code, 42, `native: ${run.stderr}`)
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  180_000,
)

it.effect('leaves direct callable parameters and calls unchanged', () =>
  Effect.gen(function* () {
    const source = `fn apply(transform: once fn(i32) -> i32, value: i32) -> i32 { return transform(value) }
pub fn main() -> i32 { return apply(i32.add(40), 2) }`
    const snapshot = yield* analyzed('stored-callable/direct-call', source)
    assert.deepEqual(messages(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
  }),
)
