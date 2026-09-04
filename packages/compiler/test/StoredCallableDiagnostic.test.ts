import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Type from '../src/Type.js'

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

const codes = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const diagnosticView = (snapshot: Analysis.FrontendSnapshot) =>
  Analysis.diagnostics(snapshot).map((diagnostic) => ({
    code: diagnostic.code,
    start: diagnostic.span.start,
    end: diagnostic.span.end,
  }))

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

const accepted = `import silk.i32 as i32
struct Parser { decode: fn(i32) -> i32 }
struct Nested { parser: Parser }
fn size(self: &Parser) -> i32 { return 1 }
fn unreachableConstruction() -> i32 {
  let parser = Parser { decode: i32.add(1) }
  return parser.decode(41)
}
pub fn main() -> i32 { return 42 }`

it.effect('keeps declaration-only and unreachable callable fields out of realization', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed('stored-callable/accepted', accepted)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.isFalse(
      Analysis.instancesOf(snapshot).instances.some(
        (instance) => instance.key.declaration.name === 'unreachableConstruction',
      ),
    )
  }),
)

/** The minimal reproducer from #184, repaired so declaration and semantic analysis accept it. */
const reproducer = `import silk.i32 as i32
struct Parser<A> {
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
    const source = `import silk.i32 as i32
struct Parser { decode: fn(i32) -> i32 }
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
    const source = `import silk.i32 as i32
struct Holder<T> { value: T }
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
    const throughField = `import silk.i32 as i32
struct Parser { decode: [fn(i32) -> i32; 2] }
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

    const bare = `import silk.i32 as i32
pub fn main() -> i32 {
  let transforms = [i32.add(1), i32.add(2)]
  return 42
}`
    const bareSnapshot = yield* analyzed('stored-callable/bare-array', bare)
    assert.deepEqual(codes(bareSnapshot), ['SEM0103'], messages(bareSnapshot).join('\n'))
  }),
)

it.effect('rejects a capturing closure stored in a nominal field', () =>
  Effect.gen(function* () {
    const source = `import silk.i32 as i32
struct Parser { decode: fn(i32) -> i32 }
pub fn main() -> i32 {
  let offset = 1
  let parser = Parser { decode: i32.add(offset) }
  return 42
}`
    const snapshot = yield* analyzed('stored-callable/capturing', source)
    assert.deepEqual(codes(snapshot), ['SEM0103'], messages(snapshot).join('\n'))
  }),
)

it.effect('keeps nested structural callable captures fenced before layout and MIR', () =>
  Effect.gen(function* () {
    const source = `import silk.i32 as i32
struct Parser<F: fn(i32) -> i32> { decode: F }
fn apply(value: i32, transform: fn(i32) -> i32) -> i32 { return transform(value) }
pub fn main() -> i32 {
  let parser = Parser { decode: apply(i32.add(1)) }
  return parser.decode(41)
}`
    const snapshot = yield* analyzed('stored-callable/nested-callable-capture', source)
    assert.deepEqual(codes(snapshot), ['SEM0103'], messages(snapshot).join('\n'))
    assert.strictEqual(snapshot.layoutCatalog._tag, 'Unavailable')
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('reports the stored callable alongside ownership findings for a once field', () =>
  Effect.gen(function* () {
    const source = `import silk.i32 as i32
struct Parser { decode: once fn(i32) -> i32 }
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
    // `some(i32.add(1))` specializes `Option<T>.Some` with a callable argument. The construction
    // that cannot receive a layout lives inside silk/option, but the callable was written at the
    // user's call, so the primary span is the user source and the stdlib construction is related
    // provenance.
    const source = `import silk.i32 as i32
import silk.option { Option }
pub fn main() -> i32 {
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

it.effect('rejects a nested anonymous body without publishing an inner executable', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 {
  let outer = fn() -> i32 {
    let nested = fn() -> i32 { return 1 }
    return 42
  }
  return outer()
}`
    const snapshot = yield* Analysis.ofSource('stored-callable/nested-anonymous', ascii(source))
    const rejected = 'fn() -> i32 { return 1 }'
    const start = source.indexOf(rejected) - 1
    assert.deepEqual(diagnosticView(snapshot), [
      { code: 'SEM0199', start, end: start + rejected.length + 1 },
    ])
    assert.strictEqual(
      snapshot.results.get('stored-callable/nested-anonymous')?.hiddenFunctions.length,
      1,
    )
  }),
)

it.effect('checks explicit anonymous contracts and derived modes against context', () =>
  Effect.gen(function* () {
    const resultSource = `fn accept(step: fn() -> i32) -> i32 { return step() }
pub fn main() -> i32 { return accept(fn() -> bool { return true }) }`
    const modeSource = `struct Token { value: i32 }
fn consume(token: Token) -> i32 { return token.value }
fn reusable(step: fn() -> i32) -> i32 { return step() }
pub fn main() -> i32 {
  let token = Token { value: 42 }
  return reusable(fn() -> i32 { return consume(move token) })
}`
    const resultMismatch = yield* Analysis.ofSource(
      'stored-callable/anonymous-result-mismatch',
      ascii(resultSource),
    )
    const consumingMismatch = yield* Analysis.ofSource(
      'stored-callable/anonymous-mode-mismatch',
      ascii(modeSource),
    )
    const resultCallable = 'fn() -> bool { return true }'
    const modeCallable = 'fn() -> i32 { return consume(move token) }'
    assert.deepEqual(diagnosticView(resultMismatch), [
      {
        code: 'SEM0076',
        start: resultSource.indexOf(resultCallable),
        end: resultSource.indexOf(resultCallable) + resultCallable.length,
      },
    ])
    assert.deepEqual(diagnosticView(consumingMismatch), [
      {
        code: 'SEM0076',
        start: modeSource.indexOf(modeCallable),
        end: modeSource.indexOf(modeCallable) + modeCallable.length,
      },
    ])
  }),
)

it.effect('infers a generic higher-order call from an explicit anonymous contract', () =>
  Effect.gen(function* () {
    const source = `fn apply<T>(transform: once fn(T) -> T, value: T) -> T {
  return transform(move value)
}
pub fn main() -> i32 {
  return apply(fn(value: i32) -> i32 { return value }, 42)
}`
    const snapshot = yield* Analysis.ofSource('stored-callable/anonymous-inference', ascii(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const result = Analysis.rootAnalysis(snapshot)
    assert.strictEqual(result.hiddenFunctions.length, 1)
    assert.strictEqual(result.hiddenFunctions.at(0)?.declaration.typeParameters.length, 0)
    const hidden = result.hir.functions.find(
      (fn) =>
        fn.declaration.canonical._tag === 'Canonical' &&
        fn.declaration.canonical.id.name.includes('$callable$'),
    )
    assert.strictEqual(hidden?.contract._tag, 'Contract')
    if (hidden?.contract._tag !== 'Contract') return
    assert.deepEqual(hidden.contract.parameters.map(Type.encode), ['i32'])
    assert.strictEqual(Type.encode(hidden.contract.result), 'i32')
  }),
)

it.effect('rejects an escaped local capture and a second once-effect invocation', () =>
  Effect.gen(function* () {
    const borrowSource = `fn leak() -> mut fn() -> i32 {
  let mut cell = 40
  return fn() -> i32 {
    cell = cell + 1
    return cell
  }
}
pub fn main() -> i32 {
  let mut read = leak()
  return read()
}`
    const effectSource = `struct Token { value: i32 }
fn consume(token: Token) -> i32 { return token.value }
pub fn main() -> i32 {
  let token = Token { value: 42 }
  let deferred = effect fn() -> i32 { return consume(move token) }
  let first = deferred()
  let second = deferred()
  return run first
}`
    const borrowed = yield* Analysis.ofSource(
      'stored-callable/anonymous-escaped-borrow',
      ascii(borrowSource),
    )
    const repeated = yield* Analysis.ofSource(
      'stored-callable/anonymous-repeated-effect',
      ascii(effectSource),
    )
    const capturedCell = borrowSource.indexOf('cell', borrowSource.indexOf('return fn'))
    const capturedExpression = borrowSource.indexOf('\n    cell', borrowSource.indexOf('return fn'))
    const firstInvocation = effectSource.indexOf('deferred()')
    const secondInvocation = effectSource.indexOf('deferred()', firstInvocation + 1)
    assert.deepEqual(diagnosticView(borrowed), [
      { code: 'OWN0018', start: capturedExpression, end: capturedCell + 'cell'.length },
    ])
    assert.deepEqual(diagnosticView(repeated), [
      {
        code: 'OWN0001',
        start: secondInvocation - 1,
        end: secondInvocation + 'deferred'.length,
      },
    ])
  }),
)
