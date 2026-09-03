import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const json = (value: unknown): string =>
  Json.stringify(value, (_, inner) => (typeof inner === 'bigint' ? inner.toString() : inner))

const codesOf = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

/** Evaluates on the interpreter and Wasm and asserts both produce `expected`. */
const runParity = Effect.fnUntraced(function* (name: string, source: string, expected: number) {
  const snapshot = yield* Analysis.ofSourceRealized(
    `anonymous-capture/${name}`,
    ascii(source),
    'wasm32-unknown-unknown',
  )
  assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
  const evaluated = Analysis.evaluate(snapshot)
  assert.strictEqual(evaluated._tag, 'Completed', `${name}: ${json(evaluated)}`)
  if (evaluated._tag !== 'Completed') return snapshot
  assert.strictEqual(evaluated.result.value, BigInt(expected), `${name} evaluator`)
  const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
  assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
  return snapshot
})

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang: Effect.runSync(Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(defaultClang()))),
  llvmAr: 'llvm-ar',
  runtimeObjectCache: NativeToolchain.makeRuntimeObjectCache(),
})
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-anonymous-capture-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

/** Compiles to a native executable and asserts its exit status is `expected`. */
const runNative = Effect.fnUntraced(function* (name: string, source: string, expected: number) {
  const outcome = yield* Driver.compile({
    compilation: { root: SourceFile.make('memory/driver', ascii(source)) },
    toolchain,
    profile: 'release',
    artifactKind: 'NativeExecutable',
    destination: join(destinationRoot, name),
  }).pipe(Effect.provide(SourceResolver.empty))
  assert.strictEqual(outcome._tag, 'Compiled', `${name}: ${json(outcome)}`)
  if (outcome._tag !== 'Compiled') return
  const run = spawnSync(outcome.path, [], { encoding: 'utf8' })
  assert.strictEqual(run.status, expected, `${name} native: signal ${run.signal ?? 'none'}`)
})

// ISSUE-7: a body writing through a captured `&mut` parameter must reach the caller's owner on
// every engine. The environment stores the reference itself, never a pointer to its slot.
const captureMutParam = `struct Counter { value: i32 }
fn work(c: &mut Counter) -> i32 {
  let mut step = fn() -> i32 { c.value = c.value + 1
    return c.value }
  let a = step()
  let b = step()
  return a * 10 + b
}
pub fn main() -> i32 {
  let mut c = Counter { value: 0 }
  let r = work(&mut c)
  return r + c.value * 100 - 170
}`

const captureMutParamScalar = `fn work(c: &mut i32) -> i32 {
  let mut step = fn() -> i32 { c.* = c.* + 1
    return c.* }
  let a = step()
  let b = step()
  return a * 10 + b
}
pub fn main() -> i32 {
  let mut c = 0
  let r = work(&mut c)
  return r + c * 100 - 170
}`

it.effect(
  'writes through a captured exclusive reference parameter on every engine',
  () =>
    Effect.gen(function* () {
      for (const [name, source] of [
        ['mut-param', captureMutParam],
        ['mut-param-scalar', captureMutParamScalar],
      ] as const) {
        yield* runParity(name, source, 42)
        yield* runNative(name, source, 42)
      }
    }),
  120_000,
)

// ISSUE-6: the evaluator re-borrowed a snapshot of an exclusively captured owner, so the second
// invocation could not see the first one's write.
const mutCaptureStruct = `struct Counter { value: i32 }
fn bump(c: &mut Counter) -> i32 { c.value = c.value + 1
  return c.value }
pub fn main() -> i32 {
  let mut c = Counter { value: 0 }
  let mut step = fn() -> i32 { return bump(&mut c) }
  let a = step()
  let b = step()
  return a * 10 + b
}`

const borrowAcrossCalls = `struct Counter { value: i32 }
fn bump(c: &mut Counter) -> i32 { c.value = c.value + 1
  return c.value }
pub fn main() -> i32 {
  let mut c = Counter { value: 0 }
  let mut step = fn() -> i32 { return bump(&mut c) }
  let a = step()
  let b = step()
  let c2 = c.value
  let read = fn() -> i32 { return c.value }
  let r = read() + read()
  c.value = 100
  return a + b + c2 + r - 9 + c.value - 100
}`

it.effect('re-borrows an exclusively captured owner in place across invocations', () =>
  Effect.gen(function* () {
    yield* runParity('mut-capture-struct', mutCaptureStruct, 12)
    yield* runParity('borrow-across-calls', borrowAcrossCalls, 0)
  }),
)

// ISSUE-1: a shared callable value is Copy, so capturing a callable-typed parameter copies it, and
// a generic owner's substitution reaches the hidden body through the section's own parameters.
const genericAnonCaptureF = `fn apply<T>(f: fn(T) -> T, v: T) -> T { return f(move v) }
fn twice<T>(f: fn(T) -> T, v: T) -> T {
  return apply<T>(fn(x: T) -> T { return f(move x) }, move v)
}
fn inc(v: i32) -> i32 { return v + 1 }
pub fn main() -> i32 {
  return twice<i32>(inc, 41)
}`

const nongenericAnonCaptureF = `fn apply(f: fn(i32) -> i32, v: i32) -> i32 { return f(v) }
fn twice(f: fn(i32) -> i32, v: i32) -> i32 {
  return apply(fn(x: i32) -> i32 { return f(f(x)) }, v)
}
fn inc(v: i32) -> i32 { return v + 1 }
pub fn main() -> i32 {
  return twice(inc, 40)
}`

const namedSectionCaptureF = `fn apply(f: fn(i32) -> i32, v: i32) -> i32 { return f(v) }
fn compose(x: i32, f: fn(i32) -> i32) -> i32 { return f(f(x)) }
fn twice(f: fn(i32) -> i32, v: i32) -> i32 { return apply(compose(f), v) }
fn inc(v: i32) -> i32 { return v + 1 }
pub fn main() -> i32 { return twice(inc, 40) }`

it.effect('captures a callable-typed parameter by copy in generic and concrete owners', () =>
  Effect.gen(function* () {
    yield* runParity('generic-anon-capture-f', genericAnonCaptureF, 42)
    yield* runParity('nongeneric-anon-capture-f', nongenericAnonCaptureF, 42)
    yield* runParity('named-section-capture-f', namedSectionCaptureF, 42)
  }),
)

// ISSUE-47: take-once mode derives from the captured type, not from a source-level `move`. A
// fresh affine temporary enters the environment by ownership and a declared `once` result keeps
// its access at the call site.
const tokenSurface = `struct Token { value: i32 }
fn addToken(value: i32, token: Token) -> i32 { return value + token.value }
`
const payloadSurface = `import silk.effect { Effect }
struct Payload { value: i32 }
`

it.effect('derives take-once invocation and run access from captured affine temporaries', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      [
        'once-twice',
        `${tokenSurface}pub fn main() -> i32 {
  let f = addToken(Token { value: 1 })
  return f(1) + f(2)
}`,
        ['OWN0001'],
      ],
      [
        'once-as-fn',
        `${tokenSurface}fn callShared(f: fn(i32) -> i32) -> i32 { return f(1) + f(2) }
pub fn main() -> i32 { return callShared(addToken(Token { value: 1 })) }`,
        ['SEM0076'],
      ],
      [
        'nomove-capture',
        `${tokenSurface}fn prepare(token: Token) -> once fn(i32) -> i32 { return addToken(token) }
pub fn main() -> i32 { let p = prepare(Token { value: 10 }) return p(1) }`,
        ['OWN0003'],
      ],
      [
        'run-twice',
        `${payloadSurface}fn prepare(payload: Payload) -> once Effect<Payload> { return effect { return move payload } }
pub fn main() -> i32 {
  let e = prepare(Payload { value: 36 })
  let p = run e
  let q = run e
  return p.value + q.value
}`,
        ['OWN0001'],
      ],
      [
        'effect-fn-once',
        `${payloadSurface}effect fn unwrap(payload: Payload) -> Payload { return move payload }
pub fn main() -> i32 {
  let e = unwrap(Payload { value: 36 })
  let p = run e
  let q = run e
  return p.value + q.value
}`,
        ['OWN0001'],
      ],
      [
        'pass-once-nomove',
        `${payloadSurface}fn prepare(payload: Payload) -> once Effect<Payload> { return effect { return move payload } }
fn take(e: once Effect<Payload>) -> i32 { return (run e).value }
pub fn main() -> i32 {
  let e = prepare(Payload { value: 36 })
  return take(e) + take(e)
}`,
        ['OWN0003', 'OWN0003'],
      ],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `anonymous-capture/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(codesOf(snapshot), expected, name)
    }
  }),
)

const hookCallsOf = (run: ReturnType<typeof Analysis.evaluate>): number =>
  run._tag === 'Completed'
    ? run.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      ).length
    : -1

it.effect('cleans a captured affine temporary exactly once', () =>
  Effect.gen(function* () {
    const section = `struct Token { value: i32 }
impl Drop for Token { fn drop(self: &mut Token) -> () { return () } }
fn addToken(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let f = addToken(Token { value: 2 })
  return f(40)
}`
    const effect = `import silk.effect { Effect }
struct Payload { value: i32 }
impl Drop for Payload { fn drop(self: &mut Payload) -> () { return () } }
fn prepare(payload: Payload) -> once Effect<Payload> { return effect { return move payload } }
pub fn main() -> i32 {
  let e = prepare(Payload { value: 42 })
  let p = run e
  return p.value
}`
    for (const [name, source] of [
      ['once-section', section],
      ['once-effect', effect],
    ] as const) {
      const snapshot = yield* runParity(name, source, 42)
      assert.strictEqual(hookCallsOf(Analysis.evaluate(snapshot)), 1, `${name} drop hooks`)
    }
  }),
)
