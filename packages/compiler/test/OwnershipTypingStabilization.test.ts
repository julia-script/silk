import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as StandardStreams from '../src/StandardStreams.js'
import { replaceCleanupProgram, replaceDropProgram } from './support/corpus.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const snapshot = (name: string, source: string, target = 'wasm32-unknown-unknown') =>
  Analysis.ofSourceRealized(`stabilization/${name}`, encoder.encode(source), target)

/** Runs one accepted program on the evaluator and Wasm, returning both results and stdout. */
const runBoth = Effect.fnUntraced(function* (name: string, source: string) {
  const self = yield* snapshot(name, source)
  assert.deepEqual(Analysis.diagnostics(self), [], name)
  assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [], name)
  const memory = StandardStreams.memory()
  const evaluated = Analysis.evaluate(self, { standardStreams: memory.provider })
  assert.strictEqual(evaluated._tag, 'Completed', name)
  const evaluatorOut = decoder.decode(Uint8Array.from(memory.events().flatMap((e) => e.bytes)))
  const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
  const wasmMemory = StandardStreams.memory()
  let instance: WebAssembly.Instance | undefined
  const imports = StandardStreams.wasmImports(wasmMemory.provider, () => {
    const exported = instance?.exports[StandardStreams.wasmMemoryExport]
    return exported instanceof WebAssembly.Memory ? exported : undefined
  })
  instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), imports)
  const wasmResult = (instance.exports.silk_main as () => number)()
  const wasmOut = decoder.decode(Uint8Array.from(wasmMemory.events().flatMap((e) => e.bytes)))
  return {
    evaluator: evaluated._tag === 'Completed' ? Number(evaluated.result.value) : undefined,
    evaluatorOut,
    wasm: wasmResult,
    wasmOut,
  }
})

const codes = Effect.fnUntraced(function* (name: string, source: string, target?: string) {
  const self = yield* snapshot(name, source, target)
  return Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)
})

// ISSUE-49 — OWN-005: replacing a live place cleans the displaced value exactly once.
it.effect('cleans a displaced local, field, array element, and slice element once', () =>
  Effect.gen(function* () {
    const run = yield* runBoth('replace-cleanup', replaceCleanupProgram)
    assert.strictEqual(run.evaluatorOut, '123546789')
    assert.strictEqual(run.wasmOut, '123546789')
    assert.strictEqual(run.evaluator, 0)
    assert.strictEqual(run.wasm, 0)
  }),
)

it.effect('orders replacement cleanup before drop and scope exit', () =>
  Effect.gen(function* () {
    const run = yield* runBoth('replace-drop', replaceDropProgram)
    assert.strictEqual(run.evaluatorOut, '1243')
    assert.strictEqual(run.wasmOut, '1243')
  }),
)

it.effect('does not clean a moved-out place on re-initialization', () =>
  Effect.gen(function* () {
    const source = replaceCleanupProgram.replace(
      'fn localReplace(log: &Shared<Log>) -> i32 {\n  let mut a = tracer(1, log)\n  a = tracer(2, log)\n  return 0\n}',
      'fn consume(t: Tracer) -> i32 { return 0 }\nfn localReplace(log: &Shared<Log>) -> i32 {\n  let mut a = tracer(1, log)\n  let c = consume(move a)\n  a = tracer(2, log)\n  return 0\n}',
    )
    assert.notStrictEqual(source, replaceCleanupProgram)
    const run = yield* runBoth('replace-reinit', source)
    assert.strictEqual(run.evaluatorOut, '123546789')
    assert.strictEqual(run.wasmOut, '123546789')
  }),
)

// ISSUE-50 — EFFECT-OWN-001: an Effect may not escape with a borrow of function-owned storage.
it.effect('rejects an Effect escaping with a borrow of a local owner', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codes(
        'effect-escape-local',
        `import silk.effect { Effect }
effect fn inspect(values: &[i32]) -> i32 { return values[0] }
fn make() -> Effect<i32> {
  let v = [21]
  return inspect(&v)
}
pub fn main() -> i32 { return run make() }`,
      ),
      ['OWN0018'],
    )
    assert.deepEqual(
      yield* codes(
        'effect-escape-temp-view',
        `import silk.effect { Effect }
fn identity(values: &[i32]) -> &[i32] { return values }
fn make() -> Effect<i32> {
  let view = identity(&[1, 2])
  return effect { return view[0] }
}
pub fn main() -> i32 { let e = make() return run e }`,
      ),
      ['OWN0018'],
    )
    assert.deepEqual(
      yield* codes(
        'effect-escape-bound',
        `import silk.effect { Effect }
effect fn inspect(values: &[i32]) -> i32 { return values[0] }
fn make() -> Effect<i32> {
  let v = [21]
  let e = inspect(&v)
  return e
}
pub fn main() -> i32 { return run make() }`,
      ),
      ['OWN0018'],
    )
  }),
)

it.effect('keeps a parameter-rooted and value-captured Effect escape valid', () =>
  Effect.gen(function* () {
    const run = yield* runBoth(
      'effect-escape-parameter',
      `import silk.effect { Effect }
effect fn inspect(values: &[i32]) -> i32 { return values[0] }
fn prepare(values: &[i32]) -> Effect<i32> { return inspect(&values) }
effect fn succeed(value: i32) -> i32 { return value }
fn wrap(self: once Effect<i32>) -> once Effect<i32> { return effect { return run self } }
pub fn main() -> i32 {
  let v = [21]
  let e = prepare(&v)
  let a = run e
  let b = run e
  let w = wrap(succeed(0))
  return a + b + run w
}`,
    )
    assert.strictEqual(run.evaluator, 42)
    assert.strictEqual(run.wasm, 42)
  }),
)

// ISSUE-52 — UNION-002 / OWN-008: a declared union is the binding's type.
it.effect('types a union-annotated binding as the union', () =>
  Effect.gen(function* () {
    const run = yield* runBoth(
      'union-binding',
      `struct Token { kind: i32 }
struct End {}
fn classify(v: Token | End) -> i32 {
  return match move v {
    Token { kind } => kind
    End {} => -1
  }
}
pub fn main() -> i32 {
  let v: Token | End = Token { kind: 1 }
  let r = match move v {
    Token { kind } => kind
    End {} => -1
  }
  let mut w: Token | End = Token { kind: 1 }
  w = End {}
  let mut s: i32 | string = 5
  s = "text"
  let t = match s {
    i32 n => n
    string x => 9
  }
  return r + classify(move w) + t + 33
}`,
    )
    assert.strictEqual(run.evaluator, 42)
    assert.strictEqual(run.wasm, 42)
    assert.deepEqual(
      yield* codes(
        'union-binding-narrow',
        `struct Token { kind: i32 }
struct End {}
fn take(t: Token) -> i32 { return t.kind }
pub fn main() -> i32 {
  let v: Token | End = Token { kind: 1 }
  return take(move v)
}`,
      ),
      ['SEM0040'],
    )
    assert.deepEqual(
      yield* codes(
        'union-binding-nomove',
        `struct Token { kind: i32 }
fn take(v: Token | i32) -> i32 { return 1 }
pub fn main() -> i32 {
  let v: Token | i32 = 5
  let w = v
  return take(move v) + take(move w)
}`,
      ),
      ['OWN0003'],
    )
  }),
)

// ISSUE-101 — INT-001: an isize literal is range-checked against the selected target word.
it.effect('rejects an isize literal outside the wasm32 word', () =>
  Effect.gen(function* () {
    for (const source of [
      'pub fn main() -> i32 { let x: isize = 2147483648 return 0 }',
      'pub fn main() -> i32 { let x: isize = -2147483649 return 0 }',
      'fn f(v: isize) -> isize { return v } pub fn main() -> i32 { let x = f(2147483648) return 0 }',
    ]) {
      assert.deepEqual(yield* codes('isize-range', source), ['LAY0001'])
      assert.deepEqual(yield* codes('isize-range-native', source, 'aarch64-apple-darwin'), [])
    }
    const run = yield* runBoth(
      'isize-bounds',
      'pub fn main() -> i32 { let x: isize = -2147483648 let y: isize = 2147483647 return 42 }',
    )
    assert.strictEqual(run.evaluator, 42)
    assert.strictEqual(run.wasm, 42)
  }),
)

// ISSUE-100 — ENUM-004 / OP-011: an inline enum member is a reachable enum value.
it.effect('lowers inline enum members in Enum.value and equality', () =>
  Effect.gen(function* () {
    const run = yield* runBoth(
      'enum-inline',
      `enum Plain { A, B, C }
enum Status { Pending, Ready }
pub fn main() -> i32 {
  let p: u8 = Plain.value(Plain.C)
  if p != 2 { return 1 }
  if Status.Pending == Status.Ready { return 10 }
  if Status.Ready != Status.Ready { return 12 }
  return 42
}`,
    )
    assert.strictEqual(run.evaluator, 42)
    assert.strictEqual(run.wasm, 42)
  }),
)

// ISSUE-102 — LEXICAL-006: a byte literal has program lifetime and may be returned as a view.
it.effect('returns a byte literal view from a function without borrowed parameters', () =>
  Effect.gen(function* () {
    const run = yield* runBoth(
      'bytes-return',
      `fn bytes() -> &[u8] { return b"Silk\\x00" }
fn viaBinding() -> &[u8] {
  let b = b"ab"
  return b
}
pub fn main() -> i32 {
  let b = bytes()
  if b[4] != 0 { return 1 }
  if b[0] != 83 { return 2 }
  if viaBinding()[1] != 98 { return 3 }
  return 42
}`,
    )
    assert.strictEqual(run.evaluator, 42)
    assert.strictEqual(run.wasm, 42)
    assert.deepEqual(
      yield* codes(
        'bytes-return-local',
        'fn bad() -> &[u8] { let a: [u8; 2] = [1, 2] return &a } pub fn main() -> i32 { return 0 }',
      ),
      ['SEM0055'],
    )
    assert.include(
      yield* codes(
        'bytes-return-exclusive',
        'fn bad() -> &mut [u8] { return b"a" } pub fn main() -> i32 { return 0 }',
      ),
      'SEM0091',
    )
  }),
)

// ISSUE-103 — CONST-001: a constant has no address and cannot be borrowed.
it.effect('rejects borrowing a constant', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codes(
        'const-borrow',
        'const x: i32 = 1\nfn peek(v: &i32) -> i32 { return 5 }\npub fn main() -> i32 { return peek(&x) }',
      ),
      ['SEM0086'],
    )
    assert.deepEqual(
      yield* codes(
        'const-mut-borrow',
        'const x: i32 = 1\nfn poke(v: &mut i32) -> i32 { return 5 }\npub fn main() -> i32 { return poke(&mut x) }',
      ),
      ['SEM0086'],
    )
  }),
)
