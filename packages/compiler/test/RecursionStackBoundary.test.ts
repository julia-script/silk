import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Projections from './support/projections.js'
import * as Driver from './support/TestDriver.js'

/**
 * Ordinary recursion is bounded by the machine stack, deliberately: the compiler adds no shadow
 * stack and no per-call allocation, so a call costs a call. These tests pin the *shape* of that
 * boundary rather than the depth at which it arrives — the depth belongs to the host's stack size,
 * the optimizer's frame layout, and the engine, and asserting one would only manufacture flakes.
 *
 * What is pinned:
 *
 * - a shallow chain traverses on all three engines and releases every link exactly once,
 * - a deep chain fails on every engine, each in that engine's own way, and
 * - the escape hatch — an explicit iterative teardown — carries a chain far beyond the depth at
 *   which the recursive form dies.
 *
 * Recorded on this branch, x86_64 Linux, Node 22.22, clang 18, release profile: the recursive walk
 * survives 500 links in the evaluator, 950 in Wasm, and 100,000 natively, and fails by 510, 1,000,
 * and 128,000 respectively. Those numbers are evidence, not contract; nothing below asserts them.
 */

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const clang = Effect.runSync(
  Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(defaultClang())),
)
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
})

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-recursion-stack-boundary-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/**
 * A singly linked chain of boxed nodes, with three phases that can be measured apart:
 *
 * - `build` is iterative, so construction never contributes call depth and a failure is always
 *   attributable to the phase under test,
 * - `stepDepth`/`viewDepth` is the ordinary recursive walk, borrowing one level at a time, and
 * - `drain` is the explicit iterative teardown: it unlinks one level per loop turn, so the chain
 *   is already flat by the time automatic cleanup sees it.
 *
 * `Intrinsic.replace` is what makes both loops expressible. A local cannot be partially moved out
 * of and a `match` arm is an expression rather than a statement, so a loop that walks ownership
 * down a chain has to swap a sentinel into the place it takes from.
 */
const prelude = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.usize as usize
import silk.box { Box, make as boxMake, get as boxGet, into as boxInto }

pub struct End {}

pub struct Link {
  next: Box<Chain>
}

pub struct Step {
  kind: End | Link
}

pub struct Chain {
  step: Step
}

pub struct Drained {
  chain: Chain
  more: bool
}

fn stepDepth(step: &Step) -> i32 {
  return match &step.kind {
    End nothing => 0
    Link { next } => viewDepth(boxGet<Chain>(&next))
  }
}

fn viewDepth(view: &[Chain]) -> i32 {
  return match &view[usize.ZERO] {
    Chain { step } => 1 + stepDepth(&step)
  }
}

fn unlink(chain: Chain) -> Drained {
  return match move chain {
    Chain { step } => unlinkStep(move step)
  }
}

fn unlinkStep(step: Step) -> Drained {
  return match move step {
    Step { kind } => unlinkKind(move kind)
  }
}

fn unlinkKind(kind: End | Link) -> Drained {
  return match move kind {
    End nothing => Drained { chain: Chain { step: Step { kind: End {} } }, more: false }
    Link { next } => Drained { chain: boxInto<Chain>(move next), more: true }
  }
}

fn drain(chain: Chain) -> i32 {
  let mut current = move chain
  let mut released = 0
  let mut going = true
  while going {
    let taken = Intrinsic.replace(current, Chain { step: Step { kind: End {} } })
    let mut stepped = unlink(move taken)
    current = Intrinsic.replace(stepped.chain, Chain { step: Step { kind: End {} } })
    going = stepped.more
    if stepped.more { released = released + 1 }
  }
  return released
}

effect fn build(depth: i32) -> Chain ! OutOfMemoryError ? &mut Allocator {
  let mut current = Chain { step: Step { kind: End {} } }
  let mut remaining = depth
  while remaining > 0 {
    let taken = Intrinsic.replace(current, Chain { step: Step { kind: End {} } })
    let boxed = run boxMake<Chain>(move taken)
    current = Chain { step: Step { kind: Link { next: move boxed } } }
    remaining = remaining - 1
  }
  return move current
}
`

const program = (body: string, depth: number): string => `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
${prelude}
${body}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(measure(${depth}), recover) }`

/** Recursive traversal, then an iterative teardown so only the walk can exhaust the stack. */
const walk = (depth: number): string =>
  program(
    `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
effect fn measure(depth: i32) -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let built = run build(depth) |> Effect.provideMut(&mut allocator)
  let counted = stepDepth(&built.step)
  let released = drain(move built)
  if counted == released { return 0 }
  return 2
}`,
    depth,
  )

/** Iterative throughout: build, unlink, release. Nothing here recurses. */
const drain = (depth: number): string =>
  program(
    `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
effect fn measure(depth: i32) -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let built = run build(depth) |> Effect.provideMut(&mut allocator)
  let released = drain(move built)
  if released == depth { return 0 }
  return 2
}`,
    depth,
  )

/**
 * Iterative construction, then ordinary automatic cleanup. `Box.drop` drops the element it holds,
 * so releasing the outermost link calls the hook of the one below it: the chain is destroyed by
 * recursion nobody wrote, and there is no call site at which to write it differently.
 */
const dropped = (depth: number): string =>
  program(
    `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
effect fn measure(depth: i32) -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let built = run build(depth) |> Effect.provideMut(&mut allocator)
  drop built
  return 0
}`,
    depth,
  )

/**
 * The case with no escape hatch at all: construction runs out of allocations partway, and the
 * half-built chain is released by the failure path. `drain` cannot be called on it — the value is
 * never named by user code, it exists only inside the cleanup the compiler runs on the way out.
 *
 * Recovering the `OutOfMemoryError` is the point: at a shallow depth this returns 1, having unwound
 * cleanly. Deep, the unwinding is what dies.
 */
const failedBuild = (depth: number): string => `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
${prelude}
struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorProvider()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn measure(depth: i32) -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: ${Math.floor(depth / 2)} }
  let built = run build(depth) |> Effect.provideMut(&mut allocator)
  drop built
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(measure(${depth}), recover) }`

const realize = (id: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(id, ascii(source), 'wasm32-unknown-unknown')
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      id,
    )
    return snapshot
  })

interface Outcome {
  readonly _tag: 'Returned' | 'Failed'
  readonly detail: string
}

const returned = (value: number): Outcome =>
  Object.freeze({ _tag: 'Returned' as const, detail: String(value) })

/**
 * Runs the module under Wasm and reports whether the host survived it.
 *
 * A host out of stack is a `RangeError` from V8. A Wasm `RuntimeError` is accepted as the same
 * observation because the trap the engine reaches first is platform-sensitive — on this machine a
 * band of depths just under the host limit traps `unreachable` instead, which is the separate
 * defect tracked in #134. Both mean "this engine could not carry the chain"; neither means the
 * traversal produced a wrong answer, which is what a returned non-zero value would mean.
 */
const runWasm = (bytes: Uint8Array): Outcome => {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
  try {
    return returned((instance.exports.silk_main as () => number)())
  } catch (error) {
    const failure = error as Error
    assert.isTrue(
      failure instanceof RangeError || failure instanceof WebAssembly.RuntimeError,
      `unexpected Wasm failure: ${failure.constructor.name}: ${failure.message}`,
    )
    return Object.freeze({
      _tag: 'Failed' as const,
      detail: `${failure.constructor.name}: ${failure.message}`,
    })
  }
}

/**
 * Compiles and runs the module natively. A machine stack overrun arrives as a signal rather than an
 * exit status, and which signal is the platform's business: Linux and macOS both raise `SIGSEGV`
 * for the guard page, and `SIGBUS` is accepted for the platforms that map it differently.
 */
const runNative = (id: string, source: string, destination: string) =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make(id, ascii(source)) },
      toolchain,
      profile: 'release',
      destination,
    })
    assert.strictEqual(compiled._tag, 'Compiled', id)
    if (compiled._tag !== 'Compiled') return Object.freeze({ _tag: 'Failed' as const, detail: id })
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    if (run.signal === null) {
      assert.strictEqual(run.stderr, '', id)
      return returned(run.status ?? -1)
    }
    assert.oneOf(run.signal, ['SIGSEGV', 'SIGBUS'], `${id}: ${run.stderr}`)
    return Object.freeze({ _tag: 'Failed' as const, detail: run.signal })
  }).pipe(Effect.provide(SourceResolver.empty))

/**
 * Doubles the depth until the engine gives out, and reports the first depth that failed.
 *
 * This is what keeps the test honest on a machine whose stack is larger than the one the numbers
 * above were measured on: the assertion is that some depth within reach fails, and that the failure
 * looks the way this engine's stack exhaustion looks. A machine that carries the cap without
 * failing is a machine on which the documented boundary did not hold, and the test should say so
 * rather than pass quietly.
 */
const escalate = <E>(
  from: number,
  cap: number,
  attempt: (depth: number) => Effect.Effect<Outcome, E, never>,
): Effect.Effect<{ readonly depth: number; readonly outcome: Outcome }, E, never> =>
  Effect.gen(function* () {
    let depth = from
    while (depth <= cap) {
      const outcome = yield* attempt(depth)
      if (outcome._tag === 'Failed') return Object.freeze({ depth, outcome })
      assert.strictEqual(outcome.detail, '0', `depth ${depth} must still compute the right answer`)
      depth = depth * 2
    }
    assert.fail(`no depth up to ${cap} exhausted the stack`)
  })

it.effect(
  'traverses, drains, and releases a shallow chain identically on all three engines',
  () =>
    Effect.gen(function* () {
      const depth = 64
      const source = walk(depth)
      const snapshot = yield* realize('recursion-stack-boundary/shallow', source)

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 0n)

      // The walk counted every link and the teardown released every one of them: 64 boxes, 64
      // acquires, 64 releases. Borrowing and ownership are unchanged by the depth question.
      const events = Projections.allocationTraceEventsOf(evaluated)
      const acquires = events.filter((event) => event._tag === 'AllocationAcquire').length
      const releases = events.filter((event) => event._tag === 'AllocationRelease').length
      assert.strictEqual(acquires, depth)
      assert.strictEqual(releases, acquires)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      assert.deepEqual(runWasm(wasm.bytes), returned(0))

      assert.deepEqual(
        yield* runNative(
          'recursion-stack-boundary/shallow',
          source,
          join(destinationRoot, 'shallow'),
        ),
        returned(0),
      )
    }),
  180_000,
)

/**
 * The evaluator is the one engine that does not fail by exhausting a real stack: the accepted
 * contract makes depth a deterministic budget, so the same program blocks identically on every
 * host instead of crashing on some of them. That is why the failure mode has to be documented per
 * engine — the same source is a `Blocked` value here and a signal on the target.
 */
it.effect('blocks a deep recursive traversal on the evaluator call-depth limit', () =>
  Effect.gen(function* () {
    const snapshot = yield* realize('recursion-stack-boundary/evaluator', walk(4_000))
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Blocked')
    if (evaluated._tag !== 'Blocked') return
    assert.strictEqual(evaluated.reason._tag, 'EvaluationLimit')
    if (evaluated.reason._tag !== 'EvaluationLimit') return
    assert.strictEqual(evaluated.reason.kind, 'CallDepth')
    // The blocked frame names the recursive walk itself after Option became an ordinary nominal
    // union and no longer inserts a separate accessor frame.
    assert.strictEqual(evaluated.reason.function.name, 'stepDepth')
  }),
)

it.effect(
  'exhausts the Wasm host stack on a deep recursive traversal',
  () =>
    Effect.gen(function* () {
      const found = yield* escalate(8_000, 512_000, (depth) =>
        Effect.gen(function* () {
          const snapshot = yield* realize(`recursion-stack-boundary/wasm/${depth}`, walk(depth))
          const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
          return runWasm(wasm.bytes)
        }),
      )
      assert.strictEqual(found.outcome._tag, 'Failed')
    }),
  300_000,
)

it.effect(
  'exhausts the native machine stack on a deep recursive traversal',
  () =>
    Effect.gen(function* () {
      const found = yield* escalate(200_000, 6_400_000, (depth) =>
        runNative(
          `recursion-stack-boundary/native/${depth}`,
          walk(depth),
          join(destinationRoot, `native-walk-${depth}`),
        ),
      )
      assert.strictEqual(found.outcome._tag, 'Failed')
    }),
  600_000,
)

/**
 * The sanctioned way out, pinned against the depth that kills the recursive form. One million
 * links is an order of magnitude past the native limit measured above and three past the Wasm one,
 * and the iterative teardown carries it on both, because its stack is one frame deep whatever the
 * chain length is.
 */
it.effect(
  'carries a chain past every recursive limit when the traversal is written iteratively',
  () =>
    Effect.gen(function* () {
      const depth = 1_000_000
      const source = drain(depth)
      const snapshot = yield* realize('recursion-stack-boundary/iterative', source)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      assert.deepEqual(runWasm(wasm.bytes), returned(0))

      assert.deepEqual(
        yield* runNative(
          'recursion-stack-boundary/iterative',
          source,
          join(destinationRoot, 'iterative'),
        ),
        returned(0),
      )
    }),
  600_000,
)

/**
 * Automatic cleanup is the same boundary reached from the other side, and it is the harder half.
 *
 * A traversal can be rewritten as a loop because the call site owns the shape of the traversal. A
 * `Drop` chain cannot: the recursion is `Box`'s hook dropping the element that is itself a `Box`,
 * and it runs wherever the value happens to go out of scope. A `Drop` hook may declare no failure
 * row and no capability requirement, so the escape offered for a traversal — express it as an
 * Effect that crosses a suspension boundary — is not open to it either. Cleanup cannot allocate,
 * cannot fail, and cannot suspend.
 *
 * What is left is an explicit iterative teardown, called before the value goes out of scope, and
 * the case below pins that it works. What it cannot cover is pinned after it.
 */
it.effect(
  'releases a shallow chain through recursive cleanup on all three engines',
  () =>
    Effect.gen(function* () {
      const depth = 64
      const source = dropped(depth)
      const snapshot = yield* realize('recursion-stack-boundary/drop-shallow', source)

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 0n)

      const events = Projections.allocationTraceEventsOf(evaluated)
      const acquires = events.filter((event) => event._tag === 'AllocationAcquire').length
      const releases = events.filter((event) => event._tag === 'AllocationRelease').length
      assert.strictEqual(acquires, depth)
      assert.strictEqual(releases, acquires)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      assert.deepEqual(runWasm(wasm.bytes), returned(0))

      assert.deepEqual(
        yield* runNative(
          'recursion-stack-boundary/drop-shallow',
          source,
          join(destinationRoot, 'drop-shallow'),
        ),
        returned(0),
      )
    }),
  180_000,
)

it.effect('blocks a deep recursive Drop on the evaluator call-depth limit', () =>
  Effect.gen(function* () {
    const snapshot = yield* realize('recursion-stack-boundary/drop-evaluator', dropped(4_000))
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Blocked')
    if (evaluated._tag !== 'Blocked') return
    assert.strictEqual(evaluated.reason._tag, 'EvaluationLimit')
    if (evaluated.reason._tag !== 'EvaluationLimit') return
    assert.strictEqual(evaluated.reason.kind, 'CallDepth')
    // The frame that ran out belongs to the standard library, not to the program: recursive drop
    // remains at the owning `Box` actor now that its optional link is an ordinary union.
    assert.strictEqual(evaluated.reason.function.module, 'silk/box')
    assert.strictEqual(evaluated.reason.function.name, 'drop@impl#0')
  }),
)

it.effect(
  'exhausts the Wasm host stack when a deep chain is dropped',
  () =>
    Effect.gen(function* () {
      const found = yield* escalate(8_000, 512_000, (depth) =>
        Effect.gen(function* () {
          const snapshot = yield* realize(
            `recursion-stack-boundary/drop-wasm/${depth}`,
            dropped(depth),
          )
          const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
          return runWasm(wasm.bytes)
        }),
      )
      assert.strictEqual(found.outcome._tag, 'Failed')
    }),
  300_000,
)

/**
 * Cleanup frames are narrower than the walk's, so this needs an order of magnitude more depth than
 * the traversal did before the guard page arrives — which is precisely why it is worth pinning
 * separately. "Deep enough to be safe for a walk" is not deep enough to be safe for a teardown.
 */
it.effect(
  'exhausts the native machine stack when a deep chain is dropped',
  () =>
    Effect.gen(function* () {
      const found = yield* escalate(1_000_000, 8_000_000, (depth) =>
        runNative(
          `recursion-stack-boundary/drop-native/${depth}`,
          dropped(depth),
          join(destinationRoot, `native-drop-${depth}`),
        ),
      )
      assert.strictEqual(found.outcome._tag, 'Failed')
    }),
  900_000,
)

/**
 * The gap the guidance cannot close, pinned so it is a known shape rather than a surprise.
 *
 * A value you hold can be drained before it goes out of scope. A value that never becomes yours
 * cannot: when construction fails partway, the half-built chain is released by the failure path,
 * and there is no point in the source at which an iterative teardown could be called on it. So a
 * program that always drains explicitly still meets recursive `Drop` on its allocation-failure
 * path, at whatever depth construction had reached.
 *
 * Shallow, this recovers the `OutOfMemoryError` and returns 1. Deep, the recovery never happens.
 */
it.effect(
  'has no teardown to offer when construction fails partway down a deep chain',
  () =>
    Effect.gen(function* () {
      const shallow = yield* realize('recursion-stack-boundary/failure-shallow', failedBuild(64))
      const shallowWasm = yield* Analysis.codegenWasm(shallow, { mode: 'release' })
      assert.deepEqual(runWasm(shallowWasm.bytes), returned(1))

      const found = yield* escalate(8_000, 512_000, (depth) =>
        Effect.gen(function* () {
          const snapshot = yield* realize(
            `recursion-stack-boundary/failure-wasm/${depth}`,
            failedBuild(depth),
          )
          const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
          const outcome = runWasm(wasm.bytes)
          // A recovered failure is a survived depth here, not a wrong answer.
          return outcome._tag === 'Returned' && outcome.detail === '1' ? returned(0) : outcome
        }),
      )
      assert.strictEqual(found.outcome._tag, 'Failed')
    }),
  300_000,
)
