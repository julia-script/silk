import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import { wasmMemoryExport } from '../src/StandardStreams.js'
import { statusAddress, statusStackOverflow } from '../src/WasmBackend.js'

/**
 * Pins the shadow stack's bound against the heap — the fix for #134, and the mechanism it replaced.
 *
 * The Wasm backend keeps two bump regions in one linear memory, and they grow toward each other's
 * data rather than away from it:
 *
 * - the shadow stack starts at the end of static data and grows **up**, reserved a frame at a time
 *   by `reserveFrame` (`WasmBackend.ts`), and
 * - the heap starts at a **fixed** address — the free-list head table at `heapTableBase`, then
 *   blocks from `heapBase` — and grows up from there.
 *
 * `reserveFrame` used to guard only the two ways a reservation can fail arithmetically: an `i32`
 * wrap (`frameEnd < frameBase`) and a refused `memory.grow`. Neither one looks at the heap, so a
 * deep enough chain of frames walked straight over the allocator's table and then over live blocks.
 * Nothing trapped at the moment of the overwrite — the corrupted value was read back later, by
 * whichever guard happened to see it first, which for a borrowed `match` was the union-tag dispatch
 * chain's impossible-state fallback. That guard was right; the memory beneath it was not. At other
 * depths there was no trap at all and the walk simply returned a wrong answer.
 *
 * `reserveFrame` now compares `frameEnd` against the heap's base before the stack pointer moves, so
 * a reservation that would cross reports instead of writing. The report is deliberate: it names
 * itself in the status word at `statusAddress` and rewinds the stack pointer, so the trap is one
 * legible event rather than a module that answers every later call the same way.
 *
 * The four facts that make this a bound rather than ordinary stack exhaustion — each pinned below:
 *
 * 1. **The budget is tiny and fixed.** The shadow stack has `heapTableBase - staticEnd` bytes,
 *    about 64 KiB, whatever the host offers. This fixture spends 68 bytes per level, so it stops
 *    just under a thousand levels deep.
 * 2. **Crossing reports.** Past that depth the module traps at the reservation and names the reason,
 *    and its frames never reach the heap page.
 * 3. **The host is not involved.** A recursion that needs no shadow-stack frame carries the depth
 *    that reported on the same engine, and at least double it, before V8 raises `RangeError`. The
 *    headroom is measured rather than assumed, because engines draw their own execution-stack
 *    bound differently across versions and platforms.
 * 4. **Only the walk pays.** Building and releasing the same chain iteratively reserves one frame,
 *    so it is unaffected at any depth.
 *
 * Because the bound is arithmetic over three module-determined constants — static end, heap base,
 * and frame bytes per level — it is the same on every host. The depths below are *derived* from the
 * emitted module rather than written down, so this test pins the mechanism and not one machine's
 * numbers.
 *
 * The last case is the one that matters most: hitting the bound must leave the allocator alone. It
 * allocates, crosses, and then checks both that the heap's bytes are exactly what an uncrossed run
 * leaves and that the allocator still answers correctly afterwards.
 *
 * The #132/#133 characterization covers the ordinary, sanctioned depth boundary; this one is about
 * the budget that runs out before it.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * A chain of boxed nodes with three phases that can be measured apart: an iterative `build`, a
 * recursive `stepDepth`/`viewDepth` walk, and an iterative `drain` teardown. Construction and
 * teardown are loops so that neither contributes call depth, which leaves the walk as the only
 * phase that can spend the stack — of either kind.
 *
 * The walk is the borrow-heavy half: each level spills the borrowed `Box` into its frame, hands
 * the frame address to `box.get`, and reads the lanes back, which is what puts a frame on the
 * shadow stack at every level. Automatic cleanup is avoided deliberately — `Box`'s drop hook
 * descends into the box it holds, so releasing a deep chain is itself a recursion, and that is the
 * separate, sanctioned boundary of #132/#133 rather than the defect under test here.
 */
const prelude = `import silk.box { Box, make as boxMake, get as boxGet, into as boxInto }

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

const program = (body: string, depth: number): string => `${prelude}
${body}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(measure(${depth}), recover) }`

/** Build the chain, walk it recursively, drain it. The walk is the only recursion. */
const walk = (depth: number): string =>
  program(
    `effect fn measure(depth: i32) -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let built = run build(depth) |> Effect.provideMut(&mut allocator)
  let counted = stepDepth(&built.step)
  let released = drain(move built)
  if counted == released { return 0 }
  return 2
}`,
    depth,
  )

/** The same allocations, the same teardown, no walk: this program never recurses at all. */
const unwalked = (depth: number): string =>
  program(
    `effect fn measure(depth: i32) -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let built = run build(depth) |> Effect.provideMut(&mut allocator)
  let released = drain(move built)
  if released == depth { return 0 }
  return 2
}`,
    depth,
  )

/**
 * Recursion with nothing to spill: no borrow, no aggregate, no frame. The backend gives these
 * functions no shadow-stack frame at all — the module has no linear memory — so their only limit
 * is the engine's own execution stack, which is the control this test needs. The probe takes its
 * depth as a parameter rather than baking it into the source, so one instance answers at every
 * depth the host-control case walks.
 */
const framelessProbe = `fn count(n: i32) -> i32 {
  if n == 0 { return 0 }
  return 1 + count(n - 1)
}

pub fn probe(depth: i32) -> i32 {
  if count(depth) == depth { return 0 }
  return 2
}

pub fn main() -> i32 { return probe(1) }`

interface Run {
  /** `silk_main`'s value, or `undefined` when the module failed instead of returning. */
  readonly value: number | undefined
  /** `Constructor: message` when it failed, so a failure names its kind and not just its presence. */
  readonly failure: string | undefined
  /**
   * Highest address below the allocator's own region that the module ever wrote, or `undefined`
   * without a memory — which no fixture measured this way may be: every one of them spends shadow
   * stack, and `measuredReach` turns that `undefined` into a named failure. (The frameless
   * host-control module also has no memory, but it never comes through here.)
   *
   * Between the end of static data and the heap page there is nothing but the shadow stack, so for
   * this fixture — whose static data is a handful of bytes — this is the high-water mark of
   * shadow-stack frames, and it stays written after the run. The window stops below the free-list
   * head table because the allocator writes that itself, and this must measure only the stack.
   */
  readonly stackReach: number | undefined
  /** The status word a deliberate trap leaves behind, or `undefined` without a memory. */
  readonly status: number | undefined
}

const globalInitializers = (wat: string): ReadonlyArray<number> =>
  Object.freeze(
    [...wat.matchAll(/\(global \(mut i32\) i32\.const (\d+)\)/g)].map((match) => Number(match[1])),
  )

/** The free-list head table opens the heap's page; blocks follow it in the same page. */
const heapPageOf = (heapBase: number): number => heapBase - (heapBase % 65536)

/**
 * The shadow stack's high-water mark: the highest nonzero word between static data and the heap
 * page, which is where its frames live and where they stay written after the run.
 *
 * The scan starts past the status word, which sits in the reserved hole at the bottom of memory and
 * belongs to the trap report rather than to any frame.
 */
const reachOf = (memory: WebAssembly.Memory, heapBase: number): number => {
  const words = new Int32Array(memory.buffer, 0, heapPageOf(heapBase) >> 2)
  let stackReach = 0
  for (let index = (statusAddress >> 2) + 1; index < words.length; index += 1) {
    if (words[index] !== 0) stackReach = index * 4
  }
  return stackReach
}

const execute = (bytes: Uint8Array, heapBase: number): Run => {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
  let value: number | undefined
  let failure: string | undefined
  try {
    value = (instance.exports.silk_main as () => number)()
  } catch (error) {
    const caught = error as Error
    failure = `${caught.constructor.name}: ${caught.message}`
  }
  // The backend's own constant, not a copy of it: every reading this test takes comes through this
  // export, so a rename must move the test with it rather than leave it reading `undefined`.
  const memory = instance.exports[wasmMemoryExport] as WebAssembly.Memory | undefined
  if (memory === undefined)
    return Object.freeze({ value, failure, stackReach: undefined, status: undefined })
  return Object.freeze({
    value,
    failure,
    stackReach: reachOf(memory, heapBase),
    status: new Int32Array(memory.buffer, statusAddress, 1)[0],
  })
}

/**
 * The stack reading a fixture that spends shadow stack must have.
 *
 * `execute` reports `undefined` when the module exposes no memory, which is a real answer for the
 * frameless control and for nothing else here. Folding it into a zero instead would let a bound
 * this test cannot see read as a bound that held: a reach of zero is below every limit, and a
 * per-level growth of zero derives a depth that straddles nothing. So it fails, and names the
 * export it wanted, rather than measuring an absence.
 */
const measuredReach = (run: Run, id: string): number => {
  const { stackReach } = run
  if (stackReach === undefined)
    throw new Error(`${id} exposed no ${wasmMemoryExport}: its shadow stack cannot be measured`)
  return stackReach
}

/**
 * Emits the source and runs it, reporting both what the module did and how far its shadow stack
 * climbed. The heap's bump base comes out of the module's own second global rather than a constant
 * repeated here, so a backend that moves the heap moves every expectation below with it.
 */
const measure = (id: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(id, ascii(source), 'wasm32-unknown-unknown')
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      id,
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const globals = globalInitializers(artifact.wat)
    // The backend declares the shadow-stack pointer first and the heap pointer second; a module
    // with neither has no linear memory and no shadow stack, which `measuredReach` names below.
    const heapBase = globals.at(1) ?? 0
    return Object.freeze({ heapBase, ...execute(artifact.bytes, heapBase) })
  })

/**
 * Bytes of shadow stack a fixture spends per level, where its frames start, and the two depths that
 * straddle the bound — all measured off the emitted module rather than written down.
 *
 * `perLevel` and `base` describe the highest byte the frames actually *wrote*, which sits a little
 * below the frame end the bound compares, and by a different amount per fixture. That difference is
 * at most one frame, and a level reserves at least one frame, so two levels of margin always covers
 * it: `bounded` is deep enough that its frame end is certainly past the limit, and `clear` shallow
 * enough that it is certainly short of it.
 */
const growthOf = (
  id: string,
  source: (depth: number) => string,
  shallowDepth: number,
  deeperDepth: number,
) =>
  Effect.gen(function* () {
    const shallow = yield* measure(`${id}/growth/${shallowDepth}`, source(shallowDepth))
    const deeper = yield* measure(`${id}/growth/${deeperDepth}`, source(deeperDepth))
    assert.strictEqual(shallow.value, 0, `${id} must still be correct at ${shallowDepth}`)
    assert.strictEqual(deeper.value, 0, `${id} must still be correct at ${deeperDepth}`)
    const shallowReach = measuredReach(shallow, `${id}/growth/${shallowDepth}`)
    const deeperReach = measuredReach(deeper, `${id}/growth/${deeperDepth}`)
    const perLevel = (deeperReach - shallowReach) / (deeperDepth - shallowDepth)
    const base = shallowReach - perLevel * shallowDepth
    // The heap's page is the first address the shadow stack may not reach: the free-list head table
    // opens it. The module's own heap global names it, so a backend that moves the heap moves this.
    const limit = heapPageOf(shallow.heapBase)
    const span = (limit - base) / perLevel
    return Object.freeze({
      heapBase: shallow.heapBase,
      limit,
      perLevel,
      base,
      shallowReach,
      deeperReach,
      span,
      bounded: Math.ceil(span),
      clear: Math.floor(span) - 2,
    })
  })

const growth = growthOf('shadow-stack-collision', walk, 400, 800)

it.effect(
  'grows a shadow-stack frame per borrowed level, straight at the heap',
  () =>
    Effect.gen(function* () {
      const measured = yield* growth

      // Linear in the depth, because every level reserves the same two frames.
      assert.isAbove(measured.perLevel, 0)
      assert.strictEqual(measured.perLevel, Math.trunc(measured.perLevel))
      assert.isBelow(measured.base, measured.perLevel)

      // Both depths are still short of the heap, which is why both are still correct.
      assert.isBelow(measured.deeperReach, measured.limit)

      // And the budget is the gap between static data and a heap base that never moves: a fixed
      // ~64 KiB, which is what makes this arrive so early. Nothing about it scales with the host.
      assert.isBelow(measured.limit - measured.base, 1 << 17)
    }),
  120_000,
)

it.effect(
  'reports the bound rather than writing its frames over the heap',
  () =>
    Effect.gen(function* () {
      const measured = yield* growth

      const before = yield* measure(
        `shadow-stack-collision/crossing/${measured.clear}`,
        walk(measured.clear),
      )
      assert.strictEqual(
        before.value,
        0,
        `depth ${measured.clear} keeps its frames below the heap and stays correct`,
      )
      // The status word is read through the module's memory export, so name a missing channel as
      // itself. Without this, losing the export reads as `undefined` here — a bound that reported
      // nothing looks exactly like a bound this test can no longer see.
      assert.isDefined(
        before.status,
        `depth ${measured.clear} must expose ${wasmMemoryExport} to report through`,
      )
      assert.strictEqual(before.status, 0, 'a run that stays inside its budget reports nothing')

      // Past the budget the reservation itself refuses, before the stack pointer moves. What used
      // to happen here — #134 — was that the frames kept climbing over the allocator, and the walk
      // read back a union tag that named no member some levels later, or returned a wrong answer
      // and raised nothing at all.
      const after = yield* measure(
        `shadow-stack-collision/crossing/${measured.bounded}`,
        walk(measured.bounded),
      )
      assert.isDefined(after.failure, `depth ${measured.bounded} must report`)
      // A deliberate Wasm trap, not the host's `RangeError`: the engine still has stack to spare,
      // as the next case shows.
      assert.include(after.failure ?? '', 'RuntimeError', after.failure ?? '')
      assert.notInclude(after.failure ?? '', 'RangeError', after.failure ?? '')

      // And it says which trap it was, which is what makes this diagnosable rather than one more
      // indistinguishable `unreachable`.
      assert.strictEqual(after.status, statusStackOverflow)

      // The point of the bound: the frames stopped below the heap's page instead of writing into
      // the allocator's table.
      assert.isBelow(after.stackReach ?? Number.POSITIVE_INFINITY, measured.limit)
    }),
  120_000,
)

it.effect(
  'has host stack to spare past the depth that reported',
  () =>
    Effect.gen(function* () {
      const measured = yield* growth

      const id = 'shadow-stack-collision/host-control'
      const snapshot = yield* Analysis.ofSourceRealized(
        id,
        ascii(framelessProbe),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        id,
      )
      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.isUndefined(
        instance.exports[wasmMemoryExport],
        'a frameless recursion needs no linear memory at all',
      )
      // Non-entry functions carry a mangled symbol; the declaration's name survives inside it.
      const probeExport = Object.keys(instance.exports).find((name) => name.includes('_probe__'))
      assert.isDefined(probeExport, 'the control must export its probe')
      const probe = instance.exports[probeExport ?? ''] as (depth: number) => number

      // The host's own bound is measured here rather than assumed, because it is the one number
      // in this file the emitted module cannot derive: V8 sizes its execution stack differently
      // across versions and platforms, so a fixed multiple of the shadow-stack depth fits some
      // engines and not others. `RangeError` is the host refusing; anything else is a real
      // failure and propagates.
      const completes = (depth: number): boolean => {
        try {
          assert.strictEqual(probe(depth), 0, `frameless recursion at ${depth} must count itself`)
          return true
        } catch (error) {
          if (error instanceof RangeError) return false
          throw error
        }
      }

      // Same engine, the exact call depth whose reservation reported, no shadow-stack frame: it
      // returns. So the report above is not the machine stack running out, and the bound that
      // fired was not the host's.
      assert.isTrue(
        completes(measured.bounded),
        `the host must carry ${measured.bounded} frameless levels — the depth that reported`,
      )

      // And with room to spare: doubling the depth until the host refuses shows the engine's own
      // bound sits at least a factor of two past the module's, which is what makes the two
      // distinguishable. The walk is capped because some hosts never refuse within useful range —
      // by then the headroom is proven many times over.
      let headroom = 1
      while (headroom < 16 && completes(measured.bounded * headroom * 2)) headroom *= 2
      assert.isAtLeast(
        headroom,
        2,
        `the host refused between ${measured.bounded} and ${measured.bounded * 2} frameless levels — too close to the shadow-stack bound to tell the module's report from the engine's`,
      )
    }),
  120_000,
)

it.effect(
  'leaves the same chain alone when nothing walks it',
  () =>
    Effect.gen(function* () {
      const measured = yield* growth
      const crossing = measured.bounded

      // The asymmetry that names the cause. Construction is a loop, so it reserves one frame no
      // matter how long the chain is; the allocations it makes are identical to the walking case's.
      // Allocating the chain is fine at any depth. Walking it is what spends the shadow stack.
      for (const depth of [crossing, crossing * 10]) {
        const id = `shadow-stack-collision/unwalked/${depth}`
        const outcome = yield* measure(id, unwalked(depth))
        assert.strictEqual(outcome.value, 0, `building ${depth} links without walking them`)
        // One frame's worth, at any length: the loop reserves once and reuses it. Measured, not
        // defaulted — a module with no memory to read would otherwise clear this bar with a zero.
        assert.isBelow(measuredReach(outcome, id), 1024)
      }
    }),
  120_000,
)

/**
 * The second fixture, for the question the first one cannot ask: what the heap looks like after the
 * bound is hit.
 *
 * `probe` builds a short chain, walks it, and drains it. That leaves the allocator with exactly the
 * bookkeeping the unbounded stack used to overwrite — a free-list head table with real addresses in
 * it, and a bump pointer that has moved. It is exported on its own so a host can ask the allocator
 * a question *after* the module has reported.
 *
 * `main` runs `probe` once and then recurses. The recursion carries a wide borrowed record so that
 * a level costs enough shadow stack to reach the bound well short of the host's own limit, and it
 * allocates nothing — so the heap's contents do not depend on the depth, and a run that crosses is
 * directly comparable, byte for byte, against one that does not.
 */
const lanes = Object.freeze(Array.from({ length: 16 }, (_, index) => index))
const allocatingCross = (depth: number): string => `${prelude}
pub struct Wide {
${lanes.map((lane) => `  lane${lane}: i32`).join('\n')}
}

fn spend(wide: &Wide, remaining: i32) -> i32 {
  if remaining == 0 { return wide.lane0 }
  let next = Wide {
${lanes.map((lane) => `    lane${lane}: wide.lane${lane}`).join(',\n')}
  }
  return spend(&next, remaining - 1)
}

effect fn probing() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let built = run build(8) |> Effect.provideMut(&mut allocator)
  let counted = stepDepth(&built.step)
  let released = drain(move built)
  if counted == released { return 0 }
  return 2
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn probe() -> i32 { return run Effect.catchAll(probing(), recover) }

pub fn main() -> i32 {
  let seed = Wide { ${lanes.map((lane) => `lane${lane}: ${lane}`).join(', ')} }
  return probe() + spend(&seed, ${depth})
}`

interface Crossing {
  readonly failure: string | undefined
  readonly value: number | undefined
  readonly status: number
  readonly stackReach: number
  /** The allocator's free-list heads, read straight out of the table that opens the heap page. */
  readonly heads: ReadonlyArray<number>
  /** The heap page's bytes, as left by the run. */
  readonly heap: string
  /** What `probe` answered when it was called again after the run, and the heap it left behind. */
  readonly after: number | string
  readonly heapAfter: string
}

/**
 * Runs the allocating fixture at one depth and reports the heap on both sides of a second `probe`.
 *
 * The two calls share one instance, so the second one sees whatever the first left in the
 * allocator: this is the module answering a question after it has reported, not a fresh start.
 */
const cross = (id: string, depth: number) =>
  Effect.gen(function* () {
    const source = allocatingCross(depth)
    const snapshot = yield* Analysis.ofSourceRealized(id, ascii(source), 'wasm32-unknown-unknown')
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      id,
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const heapBase = globalInitializers(artifact.wat).at(1) ?? 0
    const page = heapPageOf(heapBase)
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const memory = instance.exports[wasmMemoryExport] as WebAssembly.Memory | undefined
    // Every byte this case compares is read through that export, so its absence is a lost
    // observation and not a heap that happens to match.
    if (memory === undefined)
      throw new Error(`${id} exposed no ${wasmMemoryExport}: its heap cannot be read back`)
    // Non-entry functions carry a mangled symbol; the declaration's name survives inside it.
    const probeExport = Object.keys(instance.exports).find((name) => name.includes('_probe__'))
    assert.isDefined(probeExport, 'the fixture must export its probe')
    const call = (name: string): number | string => {
      try {
        return (instance.exports[name] as () => number)()
      } catch (error) {
        const caught = error as Error
        return `${caught.constructor.name}: ${caught.message}`
      }
    }
    // A window wide enough for the free-list table and every block this fixture allocates.
    const heapBytes = (): string =>
      Array.from(new Uint8Array(memory.buffer, page, 2048), (byte) =>
        byte.toString(16).padStart(2, '0'),
      ).join('')
    const outcome = call('silk_main')
    const heap = heapBytes()
    const status = new Int32Array(memory.buffer, statusAddress, 1)[0] ?? 0
    const stackReach = reachOf(memory, heapBase)
    const heads = Array.from(new Int32Array(memory.buffer, page, (heapBase - page) >> 2))
    const after = call(probeExport ?? '')
    return Object.freeze({
      heapBase,
      page,
      failure: typeof outcome === 'string' ? outcome : undefined,
      value: typeof outcome === 'number' ? outcome : undefined,
      status,
      stackReach,
      heads,
      heap,
      after,
      heapAfter: heapBytes(),
    }) satisfies Crossing & { readonly heapBase: number; readonly page: number }
  })

it.effect(
  'leaves the allocator intact and answering after the bound is hit',
  () =>
    Effect.gen(function* () {
      const measured = yield* growthOf('shadow-stack-cross', allocatingCross, 200, 600)

      // The control: the same allocations, a recursion that stays inside the budget.
      const control = yield* cross(`shadow-stack-cross/clear/${measured.clear}`, measured.clear)
      assert.strictEqual(control.value, 0, `depth ${measured.clear} stays inside the budget`)
      assert.strictEqual(control.status, 0)
      // The allocator really did leave bookkeeping behind for a crossing run to damage.
      assert.isTrue(
        control.heads.some((head) => head !== 0),
        'the probe must leave blocks on a free list',
      )

      // The same allocations, then a recursion that crosses.
      const crossed = yield* cross(
        `shadow-stack-cross/bounded/${measured.bounded}`,
        measured.bounded,
      )
      assert.isDefined(crossed.failure, `depth ${measured.bounded} must report`)
      assert.strictEqual(crossed.status, statusStackOverflow)
      assert.isBelow(crossed.stackReach, measured.limit)

      // This is the criterion. Both runs allocated identically and only one of them crossed, so any
      // difference in these bytes is a frame that reached the heap. Before the bound, the free-list
      // heads at this depth read back as frame data rather than as block addresses.
      assert.deepEqual(crossed.heads, control.heads, 'the free-list table must be untouched')
      assert.strictEqual(crossed.heap, control.heap, 'the heap page must be untouched')

      // And the allocator is not merely unwritten but still correct: asked again, on the same
      // instance that just reported, it serves and reclaims the same chain and answers the same.
      assert.strictEqual(crossed.after, 0, 'the allocator must still answer after the report')
      assert.strictEqual(crossed.heapAfter, control.heapAfter)
    }),
  180_000,
)
