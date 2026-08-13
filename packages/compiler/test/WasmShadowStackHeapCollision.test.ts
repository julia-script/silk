import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

/**
 * Characterizes the premature Wasm `unreachable` of #134, and attributes it.
 *
 * The Wasm backend keeps two bump regions in one linear memory, and they grow toward each other's
 * data rather than away from it:
 *
 * - the shadow stack starts at the end of static data and grows **up**, reserved a frame at a time
 *   by `reserveFrame` (`WasmBackend.ts`), and
 * - the heap starts at a **fixed** address — the free-list head table at `heapTableBase`, then
 *   blocks from `heapBase` — and grows up from there.
 *
 * `reserveFrame` guards the two ways a frame reservation can fail arithmetically: an `i32` wrap
 * (`frameEnd < frameBase`) and a refused `memory.grow`. It does not guard the reservation against
 * the heap, so a deep enough chain of frames walks straight over the allocator's table and then
 * over live blocks. Nothing traps at the moment of the overwrite — the corrupted value is read back
 * later, by whichever guard happens to see it first.
 *
 * That is why the trap lands where it does: the union-tag dispatch chain a `match` on a borrowed
 * union lowers to (`WasmBackend.ts` `emitDecisions`) ends in `unreachable` for a tag that names no
 * member. It is an impossible-state guard doing exactly its job on a discriminant that was
 * overwritten by a shadow-stack frame. The guard is correct; the memory beneath it is not.
 *
 * The three facts that make this a defect rather than ordinary stack exhaustion — each pinned below:
 *
 * 1. **The budget is tiny and fixed.** The shadow stack has `heapTableBase - staticEnd` bytes,
 *    about 64 KiB, whatever the host offers. This fixture spends 68 bytes per level, so it dies
 *    just under a thousand levels deep.
 * 2. **The host is not involved.** A recursion that needs no shadow-stack frame carries ten times
 *    that depth on the same engine before V8 raises `RangeError`.
 * 3. **Only the walk pays.** Building and releasing the same chain iteratively reserves one frame,
 *    so it is unaffected at any depth.
 *
 * Because the collision depth is arithmetic over three module-determined constants — static end,
 * heap base, and frame bytes per level — it is the same on every host. The depths below are
 * *derived* from the emitted module rather than written down, so this test pins the mechanism and
 * not one machine's numbers.
 *
 * This pins current, wrong behaviour on purpose: it is the tripwire #134 asked for. When the
 * backend gains a shadow-stack bound, the second case stops holding — a bounded stack should report
 * exhaustion, not corrupt the heap and trap somewhere downstream — and this test should be rewritten
 * to pin that instead. The #132/#133 characterization covers the ordinary, sanctioned boundary and
 * deliberately accepts either failure mode at these depths; this one is about the failure that
 * arrives *before* it.
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

effect fn build(depth: i32) -> Chain ! OutOfMemory ? &mut Allocator {
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

effect fn recover(error: OutOfMemory) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catch(measure(${depth}), recover) }`

/** Build the chain, walk it recursively, drain it. The walk is the only recursion. */
const walk = (depth: number): string =>
  program(
    `effect fn measure(depth: i32) -> i32 ! OutOfMemory {
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
    `effect fn measure(depth: i32) -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let built = run build(depth) |> Effect.provideMut(&mut allocator)
  let released = drain(move built)
  if released == depth { return 0 }
  return 2
}`,
    depth,
  )

/**
 * Recursion with nothing to spill: no borrow, no aggregate, no frame. The backend gives this
 * function no shadow-stack frame at all — the module has no linear memory — so its only limit is
 * the engine's, which is the control this test needs.
 */
const plainRecursion = (depth: number): string => `fn count(n: i32) -> i32 {
  if n == 0 { return 0 }
  return 1 + count(n - 1)
}

pub fn main() -> i32 {
  if count(${depth}) == ${depth} { return 0 }
  return 2
}`

interface Run {
  /** `silk_main`'s value, or `undefined` when the module failed instead of returning. */
  readonly value: number | undefined
  /** `Constructor: message` when it failed, so a failure names its kind and not just its presence. */
  readonly failure: string | undefined
  /**
   * Highest address below the allocator's own region that the module ever wrote, or `undefined`
   * without a memory.
   *
   * Between the end of static data and the heap page there is nothing but the shadow stack, so for
   * this fixture — whose static data is a handful of bytes — this is the high-water mark of
   * shadow-stack frames, and it stays written after the run. The window stops below the free-list
   * head table because the allocator writes that itself, and this must measure only the stack.
   */
  readonly stackReach: number | undefined
}

const globalInitializers = (wat: string): ReadonlyArray<number> =>
  Object.freeze(
    [...wat.matchAll(/\(global \(mut i32\) i32\.const (\d+)\)/g)].map((match) => Number(match[1])),
  )

/** The free-list head table opens the heap's page; blocks follow it in the same page. */
const heapPageOf = (heapBase: number): number => heapBase - (heapBase % 65536)

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
  const memory = instance.exports.__silk_memory_v1 as WebAssembly.Memory | undefined
  if (memory === undefined) return Object.freeze({ value, failure, stackReach: undefined })
  const words = new Int32Array(memory.buffer, 0, heapPageOf(heapBase) >> 2)
  let stackReach = 0
  for (let index = 0; index < words.length; index += 1) {
    if (words[index] !== 0) stackReach = index * 4
  }
  return Object.freeze({ value, failure, stackReach })
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
    // with neither has no linear memory and no shadow stack, which is the control case.
    const heapBase = globals.at(1) ?? 0
    return Object.freeze({ heapBase, ...execute(artifact.bytes, heapBase) })
  })

/** Bytes of shadow stack this fixture spends per level, and where its frames start, measured. */
const growth = Effect.gen(function* () {
  const shallow = yield* measure('shadow-stack-collision/growth/400', walk(400))
  const deeper = yield* measure('shadow-stack-collision/growth/800', walk(800))
  assert.strictEqual(shallow.value, 0, 'the walk must still be correct at 400')
  assert.strictEqual(deeper.value, 0, 'the walk must still be correct at 800')
  const shallowReach = shallow.stackReach ?? 0
  const deeperReach = deeper.stackReach ?? 0
  const perLevel = (deeperReach - shallowReach) / 400
  return Object.freeze({
    heapBase: shallow.heapBase,
    perLevel,
    base: shallowReach - perLevel * 400,
    shallowReach,
    deeperReach,
  })
})

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
      assert.isBelow(measured.deeperReach, heapPageOf(measured.heapBase))

      // And the budget is the gap between static data and a heap base that never moves: a fixed
      // ~64 KiB, which is what makes this arrive so early. Nothing about it scales with the host.
      assert.isBelow(measured.heapBase - measured.base, 1 << 17)
    }),
  120_000,
)

it.effect(
  'corrupts the chain and traps the level after its frames reach the heap',
  () =>
    Effect.gen(function* () {
      const measured = yield* growth
      // The first depth whose frames are written at or above the allocator's bump base.
      const crossing = Math.ceil((measured.heapBase - measured.base) / measured.perLevel)

      const before = yield* measure(
        `shadow-stack-collision/crossing/${crossing - 1}`,
        walk(crossing - 1),
      )
      assert.strictEqual(
        before.value,
        0,
        `depth ${crossing - 1} keeps its frames below the heap and stays correct`,
      )

      const after = yield* measure(`shadow-stack-collision/crossing/${crossing}`, walk(crossing))
      // #134 itself: one level further and the walk reads back a union tag that names no member,
      // so the dispatch chain's impossible-state guard traps. The failure is a Wasm trap, not the
      // host's `RangeError` — the engine still has stack to spare, as the next case shows.
      assert.isDefined(after.failure, `depth ${crossing} must fail`)
      assert.include(after.failure ?? '', 'RuntimeError', after.failure ?? '')
      assert.notInclude(after.failure ?? '', 'RangeError', after.failure ?? '')
    }),
  120_000,
)

it.effect(
  'has host stack to spare an order of magnitude past the depth that trapped',
  () =>
    Effect.gen(function* () {
      const measured = yield* growth
      const crossing = Math.ceil((measured.heapBase - measured.base) / measured.perLevel)

      // Same engine, same call depth times ten, no shadow-stack frame: it returns. So the trap
      // above is not the machine stack running out, and the guard that fired was not reporting a
      // limit the host had reached.
      const deep = yield* measure(
        `shadow-stack-collision/host-control/${crossing * 10}`,
        plainRecursion(crossing * 10),
      )
      assert.strictEqual(deep.value, 0)
      assert.isUndefined(deep.stackReach, 'a frameless recursion needs no linear memory at all')
    }),
  120_000,
)

it.effect(
  'leaves the same chain alone when nothing walks it',
  () =>
    Effect.gen(function* () {
      const measured = yield* growth
      const crossing = Math.ceil((measured.heapBase - measured.base) / measured.perLevel)

      // The asymmetry that names the cause. Construction is a loop, so it reserves one frame no
      // matter how long the chain is; the allocations it makes are identical to the walking case's.
      // Allocating the chain is fine at any depth. Walking it is what spends the shadow stack.
      for (const depth of [crossing, crossing * 10]) {
        const outcome = yield* measure(`shadow-stack-collision/unwalked/${depth}`, unwalked(depth))
        assert.strictEqual(outcome.value, 0, `building ${depth} links without walking them`)
        // One frame's worth, at any length: the loop reserves once and reuses it.
        assert.isBelow(outcome.stackReach ?? 0, 1024)
      }
    }),
  120_000,
)
