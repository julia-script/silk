import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as NameResolution from '../src/NameResolution.js'
import type * as Ownership from '../src/Ownership.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const counts = (
  events: ReadonlyArray<{ readonly _tag: string }>,
): { readonly acquires: number; readonly releases: number } =>
  Object.freeze({
    acquires: events.filter((event) => event._tag === 'AllocationAcquire').length,
    releases: events.filter((event) => event._tag === 'AllocationRelease').length,
  })

/**
 * A binary tree three levels deep: seven nodes, six of them held behind a box. This is the shape
 * the whole change exists to make expressible, and the shape whose release is invisible to every
 * other check the compiler has.
 */
const tree = `import silk.box { Box, make as boxMake, get as boxGet }

pub struct Leaf {}

pub struct Branch {
  left: Box<Tree>
  right: Box<Tree>
}

pub struct Shape {
  kind: Leaf | Branch
}

pub struct Tree {
  shape: Shape
  value: i32
}

effect fn leaf(value: i32) -> Tree ! OutOfMemoryError ? &mut Allocator {
  return Tree { shape: Shape { kind: Leaf {} }, value: value }
}

effect fn branch(left: Tree, right: Tree, value: i32) -> Tree ! OutOfMemoryError ? &mut Allocator {
  let boxedLeft = run boxMake<Tree>(move left)
  let boxedRight = run boxMake<Tree>(move right)
  return Tree {
    shape: Shape { kind: Branch { left: move boxedLeft, right: move boxedRight } },
    value: value
  }
}

fn total(self: &Tree) -> i32 {
  return self.value + shapeTotal(&self.shape)
}

fn shapeTotal(self: &Shape) -> i32 {
  return match &self.kind {
    Leaf nothing => 0
    Branch { left, right } => boxTotal(boxGet<Tree>(&left)) + boxTotal(boxGet<Tree>(&right))
  }
}

// The held value is reached through the one-element view the box hands back, which is how a
// recursive walk descends a level without unsafe code.
fn boxTotal(view: &[Tree]) -> i32 {
  return match &view[usize.ZERO] {
    Tree { shape, value } => value + shapeTotal(&shape)
  }
}

effect fn build() -> Tree ! OutOfMemoryError ? &mut Allocator {
  let leftLeft = run leaf(1)
  let leftRight = run leaf(2)
  let left = run branch(move leftLeft, move leftRight, 4)
  let rightLeft = run leaf(8)
  let rightRight = run leaf(16)
  let right = run branch(move rightLeft, move rightRight, 32)
  return run branch(move left, move right, 64)
}

effect fn sum() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let built = run build() |> Effect.provideMut(&mut allocator)
  let answer = total(&built)
  drop built
  return answer
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(sum(), recover) }`

/**
 * The pinned leak test, and the reason this lands with the feature rather than after it. Deleting
 * `impl<T> Drop for Box<T>` from byte-identical source produces no diagnostic, the same answer of
 * 127, and the same 42 out of Wasm — while tracing six acquires against two releases. Nothing but
 * the trace catches it, so the trace is the assertion.
 */
it.effect(
  'releases every level of a three-level box tree exactly once on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'box-heap-indirection/tree',
        ascii(tree),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 127n)

      // Six boxes, six acquires, six releases. An unhooked box traces six against two here.
      const traced = counts(Analysis.allocationTraceEventsOf(evaluated))
      assert.deepEqual(traced, { acquires: 6, releases: 6 })

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 127)
    }),
  120_000,
)

/**
 * The trace above only means something if the count it asserts is the count the boxed values
 * actually cost. The same tree with the boxes counted by hand: seven nodes, six of them boxed.
 * Wasm agrees on the answer, which is the part an unhooked box would also get right.
 */
it.effect('carries the same release count through the Wasm backend under both profiles', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/tree-debug',
      ascii(tree),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const debug = yield* Analysis.codegenWasm(snapshot, { mode: 'debug' })
    const debugInstance = new WebAssembly.Instance(new WebAssembly.Module(debug.bytes.slice()), {})
    assert.strictEqual((debugInstance.exports.silk_main as () => number)(), 127)
  }),
)

/**
 * The value a box holds is reachable three ways without unsafe code at the call site: shared
 * borrow, exclusive borrow, and a consuming move. `into` empties the box before handing the value
 * out, so the hook that still runs on the emptied box drops nothing and the storage releases once.
 */
const accessors = `import silk.box { Box, make as boxMake, get as boxGet, getMut as boxGetMut, into as boxInto }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut boxed = run boxMake<i32>(20) |> Effect.provideMut(&mut allocator)

  let borrowed = boxGet<i32>(&boxed)
  if borrowed[usize.ZERO] == 20 {} else { return 1 }

  let mut exclusive = boxGetMut<i32>(&mut boxed)
  exclusive[usize.ZERO] = 22

  let confirmed = boxGet<i32>(&boxed)
  if confirmed[usize.ZERO] == 22 {} else { return 2 }

  let taken = boxInto<i32>(move boxed)
  return taken + 20
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 3 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('borrows, mutates, and consumes a boxed value without unsafe code', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/accessors',
      ascii(accessors),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    // One box, one acquire, one release. Consuming the box still releases its storage exactly once.
    assert.deepEqual(counts(Analysis.allocationTraceEventsOf(evaluated)), {
      acquires: 1,
      releases: 1,
    })

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

/**
 * A box holding something that itself owns an allocation has to release both, in order: the held
 * value's own hook first, then the box's storage. This is the case `RawBufferCleanup` would
 * abandon if the box did not drop its element.
 */
const nested = `import silk.box { Box, make as boxMake, into as boxInto }
import silk.vector { Vector, make as vectorMake, append as vectorAppend, length as vectorLength }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut values = vectorMake<i32>()
  let first = run vectorAppend<i32>(&mut values, 21) |> Effect.provideMut(&mut allocator)
  let boxed = run boxMake<Vector<i32>>(move values) |> Effect.provideMut(&mut allocator)
  let recovered = boxInto<Vector<i32>>(move boxed)
  let answer = vectorLength<i32>(&recovered)
  drop recovered
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('releases an owning value held inside a box', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/nested',
      ascii(nested),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    // Two allocations, the vector's buffer and the box's storage, and both come back.
    const traced = counts(Analysis.allocationTraceEventsOf(evaluated))
    assert.strictEqual(traced.acquires, 2)
    assert.strictEqual(traced.releases, traced.acquires)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

/**
 * Depth is consumed by the runtime call stack rather than by the cleanup plan, so a long chain
 * releases every link. The plan itself stays one hook call wide however long the chain is.
 */
const chain = `import silk.box { Box, make as boxMake, into as boxInto }

pub struct End {}

pub struct Link {
  next: Box<Chain>
}

pub struct Chain {
  step: End | Link
}

effect fn extend(depth: i32) -> Chain ! OutOfMemoryError ? &mut Allocator {
  if depth == 0 {
    return Chain { step: End {} }
  }
  let inner = run extend(depth - 1)
  let boxed = run boxMake<Chain>(move inner)
  return Chain { step: Link { next: move boxed } }
}

effect fn build(depth: i32) -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let built = run extend(depth) |> Effect.provideMut(&mut allocator)
  drop built
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(build(64), recover) }`

it.effect('releases every link of a deep box chain', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/chain',
      ascii(chain),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    // Sixty-four links, sixty-four allocations, and none of them abandoned by the drop that
    // unwinds them one call frame at a time.
    assert.deepEqual(counts(Analysis.allocationTraceEventsOf(evaluated)), {
      acquires: 64,
      releases: 64,
    })

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

/**
 * The plan that releases the tree is finite because the box contributes one hook call, not the
 * held tree's own cleanup. Depth is spent by the call stack at runtime instead, which is why the
 * plan's recursion guard is never reached and no owner in the cycle is planned as having no
 * cleanup at all.
 */
it.effect('plans one hook call per box rather than inlining the held value', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/plan',
      ascii(tree),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const named = (plan: Ownership.CleanupPlan): string =>
      plan._tag === 'NoCleanup' || plan._tag === 'ParameterCleanup'
        ? plan._tag
        : `${plan._tag}(${Type.encode(plan.type)})`

    const plans = Analysis.cleanupExitsOf(snapshot, 'box-heap-indirection/plan').flatMap((exit) =>
      exit.releases.map((release) => release.cleanup),
    )

    // A box released on its own is one hook call. Nothing recursive appears in the plan.
    const boxed = plans.find(
      (plan) => plan._tag === 'HookCleanup' && plan.type.module === 'silk/box',
    )
    assert.isDefined(boxed)
    if (boxed?._tag !== 'HookCleanup') return
    assert.strictEqual(boxed.hook.module, 'silk/box')
    assert.strictEqual(named(boxed), 'HookCleanup(silk/box.Box<box-heap-indirection/plan.Tree>)')

    // The hook's own field cleanup releases the storage and nothing below it, which is exactly
    // why the hook has to drop the element itself.
    assert.deepEqual(
      boxed.inner._tag === 'StructCleanup'
        ? boxed.inner.fields.map((field) => field.cleanup._tag)
        : [boxed.inner._tag],
      ['RawBufferCleanup', 'UnionCleanup'],
    )
    // The storage cleanup is a leaf: it carries the allocation and no element plan at all, so
    // nothing in the box's own plan mentions the type it holds.
    const storage =
      boxed.inner._tag === 'StructCleanup' ? boxed.inner.fields.at(0)?.cleanup : undefined
    assert.deepEqual(storage?._tag === 'RawBufferCleanup' ? Object.keys(storage).sort() : [], [
      '_tag',
      'allocation',
      'type',
    ])

    // Tree -> Shape -> (Leaf | Branch) -> Branch -> Box<Tree>: the tree's own plan reaches the
    // same single hook call, and no owner along the way is planned as having no cleanup.
    const treePlan = plans.find(
      (plan) => plan._tag === 'StructCleanup' && plan.type.name === 'Tree',
    )
    assert.isDefined(treePlan)
    const shape = treePlan?._tag === 'StructCleanup' ? treePlan.fields.at(0)?.cleanup : undefined
    const kind = shape?._tag === 'StructCleanup' ? shape.fields.at(0)?.cleanup : undefined
    const branch =
      kind?._tag === 'UnionCleanup'
        ? kind.cases.find((entry) => Type.isNominal(entry.member) && entry.member.name === 'Branch')
            ?.cleanup
        : undefined
    const left = branch?._tag === 'StructCleanup' ? branch.fields.at(0)?.cleanup : undefined
    assert.strictEqual(left?._tag, 'HookCleanup')
  }),
)

/**
 * The self-hosting gate itself: the smallest recursive syntax node a compiler needs, and the same
 * cycle spread across two modules so it travels through the component walk rather than the
 * self-edge test.
 */
it.effect('accepts a recursive declaration through Box, within and across modules', () =>
  Effect.gen(function* () {
    const expr = `import silk.box { Box }

pub struct Expr {
  left: Box<Expr>
  right: Box<Expr>
  value: i32
}

pub fn main() -> i32 { return 0 }`
    const single = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/expr',
      ascii(expr),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(single), [])

    const index = yield* ModuleClosure.load({
      root: SourceFile.make(
        'ast/expression',
        ascii(
          'import silk.box { Box }\nimport ast.statement\n' +
            'pub struct Expression { body: Box<statement.Statement> value: i32 }\n' +
            'pub fn main() -> i32 { return 0 }',
        ),
      ),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'ast/statement',
              ascii(
                'import silk.box { Box }\nimport ast.expression\n' +
                  'pub struct Statement { head: Box<expression.Expression> }',
              ),
            ],
          ]),
        ),
      ),
      Effect.map((closure) => NameResolution.analyze(closure).index),
    )
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.code),
      [],
    )
    const facts = index.modules.flatMap((module) => module.structs)
    assert.deepEqual(
      facts
        .filter((struct) => struct.canonical._tag === 'Canonical')
        .filter(
          (struct) =>
            struct.canonical._tag === 'Canonical' && struct.canonical.id.module.startsWith('ast/'),
        )
        .map((struct) => struct.dependency._tag),
      ['Available', 'Available'],
    )
  }),
)

/**
 * Depth is spent by the call stack, so a chain deeper than the stack allows stops at the stack
 * rather than diverging or quietly returning a wrong answer. Under the evaluator that limit is
 * reported: a `CallDepth` evaluation limit, with the plan never having unrolled a single level of
 * the chain into itself.
 */
it.effect('stops a box chain at the call stack rather than in the cleanup plan', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/exhausted',
      ascii(chain.replace('build(64)', 'build(4096)')),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Blocked')
    if (evaluated._tag !== 'Blocked') return
    assert.strictEqual(evaluated.reason._tag, 'EvaluationLimit')
    if (evaluated.reason._tag !== 'EvaluationLimit') return
    assert.strictEqual(evaluated.reason.kind, 'CallDepth')
  }),
)

/**
 * A shape that would demand an infinite tower of drop instances is rejected by the existing
 * finite-discovery check rather than diverging. No new guard was added for it.
 */
it.effect('rejects a polymorphically recursive box shape', () =>
  Effect.gen(function* () {
    const polymorphic = `import silk.box { Box, make as boxMake }
import silk.vector { Vector, make as vectorMake }

pub struct Bad<T> {
  value: T
  next: Box<Bad<Box<T>>>
}

pub fn main() -> i32 {
  let held = vectorMake<Bad<i32>>()
  return 0
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/polymorphic',
      ascii(polymorphic),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0053'],
    )
  }),
)

/**
 * Box is ordinary standard-library source. No compiler phase knows it: it is not an intrinsic
 * nominal, it has no layout branch of its own, and its cleanup is the same hook-plus-fields plan
 * any other struct with a `Drop` conformance gets.
 */
it.effect('gives Box no compiler privilege', () =>
  Effect.gen(function* () {
    assert.isUndefined(Type.intrinsicNominals.get('Box'))
    assert.isFalse(Type.isIntrinsicNominal(Type.nominal('silk/box', 'Box', ['i32'])))

    const snapshot = yield* Analysis.ofSourceRealized(
      'box-heap-indirection/privilege',
      ascii(accessors),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    // The layout is an ordinary aggregate computed from its declared fields.
    const layout = Analysis.nominalLayout(snapshot, Type.nominal('silk/box', 'Box', ['i32']))
    assert.strictEqual(layout?._tag, 'LayoutEntry')
    if (layout?._tag !== 'LayoutEntry') return
    assert.strictEqual(layout.representation._tag, 'Aggregate')

    // Its drop path is a source declaration in the standard library, lowered like any other.
    const mir = Analysis.loweredMir(snapshot)
    assert.include(
      mir.functions.filter((fn) => fn.id.module === 'silk/box').map((fn) => fn.id.name),
      'drop@impl#0',
    )
  }),
)
