import * as NodeRuntime from '@effect/platform-node/NodeRuntime'
import * as Console from 'effect/Console'
import * as Data from 'effect/Data'
import * as Duration from 'effect/Duration'
import * as Effect from 'effect/Effect'
import * as Schema from 'effect/Schema'
import * as Analysis from '../dist/Analysis.js'
import * as CleanupPlan from '../dist/CleanupPlan.js'
import * as Elaboration from '../dist/Elaboration.js'
import * as Lifetime from '../dist/Lifetime.js'
import * as MovePath from '../dist/MovePath.js'
import * as NominalVariance from '../dist/NominalVariance.js'
import * as Ownership from '../dist/Ownership.js'
import * as ProjectAnalysis from '../dist/ProjectAnalysis.js'
import * as ResolutionWork from '../dist/ResolutionWork.js'
import * as SourceFile from '../dist/SourceFile.js'
import * as SourceResolver from '../dist/SourceResolver.js'
import * as Type from '../dist/Type.js'
import * as TypeCompatibility from '../dist/TypeCompatibility.js'
import * as TypeOutlives from '../dist/TypeOutlives.js'

class BenchmarkError extends Data.TaggedError('BenchmarkError') {
  /** @param {{ message: string, cause?: unknown }} details */
  constructor(details) {
    super(details)
  }
}
const bytes = (text) => Uint8Array.from(text, (character) => character.charCodeAt(0))
const range = (size) => Array.from({ length: size }, (_, ordinal) => ordinal)
const sumWork = (values) => {
  const sums = {}
  for (const value of values)
    for (const [key, count] of Object.entries(value ?? {}))
      if (typeof count === 'number') sums[key] = (sums[key] ?? 0) + count
  return sums
}

const generators = {
  wrappers: (size) => ({
    dimensions: { wrapperDepth: size },
    source: `struct W0<'a> { value: &'a i32 }\n${range(size)
      .map((index) => `struct W${index + 1}<'a> { inner: W${index}<'a> }`)
      .join('\n')}
fn read<'a>(value: W${size}<'a>) -> i32 { return value.${'inner.'.repeat(size)}value.* }`,
  }),
  unions: (size) => ({
    dimensions: { unionWidth: size, independentUnions: 1 },
    source: `union Choice<'a> { ${range(size)
      .map((index) => `V${index} { value: &'a i32 }`)
      .join(', ')} }
fn keep<'a>(value: Choice<'a>) -> Choice<'a> { return move value }`,
  }),
  loans: (size, invalid = false) => ({
    dimensions: { liveLoans: size, invalidations: invalid ? 1 : 0 },
    invalid,
    source: `fn inspect() -> i32 {
${range(size)
  .map((index) => `let mut v${index} = ${index}\nlet r${index} = &v${index}`)
  .join('\n')}
${invalid ? 'v0 = 100' : ''}
return ${range(size)
      .map((index) => `r${index}.*`)
      .join(' + ')} }`,
  }),
  reborrows: (size) => ({
    dimensions: { reborrowDepth: size },
    source: `fn inspect(value: i32) -> i32 { let r0 = &value
${range(size)
  .map((index) => `let r${index + 1} = &r${index}.*`)
  .join('\n')}
return r${size}.* }`,
  }),
  recursive: (size) => ({
    dimensions: { recursiveTypeComponent: size, recursiveCallComponent: size },
    source: `${range(size)
      .map((index) => `struct N${index}<'a> { next: &'a N${(index + 1) % size}<'a> }`)
      .join('\n')}
${range(size)
  .map((index) => `fn f${index}(value: i32) -> i32 { return f${(index + 1) % size}(value) }`)
  .join('\n')}
fn keep<'a>(value: &'a N0<'a>) -> &'a N0<'a> { return value }`,
  }),
  conformanceCandidates: (size) => ({
    dimensions: { declaredConformances: size, selectedCalls: 1 },
    source: `interface Marker {}
${range(size)
  .map((index) => `struct V${index} {} impl Marker for V${index} {}`)
  .join('\n')}
fn selected<T: Marker>(value: T) { drop value }
fn inspect() { selected(V0 {}) }`,
  }),
  movedFields: (size) => ({
    dimensions: { movedFields: size, branches: 0 },
    source: `struct Token { value: i32 }
struct Record { ${range(size)
      .map((index) => `f${index}: Token`)
      .join(' ')} }
fn inspect(record: Record) {
${range(size)
  .map((index) => `let moved${index} = move record.f${index}`)
  .join('\n')}
drop record }`,
  }),
  sparseArrays: (size) => {
    const length = 2 ** Math.min(size, 30)
    return {
      dimensions: { arrayLength: length, accessedIndices: 2 },
      source: `struct Token { value: i32 }
fn inspect(values: [Token; ${length}]) { let first = move values[0] let last = move values[${length - 1}] drop values }`,
    }
  },
  projections: (size) => ({
    dimensions: { projectionDepth: size, movedFields: 1 },
    source: `struct W0 { value: i32 }
${range(size)
  .map((index) => `struct W${index + 1} { inner: W${index} }`)
  .join('\n')}
fn inspect(value: W${size}) { let moved = move value.${Array.from({ length: size }, () => 'inner').join('.')} drop value }`,
  }),
  joins: (size, invalid = false) => ({
    dimensions: { independentlyJoinedFields: size, loopIterations: 0 },
    invalid,
    source: `struct Token { value: i32 }
struct Record { ${range(size)
      .map((index) => `f${index}: Token`)
      .join(' ')} }
fn inspect(record: Record, ${range(size)
      .map((index) => `b${index}: bool`)
      .join(', ')}) {
${range(size)
  .map((index) => `if b${index} { drop record.f${index} }`)
  .join('\n')}
${invalid ? 'let invalid = move record' : 'drop record'} }`,
  }),
  callbacks: (size) => ({
    dimensions: { anonymousCallbacks: size },
    source: `fn inspect() -> i32 {
${range(size)
  .map((index) => `let f${index} = fn(value: i32) -> i32 { return value }`)
  .join('\n')}
return ${range(size)
      .map((index) => `f${index}(${index})`)
      .join(' + ')} }`,
  }),
  effects: (size) => ({
    dimensions: { effectCaptureDepth: size },
    source: `fn inspect() -> i32 { let e0 = effect { return 1 }
${range(size)
  .map((index) => `let e${index + 1} = effect { return run e${index} }`)
  .join('\n')}
return run e${size} }`,
  }),
}

const retainedProofs = (view) => {
  const functions = [...view.results.values()].flatMap(Elaboration.executableFunctions)
  const ownership = [...view.ownership.values()].flatMap((module) => module.functions)
  const types = new Set()
  for (const fn of functions)
    Elaboration.visitStatementFacts(fn.statements, {
      expression: (expression) => {
        if (expression.type._tag === 'Available')
          Type.visit(expression.type.type, (type) => types.add(Type.key(type)))
      },
    })
  const stateNodes = new WeakSet()
  const stateCounts = new WeakMap()
  let uniqueStateNodes = 0
  let stateEdges = 0
  let maxStateNodes = 0
  const inspectState = (state) => {
    const previous = stateCounts.get(state)
    if (previous !== undefined) return previous
    if (!stateNodes.has(state)) {
      stateNodes.add(state)
      uniqueStateNodes += 1
      stateEdges += state.children.length
    }
    const size = 1 + state.children.reduce((sum, child) => sum + inspectState(child.state), 0)
    stateCounts.set(state, size)
    maxStateNodes = Math.max(maxStateNodes, size)
    return size
  }
  const conditional = new Set()
  const inspectConditional = (root, state, path = []) => {
    if (state.initialization === 'Maybe') conditional.add(`${root}/${MovePath.key(path)}`)
    for (const child of state.children)
      inspectConditional(root, child.state, [...path, child.selector])
  }
  for (const fn of ownership) {
    for (const transition of fn.transitions) {
      inspectState(transition.before)
      inspectState(transition.after)
      inspectConditional(
        `${fn.declaration.id.sourceId}:${fn.declaration.id.ordinal}:${Ownership.siteKey(transition.root)}`,
        transition.before,
      )
      inspectConditional(
        `${fn.declaration.id.sourceId}:${fn.declaration.id.ordinal}:${Ownership.siteKey(transition.root)}`,
        transition.after,
      )
    }
    for (const exit of fn.exits)
      for (const release of exit.releases) {
        inspectState(release.initialization)
        inspectConditional(
          `${fn.declaration.id.sourceId}:${fn.declaration.id.ordinal}:${Ownership.siteKey(release.binding.site)}`,
          release.initialization,
        )
      }
  }
  return {
    functions: functions.length,
    hiddenFunctions: [...view.results.values()].reduce(
      (sum, result) => sum + result.hiddenFunctions.length,
      0,
    ),
    retainedTypeKeys: types.size,
    syntaxPoints: functions.reduce((sum, fn) => sum + (fn.lifetimeFlow?.syntaxPointCount ?? 0), 0),
    lifetimeSolver: sumWork(
      functions.map((fn) =>
        fn.lifetimeFlow?.solution._tag === 'Solved' ? fn.lifetimeFlow.solution.work : undefined,
      ),
    ),
    finalBodyComparisons: sumWork(functions.map((fn) => fn.comparisonWork)),
    ownershipOperations: sumWork(ownership.map((fn) => fn.work)),
    loans: ownership.reduce((sum, fn) => sum + fn.loans.length, 0),
    loanReferents: ownership.reduce(
      (sum, fn) => sum + fn.loans.reduce((count, loan) => count + loan.referents.length, 0),
      0,
    ),
    transitions: ownership.reduce((sum, fn) => sum + fn.transitions.length, 0),
    uniqueStateNodes,
    retainedStateEdges: stateEdges,
    maximumStateNodes: maxStateNodes,
    conditionalPaths: conditional.size,
    nominalVariance: NominalVariance.derive(view.index).work,
    typeOutlives: TypeOutlives.context(view.index.modules).work,
    cleanupDerivation: CleanupPlan.work(view.index),
  }
}

const observe = (project, root, duration) => {
  const view = ProjectAnalysis.view(project, root)
  if (view === undefined) return { unavailable: root }
  const resolution = ResolutionWork.snapshot(ResolutionWork.ofIndex(view.index))
  return {
    elapsedMs: Duration.toMillis(duration),
    diagnostics: Analysis.diagnostics(view).map((diagnostic) => ({
      code: diagnostic.code,
      source: diagnostic.span.sourceId,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    queries: project.report.find((phase) => phase.phase === 'body-queries')?.counters,
    resolution: { totals: sumWork(resolution), byInitiator: resolution },
    phases: project.report.map(({ phase, elapsedMs }) => ({ phase, elapsedMs })),
    retainedProofs: retainedProofs(view),
  }
}

const checkSource = Effect.fnUntraced(
  /**
   * @param {string} family
   * @param {number} size
   * @param {{ source: string, dimensions: Record<string, number>, invalid?: boolean }} input
   */ function* (family, size, input) {
    const root = `benchmark/${family}/${size}`
    const [duration, project] = yield* Effect.timed(
      ProjectAnalysis.make([SourceFile.make(root, bytes(input.source))]).pipe(
        Effect.provide(SourceResolver.memory(new Map())),
      ),
    )
    const result = observe(project, root, duration)
    return {
      family,
      size,
      dimensions: input.dimensions,
      sourceBytes: input.source.length,
      expectedInvalid: input.invalid ?? false,
      expectationMatched: result.diagnostics?.length > 0 === (input.invalid ?? false),
      ...result,
    }
  },
)

const editWorkload = Effect.fnUntraced(
  /** @param {number} size */ function* (size) {
    const initialLeaf = `pub fn borrow<'a>(value: &'a i32) -> &'a i32 { return value }\nfn privateValue() -> i32 { return 1 }`
    const roots = range(size).map((index) =>
      SourceFile.make(
        `growth/Client${index}`,
        bytes(
          `import growth.Leaf\npub fn read<'a>(value: &'a i32) -> &'a i32 { return Leaf.borrow(value) }`,
        ),
      ),
    )
    /** @type {Array<[string, string, Array<SourceFile.SourceFile>]>} */
    const revisions = [
      ['cold', initialLeaf, roots],
      ['warm', initialLeaf, roots],
      ['private-body-edit', initialLeaf.replace('return 1', 'return 22'), roots],
      [
        'alpha-rename',
        initialLeaf.replaceAll("'a", "'renamed").replace('return 1', 'return 22'),
        roots,
      ],
      [
        'exported-bound-edit',
        initialLeaf.replace("borrow<'a>", "borrow<'a: 'static>").replace('return 1', 'return 22'),
        roots,
      ],
      [
        'additional-generic-call',
        initialLeaf,
        roots.map((root, index) =>
          index === 0
            ? SourceFile.make(
                root.id,
                bytes(
                  `import growth.Leaf\npub fn read<'a>(value: &'a i32) -> &'a i32 { return Leaf.borrow(value) }\nfn additional<'a>(value: &'a i32) -> &'a i32 { return Leaf.borrow(value) }`,
                ),
              )
            : root,
        ),
      ],
    ]
    const samples = []
    /** @type {ProjectAnalysis.ProjectAnalysis | undefined} */
    let previous
    /** @type {ProjectAnalysis.ProjectAnalysis | undefined} */
    let stable
    for (const [revision, leaf, currentRoots] of revisions) {
      // The additional-call edit branches from the initial checked interface, isolating call count.
      const basis = revision === 'additional-generic-call' ? stable : previous
      const computation =
        basis === undefined
          ? ProjectAnalysis.make(currentRoots)
          : ProjectAnalysis.revise(basis, currentRoots)
      const [duration, project] = yield* Effect.timed(
        computation.pipe(
          Effect.provide(SourceResolver.memory(new Map([['growth/Leaf', bytes(leaf)]]))),
        ),
      )
      if (stable === undefined) stable = project
      previous = project
      samples.push({
        family: 'moduleFanout',
        size,
        dimensions: { importingModules: size, revision },
        ...observe(project, currentRoots[0]?.id ?? '', duration),
      })
    }
    return samples
  },
)

const binderWorkload = (size) => {
  const parameters = (name) =>
    range(size).map((ordinal) =>
      Lifetime.bound({ module: 'benchmark', name }, ordinal, `a${ordinal}`, [0]),
    )
  const left = parameters('left')
  const right = parameters('right')
  const callable = (binders) =>
    Type.callable(
      binders.map((lifetime) => Type.reference('Shared', 'i32', lifetime)),
      Type.reference('Shared', 'i32', binders[0] ?? Lifetime.staticLifetime),
      {
        environment: Lifetime.staticLifetime,
        lifetimeBinders: binders,
        lifetimeBounds: [],
        typeOutlives: [],
      },
    )
  const context = TypeCompatibility.context()
  const source = callable(left)
  const target = callable(right)
  let result
  for (let repetition = 0; repetition < 32; repetition += 1)
    result = TypeCompatibility.check(source, target, context)
  return {
    family: 'binderWidth',
    size,
    dimensions: { binderWidth: size, repeatedComparisons: 32 },
    compatible: result !== undefined && TypeCompatibility.isCompatible(result),
    work: { ...context.work },
  }
}

const main = Effect.fn('LifetimeBenchmark.main')(function* () {
  const argument = process.argv
    .find((argument) => argument.startsWith('--sizes='))
    ?.slice('--sizes='.length)
  const sizes = (argument ?? '4,8,16').split(',').map(Number)
  if (sizes.some((size) => !Number.isSafeInteger(size) || size < 2 || size > 128))
    return yield* new BenchmarkError({ message: 'sizes must be integers from 2 through 128' })
  const results = []
  for (const size of sizes) {
    for (const [family, generate] of Object.entries(generators))
      results.push(yield* checkSource(family, size, generate(size)))
    for (const family of ['loans', 'joins'])
      results.push(yield* checkSource(`${family}-invalid`, size, generators[family](size, true)))
    results.push(binderWorkload(size))
    results.push(...(yield* editWorkload(size)))
  }
  const memory = yield* Effect.try({
    try: () => ({
      heapUsed: process.memoryUsage().heapUsed,
      processMaxRss: process.resourceUsage().maxRSS,
    }),
    catch: (cause) =>
      new BenchmarkError({ message: 'cannot read process resource observation', cause }),
  })
  yield* Console.log(
    yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown, { space: 2 }))({
      environment: {
        node: process.version,
        platform: process.platform,
        arch: process.arch,
        pipeline: 'target-neutral frontend; no backend emission or optimizer',
        memoryUnits: { heapUsed: 'bytes', processMaxRss: 'host resourceUsage maxRSS units' },
        ...memory,
      },
      dimensions: sizes,
      attribution: {
        retainedProofs:
          'Cumulative proof artifacts in this snapshot, including reused bodies; query counters report actual work for this revision.',
        comparisons:
          'Final source-body comparison contexts; anonymous preliminary analysis is not included.',
        conditionalPaths:
          'Required conditional state paths; MIR flag construction is not executed in this frontend workload.',
        resolution:
          'Actual reachable-index name/path/associated/conformance discovery and explicitly observed selected-call provider loops, keyed by their initiating request. Intermediate discarded declaration indexes and unobserved standalone provider oracles are outside this report. Lifetime actors receive no resolver.',
        residualOwnership: 'Not requested by this frontend-only workload.',
      },
      results,
    }),
  )
})

NodeRuntime.runMain(main())
