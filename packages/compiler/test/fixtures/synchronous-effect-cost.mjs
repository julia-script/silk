import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Mir from '../../dist/Mir.js'

const encoder = new TextEncoder()
const hash = (value) => createHash('sha256').update(value).digest('hex')
const occurrences = (text, pattern) => [...text.matchAll(pattern)].length
const suspensionVocabulary = Object.freeze([
  'suspend',
  'suspended',
  'continuation',
  'scheduler',
  'fiber',
  'dispatch',
  'resume',
  'trampoline',
  'pending',
  'atomic',
])
const suspensionTerm = new RegExp(`(?:${suspensionVocabulary.join('|')})`, 'i')
const containsSuspensionTerm = (value) =>
  suspensionTerm.test(value.replaceAll(/\bsuspended=false\b/gi, ''))
const normalize = (text) =>
  text
    .replaceAll(/cost\/[a-z0-9-]+\/(?:native|wasm)/g, 'cost/<case>/<target>')
    .replaceAll(/\[\d+,\s*\d+\)/g, '[span)')
    .replaceAll(/\/var\/folders\/[^\s"']+/g, '<temporary>')

const sources = Object.freeze([
  {
    id: 'pure-imperative',
    pair: 'pure-pipe',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'pure-piped',
    pair: 'pure-pipe',
    kind: 'effect',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return 41 |> addOne }`,
  },
  {
    id: 'map-imperative',
    pair: 'map',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'map-effect',
    pair: 'map',
    kind: 'effect',
    expected: 42,
    source: `import silk.effects as Effect

fn addOne(value: i32) -> i32 { return value + 1 }
effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(41) |> Effect.map(addOne) }`,
  },
  {
    id: 'map-both-success-imperative',
    pair: 'map-both-success',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'map-both-success-effect',
    pair: 'map-both-success',
    kind: 'effect',
    expected: 42,
    source: `import silk.effects as Effect

pub struct Problem { code: i32 }
fn addOne(value: i32) -> i32 { return value + 1 }
fn recover(problem: Problem) -> Problem { return move problem }
effect fn succeed() -> i32 ! Problem { return 41 }
effect fn handle(problem: Problem) -> i32 { return problem.code }
pub fn main() -> i32 {
  return run succeed() |> Effect.mapBoth(addOne, recover) |> Effect.catchAll(handle)
}`,
  },
  {
    id: 'map-both-failure-imperative',
    pair: 'map-both-failure',
    kind: 'baseline',
    expected: 42,
    source: `struct Problem { code: i32 }
fn recover(problem: Problem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 { return recover(Problem { code: 41 }) }`,
  },
  {
    id: 'map-both-failure-effect',
    pair: 'map-both-failure',
    kind: 'effect',
    expected: 42,
    source: `import silk.effects as Effect

struct Problem { code: i32 }
struct OtherProblem { code: i32 }
fn keep(value: i32) -> i32 { return value }
fn translate(problem: Problem) -> OtherProblem { return OtherProblem { code: problem.code } }
effect fn failValue() -> i32 ! Problem { fail Problem { code: 41 } }
effect fn recover(problem: OtherProblem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 {
  return run failValue()
    |> Effect.mapBoth(keep, translate)
    |> Effect.catchAll(recover)
}`,
  },
  {
    id: 'flat-map-imperative',
    pair: 'flat-map',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return addOne(41) }`,
  },
  {
    id: 'flat-map-effect',
    pair: 'flat-map',
    kind: 'effect',
    expected: 42,
    source: `import silk.effects as Effect

effect fn succeed(value: i32) -> i32 { return value }
effect fn addOne(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return run succeed(41) |> Effect.flatMap(addOne) }`,
  },
  {
    id: 'provide-imperative',
    pair: 'provide',
    kind: 'baseline',
    expected: 42,
    source: `struct Clock { value: i32 }
fn read(clock: &Clock) -> i32 { return clock.value }
pub fn main() -> i32 { let clock = Clock { value: 42 } return read(&clock) }`,
  },
  {
    id: 'provide-effect',
    pair: 'provide',
    kind: 'effect',
    expected: 42,
    source: `import silk.effects as Effect

service Clock {}
struct FixedClock { value: i32 }
impl Clock for FixedClock {}
effect fn read() -> i32 ? &Clock { return 42 }
fn adapt<A, E, ?R>(self: once Effect<A ! E ? R>) -> once Effect<A ! E ? R> {
  return move self
}
pub fn main() -> i32 {
  let clock = FixedClock { value: 42 }
  return run adapt(read()) |> Effect.provide(&clock)
}`,
  },
  {
    id: 'stored-imperative',
    pair: 'stored',
    kind: 'baseline',
    expected: 42,
    source: `fn addOne(value: i32) -> i32 { return value + 1 }
fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return double(addOne(20)) }`,
  },
  {
    id: 'stored-effect',
    pair: 'stored',
    kind: 'effect',
    expected: 42,
    source: `import silk.effects as Effect

fn addOne(value: i32) -> i32 { return value + 1 }
fn double(value: i32) -> i32 { return value * 2 }
effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let first = succeed(20) |> Effect.map(addOne)
  let second = move first |> Effect.map(double)
  return run second
}`,
  },
  {
    id: 'affine-imperative',
    pair: 'affine',
    kind: 'baseline',
    expected: 42,
    source: `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effects as Effect
import silk.layout { Layout }

struct Payload { storage: Allocation value: i32 }
impl Drop for Payload { fn drop(self: &mut Payload) -> () { return () } }
fn consume(payload: Payload) -> i32 { return payload.value }
effect fn produce() -> Payload ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
    |> Effect.provideMut(&mut allocator)
  return Payload { storage: move storage, value: 42 }
}
effect fn program() -> i32 ! OutOfMemoryError {
  let payload = run produce()
  return consume(move payload)
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run program() |> Effect.catchAll(recover) }`,
  },
  {
    id: 'affine-effect',
    pair: 'affine',
    kind: 'effect',
    expected: 42,
    source: `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effects as Effect
import silk.layout { Layout }

struct Payload { storage: Allocation value: i32 }
impl Drop for Payload { fn drop(self: &mut Payload) -> () { return () } }
fn consume(payload: Payload) -> i32 { return payload.value }
effect fn produce() -> Payload ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
    |> Effect.provideMut(&mut allocator)
  return Payload { storage: move storage, value: 42 }
}
effect fn program() -> i32 ! OutOfMemoryError {
  return run produce() |> Effect.map(consume)
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run program() |> Effect.catchAll(recover) }`,
  },
  {
    id: 'trap-imperative',
    pair: 'trap',
    kind: 'baseline',
    expected: 'trap',
    source: `fn divide(value: i32) -> i32 { return 1 / value }
pub fn main() -> i32 { return divide(0) }`,
  },
  {
    id: 'trap-effect',
    pair: 'trap',
    kind: 'effect',
    expected: 'trap',
    source: `import silk.effects as Effect

fn divide(value: i32) -> i32 { return 1 / value }
effect fn zero() -> i32 { return 0 }
pub fn main() -> i32 { return run zero() |> Effect.map(divide) }`,
  },
])

const temporary = mkdtempSync(join(tmpdir(), 'silk-effect-cost-'))
const clang = process.env.SILK_EFFECT_COST_CLANG ?? 'clang'
const artifactDirectory = process.env.SILK_EFFECT_COST_ARTIFACT_DIR
const directStaticCases = new Set([
  'map-effect',
  'map-both-success-effect',
  'map-both-failure-effect',
  'flat-map-effect',
  'provide-effect',
  'stored-effect',
  'affine-imperative',
  'affine-effect',
  'trap-effect',
])

const clangText = (bitcode, id, arguments_) => {
  const bitcodePath = join(temporary, `${id}.bc`)
  writeFileSync(bitcodePath, bitcode)
  const result = spawnSync(clang, [...arguments_, '-x', 'ir', bitcodePath, '-o', '-'], {
    encoding: 'utf8',
  })
  if (result.status !== 0) {
    throw new Error(`${clang} ${arguments_.join(' ')} failed: ${result.stderr}`)
  }
  return normalize(result.stdout)
}

const summarize = (text, includeHash = true) => {
  const suspensionMatches = text
    .split('\n')
    .filter(containsSuspensionTerm)
    .map((line) => line.trim())
  const allocationSites = [
    ...text.matchAll(
      /\b(?:call\s+[^\n@]*@((?:malloc|calloc|realloc|aligned_alloc))|memory\.grow)\b/gi,
    ),
  ].map((match) => match[1]?.toLowerCase() ?? 'memory.grow')
  return Object.freeze({
    bytes: Buffer.byteLength(text),
    ...(includeHash ? { hash: hash(text) } : {}),
    calls: occurrences(text, /\bcall\b/g),
    branches: occurrences(text, /\b(?:br|if|switch)\b/g),
    allocations: allocationSites.length,
    allocationSites: Object.freeze(allocationSites),
    indirectCalls: occurrences(text, /\bcall\b[^\n@]*%[A-Za-z0-9_.]+/g),
    suspensionTerms: suspensionMatches.length,
    suspensionMatches,
    effectTerms: occurrences(text, /\bEffect\b|silk\/effects|effects\./g),
  })
}

const summarizeAssembly = (text) =>
  Object.freeze({
    ...summarize(text, false),
    calls: occurrences(text, /^\s*bl\s+/gm),
    branches: occurrences(text, /^\s*(?:b(?:\.[a-z]+)?|cbnz|cbz|tbnz|tbz)\s+/gm),
  })

const llvmEntry = (text) => {
  const marker = '@silk_main('
  const markerIndex = text.indexOf(marker)
  if (markerIndex < 0) return ''
  const start = text.lastIndexOf('define ', markerIndex)
  const body = text.indexOf('{', markerIndex)
  if (start < 0 || body < 0) return ''
  const end = text.indexOf('\n}', body)
  return end < 0 ? '' : text.slice(start, end + 2)
}

const llvmEntryAbi = (text) => {
  const signature = /^define\s+(.+?)\s+@silk_main\(([^)]*)\)/m.exec(text)
  if (signature === null) return undefined
  const result = /(?:^|\s)(void|i\d+|float|double|ptr|\{[^}]+\})$/.exec(signature[1])?.[1]
  if (result === undefined) return undefined
  const parameters = signature[2].trim()
  return Object.freeze({
    parameters: Object.freeze(
      parameters === '' ? [] : parameters.split(',').map((value) => value.trim()),
    ),
    results: Object.freeze(result === 'void' ? [] : [result]),
  })
}

const assemblyEntry = (text) => {
  const match = /^_?silk_main:.*\n[\s\S]*?^\s*; -- End function/m.exec(text)
  return match?.[0] ?? ''
}

const topLevelWasmForms = (text) => {
  const forms = []
  let depth = 0
  let start = -1
  let quoted = false
  let escaped = false
  for (let index = 0; index < text.length; index += 1) {
    const character = text[index]
    if (quoted) {
      if (escaped) escaped = false
      else if (character === '\\') escaped = true
      else if (character === '"') quoted = false
      continue
    }
    if (character === '"') {
      quoted = true
      continue
    }
    if (character === '(') {
      if (depth === 1) start = index
      depth += 1
      continue
    }
    if (character !== ')') continue
    depth -= 1
    if (depth === 1 && start >= 0) {
      forms.push(text.slice(start, index + 1))
      start = -1
    }
  }
  return Object.freeze(forms)
}

const wasmEntry = (text) => {
  const forms = topLevelWasmForms(text)
  const export_ = forms.find((form) => /^\(export\s+"silk_main"\s+\(func\s+\d+\)\)/.test(form))
  const index = export_ === undefined ? undefined : Number(/\(func\s+(\d+)\)/.exec(export_)?.[1])
  if (index === undefined || !Number.isInteger(index)) return ''
  const imported = forms.filter((form) => /^\(import\b/.test(form) && /\(func\b/.test(form)).length
  return forms.filter((form) => /^\(func\b/.test(form)).at(index - imported) ?? ''
}

const wasmEntryAbi = (text) => {
  const forms = topLevelWasmForms(text)
  const entry = wasmEntry(text)
  const typeIndex = Number(/^\(func\s+\(type\s+(\d+)\)/.exec(entry)?.[1])
  if (!Number.isInteger(typeIndex)) return undefined
  const type = forms.filter((form) => /^\(type\b/.test(form)).at(typeIndex)
  if (type === undefined) return undefined
  const values = (kind) =>
    Object.freeze(
      [...type.matchAll(new RegExp(`\\(${kind}((?:\\s+[a-z][0-9]+)*)\\)`, 'g'))].flatMap((match) =>
        match[1].trim().split(/\s+/).filter(Boolean),
      ),
    )
  return Object.freeze({ parameters: values('param'), results: values('result') })
}

const llvmDeclarations = (text) =>
  Object.freeze([...text.matchAll(/^declare\s+[^@\n]*@([^\s(]+)/gm)].map((match) => match[1]))

const wasmImports = (text) =>
  Object.freeze(topLevelWasmForms(text).filter((form) => /^\(import\b/.test(form)))

const wasmBehavior = (artifact, id) => {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
  const main = instance.exports.silk_main
  if (typeof main !== 'function') throw new Error(`${id} has no Wasm silk_main`)
  try {
    return main()
  } catch (cause) {
    if (!(cause instanceof WebAssembly.RuntimeError)) throw cause
    return 'trap'
  }
}

const identity = (value) =>
  JSON.stringify(value, (_, candidate) =>
    typeof candidate === 'bigint' ? candidate.toString() : candidate,
  )

const structuralTargets = (region) => {
  switch (region._tag) {
    case 'OperationRegion':
    case 'CleanupRegion':
      return region.outcome._tag === 'Forward' ? [region.outcome.target.ordinal] : []
    case 'ConditionalRegion':
      return [
        region.taken.ordinal,
        region.otherwise.ordinal,
        ...(region.following === undefined ? [] : [region.following.ordinal]),
      ]
    case 'LoopRegion':
      return [region.condition.ordinal, region.body.ordinal, region.following.ordinal]
  }
}

const hasStructuralCycle = (fn) => {
  const edges = new Map(fn.regions.map((region) => [region.id.ordinal, structuralTargets(region)]))
  const active = new Set()
  const complete = new Set()
  const visit = (region) => {
    if (active.has(region)) return true
    if (complete.has(region)) return false
    active.add(region)
    const cyclic = (edges.get(region) ?? []).some(visit)
    active.delete(region)
    complete.add(region)
    return cyclic
  }
  return fn.regions.some((region) => visit(region.id.ordinal))
}

const countTags = (values) =>
  Object.freeze(
    [
      ...values.reduce(
        (counts, value) => counts.set(value._tag, (counts.get(value._tag) ?? 0) + 1),
        new Map(),
      ),
    ]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([tag, count]) => Object.freeze({ tag, count })),
  )

const runnerClassifications = (program) => {
  const classifications = []
  for (const owner of program.functions) {
    for (const operation of Mir.operations(owner)) {
      if (operation._tag !== 'RunStaticEffect') continue
      const runner = program.functions.find((candidate) =>
        Mir.matchesInstance(candidate, operation.runner, operation.runnerTypeArguments),
      )
      if (runner === undefined) {
        throw new Error(`missing static runner ${identity(operation.runner)}`)
      }
      const regions = Mir.topologicalRegions(runner)
      const operations = Mir.operations(runner)
      const outcomes = Mir.outcomes(runner)
      const operationTags = countTags(operations)
      const regionTags = countTags(regions)
      const outcomeTags = countTags(outcomes)
      const nestedMatches = operations.filter((candidate) => candidate._tag === 'Match').length
      const directCalls = operations.filter((candidate) => candidate._tag === 'Call').length
      const dynamicCalls = operations.filter(
        (candidate) => candidate._tag === 'ApplyCallable' && candidate.target === undefined,
      ).length
      const effectOperations = operations.filter((candidate) =>
        [
          'MakeEffect',
          'RunEffect',
          'RunEffectValue',
          'RunStaticEffect',
          'ReifyEffect',
          'CloseEffectEntry',
        ].includes(candidate._tag),
      ).length
      const loans = operations.filter(
        (candidate) => candidate._tag === 'BeginLoan' || candidate._tag === 'EndLoan',
      ).length
      const releases = operations.filter(
        (candidate) => candidate._tag === 'Drop' || candidate._tag === 'EndLoan',
      ).length
      const cleanup =
        regions.filter((region) => region._tag === 'CleanupRegion').length +
        operations.filter((candidate) => candidate._tag === 'Drop' || candidate._tag === 'SlotDrop')
          .length
      const recursive = operations.some(
        (candidate) =>
          candidate._tag === 'Call' && identity(candidate.target) === identity(runner.id),
      )
      const affineAccesses =
        operations
          .flatMap((candidate) => ('captures' in candidate ? candidate.captures : []))
          .filter((capture) => capture.access === 'Exclusive' || capture.access === 'Take').length +
        operations.filter(
          (candidate) =>
            candidate._tag === 'Move' ||
            candidate._tag === 'Drop' ||
            candidate._tag === 'SlotTake' ||
            candidate._tag === 'SlotDrop',
        ).length
      const returns = outcomes.filter((outcome) => outcome._tag === 'Return').length
      const lexicalExits = outcomes.filter(
        (outcome) => outcome._tag === 'Repeat' || outcome._tag === 'Exit',
      ).length
      const blockers = [
        ...(regions.some((region) => region._tag !== 'OperationRegion')
          ? ['StructuredRegion']
          : []),
        ...(nestedMatches > 0 ? ['NestedMatch'] : []),
        ...(dynamicCalls > 0 ? ['DynamicCallable'] : []),
        ...(effectOperations > 0 ? ['NestedEffectExecution'] : []),
        ...(loans > 0 ? ['Loan'] : []),
        ...(cleanup > 0 ? ['Cleanup'] : []),
        ...(recursive ? ['Recursive'] : []),
        ...(affineAccesses > 0 ? ['AffineOperation'] : []),
        ...(returns !== 1 ? ['AmbiguousReturn'] : []),
        ...(lexicalExits > 0 ? ['LexicalExit'] : []),
        ...(hasStructuralCycle(runner) ? ['CyclicControl'] : []),
      ].sort()
      classifications.push(
        Object.freeze({
          site: Object.freeze({
            owner: identity(owner.instance),
            region: operation.provenance.span,
          }),
          runner: identity(runner.instance),
          regions: regionTags,
          outcomes: outcomeTags,
          operations: operationTags,
          nestedMatches,
          directCalls,
          dynamicCalls,
          effectOperations,
          loans,
          releases,
          cleanup,
          recursive,
          affineAccesses,
          estimatedClonedSize: regions.length + operations.length + outcomes.length,
          blockers: Object.freeze(blockers),
          prototypeEligible: blockers.length === 0,
        }),
      )
    }
  }
  return Object.freeze(classifications)
}

try {
  const cases = []
  for (const sample of sources) {
    try {
      const sourceId = `cost/${sample.id}`
      const native = await Effect.runPromise(
        Analysis.ofSourceRealized(
          `${sourceId}/native`,
          encoder.encode(sample.source),
          'aarch64-apple-darwin',
        ),
      )
      const wasm = await Effect.runPromise(
        Analysis.ofSourceRealized(
          `${sourceId}/wasm`,
          encoder.encode(sample.source),
          'wasm32-unknown-unknown',
        ),
      )
      const unnormalizedWasm = await Effect.runPromise(
        Analysis.ofSourceRealized(
          `${sourceId}/wasm`,
          encoder.encode(sample.source),
          'wasm32-unknown-unknown',
          { normalizeMir: false },
        ),
      )
      const diagnostics = Analysis.diagnostics(native)
      const wasmDiagnostics = Analysis.diagnostics(wasm)
      if (diagnostics.length > 0 || wasmDiagnostics.length > 0) {
        throw new Error(
          `${sample.id} diagnostics: ${JSON.stringify({ native: diagnostics, wasm: wasmDiagnostics })}`,
        )
      }

      const evaluated = Analysis.evaluate(native)
      const unnormalizedEvaluated = Analysis.evaluate(unnormalizedWasm)
      const evaluatorBehavior =
        evaluated._tag === 'Completed'
          ? Number(evaluated.result.value)
          : evaluated._tag === 'Trap'
            ? 'trap'
            : evaluated._tag
      if (evaluatorBehavior !== sample.expected) {
        throw new Error(
          `${sample.id} did not complete: ${JSON.stringify(evaluated, (_, value) =>
            typeof value === 'bigint' ? value.toString() : value,
          )}`,
        )
      }
      const debug = await Effect.runPromise(Analysis.codegen(native, { mode: 'debug' }))
      const release = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
      const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
      const unnormalizedWasmArtifact = await Effect.runPromise(
        Analysis.codegenWasm(unnormalizedWasm, { mode: 'release' }),
      )
      const directWasmBehavior = wasmBehavior(wasmArtifact, sample.id)
      const unnormalizedDirectWasmBehavior = wasmBehavior(
        unnormalizedWasmArtifact,
        `${sample.id} (unnormalized)`,
      )
      const hir = normalize(Hir.encode(Analysis.rootAnalysis(native).hir))
      const mir = normalize(Mir.encode(Analysis.loweredMir(native)))
      const debugLlvm = normalize(debug.ir)
      const releaseLlvm = normalize(release.ir)
      const optimizedLlvm = clangText(release.bitcode, `${sample.id}-optimized`, [
        '-O2',
        '-S',
        '-emit-llvm',
      ])
      const assembly = clangText(release.bitcode, `${sample.id}-assembly`, ['-O2', '-S'])
      const wat = normalize(wasmArtifact.wat)
      const unnormalizedWat = normalize(unnormalizedWasmArtifact.wat)
      if (artifactDirectory !== undefined) {
        const destination = join(artifactDirectory, sample.id)
        mkdirSync(destination, { recursive: true })
        for (const [name, text] of [
          ['hir.txt', hir],
          ['mir.txt', mir],
          ['llvm-debug.ll', debugLlvm],
          ['llvm-release.ll', releaseLlvm],
          ['llvm-optimized.ll', optimizedLlvm],
          ['native.s', assembly],
          ['direct.wat', wat],
          ['direct-unnormalized.wat', unnormalizedWat],
        ]) {
          writeFileSync(join(destination, name), text)
        }
        writeFileSync(join(destination, 'native.bc'), release.bitcode)
        writeFileSync(join(destination, 'direct.wasm'), wasmArtifact.bytes)
      }
      const dropCalls = evaluated.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl#'),
      ).length
      const unnormalizedDropCalls = unnormalizedEvaluated.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl#'),
      ).length
      const verdicts = Analysis.effectNormalizationOf(wasm)
      const loweredWasm = Analysis.loweredMir(wasm)
      const runners = runnerClassifications(loweredWasm)
      const mirOperationTags = countTags(loweredWasm.functions.flatMap(Mir.operations))
      const coroutineFrameDescriptors = loweredWasm.functions.filter(
        (fn) => fn.suspension?.frame !== undefined,
      ).length
      const applicability = directStaticCases.has(sample.id) ? 'DirectStaticRun' : 'None'

      cases.push(
        Object.freeze({
          id: sample.id,
          pair: sample.pair,
          kind: sample.kind,
          expected: sample.expected,
          behavior: Object.freeze({
            evaluator: evaluatorBehavior,
            unnormalizedEvaluator:
              unnormalizedEvaluated._tag === 'Completed'
                ? Number(unnormalizedEvaluated.result.value)
                : unnormalizedEvaluated._tag === 'Trap'
                  ? 'trap'
                  : unnormalizedEvaluated._tag,
            wasm: directWasmBehavior,
            unnormalizedWasm: unnormalizedDirectWasmBehavior,
            dropCalls,
            unnormalizedDropCalls,
          }),
          applicability,
          normalization: Object.freeze({
            accepted: verdicts.filter((verdict) => verdict._tag === 'Normalized').length,
            rejected: verdicts.filter((verdict) => verdict._tag === 'Rejected').length,
            foldedConstructors: verdicts.filter(
              (verdict) => verdict._tag === 'Normalized' && verdict.kind === 'FoldedConstructor',
            ).length,
            directStaticRuns: verdicts.filter(
              (verdict) => verdict._tag === 'Normalized' && verdict.kind === 'DirectStaticRun',
            ).length,
          }),
          runners,
          suspendability: Object.freeze({
            instances: Analysis.suspendableInstancesOf(wasm).map(identity),
            executions: Analysis.suspendableExecutionsOf(wasm).map(identity),
            effects: Analysis.suspendableEffectsOf(wasm),
          }),
          coroutineFrameDescriptors,
          pipeTokens: Object.freeze({
            hir: occurrences(hir, /\|>/g),
            mir: occurrences(mir, /\|>/g),
          }),
          mirOperationTags,
          suspensionOperationTags: Object.freeze(
            mirOperationTags.filter(({ tag }) => containsSuspensionTerm(tag)),
          ),
          hir: summarize(hir),
          mir: summarize(mir),
          debugLlvm: summarize(debugLlvm),
          releaseLlvm: summarize(releaseLlvm),
          optimizedLlvm: summarize(optimizedLlvm, false),
          optimizedLlvmEntry: Object.freeze({
            ...summarize(llvmEntry(optimizedLlvm), false),
            abi: llvmEntryAbi(optimizedLlvm),
          }),
          assembly: summarizeAssembly(assembly),
          assemblyEntry: summarizeAssembly(assemblyEntry(assembly)),
          wasm: Object.freeze({ ...summarize(wat), binaryBytes: wasmArtifact.bytes.length }),
          wasmEntry: Object.freeze({ ...summarize(wasmEntry(wat)), abi: wasmEntryAbi(wat) }),
          unnormalizedWasm: Object.freeze({
            ...summarize(unnormalizedWat),
            binaryBytes: unnormalizedWasmArtifact.bytes.length,
          }),
          unnormalizedWasmEntry: Object.freeze({
            ...summarize(wasmEntry(unnormalizedWat)),
            abi: wasmEntryAbi(unnormalizedWat),
          }),
          symbols: Object.freeze({
            native: release.symbols.map((entry) => entry.declaration.name),
            wasm: wasmArtifact.symbols.map((entry) => entry.declaration.name),
          }),
          linkage: Object.freeze({
            nativeRuntime: release.nativeRuntimeSymbols,
            nativeDeclarations: llvmDeclarations(optimizedLlvm),
            wasmImports: wasmImports(wat),
            unnormalizedWasmImports: wasmImports(unnormalizedWat),
          }),
          suspensionLinkage: Object.freeze(
            [
              ...release.nativeRuntimeSymbols,
              ...llvmDeclarations(optimizedLlvm),
              ...wasmImports(wat),
              ...wasmImports(unnormalizedWat),
              ...release.symbols.map((entry) => entry.declaration.name),
              ...wasmArtifact.symbols.map((entry) => entry.declaration.name),
            ].filter(containsSuspensionTerm),
          ),
        }),
      )
    } catch (cause) {
      throw new Error(`${sample.id}: ${cause instanceof Error ? cause.stack : String(cause)}`)
    }
  }

  const version = spawnSync(clang, ['--version'], { encoding: 'utf8' })
  if (version.status !== 0) throw new Error(`${clang} --version failed: ${version.stderr}`)
  process.stdout.write(
    JSON.stringify({
      schema: 3,
      clang: version.stdout.split('\n').at(0),
      node: process.version,
      cases,
    }),
  )
} finally {
  rmSync(temporary, { recursive: true, force: true })
}
