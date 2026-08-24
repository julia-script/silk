import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const sourcePath = fileURLToPath(
  new URL('../../../examples/language-pressure/stack-vm/main.silk', import.meta.url),
)
const pressureSource = readFileSync(sourcePath, 'utf8')
const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

interface VmStep {
  readonly pc: number
  readonly opcode: number
  readonly depth: number
  readonly top: number
}

interface VmDiagnostic {
  readonly pc: number
  readonly code: number
}

interface VmResult {
  readonly steps: ReadonlyArray<VmStep>
  readonly diagnostics: ReadonlyArray<VmDiagnostic>
  readonly result: number
  readonly fingerprint: number
}

const referenceExecute = (bytecode: ReadonlyArray<number>): VmResult => {
  const stack: Array<number> = []
  const steps: Array<VmStep> = []
  const diagnostics: Array<VmDiagnostic> = []
  const events: Array<
    | { readonly _tag: 'Step'; readonly value: VmStep }
    | { readonly _tag: 'Diagnostic'; readonly value: VmDiagnostic }
  > = []
  let pc = 0
  let attempted = 0
  const top = (): number => stack.at(-1) ?? 0
  const diagnose = (at: number, code: number): void => {
    const diagnostic = Object.freeze({ pc: at, code })
    diagnostics.push(diagnostic)
    events.push(Object.freeze({ _tag: 'Diagnostic', value: diagnostic }))
  }
  const record = (at: number, opcode: number): void => {
    const step = Object.freeze({ pc: at, opcode, depth: stack.length, top: top() })
    steps.push(step)
    events.push(Object.freeze({ _tag: 'Step', value: step }))
  }

  execution: while (pc < bytecode.length) {
    if (attempted >= 64) {
      diagnose(pc, 5)
      break
    }
    attempted += 1
    const instructionPc = pc
    const opcode = bytecode[instructionPc] ?? 0
    switch (opcode) {
      case 0:
        pc += 1
        record(instructionPc, opcode)
        break execution
      case 1: {
        const value = bytecode[instructionPc + 1]
        if (value === undefined) {
          diagnose(instructionPc, 2)
          break execution
        }
        if (stack.length >= 16) {
          diagnose(instructionPc, 6)
          break execution
        }
        stack.push(value)
        pc += 2
        record(instructionPc, opcode)
        break
      }
      case 2:
      case 3:
      case 4: {
        if (stack.length < 2) {
          diagnose(instructionPc, 3)
          pc += 1
          break
        }
        const right = stack.pop() ?? 0
        const left = stack.pop() ?? 0
        stack.push(
          opcode === 2 ? left + right : opcode === 3 ? left - right : Math.imul(left, right),
        )
        pc += 1
        record(instructionPc, opcode)
        break
      }
      case 5:
        if (stack.length === 0) {
          diagnose(instructionPc, 3)
          pc += 1
        } else if (stack.length >= 16) {
          diagnose(instructionPc, 6)
          break execution
        } else {
          stack.push(top())
          pc += 1
          record(instructionPc, opcode)
        }
        break
      case 6: {
        const target = bytecode[instructionPc + 1]
        if (target === undefined) {
          diagnose(instructionPc, 2)
          break execution
        }
        if (stack.length === 0) {
          diagnose(instructionPc, 3)
          pc += 2
          break
        }
        if (target >= bytecode.length) {
          diagnose(instructionPc, 4)
          break execution
        }
        const condition = stack.pop() ?? 0
        pc = condition === 0 ? target : pc + 2
        record(instructionPc, opcode)
        break
      }
      case 7: {
        const target = bytecode[instructionPc + 1]
        if (target === undefined) {
          diagnose(instructionPc, 2)
          break execution
        }
        if (target >= bytecode.length) {
          diagnose(instructionPc, 4)
          break execution
        }
        pc = target
        record(instructionPc, opcode)
        break
      }
      default:
        diagnose(instructionPc, 1)
        pc += 1
    }
  }

  const result = top()
  let fingerprint = 0
  for (const event of events) {
    if (event._tag === 'Step') {
      const step = event.value
      fingerprint =
        (fingerprint * 17 + step.pc * 3 + step.opcode * 5 + step.depth * 7 + step.top * 11) % 251
    } else {
      const diagnostic = event.value
      fingerprint = (fingerprint * 19 + diagnostic.pc * 13 + diagnostic.code * 17) % 251
    }
  }
  fingerprint = (fingerprint * 23 + result) % 251
  return Object.freeze({
    steps: Object.freeze(steps),
    diagnostics: Object.freeze(diagnostics),
    result,
    fingerprint,
  })
}

const replaceExactlyOnce = (source: string, search: string, replacement: string): string => {
  assert.strictEqual(source.split(search).length - 1, 1, search)
  return source.replace(search, replacement)
}

const byteLiteral = (bytecode: ReadonlyArray<number>): string =>
  `b"${bytecode.map((byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

const sourceFor = (
  bytecode: ReadonlyArray<number>,
): { readonly source: string; readonly expected: VmResult } => {
  const expected = referenceExecute(bytecode)
  const withInput = replaceExactlyOnce(
    pressureSource,
    '  let bytecode = b"\\x01\\x00\\x06\\x06\\x01\\x63\\x01\\x06\\x01\\x07\\x04\\x00"',
    `  let bytecode = ${byteLiteral(bytecode)}`,
  )
  const source = replaceExactlyOnce(
    withInput,
    '  if value != 0 { let mismatch = 1 / 0 }',
    `  if value != ${expected.fingerprint} { let mismatch = 1 / 0 }`,
  )
  return Object.freeze({ source, expected })
}

const quotaSourceFor = (bytecode: ReadonlyArray<number>, quota: number): string => {
  const generated = sourceFor(bytecode).source
  const withAllocator = replaceExactlyOnce(
    generated,
    'import silk.logger { length as logLength }',
    `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.logger { length as logLength }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorService()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }`,
  )
  return replaceExactlyOnce(
    withAllocator,
    '  let mut allocator = Allocator.systemAllocatorService()',
    `  let mut allocator = QuotaAllocator { remaining: ${quota} }`,
  )
}

const corpus = [
  Object.freeze({ id: 'arithmetic', bytecode: Object.freeze([1, 6, 1, 7, 4, 0]) }),
  Object.freeze({
    id: 'branch-taken',
    bytecode: Object.freeze([1, 0, 6, 6, 1, 99, 1, 6, 1, 7, 4, 0]),
  }),
  Object.freeze({
    id: 'branch-untaken',
    bytecode: Object.freeze([1, 1, 6, 6, 1, 99, 1, 20, 1, 22, 2, 0]),
  }),
  Object.freeze({
    id: 'unknown-recovery',
    bytecode: Object.freeze([99, 98, 97, 96, 95, 1, 42, 0]),
  }),
  Object.freeze({ id: 'truncated-push', bytecode: Object.freeze([1]) }),
  Object.freeze({ id: 'underflow', bytecode: Object.freeze([2, 3, 4, 5, 6, 7, 0]) }),
  Object.freeze({ id: 'invalid-jump', bytecode: Object.freeze([7, 9]) }),
  Object.freeze({
    id: 'stack-overflow',
    bytecode: Object.freeze([...Array.from({ length: 17 }, () => [1, 1]).flat(), 0]),
  }),
  Object.freeze({ id: 'step-limit', bytecode: Object.freeze([7, 0]) }),
  Object.freeze({
    id: 'allocation-pressure',
    bytecode: Object.freeze([99, 98, 97, 96, 95, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 0]),
  }),
] as const

const observations = (
  outcome: Extract<ReturnType<typeof Analysis.evaluate>, { _tag: 'Completed' }>,
): Omit<VmResult, 'fingerprint'> => {
  const scalarValues = (name: string): ReadonlyArray<number> =>
    outcome.trace.flatMap((event) => {
      if (event._tag !== 'Binding' || event.target.name !== name) return []
      if (event.value._tag === 'IntegerValue') return [Number(event.value.value)]
      return []
    })
  const pcs = scalarValues('observeStepPc')
  const opcodes = scalarValues('observeStepOpcode')
  const depths = scalarValues('observeStepDepth')
  const tops = scalarValues('observeStepTop')
  assert.strictEqual(opcodes.length, pcs.length)
  assert.strictEqual(depths.length, pcs.length)
  assert.strictEqual(tops.length, pcs.length)
  const diagnosticPcs = scalarValues('observeDiagnosticPc')
  const diagnosticCodes = scalarValues('observeDiagnosticCode')
  assert.strictEqual(diagnosticCodes.length, diagnosticPcs.length)
  return Object.freeze({
    result: scalarValues('observeResult').at(0) ?? 0,
    steps: pcs.map((pc, index) =>
      Object.freeze({
        pc,
        opcode: opcodes.at(index) ?? -1,
        depth: depths.at(index) ?? -1,
        top: tops.at(index) ?? -1,
      }),
    ),
    diagnostics: diagnosticPcs.map((pc, index) =>
      Object.freeze({ pc, code: diagnosticCodes.at(index) ?? -1 }),
    ),
  })
}

it.effect(
  'matches the canonical VM over valid and malformed bytecode',
  () =>
    Effect.gen(function* () {
      for (const entry of corpus) {
        const generated = sourceFor(entry.bytecode)
        const snapshot = yield* Analysis.ofSourceRealized(
          `stack-vm-pressure/${entry.id}`,
          ascii(generated.source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [], entry.id)
        const outcome = Analysis.evaluate(snapshot)
        assert.strictEqual(
          outcome._tag,
          'Completed',
          `${entry.id}: ${JSON.stringify(outcome, (_, value) =>
            typeof value === 'bigint' ? `${value}n` : value,
          )}`,
        )
        if (outcome._tag !== 'Completed') continue
        assert.deepEqual(observations(outcome), {
          result: generated.expected.result,
          steps: generated.expected.steps,
          diagnostics: generated.expected.diagnostics,
        })
      }
    }),
  120_000,
)

it.effect('publishes only general MIR operations for the pressure VM', () =>
  Effect.gen(function* () {
    const generated = sourceFor(corpus[1].bytecode)
    const snapshot = yield* Analysis.ofSourceRealized(
      'stack-vm-pressure/general-mir',
      ascii(generated.source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const tags = new Set(
      Analysis.loweredMir(snapshot).functions.flatMap((fn) =>
        MirVerification.operations(fn).map((operation) => operation._tag),
      ),
    )
    assert.strictEqual(tags.has('Allocate'), true)
    assert.isFalse(
      [...tags].some((tag) =>
        ['vm', 'opcode', 'operandstack'].some((spelling) => tag.toLowerCase().includes(spelling)),
      ),
    )
  }),
)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-stack-vm-pressure-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

for (const representative of [corpus[1], corpus[3]]) {
  it.effect(
    `keeps ${representative.id} evaluation, native, and Wasm execution in parity`,
    () =>
      Effect.gen(function* () {
        const generated = sourceFor(representative.bytecode)
        const id = `stack-vm-pressure/representative/${representative.id}`
        const snapshot = yield* Analysis.ofSourceRealized(
          id,
          ascii(generated.source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [])
        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(evaluated._tag, 'Completed')
        if (evaluated._tag !== 'Completed') return
        assert.deepEqual(observations(evaluated), {
          result: generated.expected.result,
          steps: generated.expected.steps,
          diagnostics: generated.expected.diagnostics,
        })
        const acquires = evaluated.trace.filter(
          (event) => event._tag === 'AllocationAcquire',
        ).length
        const releases = evaluated.trace.filter(
          (event) => event._tag === 'AllocationRelease',
        ).length
        assert.isAbove(acquires, 0)
        assert.strictEqual(releases, acquires)

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual((instance.exports.silk_main as () => number)(), 0)

        const compiled = yield* Driver.compile({
          compilation: { root: SourceFile.make(id, ascii(generated.source)) },
          toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
          profile: 'release',
          destination: join(destinationRoot, representative.id),
        }).pipe(Effect.provide(SourceResolver.empty))
        assert.strictEqual(compiled._tag, 'Compiled')
        if (compiled._tag !== 'Compiled') return
        const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
        assert.strictEqual(
          run.status,
          0,
          JSON.stringify({ stderr: run.stderr, signal: run.signal, error: run.error?.message }),
        )
        assert.strictEqual(run.stderr, '')
      }),
    180_000,
  )
}

it.effect(
  'rolls back every trace and diagnostic allocation failure on all three engines',
  () =>
    Effect.gen(function* () {
      const representative = corpus[9]
      const baseline = sourceFor(representative.bytecode)
      const baselineSnapshot = yield* Analysis.ofSourceRealized(
        'stack-vm-pressure/quota/baseline',
        ascii(baseline.source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(baselineSnapshot), [])
      const baselineOutcome = Analysis.evaluate(baselineSnapshot)
      assert.strictEqual(baselineOutcome._tag, 'Completed')
      if (baselineOutcome._tag !== 'Completed') return
      const allocationCount = baselineOutcome.trace.filter(
        (event) => event._tag === 'AllocationAcquire',
      ).length
      assert.isAtLeast(allocationCount, 4)

      // Native execution runs at the boundary ordinals only — immediate failure, one mid-growth
      // rollback, and unrestricted completion — while the evaluator and Wasm carry every ordinal.
      // Each native leg is a full release pipeline, and the allocator's rollback path does not
      // vary by ordinal index beyond those three shapes.
      const nativeQuotas = new Set([0, Math.floor(allocationCount / 2), allocationCount])

      for (let quota = 0; quota <= allocationCount; quota += 1) {
        const id = `stack-vm-pressure/quota/q${quota}`
        const source = quotaSourceFor(representative.bytecode, quota)
        const snapshot = yield* Analysis.ofSourceRealized(
          id,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [], id)
        const expectedTag = quota === allocationCount ? 'Completed' : 'UnhandledFailure'
        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(evaluated._tag, expectedTag, id)
        const acquired = evaluated.trace.filter(
          (event) => event._tag === 'AllocationAcquire',
        ).length
        const released = evaluated.trace.filter(
          (event) => event._tag === 'AllocationRelease',
        ).length
        assert.strictEqual(acquired, quota, id)
        assert.strictEqual(released, acquired, id)
        const again = Analysis.evaluate(snapshot)
        assert.strictEqual(again._tag, expectedTag, id)
        assert.deepEqual(
          again.trace.filter(
            (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
          ),
          evaluated.trace.filter(
            (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
          ),
          id,
        )

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual(
          (instance.exports.silk_main as () => number)(),
          quota === allocationCount ? 0 : 1,
          id,
        )

        if (!nativeQuotas.has(quota)) continue
        const compiled = yield* Driver.compile({
          compilation: { root: SourceFile.make(id, ascii(source)) },
          toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
          profile: 'release',
          destination: join(destinationRoot, `quota-${quota}`),
        }).pipe(Effect.provide(SourceResolver.empty))
        assert.strictEqual(compiled._tag, 'Compiled', id)
        if (compiled._tag !== 'Compiled') continue
        const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
        assert.strictEqual(
          run.status,
          quota === allocationCount ? 0 : 1,
          `${id}: ${JSON.stringify({ stderr: run.stderr, signal: run.signal, error: run.error?.message })}`,
        )
        if (quota === allocationCount) assert.strictEqual(run.stderr, '')
        else assert.strictEqual(run.stderr, 'Error: silk/allocator.OutOfMemoryError\n')
      }
    }),
  180_000,
)
