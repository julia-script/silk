import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

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
        let result: number
        if (opcode === 2) result = left + right
        else if (opcode === 3) result = left - right
        else result = Math.imul(left, right)
        stack.push(result)
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
    '  if value != 184 { let mismatch = 1 / 0 }',
    `  if value != ${expected.fingerprint} { let mismatch = 1 / 0 }`,
  )
  return Object.freeze({ source, expected })
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
